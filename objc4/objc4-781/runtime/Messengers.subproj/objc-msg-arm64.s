/*
 * @APPLE_LICENSE_HEADER_START@
 * 
 * Copyright (c) 2011 Apple Inc.  All Rights Reserved.
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 */
/********************************************************************
 * 
 *  objc-msg-arm64.s - ARM64 code to support objc messaging
 *
 ********************************************************************/

#ifdef __arm64__

#include <arm/arch.h>
#include "isa.h"
#include "arm64-asm.h"
#include "objc-config.h"

.data

// _objc_restartableRanges is used by method dispatch
// caching code to figure out whether any threads are actively 
// in the cache for dispatching.  The labels surround the asm code
// that do cache lookups.  The tables are zero-terminated.

.macro RestartableEntry
#if __LP64__
	.quad	LLookupStart$0
#else
	.long	LLookupStart$0
	.long	0
#endif
	.short	LLookupEnd$0 - LLookupStart$0
	.short	LLookupRecover$0 - LLookupStart$0
	.long	0
.endmacro

	.align 4
	.private_extern _objc_restartableRanges
_objc_restartableRanges:
	RestartableEntry _cache_getImp
	RestartableEntry _objc_msgSend
	RestartableEntry _objc_msgSendSuper
	RestartableEntry _objc_msgSendSuper2
	RestartableEntry _objc_msgLookup
	RestartableEntry _objc_msgLookupSuper2
	.fill	16, 1, 0


/* objc_super parameter to sendSuper */
#define RECEIVER         0
#define CLASS            __SIZEOF_POINTER__

/* Selected field offsets in class structure */
#define SUPERCLASS       __SIZEOF_POINTER__
#define CACHE            (2 * __SIZEOF_POINTER__)

/* Selected field offsets in method structure */
#define METHOD_NAME      0
#define METHOD_TYPES     __SIZEOF_POINTER__
#define METHOD_IMP       (2 * __SIZEOF_POINTER__)

#define BUCKET_SIZE      (2 * __SIZEOF_POINTER__)


/********************************************************************
 * GetClassFromIsa_p16 src
 * src is a raw isa field. Sets p16 to the corresponding class pointer.
 * The raw isa might be an indexed isa to be decoded, or a
 * packed isa that needs to be masked.
 *
 * On exit:
 *   $0 is unchanged
 *   p16 is a class pointer
 *   x10 is clobbered
 ********************************************************************/

#if SUPPORT_INDEXED_ISA
	.align 3
	.globl _objc_indexed_classes
_objc_indexed_classes:
	.fill ISA_INDEX_COUNT, PTRSIZE, 0
#endif

.macro GetClassFromIsa_p16 /* src */

#if SUPPORT_INDEXED_ISA
	// Indexed isa
	mov	p16, $0			// optimistically set dst = src
	tbz	p16, #ISA_INDEX_IS_NPI_BIT, 1f	// done if not non-pointer isa  // 测试位不为 0 发生跳转
	// isa in p16 is indexed
	adrp	x10, _objc_indexed_classes@PAGE
	add	x10, x10, _objc_indexed_classes@PAGEOFF
	ubfx	p16, p16, #ISA_INDEX_SHIFT, #ISA_INDEX_BITS  // extract index // 位段提取指令 将 p16 中 ISA_INDEX_SHIFT 位起，偏移 ISA_INDEX_BITS 位提取到 p16 最低有效位
	ldr	p16, [x10, p16, UXTP #PTRSHIFT]	// load class from array
1:

#elif __LP64__
	// 64-bit packed isa
	and	p16, $0, #ISA_MASK          // $0 & ISA_MASK 存储 p16（#define ISA_MASK 0x0000000ffffffff8ULL）

#else
	// 32-bit raw isa
	mov	p16, $0

#endif

.endmacro


/********************************************************************
 * ENTRY functionName
 * STATIC_ENTRY functionName
 * END_ENTRY functionName
 ********************************************************************/

.macro ENTRY /* name */
	.text
	.align 5
	.globl    $0
$0:
.endmacro

.macro STATIC_ENTRY /*name*/
	.text
	.align 5
	.private_extern $0
$0:
.endmacro

.macro END_ENTRY /* name */
LExit$0:
.endmacro


/********************************************************************
 * UNWIND name, flags
 * Unwind info generation	
 ********************************************************************/
.macro UNWIND
	.section __LD,__compact_unwind,regular,debug
	PTR $0
	.set  LUnwind$0, LExit$0 - $0
	.long LUnwind$0
	.long $1
	PTR 0	 /* no personality */
	PTR 0  /* no LSDA */
	.text
.endmacro

#define NoFrame 0x02000000  // no frame, no SP adjustment
#define FrameWithNoSaves 0x04000000  // frame, no non-volatile saves


/********************************************************************
 *
 * CacheLookup NORMAL|GETIMP|LOOKUP <function>
 *
 * Locate the implementation for a selector in a class method cache.
 *
 * When this is used in a function that doesn't hold the runtime lock,
 * this represents the critical section that may access dead memory.
 * If the kernel causes one of these functions to go down the recovery
 * path, we pretend the lookup failed by jumping the JumpMiss branch.
 *
 * Takes:
 *	 x1 = selector
 *	 x16 = class to be searched
 *
 * Kills:
 * 	 x9,x10,x11,x12, x17
 *
 * On exit: (found) calls or returns IMP
 *                  with x16 = class, x17 = IMP
 *          (not found) jumps to LCacheMiss
 *
 ********************************************************************/

#define NORMAL 0
#define GETIMP 1
#define LOOKUP 2

// CacheHit: x17 = cached IMP, x12 = address of cached IMP, x1 = SEL, x16 = isa
.macro CacheHit
.if $0 == NORMAL
	TailCallCachedImp x17, x12, x1, x16	// authenticate and call imp
.elseif $0 == GETIMP
	mov	p0, p17
	cbz	p0, 9f			// don't ptrauth a nil imp
	AuthAndResignAsIMP x0, x12, x1, x16	// authenticate imp and re-sign as IMP
9:	ret				// return IMP
.elseif $0 == LOOKUP
	// No nil check for ptrauth: the caller would crash anyway when they
	// jump to a nil IMP. We don't care if that jump also fails ptrauth.
	AuthAndResignAsIMP x17, x12, x1, x16	// authenticate imp and re-sign as IMP
	ret				// return imp via x17
.else
.abort oops
.endif
.endmacro

.macro CheckMiss
	// miss if bucket->sel == 0
.if $0 == GETIMP
	cbz	p9, LGetImpMiss                 // p9 存放 bucket->sel
.elseif $0 == NORMAL
	cbz	p9, __objc_msgSend_uncached     // cbz 比较，如果结果为 0 就跳转(只能跳到后面的指令)
.elseif $0 == LOOKUP
	cbz	p9, __objc_msgLookup_uncached
.else
.abort oops
.endif
.endmacro

.macro JumpMiss
.if $0 == GETIMP
	b	LGetImpMiss
.elseif $0 == NORMAL
	b	__objc_msgSend_uncached
.elseif $0 == LOOKUP
	b	__objc_msgLookup_uncached
.else
.abort oops
.endif
.endmacro

.macro CacheLookup
	//
	// Restart protocol:
	//
	//   As soon as we're past the LLookupStart$1 label we may have loaded
	//   an invalid cache pointer or mask.
	//
	//   When task_restartable_ranges_synchronize() is called,
	//   (or when a signal hits us) before we're past LLookupEnd$1,
	//   then our PC will be reset to LLookupRecover$1 which forcefully
	//   jumps to the cache-miss codepath which have the following
	//   requirements:
	//
	//   GETIMP:
	//     The cache-miss is just returning NULL (setting x0 to 0)
	//
	//   NORMAL and LOOKUP:
	//   - x0 contains the receiver
	//   - x1 contains the selector
	//   - x16 contains the isa
	//   - other registers are set as per calling conventions
	//
LLookupStart$1:

	// p1 = SEL, p16 = isa
	ldr	p11, [x16, #CACHE]				// p11 = mask|buckets   // 从 x16 寄存器中偏移 CACHE 的位置取出 8 字节数据。CACHE 定义 => line 73

#if CACHE_MASK_STORAGE == CACHE_MASK_STORAGE_HIGH_16
	and	p10, p11, #0x0000ffffffffffff	// p10 = buckets        // 取出 buckets 地址
	and	p12, p1, p11, LSR #48		    // x12 = _cmd & mask
#elif CACHE_MASK_STORAGE == CACHE_MASK_STORAGE_LOW_4
	and	p10, p11, #~0xf			        // p10 = buckets
	and	p11, p11, #0xf			        // p11 = maskShift
	mov	p12, #0xffff
	lsr	p11, p12, p11				    // p11 = mask = 0xffff >> p11  // p12>>p11 写入 p11
	and	p12, p1, p11				    // x12 = _cmd & mask
#else
#error Unsupported cache mask storage for ARM64.
#endif


	add	p12, p10, p12, LSL #(1+PTRSHIFT)// PTRSHIFT = 3，表中每一项大小为 16 字节，左移 4 位，相当于乘以 16。
                                        // p12 = buckets + ((_cmd & mask) << (1+PTRSHIFT)) // p12(索引项地址) = p10(缓存表地址) + (p12<<(1+PTRSHIFT))(偏移量)

	ldp	p17, p9, [x12]		            // {imp, sel} = *bucket，p17 = bucket->imp p9 = bucket->sel
1:	cmp	p9, p1			                // if (bucket->sel != _cmd)
	b.ne	2f			                // scan more
	CacheHit $0			                // call or return imp
	
2:	// not hit: p12 = not-hit bucket
	CheckMiss $0			            // miss if bucket->sel == 0
	cmp	p12, p10		                // wrap if bucket == buckets
	b.eq	3f
	ldp	p17, p9, [x12, #-BUCKET_SIZE]!	// {imp, sel} = *--bucket，移动到下一个
	b	1b			                    // loop，b 是 backward 的意思，既然有 backward 就有 forward，所有就有 f

3:	// wrap: p12 = first bucket, w11 = mask
#if CACHE_MASK_STORAGE == CACHE_MASK_STORAGE_HIGH_16
	add	p12, p12, p11, LSR #(48 - (1+PTRSHIFT))
                                        // p12 = buckets + (mask << 1+PTRSHIFT) // mask = p11>>48，p12 指向表尾
#elif CACHE_MASK_STORAGE == CACHE_MASK_STORAGE_LOW_4
	add	p12, p12, p11, LSL #(1+PTRSHIFT)
                                        // p12 = buckets + (mask << 1+PTRSHIFT)
#else
#error Unsupported cache mask storage for ARM64.
#endif

	// Clone scanning loop to miss instead of hang when cache is corrupt.
	// The slow path may detect any corruption and halt later.

	ldp	p17, p9, [x12]		            // {imp, sel} = *bucket
1:	cmp	p9, p1			                // if (bucket->sel != _cmd)
	b.ne	2f			                //     scan more
	CacheHit $0			                // call or return imp
	
2:	// not hit: p12 = not-hit bucket
	CheckMiss $0			            // miss if bucket->sel == 0
	cmp	p12, p10		                // wrap if bucket == buckets
	b.eq	3f
	ldp	p17, p9, [x12, #-BUCKET_SIZE]!	// {imp, sel} = *--bucket
	b	1b			                    // loop

LLookupEnd$1:
LLookupRecover$1:
3:	// double wrap
	JumpMiss $0

.endmacro


/********************************************************************
 *
 * id objc_msgSend(id self, SEL _cmd, ...);
 * IMP objc_msgLookup(id self, SEL _cmd, ...);
 * 
 * objc_msgLookup ABI:
 * IMP returned in x17
 * x16 reserved for our use but not used
 *
 ********************************************************************/

#if SUPPORT_TAGGED_POINTERS
	.data
	.align 3
	.globl _objc_debug_taggedpointer_classes
_objc_debug_taggedpointer_classes:
	.fill 16, 8, 0
	.globl _objc_debug_taggedpointer_ext_classes
_objc_debug_taggedpointer_ext_classes:
	.fill 256, 8, 0
#endif

	ENTRY _objc_msgSend
	UNWIND _objc_msgSend, NoFrame

	cmp	p0, #0			                // nil check and tagged pointer check
#if SUPPORT_TAGGED_POINTERS
	b.le	LNilOrTagged		        //  (MSB tagged pointer looks negative)，小于等于（带符号）
#else
	b.eq	LReturnZero
#endif
	ldr	p13, [x0]		                // p13 = isa，表示从 x0 所表示的地址中取出 8 byte 数据，放到 x13 中。x0 中是 self 的地址，所以取出来的数据是 isa 的值
	GetClassFromIsa_p16 p13		        // p16 = class
LGetIsaDone:
	// calls imp or objc_msgSend_uncached
	CacheLookup NORMAL, _objc_msgSend

#if SUPPORT_TAGGED_POINTERS
LNilOrTagged:
	b.eq	LReturnZero		            // nil check

	// tagged，高 4 位为索引，到 Tagged Pointer Table 中查找
	adrp	x10, _objc_debug_taggedpointer_classes@PAGE     // 得到一个大小为 4 KB的页的基址，该页包含全局变量 objc_debug_taggedpointer_classes 的地址
	add	x10, x10, _objc_debug_taggedpointer_classes@PAGEOFF // 得到 objc_debug_taggedpointer_classes 在 4 KB页中的偏移量
	ubfx	x11, x0, #60, #4                                // 位段提取指令 将 x0 中 60 位起偏移 4 位提取到 x11 最低有效位（其实就是提取 index）
	ldr	x16, [x10, x11, LSL #3]         // p16 = class      // 左移三位是因为 _objc_debug_taggedpointer_classes 是 8 byte 为单位来偏移的
	adrp	x10, _OBJC_CLASS_$___NSUnrecognizedTaggedPointer@PAGE   // 当为 extend tagged pointer 时，得到的 cls 会是 __NSUnrecognizedTaggedPointer
	add	x10, x10, _OBJC_CLASS_$___NSUnrecognizedTaggedPointer@PAGEOFF
	cmp	x10, x16
	b.ne	LGetIsaDone

	// ext tagged，由于高 4 位都为 1，那么接下来的 8 位表示索引，到 Extend Tagged Pointer Table 表中查找
	adrp	x10, _objc_debug_taggedpointer_ext_classes@PAGE
	add	x10, x10, _objc_debug_taggedpointer_ext_classes@PAGEOFF
	ubfx	x11, x0, #52, #8
	ldr	x16, [x10, x11, LSL #3]         // p16 = class
	b	LGetIsaDone
// SUPPORT_TAGGED_POINTERS
#endif

LReturnZero:
	// x0 is already zero
	mov	x1, #0
	movi	d0, #0
	movi	d1, #0
	movi	d2, #0
	movi	d3, #0
	ret                                 // 近返回指令。执行的时候，处理器从栈中弹出一个字到IP中

	END_ENTRY _objc_msgSend


	ENTRY _objc_msgLookup
	UNWIND _objc_msgLookup, NoFrame
	cmp	p0, #0			                // nil check and tagged pointer check
#if SUPPORT_TAGGED_POINTERS
	b.le	LLookup_NilOrTagged	        //  (MSB tagged pointer looks negative)
#else
	b.eq	LLookup_Nil
#endif
	ldr	p13, [x0]		                // p13 = isa
	GetClassFromIsa_p16 p13		        // p16 = class
LLookup_GetIsaDone:
	// returns imp
	CacheLookup LOOKUP, _objc_msgLookup

#if SUPPORT_TAGGED_POINTERS
LLookup_NilOrTagged:
	b.eq	LLookup_Nil	// nil check

	// tagged
	adrp	x10, _objc_debug_taggedpointer_classes@PAGE
	add	x10, x10, _objc_debug_taggedpointer_classes@PAGEOFF
	ubfx	x11, x0, #60, #4
	ldr	x16, [x10, x11, LSL #3]
	adrp	x10, _OBJC_CLASS_$___NSUnrecognizedTaggedPointer@PAGE
	add	x10, x10, _OBJC_CLASS_$___NSUnrecognizedTaggedPointer@PAGEOFF
	cmp	x10, x16
	b.ne	LLookup_GetIsaDone

LLookup_ExtTag:	
	adrp	x10, _objc_debug_taggedpointer_ext_classes@PAGE
	add	x10, x10, _objc_debug_taggedpointer_ext_classes@PAGEOFF
	ubfx	x11, x0, #52, #8
	ldr	x16, [x10, x11, LSL #3]
	b	LLookup_GetIsaDone
// SUPPORT_TAGGED_POINTERS
#endif

LLookup_Nil:
	adrp	x17, __objc_msgNil@PAGE
	add	x17, x17, __objc_msgNil@PAGEOFF
	ret

	END_ENTRY _objc_msgLookup

	
	STATIC_ENTRY __objc_msgNil

	// x0 is already zero
	mov	x1, #0
	movi	d0, #0
	movi	d1, #0
	movi	d2, #0
	movi	d3, #0
	ret
	
	END_ENTRY __objc_msgNil


	ENTRY _objc_msgSendSuper
	UNWIND _objc_msgSendSuper, NoFrame

	ldp	p0, p16, [x0]		            // p0 = real receiver, p16 = class
	// calls imp or objc_msgSend_uncached
	CacheLookup NORMAL, _objc_msgSendSuper

	END_ENTRY _objc_msgSendSuper

	// no _objc_msgLookupSuper

	ENTRY _objc_msgSendSuper2
	UNWIND _objc_msgSendSuper2, NoFrame

	ldp	p0, p16, [x0]		            // p0 = real receiver, p16 = class
	ldr	p16, [x16, #SUPERCLASS]	        // p16 = class->superclass
	CacheLookup NORMAL, _objc_msgSendSuper2

	END_ENTRY _objc_msgSendSuper2

	
	ENTRY _objc_msgLookupSuper2
	UNWIND _objc_msgLookupSuper2, NoFrame

	ldp	p0, p16, [x0]		            // p0 = real receiver, p16 = class
	ldr	p16, [x16, #SUPERCLASS]	        // p16 = class->superclass
	CacheLookup LOOKUP, _objc_msgLookupSuper2

	END_ENTRY _objc_msgLookupSuper2


.macro MethodTableLookup
	
	// push frame                       // 将各个寄存器的值先存储到栈上，内部函数帧释放时便于复位寄存器的值
	SignLR
	stp	fp, lr, [sp, #-16]!             // ldp/stp 是 ldr/str 的衍生, 可以同时读/写两个寄存器, ldr/str只能读写一个
	mov	fp, sp

	// save parameter registers: x0..x8, q0..q7
	sub	sp, sp, #(10*8 + 8*16)          // 拉伸栈空间
	stp	q0, q1, [sp, #(0*16)]
	stp	q2, q3, [sp, #(2*16)]
	stp	q4, q5, [sp, #(4*16)]
	stp	q6, q7, [sp, #(6*16)]
	stp	x0, x1, [sp, #(8*16+0*8)]
	stp	x2, x3, [sp, #(8*16+2*8)]
	stp	x4, x5, [sp, #(8*16+4*8)]
	stp	x6, x7, [sp, #(8*16+6*8)]
	str	x8,     [sp, #(8*16+8*8)]

	// lookUpImpOrForward(obj, sel, cls, LOOKUP_INITIALIZE | LOOKUP_RESOLVER)
	// receiver and selector already in x0 and x1
	mov	x2, x16                         // 把 x16 的值复制到 x2 中（ x16 存储的就是 GetClassFromIsa_p16 代码找到的 Class ）
	mov	x3, #3
	bl	_lookUpImpOrForward             // (x0, x1, x2) -> (id obj, SEL sel, Class cls)

	// IMP in x0
	mov	x17, x0
	
	// restore registers and return
	ldp	q0, q1, [sp, #(0*16)]
	ldp	q2, q3, [sp, #(2*16)]
	ldp	q4, q5, [sp, #(4*16)]
	ldp	q6, q7, [sp, #(6*16)]
	ldp	x0, x1, [sp, #(8*16+0*8)]
	ldp	x2, x3, [sp, #(8*16+2*8)]
	ldp	x4, x5, [sp, #(8*16+4*8)]
	ldp	x6, x7, [sp, #(8*16+6*8)]
	ldr	x8,     [sp, #(8*16+8*8)]

	mov	sp, fp                          // 这里直接回到之前的栈地址，无需 add (10*8 + 8*16)
	ldp	fp, lr, [sp], #16
	AuthenticateLR

.endmacro

	STATIC_ENTRY __objc_msgSend_uncached
	UNWIND __objc_msgSend_uncached, FrameWithNoSaves

	// THIS IS NOT A CALLABLE C FUNCTION
	// Out-of-band p16 is the class to search
	
	MethodTableLookup                   // 开始方法查找过程
	TailCallFunctionPointer x17

	END_ENTRY __objc_msgSend_uncached


	STATIC_ENTRY __objc_msgLookup_uncached
	UNWIND __objc_msgLookup_uncached, FrameWithNoSaves

	// THIS IS NOT A CALLABLE C FUNCTION
	// Out-of-band p16 is the class to search
	
	MethodTableLookup
	ret

	END_ENTRY __objc_msgLookup_uncached


	STATIC_ENTRY _cache_getImp

	GetClassFromIsa_p16 p0
	CacheLookup GETIMP, _cache_getImp

LGetImpMiss:
	mov	p0, #0
	ret

	END_ENTRY _cache_getImp


/********************************************************************
*
* id _objc_msgForward(id self, SEL _cmd,...);
*
* _objc_msgForward is the externally-callable
*   function returned by things like method_getImplementation().
* _objc_msgForward_impcache is the function pointer actually stored in
*   method caches.
*
********************************************************************/

	STATIC_ENTRY __objc_msgForward_impcache

	// No stret specialization.
	b	__objc_msgForward

	END_ENTRY __objc_msgForward_impcache

	
	ENTRY __objc_msgForward

	adrp	x17, __objc_forward_handler@PAGE
	ldr	p17, [x17, __objc_forward_handler@PAGEOFF]
	TailCallFunctionPointer x17
	
	END_ENTRY __objc_msgForward
	
	
	ENTRY _objc_msgSend_noarg
	b	_objc_msgSend
	END_ENTRY _objc_msgSend_noarg

	ENTRY _objc_msgSend_debug
	b	_objc_msgSend
	END_ENTRY _objc_msgSend_debug

	ENTRY _objc_msgSendSuper2_debug
	b	_objc_msgSendSuper2
	END_ENTRY _objc_msgSendSuper2_debug

	
	ENTRY _method_invoke
	// x1 is method triplet instead of SEL
	add	p16, p1, #METHOD_IMP
	ldr	p17, [x16]
	ldr	p1, [x1, #METHOD_NAME]
	TailCallMethodListImp x17, x16
	END_ENTRY _method_invoke

#endif


/*
 * LSL 逻辑左移：高位移出，低位补零
 * LSR 逻辑右移：低位移出，高位补零
 * ASL 算术左移：高位移出，低位补零
 * ASR 算术右移：低位移出，高位补符号位
 * ROR 循环右移：低位移出，高位补低位移出位
 *
 */

/*
 * ldp 是 Load Register Pair 缩写
 * ldr 是 Load Register 缩写
 * [] 为间接寻址
 * ldp 是 ldr 的衍生, 可以同时读两个寄存器, ldr 只能读一个
 *
 * ldr x0，[x1]               将存储器地址为 x1 的字数据读入寄存器 x0
 * ldr x0，[x1, x2]           将存储器地址为 x1+x2 的字数据读入寄存器 x0
 * ldr x0，[x1, #8]           将存储器地址为 x1+8 的字数据读入寄存器 x0
 * ldr x0，[x1, x2]!          将存储器地址为 x1+x2 的字数据读入寄存器 x0, 并将新地址 x1＋x2 写入 x1
 * ldr x0，[x1, #8]!          将存储器地址为 x1+8 的字数据读入寄存器 x0, 并将新地址 x1＋8 写入 x1
 * ldr x0，[x1], x2           将存储器地址为 x1 的字数据读入寄存器 x0, 并将新地址 x1＋x2 写入 x1
 * ldr x0，[x1, x2, LSL #2]!  将存储器地址为 x1＋x2×4 的字数据读入寄存器 x0, 并将新地址 x1＋x2×4 写入 x1
 * ldr x0，[x1], x2, LSL #2   将存储器地址为 x1 的字数据读入寄存器 x0, 并将新地址 x1＋x2×4 写入 x1
 *
 */

/*
 * .align $
 *
 * 我们知道 arm 是 32 位处理器，如果在ARM指令状态下，所有指令的执行都是按照 4 的倍数进行执行的，而到 reset
 * 这个地址处时，发现地址为 30008026 不是 4 的倍数，于是就自动归到 30008024 处执行，此时便会出错
 *
 * 在指令出现非对齐情况下，可以在下面插入 .align，加上 .align 汇编语句后，指令就对齐
 *
 */
