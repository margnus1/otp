/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#define ASM
#include "hipe_amd64_asm.h"
#include "hipe_literals.h"
#include "hipe_mode_switch.h"

/*
 * Note: the mode-switch entry points in hipe_amd64_glue.S have
 * the same names as in hipe_x86_glue.S. This is intentional,
 * as it allows using hipe_x86_glue.h with AMD64.
 */

/*
 * Set up frame on C stack,
 * save C callee-save registers,
 * retrieve the process pointer from the parameters from C,
 * SWITCH_C_TO_ERLANG.
 *
 * The end of the frame must be 16-byte aligned, otherwise
 * calls to C may break. %rsp+8 is 16-byte aligned on entry,
 * and six registers are to be saved, so a seventh word is
 * added to make the resulting %rsp 16-byte aligned.
 */

/*
 * On Windows, the stack frame is 8 words larger
 * Two words on the high end to store the extra callee saves %rsi & %rdi.
 * Six words on the low end to make room for a call to C with 6 arguments; two
 * on stack and 4 in registers but with shadow space on stack.
 */

#ifdef __WIN32__
#  define PUSH_EXTRA_WIN32_CALLEE_SAVES	\
	pushq	%rsi;			\
	pushq	%rdi
#  define POP_EXTRA_WIN32_CALLEE_SAVES	\
	popq	%rdi;			\
	popq	%rsi
#else
#  define PUSH_EXTRA_WIN32_CALLEE_SAVES
#  define POP_EXTRA_WIN32_CALLEE_SAVES
#endif

#define ENTER_FROM_C						\
	/* save C callee-save registers on the C stack */	\
	PUSH_EXTRA_WIN32_CALLEE_SAVES;				\
	pushq	%r15;						\
	pushq	%r14;						\
	pushq	%r13;						\
	pushq	%r12;						\
	pushq	%rbx;						\
	pushq	%rbp;						\
	/* 16-byte align stack; allocate shadow space */	\
	subq	$(8 + C_SHDW_SPC + C_ARGS45_SPC), %rsp;		\
	/* get the process pointer */				\
	movq	C_ARG0, P;					\
	/* switch to native stack */				\
	SWITCH_C_TO_ERLANG

	TEXT

/*
 * int x86_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.align	4
	GLOBAL(CSYM(x86_call_to_native))
	GLOBAL(ASYM(nbif_return))
CSYM(x86_call_to_native):
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* call the target */
	NSP_CALL(*P_NCALLEE(P))
/*
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
ASYM(nbif_return):
	movq	%rax, P_ARG0(P)			# save retval
	movl	$HIPE_MODE_SWITCH_RES_RETURN, %eax
/* FALLTHROUGH to .flush_exit
 *
 * Return to the calling C function with result token in %eax.
 *
 * .nosave_exit saves no state
 * .flush_exit saves cached P state
 * .suspend_exit also saves RA
 */
.suspend_exit:
	/* save RA, no-op on x86 */
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK
	SET_GC_SAFE
	/* restore C callee-save registers, drop frame, return */
	addq	$(8 + C_SHDW_SPC + C_ARGS45_SPC), %rsp
	popq	%rbp	# kills P
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15	# kills HP
	POP_EXTRA_WIN32_CALLEE_SAVES
	ret

/*
 * Native code calls emulated code via a linker-generated
 * stub (hipe_x86_loader.erl) which should look as follows:
 *
 * stub for f/N:
 *	movq	$<f's export entry address>, P_CALLEE_EXP(P)
 *	movb	$<N>, P_ARITY(P)
 *	jmp	nbif_callemu
 *
 * XXX: Different stubs for different number of register parameters?
 */
	.align	4
	GLOBAL(ASYM(nbif_callemu))
ASYM(nbif_callemu):
	STORE_ARG_REGS
	movl	$HIPE_MODE_SWITCH_RES_CALL_EXPORTED, %eax
	jmp	.suspend_exit

/*
 * nbif_apply
 */
	.align	4
	GLOBAL(ASYM(nbif_apply))
ASYM(nbif_apply):
	STORE_ARG_REGS
	movl	$HIPE_MODE_SWITCH_RES_APPLY, %eax
	jmp	.suspend_exit

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first few passed in registers are pushed onto the stack in
 * left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */
#if NR_ARG_REGS >= 6
	.align	4
	GLOBAL(ASYM(nbif_ccallemu6))
ASYM(nbif_ccallemu6):
	movq	ARG5, P_ARG5(P)
#if NR_ARG_REGS > 6
	movq	ARG6, ARG5
#else
	movq	8(NSP), ARG5
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	.align	4
	GLOBAL(ASYM(nbif_ccallemu5))
ASYM(nbif_ccallemu5):
	movq	ARG4, P_ARG4(P)
#if NR_ARG_REGS > 5
	movq	ARG5, ARG4
#else
	movq	8(NSP), ARG4
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	.align	4
	GLOBAL(ASYM(nbif_ccallemu4))
ASYM(nbif_ccallemu4):
	movq	ARG3, P_ARG3(P)
#if NR_ARG_REGS > 4
	movq	ARG4, ARG3
#else
	movq	8(NSP), ARG3
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	.align	4
	GLOBAL(ASYM(nbif_ccallemu3))
ASYM(nbif_ccallemu3):
	movq	ARG2, P_ARG2(P)
#if NR_ARG_REGS > 3
	movq	ARG3, ARG2
#else
	movq	8(NSP), ARG2
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	.align	4
	GLOBAL(ASYM(nbif_ccallemu2))
ASYM(nbif_ccallemu2):
	movq	ARG1, P_ARG1(P)
#if NR_ARG_REGS > 2
	movq	ARG2, ARG1
#else
	movq	8(NSP), ARG1
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	.align	4
	GLOBAL(ASYM(nbif_ccallemu1))
ASYM(nbif_ccallemu1):
	movq	ARG0, P_ARG0(P)
#if NR_ARG_REGS > 1
	movq	ARG1, ARG0
#else
	movq	8(NSP), ARG0
#endif
	/*FALLTHROUGH*/
#endif

	.align	4
	GLOBAL(ASYM(nbif_ccallemu0))
ASYM(nbif_ccallemu0):
#if NR_ARG_REGS == 0
	movq	8(NSP), _ARG0
#endif
	movq	_ARG0, P_CLOSURE(P)
	movl	$HIPE_MODE_SWITCH_RES_CALL_CLOSURE, %eax
	jmp	.suspend_exit

/*
 * This is where native code suspends.
 */
	.align	4
	GLOBAL(ASYM(nbif_suspend_0))
ASYM(nbif_suspend_0):
	movl	$HIPE_MODE_SWITCH_RES_SUSPEND, %eax
	jmp	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	.align	4
	GLOBAL(ASYM(nbif_suspend_msg))
ASYM(nbif_suspend_msg):
	movl	$HIPE_MODE_SWITCH_RES_WAIT, %eax
	jmp	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if (!(p->flags & F_TIMO)) { suspend }
 *	else { return 0; }
 */
	.align	4
	GLOBAL(ASYM(nbif_suspend_msg_timeout))
ASYM(nbif_suspend_msg_timeout):
	movq	P_FLAGS(P), %rax
	/* this relies on F_TIMO (1<<2) fitting in a byte */
	testb	$F_TIMO, %al			# F_TIMO set?
	jz	.no_timeout			# if not set, suspend
	/* timeout has occurred */
	xorl	%eax, %eax			# return 0 to signal timeout
	NSP_RET0
.no_timeout:
	movl	$HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT, %eax
	jmp	.suspend_exit

/*
 * int x86_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.align	4
	GLOBAL(CSYM(x86_return_to_native))
CSYM(x86_return_to_native):
	ENTER_FROM_C
	/* get return value */
	movq	P_ARG0(P), %rax
	/*
	 * Return using the stacked return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	NSP_RET0

/*
 * int x86_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	.align	4
	GLOBAL(CSYM(x86_tailcall_to_native))
CSYM(x86_tailcall_to_native):
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* jump to the target label */
	jmp	*P_NCALLEE(P)

/*
 * int x86_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.align	4
	GLOBAL(CSYM(x86_throw_to_native))
CSYM(x86_throw_to_native):
	ENTER_FROM_C
	/* invoke the handler */
	jmp	*P_NCALLEE(P)		# set by hipe_find_handler()

/*
 * This is the default exception handler for native code.
 */
	.align	4
	GLOBAL(ASYM(nbif_fail))
ASYM(nbif_fail):
	movl	$HIPE_MODE_SWITCH_RES_THROW, %eax
	jmp	.flush_exit
	
	GLOBAL(nbif_0_gc_after_bif)
	GLOBAL(nbif_1_gc_after_bif)
	GLOBAL(nbif_2_gc_after_bif)
	GLOBAL(nbif_3_gc_after_bif)
	GLOBAL(nbif_4_gc_after_bif)
	.align	4
nbif_0_gc_after_bif:
	xorl	%edx, %edx
	jmp	.gc_after_bif
	.align	4
nbif_1_gc_after_bif:
	movl	$1, %edx
	jmp	.gc_after_bif
	.align	4
nbif_2_gc_after_bif:
	movl	$2, %edx
	jmp	.gc_after_bif
	.align	4
nbif_3_gc_after_bif:
	movl	$3, %edx
	jmp	.gc_after_bif
	.align	4
nbif_4_gc_after_bif:
	movl	$4, %edx
	/*FALLTHROUGH*/
	.align	4
.gc_after_bif:
	movl	%edx, P_NARITY(P)	# Note: narity is a 32-bit field
	movq	P, C_ARG0
	movq	%rax, C_ARG1
	xorl	C_ARG2l, C_ARG2l	# Pass NULL in regs
	xorl	C_ARG3l, C_ARG3l	# Pass 0 in arity
	popq	TEMP_RV			# RA; align stack, free shadow space
	call	CSYM(erts_gc_after_bif_call)
	movl	$0, P_NARITY(P)		# Note: narity is a 32-bit field
	jmpq    *TEMP_RV

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition.
 * The stack/heap registers were just read from P.
 */
	GLOBAL(nbif_0_simple_exception)
	GLOBAL(nbif_1_simple_exception)
	GLOBAL(nbif_2_simple_exception)
	GLOBAL(nbif_3_simple_exception)
	GLOBAL(nbif_4_simple_exception)
	.align	4
nbif_0_simple_exception:
	xorl	%eax, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_1_simple_exception:
	movl	$1, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_2_simple_exception:
	movl	$2, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_3_simple_exception:
	movl	$3, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_4_simple_exception:
	movl	$4, %eax
	/*FALLTHROUGH*/
	.align	4
.nbif_simple_exception:
	cmpq	$FREASON_TRAP, P_FREASON(P)
	je	.handle_trap
	/*
	 * Find and invoke catch handler (it must exist).
	 * The stack/heap registers were just read from P.
	 * - %eax should contain the current call's arity
	 */
	movl	%eax, P_NARITY(P)	# Note: narity is a 32-bit field
	/* find and prepare to invoke the handler */
	SWITCH_ERLANG_TO_C_QUICK	# The cached state is clean and need not be saved.
	SET_GC_SAFE
	movq	P, C_ARG0
	call	CSYM(hipe_handle_exception)	# Note: hipe_handle_exception() conses
	SWITCH_C_TO_ERLANG		# %rsp updated by hipe_find_handler()
	/* now invoke the handler */
	jmp	*P_NCALLEE(P)		# set by hipe_find_handler()

	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF's arity is in %rax
	 * - the native heap/stack/reds registers are saved in P
	 */
.handle_trap:
	movl	%eax, P_NARITY(P)	# Note: narity is a 32-bit field
	movl	$HIPE_MODE_SWITCH_RES_TRAP, %eax
	jmp	.nosave_exit

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	GLOBAL(ASYM(nbif_stack_trap_ra))
	.align	4
ASYM(nbif_stack_trap_ra):			# a return address, not a function
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	movq	%rax, TEMP_RV		# save retval
	SWITCH_ERLANG_TO_C_QUICK
	movq	P, C_ARG0
	call	CSYM(hipe_handle_stack_trap)	# must not cons; preserves TEMP_RV
	movq	%rax, %rdx		# original RA
	SWITCH_C_TO_ERLANG_QUICK
	movq	TEMP_RV, %rax		# restore retval
	jmp	*%rdx			# resume at original RA

/*
 * nbif_inc_stack_0
 */
	GLOBAL(ASYM(nbif_inc_stack_0))
	.align	4
ASYM(nbif_inc_stack_0):
	SWITCH_ERLANG_TO_C_QUICK
	STORE_ARG_REGS
	movq	P, C_ARG0
	# hipe_inc_nstack reads and writes NSP and NSP_LIMIT,
	# but does not access HP or FCALLS (or the non-amd64 NRA).
	call	CSYM(hipe_inc_nstack)
	LOAD_ARG_REGS
	SWITCH_C_TO_ERLANG_QUICK
	NSP_RET0

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
