/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2016. All Rights Reserved.
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


#ifndef HIPE_SPARC_H
#define HIPE_SPARC_H

ERTS_GLB_INLINE void hipe_flush_icache_word(void *address);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE void hipe_flush_icache_word(void *address)
{
    asm volatile("flush %0"
		 : /* no outputs */
		 : "r"(address)
		 : "memory");
}
#endif

extern void hipe_flush_icache_range(void *address, unsigned int nbytes);

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	2	/* low 2 bits are always zero */

ERTS_GLB_INLINE int hipe_word32_address_ok(void *address);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF
/* for hipe_bifs_{read,write}_{s,u}32 */
ERTS_GLB_INLINE int hipe_word32_address_ok(void *address)
{
    return ((UWord)address & 0x3) == 0;
}
#endif

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#define hipe_arch_name	am_ultrasparc

extern void hipe_sparc_inc_stack(void);

#endif /* HIPE_SPARC_H */
