/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
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


#ifndef HIPE_WIN_COMPAT_H
#define HIPE_WIN_COMPAT_H

#ifdef __WIN32__
#  if defined(_M_X64)
#    define __x86_64__ 1
#  elif defined(_M_X86)
#    define __i386__ 1
#  else
#    error "Unsupported CPU architecture for Windows"
#  endif
#endif

#endif /* HIPE_WIN_COMPAT_H */
