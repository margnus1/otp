%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------

%% Transform source code location information from separate instructions to a
%% field of call instructions.

%%--------------------------------------------------------------------

-module(hipe_rtl_propagate_line).

-export([propagate/1]).

-include("hipe_rtl.hrl").
-include("../flow/cfg.hrl"). % for the cfg() type only

-define(UNKNOWN_LOC, 0).

-type label()   :: non_neg_integer().
-type loc()     :: hipe_icode:line_loc().
-type bb_locs() :: #{label() => loc()}.
-type rtl_instr() :: tuple(). % for now

%%--------------------------------------------------------------------

-spec propagate(cfg()) -> cfg().
propagate(RtlCfg) ->
  propagate_bbs(hipe_rtl_cfg:preorder(RtlCfg), #{}, RtlCfg).

-spec propagate_bbs([label()], bb_locs(), cfg()) -> cfg().
propagate_bbs([], _BBLocs, RtlCfg) -> RtlCfg;
propagate_bbs([Lbl|Lbls], BBLocs0, RtlCfg0) ->
  BB0 = hipe_rtl_cfg:bb(RtlCfg0, Lbl),
  Code0 = hipe_bb:code(BB0),
  InLoc = maps:get(Lbl, BBLocs0, ?UNKNOWN_LOC),
  {Code, OutLoc} = propagate_insns(Code0, InLoc, []),
  BB = hipe_bb:code_update(BB0, Code),
  RtlCfg = hipe_rtl_cfg:bb_add(RtlCfg0, Lbl, BB),
  BBLocs = lists:foldl(fun(Succ, BBLocs1) ->
			   BBLocs1#{Succ => OutLoc}
		       end, BBLocs0, hipe_rtl_cfg:succ(RtlCfg, Lbl)),
  propagate_bbs(Lbls, BBLocs, RtlCfg).

-spec propagate_insns([rtl_instr()], loc(), [rtl_instr()])
		     -> {[rtl_instr()], loc()}.
propagate_insns([], Loc, Acc) -> {lists:reverse(Acc), Loc};
propagate_insns([I|Is], Loc0, Acc0) ->
  case I of
    #line{loc=Loc} -> propagate_insns(Is, Loc, Acc0);
    #call{} -> propagate_insns(Is, Loc0, [I#call{loc=Loc0}|Acc0]);
    _ -> propagate_insns(Is, Loc0, [I|Acc0])
  end.
