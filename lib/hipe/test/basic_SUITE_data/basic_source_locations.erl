%%% -*- erlang-indent-level: 2 -*-
%%%---------------------------------------------------------------------
%%% Author: Magnus Lång
%%%
%%% Contains tests cases that source code locations are provided and are correct
%%% in various exceptions.
%%% ---------------------------------------------------------------------

-module(basic_source_locations).

-export([test/0, to_llvm/0]).

-define(FILENAME, "basic_source_locations.erl").
-define(INC_FILENAME, "basic_source_locations.inc").
-define(UNICODE_FILENAME, "./✓/file.erl").
-define(LONG_FILENAME, "/directory/path/deeper/than/255/characters/which/is/the"
	"/maximum/number/of/characters/allowed/in/an/atom/at/the/current/time"
	"/lorem/ipsum/dolor/sit/amet/consectetur/adipiscing/elit/morbi/a/enim"
	"/arcu/nam/et/tempor/quam/morbi/posuere/dolor"
	"/basic_source_locations.inc").

-define(ERR_PAT_F(Err, F, File, Line),
	{'EXIT', {Err, [{?MODULE, F, _, [{file, File}, {line, Line}]}|_]}}).
-define(TEST_F(Err, File, Line, Fun, Args),
	?ERR_PAT_F(Err, Fun, File, Line) = (catch Fun Args)).
-define(TEST(Err, Line, Fun, Args), ?TEST_F(Err, ?FILENAME, Line, Fun, Args)).

to_llvm() -> false. % XXX: the LLVM backend does not do line numbers yet

test() ->
  ok = test_basic(),
  ok = test_arith(),
  ok = test_bit_syntax(),
  ok = test_maps(),
  ok.

%%% ---------------------------------------------------------------------

test_basic() ->
  ?TEST(function_clause, 1001, basic_clauses, (0)),
  ?TEST({case_clause, bad}, 1005, basic_case, (bad)),
  ?TEST({case_clause, half}, 1008, basic_case, (half)),
  ?TEST_F(function_clause, ?INC_FILENAME, 1, basic_include, (bad)),
  %% XXX: Unicode does not even survive in BEAM; ignore the filename for now
  ?TEST_F(function_clause, _, 1, basic_unicode, (bad)),
  {'EXIT', {system_limit, _}} = (catch list_to_atom(?LONG_FILENAME)),
  Truncated = string:substr(?LONG_FILENAME, 1, 255),
  ?TEST_F(function_clause, Truncated, 1, basic_long, (bad)),
  ok.

test_arith() ->
  ?TEST(badarith, 1102, 'wrap_+', (a, hej)),
  ?TEST(badarith, 1105, 'wrap_/', (1, 0)),
  ?TEST(system_limit, 1108, 'wrap_bsl', (1, 100000000)),
  ok.

test_bit_syntax() ->
  ?TEST(badarg, 1202, bin_append_42, (not_a_binary)),
  ?TEST(badarg, 1202, bin_append_42, (<<1:3>>)),
  ?TEST(system_limit, 1205, bin_of_size, ((1 bsl 67))),
  ok.

test_maps() ->
  ?TEST({badmap,not_a_map}, 1302, map_update_q, (not_a_map)),
  ?TEST({badkey,q}, 1302, map_update_q, (#{p => 64})),
  ok.

%%% ---------------------------------------------------------------------
%% test_basic

-file(?FILENAME, 1000).
basic_clauses(q) -> ok; % line 1001
basic_clauses(Num) when Num > 42 -> ok.

basic_case(Val) ->
  case Val of % line 1005
    good -> ok; half -> ok
  end,
  case Val of % line 1008
    good -> ok
  end.

-file(?INC_FILENAME, 0).
basic_include(acceptable_argument) -> ok.

-file(?UNICODE_FILENAME, 0).
basic_unicode(acceptable_argument) -> ok.

-file(?LONG_FILENAME, 0).
basic_long(acceptable_argument) -> ok.

%%% ---------------------------------------------------------------------
%% test_arith

-file(?FILENAME, 1100).
'wrap_+'(A, B) ->
  A + B. % line 1102

'wrap_/'(A, B) ->
  A / B. % line 1105

wrap_bsl(A, B) ->
  A bsl B. % line 1108

%%% ---------------------------------------------------------------------
%% test_bit_syntax

-file(?FILENAME, 1200).
bin_append_42(Bin) ->
  <<Bin/binary, 42>>. % line 1202

bin_of_size(Size) ->
  <<0:Size>>. % line 1205

%%% ---------------------------------------------------------------------
%% test_maps

-file(?FILENAME, 1300).
map_update_q(Map) ->
  Map#{q := 42}. % line 1302
