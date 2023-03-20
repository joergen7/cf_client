%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2013 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2013
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_shell_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_shell, [brace_level/2] ).

brace_level_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"then token increases brace level",
     fun then_token_increases_brace_level/0},
    {"do token increases brace level",
     fun do_token_increases_brace_level/0},
    {"halt token decreases brace level",
     fun halt_token_decreases_brace_level/0}
   ]
  }.


then_token_increases_brace_level() ->
  TokenLst = [{then, 1, "then"}],
  ?assertEqual( 1, brace_level( TokenLst, 0 ) ).

do_token_increases_brace_level() ->
  TokenLst = [{do, 1, "do"}],
  ?assertEqual( 1, brace_level( TokenLst, 0 ) ).

halt_token_decreases_brace_level() ->
  TokenLst = [{halt, 1, "end"}],
  ?assertEqual( 0, brace_level( TokenLst, 1 ) ).
