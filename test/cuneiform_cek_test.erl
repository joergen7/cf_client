%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @version 0.1.7
%% @copyright 2015-2019
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_cek_test ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "eunit/include/eunit.hrl" ).

%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_cek, [load/1, ev/1, unload/1] ).
-import( cuneiform_lang, [str/1, true/0, false/0, file/1, lam/2, null/1, app/2,
                          fut/2] ).
-import( cuneiform_lang, [t_bool/0, t_str/0] ).
-import( cuneiform_lang, [l_bash/0] ).

ev_deflect_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"str ev itself", fun str_ev_itself/0},
    {"true ev itself", fun true_ev_itself/0},
    {"false ev itself", fun false_ev_itself/0},
    {"file ev itself", fun file_ev_itself/0},
    {"fn ev closure", fun fn_ev_closure/0},
    {"null ev itself", fun null_ev_itself/0},
    {"fut ev itself", fun fut_ev_itself/0}
   ]}.

str_ev_itself() ->
  E1 = str( <<"blub">> ),
  E2 = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

true_ev_itself() ->
  E1 = true(),
  E2 = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

false_ev_itself() ->
  E1 = false(),
  E2 = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

file_ev_itself() ->
  E1 = file( <<"bla.txt">> ),
  E2 = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

fn_ev_closure() ->
  E1 = lam( [], {ntv, true()} ),
  {close, _, E2, _} = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

null_ev_itself() ->
  E1 = null( t_bool() ),
  E2 = unload( ev( load( E1 ) ) ),
  ?assertEqual( E1, E2 ).

fut_ev_itself() ->
  E1 = fut( t_str(), <<"123">> ),
  {_, E2, _, _, _} = ev( load( E1 ) ),
  ?assertEqual( E1, E2 ).

