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

-import( cuneiform_cek, [load/1, ev/1, unload/1, step/1] ).
-import( cuneiform_lang, [str/1, true/0, false/0, file/1, lam/2, null/1, app/2,
                          fut/2, var/1, fix/1, alet/2, close/2] ).
-import( cuneiform_lang, [t_bool/0, t_str/0, t_fn/2] ).
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



% def id( x : Str ) -> Str { x }
% id;
parrot_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"parrot step 1", fun parrot_step_1/0},
    {"parrot step 2", fun parrot_step_2/0},
    {"parrot step 3", fun parrot_step_3/0},
    {"parrot step 4", fun parrot_step_4/0},
    {"parrot step 5", fun parrot_step_5/0},
    {"parrot step 6", fun parrot_step_6/0},
    {"parrot step 7", fun parrot_step_7/0}
   ]
  }.

parrot_step_1() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),
  EFix = fix( ELam ),

  E0 = alet( [{id, TLam, EFix}], var( id ) ),

  P0 = load( E0 ),
  P1 = step( P0 ),
  {_, E1, Env1, K1, Prop1} = P1,

  EZ = lam( [{id, TLam}], {ntv, var( id )} ),
  EnvZ = #{},
  KZ = {app_fn, na, [{id, EFix}], #{}, mt},
  PropZ = unknown,

  ?assertEqual( EZ, E1 ),
  ?assertEqual( EnvZ, Env1 ),
  ?assertEqual( KZ, K1 ),
  ?assertEqual( PropZ, Prop1 ).

parrot_step_2() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),
  EFix = fix( ELam ),

  E1 = lam( [{id, TLam}], {ntv, var( id )} ),
  Env1 = #{},
  K1 = {app_fn, na, [{id, EFix}], #{}, mt},
  Prop1 = unknown,
  Comm1 = {[], sets:new(), #{}},

  P1 = {Comm1, E1, Env1, K1, Prop1},
  P2 = step( P1 ),
  {_, E2, Env2, K2, Prop2} = P2,

  EZ = close( E1, Env1 ),
  EnvZ = Env1,
  KZ = K1,
  PropZ = value,

  ?assertEqual( EZ, E2 ),
  ?assertEqual( EnvZ, Env2 ),
  ?assertEqual( KZ, K2 ),
  ?assertEqual( PropZ, Prop2 ).

parrot_step_3() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),
  EFix = fix( ELam ),
  E1 = lam( [{id, TLam}], {ntv, var( id )} ),

  E2 = close( E1, #{} ),
  Env2 = #{},
  K2 = {app_fn, na, [{id, EFix}], #{}, mt},
  Prop2 = value,
  Comm2 = {[], sets:new(), #{}},

  P2 = {Comm2, E2, Env2, K2, Prop2},
  P3 = step( P2 ),
  {_, E3, Env3, K3, Prop3} = P3,

  EZ = var( id ),
  EnvZ = #{ id => {EFix, #{}} },
  KZ = mt,
  PropZ = unknown,
  
  ?assertEqual( EZ, E3 ),
  ?assertEqual( EnvZ, Env3 ),
  ?assertEqual( KZ, K3 ),
  ?assertEqual( PropZ, Prop3 ).

parrot_step_4() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),
  EFix = fix( ELam ),

  E3 = var( id ),
  Env3 = #{ id => {EFix, #{}}},
  K3 = mt,
  Prop3 = unknown,
  Comm3 = {[], sets:new(), #{}},

  P3 = {Comm3, E3, Env3, K3, Prop3},
  P4 = step( P3 ),
  {_, E4, Env4, K4, Prop4} = P4,

  EZ = EFix,
  EnvZ = #{},
  KZ = K3,
  PropZ = unknown,

  ?assertEqual( EZ, E4 ),
  ?assertEqual( EnvZ, Env4 ),
  ?assertEqual( KZ, K4 ),
  ?assertEqual( PropZ, Prop4 ).

parrot_step_5() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),
  EFix = fix( ELam ),
  
  E4 = EFix,
  Env4 = #{},
  K4 = mt,
  Prop4 = unknown,
  Comm4 = {[], sets:new(), #{}},

  P4 = {Comm4, E4, Env4, K4, Prop4},
  P5 = step( P4 ),
  {_, E5, Env5, K5, Prop5} = P5,

  EZ = ELam,
  EnvZ = Env4,
  KZ = {fix_op, na, K4},
  PropZ = unknown,

  ?assertEqual( EZ, E5 ),
  ?assertEqual( EnvZ, Env5 ),
  ?assertEqual( KZ, K5 ),
  ?assertEqual( PropZ, Prop5 ).


parrot_step_6() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),

  E5 = ELam,
  Env5 = #{},
  K5 = {fix_op, na, mt},
  Prop5 = unknown,
  Comm5 = {[], sets:new(), #{}},

  P5 = {Comm5, E5, Env5, K5, Prop5},
  P6 = step( P5 ),
  {_, E6, Env6, K6, Prop6} = P6,

  EZ = close( E5, Env5 ),
  EnvZ = Env5,
  KZ = K5,
  PropZ = value,

  ?assertEqual( EZ, E6 ),
  ?assertEqual( EnvZ, Env6 ),
  ?assertEqual( KZ, K6 ),
  ?assertEqual( PropZ, Prop6 ).

parrot_step_7() ->
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ELam =
    lam( [{id, TLam}, {x, t_str()}],
         {ntv, var( x )} ),

  E6 = close( ELam, #{} ),
  Env6 = #{},
  K6 = {fix_op, na, mt},
  Prop6 = value,
  Comm6 = {[], sets:new(), #{}},

  P6 = {Comm6, E6, Env6, K6, Prop6},
  P7 = step( P6 ),
  {_, E7, Env7, K7, Prop7} = P7,

  EBody = app( lam( [{id, TLam}], {ntv, var( x )} ), [{id, fix( ELam )}]),
  EZ = close( lam( [{x, t_str()}], {ntv, EBody} ), #{} ),
  EnvZ = Env6,
  KZ = mt,
  PropZ = value,

  ?assertEqual( EZ, E7 ),
  ?assertEqual( EnvZ, Env7 ),
  ?assertEqual( KZ, K7 ),
  ?assertEqual( PropZ, Prop7 ).
