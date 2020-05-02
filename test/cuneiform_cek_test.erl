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
%% @copyright 2013-2020
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

-import( cuneiform_cek, [load/1, ev/1, unload/1, step/1, is_finished/1] ).
-import( cuneiform_lang, [str/1, true/0, false/0, file/1, lam/2, null/1, app/2,
                          fut/2, var/1, fix/1, alet/2, close/2, rcd/1, proj/2,
                          err/2, neg/1] ).
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

% ( <x = "a">|x );
proj_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"proj step 1", fun proj_step_1/0},
    {"proj step 2", fun proj_step_2/0},
    {"proj step 3", fun proj_step_3/0},
    {"proj step 4", fun proj_step_4/0},
    {"proj step 5", fun proj_step_5/0}
   ]
  }.


proj_step_1() ->
  EStr = str( <<"a">> ),
  ERcd = rcd( [{x, EStr}] ),

  Comm0 = {[], sets:new(), #{}},
  E0 = proj( x, ERcd ),
  P0 = {Comm0, E0, #{}, mt, unknown},

  P1 = step( P0 ),
  {Comm1, E1, Env1, K1, Prop1} = P1,

  ?assertEqual( Comm0, Comm1 ),
  ?assertEqual( ERcd, E1 ),
  ?assertEqual( #{}, Env1 ),
  ?assertEqual( {proj_op, na, x, mt}, K1 ),
  ?assertEqual( unknown, Prop1 ).

proj_step_2() ->
  EStr = str( <<"a">> ),

  Comm1 = {[], sets:new(), #{}},
  E1 = rcd( [{x, EStr}] ),
  K1 = {proj_op, na, x, mt},
  P1 = {Comm1, E1, #{}, K1, unknown},

  P2 = step( P1 ),
  {Comm2, E2, Env2, K2, Prop2} = P2,

  ?assertEqual( Comm1, Comm2 ),
  ?assertEqual( EStr, E2 ),
  ?assertEqual( #{}, Env2 ),
  ?assertEqual( {rcd_field, na, [], [], x, [], #{}, {proj_op, na, x, mt}}, K2 ),
  ?assertEqual( unknown, Prop2 ).

proj_step_3() ->
  Comm2 = {[], sets:new(), #{}},
  E2 = str( <<"a">> ),
  K2 = {rcd_field, na, [], [], x, [], #{}, {proj_op, na, x, mt}},
  P2 = {Comm2, E2, #{}, K2, unknown},

  P3 = step( P2 ),
  {Comm3, E3, Env3, K3, Prop3} = P3,

  ?assertEqual( Comm2, Comm3 ),
  ?assertEqual( E2, E3 ),
  ?assertEqual( #{}, Env3 ),
  ?assertEqual( K2, K3 ),
  ?assertEqual( value, Prop3 ).

proj_step_4() ->
  Comm3 = {[], sets:new(), #{}},
  E3 = str( <<"a">> ),
  K3 = {rcd_field, na, [], [], x, [], #{}, {proj_op, na, x, mt}},
  P3 = {Comm3, E3, #{}, K3, value},

  P4 = step( P3 ),
  {Comm4, E4, Env4, K4, Prop4} = P4,

  ?assertEqual( Comm3, Comm4 ),
  ?assertEqual( rcd( [{x, E3}] ), E4 ),
  ?assertEqual( {proj_op, na, x, mt}, K4 ),
  ?assertEqual( #{}, Env4 ),
  ?assertEqual( value, Prop4 ).

proj_step_5() ->
  EStr = str( <<"a">> ),

  Comm4 = {[], sets:new(), #{}},
  E4 = rcd( [{x, EStr}] ),
  K4 = {proj_op, na, x, mt},
  P4 = {Comm4, E4, #{}, K4, value},

  P5 = step( P4 ),
  {Comm5, E5, Env5, K5, Prop5} = P5,

  ?assertEqual( Comm4, Comm5 ),
  ?assertEqual( EStr, E5 ),
  ?assertEqual( #{}, Env5 ),
  ?assertEqual( mt, K5 ),
  ?assertEqual( value, Prop5 ).



% not error "blub" : Bool;
error_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"error step 1", fun error_step_1/0},
    {"error step 2", fun error_step_2/0}
   ]
  }.

error_step_1() ->
  EErr = err( t_bool(), {user, <<"blub">>} ),

  Comm0 = {[], sets:new(), #{}},
  E0 = neg( EErr ),
  P0 = {Comm0, E0, #{}, mt, unknown},

  P1 = step( P0 ),
  {Comm1, E1, Env1, K1, Prop1} = P1,

  ?assertEqual( Comm0, Comm1 ),
  ?assertEqual( EErr, E1 ),
  ?assertEqual( #{}, Env1 ),
  ?assertEqual( {neg_op, na, mt}, K1 ),
  ?assertEqual( unknown, Prop1 ).

error_step_2() ->
  Comm1 = {[], sets:new(), #{}},
  E1 = err( t_bool(), {user, <<"blub">>} ),
  K1 = {neg_op, na, mt},
  P1 = {Comm1, E1, #{}, K1, unknown},

  P2 = step( P1 ),
  {Comm2, E2, Env2, K2, Prop2} = P2,

  ?assertEqual( {[], sets:new(), #{}}, Comm2 ),
  ?assertEqual( E1, E2 ),
  ?assertEqual( #{}, Env2 ),
  ?assertEqual( mt, K2 ),
  ?assertEqual( unknown, Prop2 ).




is_finished_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"str is finished", fun str_is_finished/0},
    {"not true is not finished", fun not_true_is_not_finished/0},
    {"error is finished", fun error_is_finished/0}
   ]
  }.

str_is_finished() ->
  Comm = {[], sets:new(), #{}},
  E = str( <<"blub">> ),
  P = {Comm, E, #{}, mt, value},
  ?assert( is_finished( P ) ).

not_true_is_not_finished() ->
  Comm = {[], sets:new(), #{}},
  E = neg( true() ),
  P = {Comm, E, #{}, mt, unknown},
  ?assertNot( is_finished( P ) ).

error_is_finished() ->
  Comm = {[], sets:new(), #{}},
  E = err( t_str(), {user, <<"blub">>} ),
  P = {Comm, E, #{}, mt, unknown},
  ?assert( is_finished( P ) ).