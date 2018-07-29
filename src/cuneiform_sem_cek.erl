%% -*- erlang -*-
%%
%% cf_client
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_sem_cek ).
-behavior( cuneiform_sem ).


%%====================================================================
%% Exports
%%====================================================================

-export( [step/1, split_zip/1, bind_all/3] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_sem, [is_value/1] ).

%%====================================================================
%% Type definitions
%%====================================================================


%% Environment

-type env() :: #{ x() => {e(), env()} }.


%% Continuation Element

-type k() :: {cmp_lhs, info(), e(), env()}
           | {cmp_rhs, info(), e()}
           | {cnd_pred, e(), e(), env()}
           | {neg_op, info()}
           | {conj_lhs, info(), e(), env()}
           | {conj_rhs, info(), e()}
           | {disj_lhs, info(), e(), env()}
           | {disj_rhs, info(), e()}
           | {app_fn, info(), [e_bind()], env()}
           | {app_arg, info(), e(), [e_bind()], x(), [e_bind()], env()}
           | {cons_hd, info(), e(), env()}
           | {cons_tl, info(), e()}
           | {append_lhs, e(), env()}
           | {append_rhs, e()}
           | {isnil_op, info()}
           | {rcd_field, info(), [e_bind()], x(), [e_bind()], env()}
           | {proj_op, x()}
           | {fix_op, info()}
           | {for_arg, info(), t(), [e_bind()], x(), [e_bind()], e(), env()}
           | {fold_arg, info(), e_bind(), x(), e(), env()}.


%% Program

-type prog() :: {{e(), env()}, % closure (control string + environment)
                 [k()],        % continuation
                 [e()]}.       % foreign function applications




%%====================================================================
%% cuneiform_sem callback implementation
%%====================================================================

-spec step( E ) -> Result
when E      :: e(),
     Result :: {ok, e(), [e()]}
             | norule.

step( E ) ->
  P = {{E, #{}}, [], []},
  {{E1, _}, [], Outbox} = eval_cek( P ),
  {ok, E1, Outbox}.


%%====================================================================
%% CEK machine
%%====================================================================

-spec eval_cek( P :: prog() ) -> prog().

eval_cek( P ) ->
  case step_cek( P ) of
    norule -> P;
    P1     -> eval_cek( P1 )
  end.


-spec step_cek( P :: prog() ) -> prog() | norule.

step_cek( P ) ->
  case try_descend( P ) of
    norule -> try_ascend( P );
    P1     -> P1
  end.

% descending rules

-spec try_descend( P :: prog() ) -> prog() | norule.

try_descend( {{{err, _, _, _}, _}, [], []} ) ->
  norule;

try_descend( {{E = {err, _, _, _}, _}, _, _} ) ->
  {{E, #{}}, [], []};

try_descend( {{{var, _, X}, Env}, K, Outbox} ) ->
  #{ X := {E1, Env1} } = Env,
  {{E1, Env1}, K, Outbox};

try_descend( {{{cmp, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{cmp_lhs, Info, E2, Env}|K], Outbox};

try_descend( {{{cnd, _, E1, E2, E3}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{cnd_pred, E2, E3, Env}|K], Outbox};

try_descend( {{{neg, Info, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{neg_op, Info}|K], Outbox};

try_descend( {{{conj, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{conj_lhs, Info, E2, Env}|K], Outbox};

try_descend( {{{disj, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{disj_lhs, Info, E2, Env}|K], Outbox};

try_descend( {{{app, Info, E1, EBindLst}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{app_fn, Info, EBindLst, Env}|K], Outbox};

try_descend( {{E = {cons, Info, E1, E2}, Env}, K, Outbox} ) ->
  case is_value( E ) of
    true  -> norule;
    false -> {{E1, Env}, [{cons_hd, Info, E2, Env}|K], Outbox}
  end;

try_descend( {{{append, _, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{append_lhs, E2, Env}|K], Outbox};

try_descend( {{{isnil, Info, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{isnil_op, Info}|K], Outbox};

try_descend( {{E = {rcd, Info, [{X1, E1}|BindLst]}, Env}, K, Outbox} ) ->
  case is_value( E ) of
    true  -> norule;
    false -> {{E1, Env}, [{rcd_field, Info, [], X1, BindLst, Env}|K], Outbox}
  end;

try_descend( {{{proj, _, X, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{proj_op, X}|K], Outbox};

try_descend( {{{fix, Info, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{fix_op, Info}|K], Outbox};

try_descend( {{{for, Info, Type, [{X1, E1}|EBindLst], EBody}, Env},
              K,
              Outbox} ) ->
  {{E1, Env},
   [{for_arg, Info, Type, [], X1, EBindLst, EBody, Env}|K],
   Outbox};

try_descend( {{{fold, Info, {XAcc, EAcc}, {XLst, ELst}, EBody}, Env},
              K,
              Outbox} ) ->
  {{ELst, Env}, [{fold_arg, Info, {XAcc, EAcc}, XLst, EBody, Env}|K], Outbox};

try_descend( _ ) ->
  norule.


% ascending rules

try_ascend( {{E1, _}, [{cmp_lhs, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  -> {{E2, Env}, [{cmp_rhs, Info, E1}|K], Outbox}
  end;

try_ascend( {{{str, _, S2}, _}, [{cmp_rhs, Info, {str, _, S1}}|K], Outbox} ) ->
  {{{S1 =:= S2, Info}, #{}}, K, Outbox};

try_ascend( {{{B2, _}, _}, [{cmp_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 =:= B2, Info}, #{}}, K, Outbox};

try_ascend( {{{true, _}, _}, [{cnd_pred, E2, _, Env}|K], Outbox} ) ->
  {{E2, Env}, K, Outbox};

try_ascend( {{{false, _}, _}, [{cnd_pred, _, E3, Env}|K], Outbox} ) ->
  {{E3, Env}, K, Outbox};

try_ascend( {{{B1, _}, _}, [{neg_op, Info}|K], Outbox} ) ->
  {{{not B1, Info}, #{}}, K, Outbox};

try_ascend( {{{B1, Info1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{conj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{{B2, _}, _}, [{conj_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 and B2, Info}, #{}}, K, Outbox};

try_ascend( {{{B1, Info1}, _}, [{disj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{disj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{{B2, _}, _}, [{disj_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 or B2, Info}, #{}}, K, Outbox};

try_ascend( {{{lam_ntv, _, _, EBody}, _},
          [{app_fn, _, EBindLst, Env}|K],
          Outbox} ) ->
  Env1 = maps:from_list( [{X, {E, Env}} || {X, E} <- EBindLst] ),
  Env2 = maps:merge( Env, Env1 ),
  {{EBody, Env2}, K, Outbox};

try_ascend( {{{lam_frn, LamInfo, FName, ArgLst, RetType, Lang, Body}, _},
          [{app_fn, Info, [], _}|K],
          Outbox} ) ->
  E1 = {app,
         Info,
         {lam_frn, LamInfo, FName, ArgLst, RetType, Lang, Body},
         []},
  EffiRequest = cf_client_effi:app_to_effi_request( E1 ),
  #{ app_id := AppId } = EffiRequest,
  {{{fut, Info, RetType, AppId}, #{}}, K, [EffiRequest|Outbox]};

try_ascend( {{Lam = {lam_frn, _, _, _, _, _, _}, _},
          [{app_fn, Info, [{X1, E1}|EBindLst], Env}|K],
          Outbox} ) ->
  {{E1, Env}, [{app_arg, Info, Lam, [], X1, EBindLst, Env}|K], Outbox};

try_ascend( {{E1, _},
           [{app_arg, Info,
                      Lam = {lam_frn, _, _, _, RetType, _, _},
                      PreBindLst,
                      X1,
                      [],
                      _}|K],
           Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  ->
      E = {app,
            Info,
            Lam,
            lists:reverse( [{X1, E1}|PreBindLst] )},
      EffiRequest = cf_client_effi:app_to_effi_request( E ),
      #{ app_id := AppId } = EffiRequest,
      {{{fut, Info, RetType, AppId}, #{}}, K, [EffiRequest|Outbox]}
  end;

try_ascend( {{E1, _},
           [{app_arg, Info,
                      Lam,
                      PreBindLst,
                      X1,
                      [{X2, E2}|PostBindLst],
                      Env}|K],
           Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  ->
      {{E2, Env},
       [{app_arg, Info,
                  Lam,
                  [{X1, E1}|PreBindLst],
                  X2,
                  PostBindLst,
                  Env}|K],
       Outbox}
  end;

try_ascend( {{E1, _}, [{cons_hd, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  -> {{E2, Env}, [{cons_tl, Info, E1}|K], Outbox}
  end;

try_ascend( {{E2, _}, [{cons_tl, Info, E1}|K], Outbox} ) ->
  case is_value( E2 ) of
    false -> norule;
    true  -> {{{cons, Info, E1, E2}, #{}}, K, Outbox}
  end;

try_ascend( {{E1, _}, [{append_lhs, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  -> {{E2, Env}, [{append_rhs, E1}|K], Outbox}
  end;

try_ascend( {{E2, _}, [{append_rhs, E1}|K], Outbox} ) ->
  F =
    fun
      F( {null, _, _}, EE2 )             -> EE2;
      F( {cons, Info, EE11, EE12}, EE2 ) -> {cons, Info, EE11, F( EE12, EE2 )}
    end,
  case is_value( E2 ) of
    false -> norule;
    true  -> {{F( E1, E2 ), #{}}, K, Outbox}
  end;

try_ascend( {{{null, _, _}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  {{{true, Info}, #{}}, K, Outbox};

try_ascend( {{E = {cons, _, _, _}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  case is_value( E ) of
    false -> norule;
    true  -> {{{false, Info}, #{}}, K, Outbox}
  end;

try_ascend( {{E1, _}, [{rcd_field, Info, PreLst, X1, [], _}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  -> {{{rcd, Info, lists:reverse( [{X1, E1}|PreLst] )}, #{}}, K, Outbox}
  end;

try_ascend( {{E1, _},
            [{rcd_field, Info, PreLst, X1, [{X2, E2}|PostLst], Env}|K],
            Outbox} ) ->
  case is_value( E1 ) of
    false -> norule;
    true  ->
      {{E2, Env},
       [{rcd_field, Info, [{X1, E1}|PreLst], X2, PostLst, Env}|K],
       Outbox}
  end;

try_ascend( {{{rcd, _, EBindLst}, _}, [{proj_op, X}|K], Outbox} ) ->
  {_, EX} = lists:keyfind( X, 1, EBindLst ),
  case is_value( EX ) of
    false -> norule;
    true  -> {{EX, #{}}, K, Outbox}
  end;

try_ascend( {{E = {lam_ntv, LamInfo, [FArg = {_, ExtName, _}|ArgLst], EBody}, _},
            [{fix_op, Info}|K],
            Outbox} ) ->
  EBody1 = {app, Info,
                 {lam_ntv, Info, [FArg], EBody},
                 [{ExtName, {fix, Info, E}}]},
  {{{lam_ntv, LamInfo, ArgLst, EBody1}, #{}}, K, Outbox};

try_ascend( {{E1, _},
            [{for_arg, Info, Type, PreLst, X1, [], EBody, Env}|K],
            Outbox} ) ->
  PreLst1 = [{X1, E1}|PreLst],
  case split_zip( PreLst1 ) of
    stalled      -> error( nyi );
    null         -> {{{null, Info, Type}, #{}}, K, Outbox};
    {ok, L1, L2} ->
      EBody1 = bind_all( Info, L1, EBody ),
      EFor1 = {for, Info, Type, L2, EBody},
      {{EBody1, Env}, [{cons_hd, Info, EFor1, Env}|K], Outbox}
  end;

try_ascend( {{E1, _},
            [{for_arg, Info, Type, PreLst, X1, [{X2, E2}|PostLst], EBody, Env}|K],
            Outbox} ) ->
{{E2, Env},
 [{for_arg, Info, Type, [{X1, E1}|PreLst], X2, PostLst, EBody, Env}|K],
 Outbox};

try_ascend( {{ELst = {cons, _, EHd, ETl}, _},
             [{fold_arg, Info, {XAcc, EAcc}, XLst, EBody, Env}|K],
             Outbox} ) ->
  case is_value( ELst ) of
    false -> norule;
    true  ->
      EAcc1 = bind_all( Info, [{XAcc, EAcc}, {XLst, EHd}], EBody ),
      {{ETl, Env},
       [{fold_arg, Info, {XAcc, EAcc1}, XLst, EBody, Env}|K],
       Outbox}
  end;

try_ascend( {{{null, _, _}, _},
             [{fold_arg, _, {_, EAcc}, _, _, Env}|K],
             Outbox} ) ->
  {{EAcc, Env}, K, Outbox};



try_ascend( _ ) ->
  norule.















-spec split_zip( EBindLst ) -> Result
when EBindLst :: [{x(), e()}],
     Result   :: {ok, [{x(), e()}], []}
               | null
               | stalled.

split_zip( EBindLst ) ->

  F =
    fun
      ( {X, {cons, _, Hd, Tl}}, {L1, L2} ) ->
        {[{X, Hd}|L1], [{X, Tl}|L2]};
      ( {_, {null, _, _}}, _ ) ->
        throw( null );
      ( _, _ ) ->
        throw( stalled )
    end,

  try lists:foldr( F, {[], []}, EBindLst ) of
    {L1, L2} -> {ok, L1, L2}
  catch
    throw:null    -> null;
    throw:stalled -> stalled
  end.

-spec bind_all( Info, EBindLst, EBody ) -> e()
when Info     :: info(),
     EBindLst :: [e_bind()],
     EBody    :: e().

bind_all( _, [], EBody ) ->
  EBody;

bind_all( Info, [{X1, E1}|EBindLst], EBody ) ->
  Lam = {lam_ntv, Info, [{X1, X1, undefined_type}], EBody},
  EBody1 = {app, Info, Lam, [{X1, E1}]},
  bind_all( Info, EBindLst, EBody1 ).
