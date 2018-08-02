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
           | {cnd_pred, info(), e(), e(), env()}
           | {neg_op, info()}
           | {conj_lhs, info(), e(), env()}
           | {conj_rhs, info(), e()}
           | {disj_lhs, info(), e(), env()}
           | {disj_rhs, info(), e()}
           | {app_fn, info(), [e_bind()], env()}
           | {app_body, info(), e(), [e_bind()]}
           | {app_arg, info(), e(), [e_bind()], x(), [e_bind()], env()}
           | {cons_hd, info(), e(), env()}
           | {cons_tl, info(), e()}
           | {append_lhs, info(), e(), env()}
           | {append_rhs, info(), e()}
           | {isnil_op, info()}
           | {rcd_field, info(), [e_bind()], x(), [e_bind()], env()}
           | {proj_op, info(), x()}
           | {fix_op, info()}
           | {for_arg, info(), t(), [e_bind()], x(), [e_bind()], e(), env()}
           | {fold_arg, info(), e_bind(), x(), e(), env()}.


%% Program

-type prog() :: {{e() | {stalled, e()}, env()}, % closure (control string + environment)
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

  E2 =
    case E1 of
      {stalled, E11} -> E11;
      _              -> E1
    end,

  case E2 of
    E -> norule;
    _ ->
      io:format( "~p~n~n", [E2] ),
      {ok, E2, Outbox}
  end.


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

%% descending rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

try_descend( {{{cnd, Info, E1, E2, E3}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{cnd_pred, Info, E2, E3, Env}|K], Outbox};

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

try_descend( {{{append, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{append_lhs, Info, E2, Env}|K], Outbox};

try_descend( {{{isnil, Info, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{isnil_op, Info}|K], Outbox};

try_descend( {{E = {rcd, Info, [{X1, E1}|BindLst]}, Env}, K, Outbox} ) ->
  case is_value( E ) of
    true  -> norule;
    false -> {{E1, Env}, [{rcd_field, Info, [], X1, BindLst, Env}|K], Outbox}
  end;

try_descend( {{{proj, Info, X, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{proj_op, Info, X}|K], Outbox};

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

try_descend( {{E1 = {fut, _, _, _}, _}, K, Output} ) ->
  {{{stalled, E1}, #{}}, K, Output};

try_descend( _ ) ->
  norule.

% We obviously do not descend into values. However, we also do not descend into
% stalled expressions.



%% ascending rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% comparison lhs

try_ascend( {{{stalled, E1}, _}, [{cmp_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{cmp_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{E1, _}, [{cmp_lhs, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  -> {{E2, Env}, [{cmp_rhs, Info, E1}|K], Outbox}
  end;


% comparison rhs

% both operands are stalled
try_ascend( {{{stalled, E2}, _},
             [{cmp_rhs, Info, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, {cmp, Info, E1, E2}}, #{}}, K, Outbox};

% only rhs stalled
try_ascend( {{{stalled, E2}, _}, [{cmp_rhs, Info, E1}|K], Outbox} ) ->
  {{{stalled, {cmp, Info, E1, E2}}, #{}}, K, Outbox};

% only lhs stalled
try_ascend( {{E2, _}, [{cmp_rhs, Info, {stalled, E1}}|K], Outbox} ) ->
  case is_value( E2 ) of
    false -> error( "stuck: bad state" );
    true  -> {{{stalled, {cmp, Info, E1, E2}}, #{}}, K, Outbox}
  end;

% neither is stalled and comparing strings
try_ascend( {{{str, _, S2}, _}, [{cmp_rhs, Info, {str, _, S1}}|K], Outbox} ) ->
  {{{S1 =:= S2, Info}, #{}}, K, Outbox};

% neither is stalled and comparing Booleans
try_ascend( {{{B2, _}, _}, [{cmp_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 =:= B2, Info}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{cmp_rhs, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% condition predicate

% predicate is stalled
try_ascend( {{{stalled, E1}, _}, [{cnd_pred, Info, E2, E3, _}|K], Outbox} ) ->
  {{{stalled, {cnd, Info, E1, E2, E3}}, #{}}, K, Outbox};

% predicate is true
try_ascend( {{{true, _}, _}, [{cnd_pred, _, E2, _, Env}|K], Outbox} ) ->
  {{E2, Env}, K, Outbox};

% predicate is false
try_ascend( {{{false, _}, _}, [{cnd_pred, _, _, E3, Env}|K], Outbox} ) ->
  {{E3, Env}, K, Outbox};

try_ascend( {{_, _}, [{cnd_pred, _, _, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% negation operand

try_ascend( {{{stalled, E1}, _}, [{neg_op, Info}|K], Outbox} ) ->
  {{{stalled, {neg, Info, E1}}, #{}}, K, Outbox};

try_ascend( {{{B1, _}, _}, [{neg_op, Info}|K], Outbox} )
when is_boolean( B1 ) ->
  {{{not B1, Info}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{neg_op, _}|_], _} ) ->
  error( "stuck: bad state" );



% conjugation lhs

try_ascend( {{{stalled, E1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{conj_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{{B1, Info1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} )
when is_boolean( B1 ) ->
  {{E2, Env}, [{conj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{_, _}, [{conj_lhs, _, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% conjugation rhs

% both operands are stalled
try_ascend( {{{stalled, E2}, _},
             [{conj_rhs, Info, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, {conj, Info, E1, E2}}, #{}}, K, Outbox};

% lhs is true and rhs operand is stalled
try_ascend( {{{stalled, E2}, _},
             [{conj_rhs, _, {true, _}}|K],
             Outbox} ) ->
  {{{stalled, E2}, #{}}, K, Outbox};

% lhs is false and rhs operand is stalled
try_ascend( {{{stalled, _}, _},
             [{conj_rhs, _, E1 = {false, _}}|K],
             Outbox} ) ->
  {{E1, #{}}, K, Outbox};

% lhs operand is stalled and rhs operand is true
try_ascend( {{{true, _}, _},
             [{conj_rhs, _, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, E1}, #{}}, K, Outbox};

% lhs operand is stalled and rhs operand is false
try_ascend( {{E2 = {false, _}, _},
             [{conj_rhs, _, {stalled, _}}|K],
             Outbox} ) ->
  {{E2, #{}}, K, Outbox};

% neither operand is stalled and lhs is true
try_ascend( {{E2 = {B2, _}, _}, [{conj_rhs, _, {true, _}}|K], Outbox} )
when is_boolean( B2 ) ->
  {{E2, #{}}, K, Outbox};

% neither operand is stalled and lhs is false
try_ascend( {{{B2, _}, _}, [{conj_rhs, _, E1 = {false, _}}|K], Outbox} )
when is_boolean( B2 ) ->
  {{E1, #{}}, K, Outbox};

try_ascend( {{_, _}, [{conj_rhs, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% disjunction lhs

try_ascend( {{{stalled, E1}, _}, [{disj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{disj_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{{B1, Info1}, _}, [{disj_lhs, Info, E2, Env}|K], Outbox} )
when is_boolean( B1 ) ->
  {{E2, Env}, [{disj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{_, _}, [{disj_lhs, _, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% disjunction rhs

% both operands are stalled
try_ascend( {{{stalled, E2}, _},
             [{disj_rhs, Info, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, {disj, Info, E1, E2}}, #{}}, K, Outbox};

% lhs is true and rhs is stalled
try_ascend( {{{stalled, _}, _},
             [{disj_rhs, _, E1 = {true, _}}|K],
             Outbox} ) ->
  {{E1, #{}}, K, Outbox};

% lhs is false and rhs operand is stalled
try_ascend( {{{stalled, E2}, _},
             [{disj_rhs, _, {false, _}}|K],
             Outbox} ) ->
  {{{stalled, E2}, #{}}, K, Outbox};

% lhs operand is stalled and rhs is true
try_ascend( {{E2 = {true, _}, _},
             [{disj_rhs, _, {stalled, _}}|K],
             Outbox} ) ->
  {{E2, #{}}, K, Outbox};

% lhs operand is stalled and rhs is false
try_ascend( {{{false, _}, _},
             [{disj_rhs, _, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, E1}, #{}}, K, Outbox};

% lhs is true
try_ascend( {{{B2, _}, _}, [{disj_rhs, _, E1 = {true, _}}|K], Outbox} )
when is_boolean( B2 ) ->
  {{E1, #{}}, K, Outbox};

% lhs is false
try_ascend( {{E2 = {B2, _}, _}, [{disj_rhs, _, {false, _}}|K], Outbox} )
when is_boolean( B2 ) ->
  {{E2, #{}}, K, Outbox};

try_ascend( {{_, _}, [{disj_rhs, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% application function argument

try_ascend( {{{stalled, E1}, _}, [{app_fn, Info, EBindLst, _}|K], Outbox} ) ->
  {{{stalled, {app, Info, E1, EBindLst}}, #{}}, K, Outbox};

try_ascend( {{E1 = {lam_ntv, _, _, EBody}, _},
          [{app_fn, Info, EBindLst, Env}|K],
          Outbox} ) ->
  Env1 = maps:from_list( [{X, {E, Env}} || {X, E} <- EBindLst] ),
  Env2 = maps:merge( Env, Env1 ),
  {{EBody, Env2}, [{app_body, Info, E1, EBindLst}|K], Outbox};

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

try_ascend( {{_, _}, [{app_fn, _, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% application function body

try_ascend( {{{stalled, EBody}, _},
             [{app_body, Info, {lam_ntv, Info, ArgLst, _}, EBindLst}|K],
             Outbox} ) ->
  Lam = {lam_ntv, Info, ArgLst, EBody},
  {{{stalled, {app, Info, Lam, EBindLst}}, #{}}, K, Outbox};

try_ascend( {{EBody, _}, [{app_body, _, _, _}|K], Outbox} ) ->
  case is_value( EBody ) of
    false -> error( "stuck: bad state" );
    true  -> {{EBody, #{}}, K, Outbox}
  end;


% application argument

try_ascend( {{{stalled, E1}, _},
             [{app_arg, Info, Lam, PreBindLst, X1, []}|K],
             Outbox} ) ->
  EBindLst1 = lists:reverse( [{X1, E1}|PreBindLst] ),
  EBindLst2 = unstall( EBindLst1 ),
  {{{stalled, {app, Info, Lam, EBindLst2}}, #{}}, K, Outbox};

try_ascend( {{E1, _},
           [{app_arg, Info,
                      Lam = {lam_frn, _, _, _, RetType, _, _},
                      PreBindLst,
                      X1,
                      [],
                      _}|K],
           Outbox} ) ->

  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  ->
      EBindLst1 = lists:reverse( [{X1, E1}|PreBindLst] ),
      case is_stalled( EBindLst1 ) of

        true ->
          EBindLst2 = unstall( EBindLst1 ),
          {{{stalled, {app, Info, Lam, EBindLst2}}, #{}},
           K,
           Outbox};

        false ->
          E = {app,
                Info,
                Lam,
                EBindLst1},
          EffiRequest = cf_client_effi:app_to_effi_request( E ),
          #{ app_id := AppId } = EffiRequest,
          {{{fut, Info, RetType, AppId}, #{}},
           K,
           [EffiRequest|Outbox]}

      end
  end;

try_ascend( {{{stalled, E1}, _},
             [{app_arg, Info,
                        Lam,
                        PreBindLst,
                        X1,
                        [{X2, E2}|PostBindLst],
                        Env}|K],
             Outbox} ) ->
  {{E2, Env},
   [{app_arg, Info,
              Lam,
              [{X1, {stalled, E1}}|PreBindLst],
              X2,
              PostBindLst,
              Env}|K],
   Outbox};


try_ascend( {{E1, _},
           [{app_arg, Info,
                      Lam,
                      PreBindLst,
                      X1,
                      [{X2, E2}|PostBindLst],
                      Env}|K],
           Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
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


% cons head

try_ascend( {{{stalled, E1}, _}, [{cons_hd, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{cons_tl, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{E1, _}, [{cons_hd, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  -> {{E2, Env}, [{cons_tl, Info, E1}|K], Outbox}
  end;


% cons tail

% both operands stalled
try_ascend( {{{stalled, E2}, _},
             [{cons_tl, Info, {stalled, E1}}|K],
             Outbox} ) ->
  {{{stalled, {cons, Info, E1, E2}}, #{}}, K, Outbox};

% only cons rhs stalled
try_ascend( {{{stalled, E2}, _}, [{cons_tl, Info, E1}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  -> {{{stalled, {cons, Info, E1, E2}}, #{}}, K, Outbox}
  end;

% only cons lhs stalled
try_ascend( {{E2, _}, [{cons_tl, Info, {stalled, E1}}|K], Outbox} ) ->
  case is_value( E2 ) of
    false -> error( "stuck: bad state" );
    true  -> {{{stalled, {cons, Info, E1, E2}}, #{}}, K, Outbox}
  end;

try_ascend( {{E2, _}, [{cons_tl, Info, E1}|K], Outbox} ) ->
  case is_value( E1 ) andalso is_value( E2 ) of
    false -> error( "stuck: bad state" );
    true  -> {{{cons, Info, E1, E2}, #{}}, K, Outbox}
  end;


% append lhs

try_ascend( {{{stalled, E1}, _}, [{append_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{append_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{E1, _}, [{append_lhs, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  -> {{E2, Env}, [{append_rhs, Info, E1}|K], Outbox}
  end;


% append rhs

try_ascend( {{{stalled, E2}, _}, [{append_rhs, Info, {stalled, E1}}|K], Outbox} ) ->
  {{{stalled, {append, Info, E1, E2}}, #{}}, K, Outbox};

try_ascend( {{{stalled, E2}, _}, [{append_rhs, Info, E1}|K], Outbox} ) ->
  {{{stalled, {append, Info, E1, E2}}, #{}}, K, Outbox};

try_ascend( {{E2, _}, [{append_rhs, Info, {stalled, E1}}|K], Outbox} ) ->
  {{{stalled, {append, Info, E1, E2}}, #{}}, K, Outbox};

try_ascend( {{E2, _}, [{append_rhs, _, E1}|K], Outbox} ) ->

  F =
    fun
      F( {null, _, _}, EE2 )             -> EE2;
      F( {cons, I, EE11, EE12}, EE2 ) -> {cons, I, EE11, F( EE12, EE2 )}
    end,

  case is_value( E1 ) andalso is_value( E2 ) of
    false -> error( "stuck: bad state" );
    true  -> {{F( E1, E2 ), #{}}, K, Outbox}
  end;


% isnil operand

try_ascend( {{{stalled, {cons, _, _, _}}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  {{{false, Info}, #{}}, K, Outbox};

try_ascend( {{{stalled, E1}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  {{{stalled, {isnil, Info, E1}}, #{}}, K, Outbox};

try_ascend( {{{null, _, _}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  {{{true, Info}, #{}}, K, Outbox};

try_ascend( {{{cons, _, _, _}, _}, [{isnil_op, Info}|K], Outbox} ) ->
  {{{false, Info}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{isnil_op, _}|_], _} ) ->
  error( "stuck: bad state" );


% record field

try_ascend( {{{stalled, E1}, _},
             [{rcd_field, Info, PreLst, X1, [], _}|K],
             Outbox} ) ->
  EBindLst1 = lists:reverse( [{X1, E1}|PreLst] ),
  EBindLst2 = unstall( EBindLst1 ),
  {{{stalled, {rcd, Info, EBindLst2}}, #{}}, K, Outbox};

try_ascend( {{E1, _}, [{rcd_field, Info, PreLst, X1, [], _}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  ->
      EBindLst1 = lists:reverse( [{X1, E1}|PreLst] ),
      case is_stalled( EBindLst1 ) of

        true ->
          EBindLst2 = unstall( EBindLst1 ),
          {{{stalled, {rcd, Info, EBindLst2}}, #{}}, K, Outbox};

        false ->
          {{{rcd, Info, EBindLst1}, #{}}, K, Outbox}

      end
  end;

try_ascend( {{{stalled, E1}, _},
             [{rcd_field, Info, PreLst, X1, [{X2, E2}|PostLst], Env}|K],
             Outbox} ) ->
  {{E2, Env},
   [{rcd_field, Info, [{X1, {stalled, E1}}|PreLst], X2, PostLst, Env}|K],
   Outbox};

try_ascend( {{E1, _},
            [{rcd_field, Info, PreLst, X1, [{X2, E2}|PostLst], Env}|K],
            Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  ->
      {{E2, Env},
       [{rcd_field, Info, [{X1, E1}|PreLst], X2, PostLst, Env}|K],
       Outbox}
  end;

% record projection

% TODO: reach under stalled projection if operand has the form of a record

try_ascend( {{{stalled, E1}, _}, [{proj_op, Info, X}|K], Outbox} ) ->
  {{{stalled, {proj, Info, X, E1}}, #{}}, K, Outbox};

try_ascend( {{{rcd, _, EBindLst}, _}, [{proj_op, _, X}|K], Outbox} ) ->
  {_, EX} = lists:keyfind( X, 1, EBindLst ),
  {{EX, #{}}, K, Outbox};

try_ascend( {{_, _}, [{proj_op, _, _}|_], _} ) ->
  error( "stuck: bad state" );


% fixpoint operator

try_ascend( {{{stalled, E1}, _}, [{fix_op, Info}|K], Outbox} ) ->
  {{{stalled, {fix, Info, E1}}, #{}}, K, Outbox};

try_ascend( {{E = {lam_ntv, LamInfo, [FArg = {_, ExtName, _}|ArgLst], EBody}, _},
            [{fix_op, Info}|K],
            Outbox} ) ->
  EBody1 = {app, Info,
                 {lam_ntv, Info, [FArg], EBody},
                 [{ExtName, {fix, Info, E}}]},
  {{{lam_ntv, LamInfo, ArgLst, EBody1}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{fix_op, _}|_], _} ) ->
  error( "stuck: bad state" );


% for argument

try_ascend( {{E1, _},
            [{for_arg, Info, Type, PreLst, X1, [], EBody, Env}|K],
            Outbox} ) ->
  EBindLst1 = lists:reverse( [{X1, E1}|PreLst] ),
  case split_zip( EBindLst1 ) of
    stuck   -> error( "stuck: bad state" );
    null     -> {{{null, Info, Type}, #{}}, K, Outbox};
    {L1, L2} ->
      EBody1 = bind_all( Info, L1, EBody ),
      EFor1 = {for, Info, Type, L2, EBody},
      {{EBody1, Env}, [{cons_hd, Info, EFor1, Env}|K], Outbox};
    stalled  ->
      EBindLst2 = unstall( EBindLst1 ),
      {{{stalled, {for, Info, Type, EBindLst2, EBody}}, #{}}, K, Outbox}
  end;

try_ascend( {{{stalled, E1}, _},
             [{for_arg, Info, Type, PreLst, X1, [{X2, E2}|PostLst], EBody, Env}|K],
             Outbox} ) ->
  {{E2, Env},
   [{for_arg, Info, Type, [{X1, {stalled, E1}}|PreLst], X2, PostLst, EBody, Env}|K],
   Outbox};

try_ascend( {{E1, _},
            [{for_arg, Info, Type, PreLst, X1, [{X2, E2}|PostLst], EBody, Env}|K],
            Outbox} ) ->
  case is_value( E1 ) of
    false -> error( "stuck: bad state" );
    true  ->
      {{E2, Env},
       [{for_arg, Info, Type, [{X1, E1}|PreLst], X2, PostLst, EBody, Env}|K],
       Outbox}
  end;

try_ascend( {{{stalled, {cons, _, EHd, ETl}}, _},
             [{fold_arg, Info, {XAcc, EAcc}, XLst, EBody, Env}|K],
             Outbox} ) ->
  EHd1 =
    case is_value( EHd ) of
      true  -> EHd;
      false -> {stalled, EHd}
    end,

  ETl1 =
    case is_value( ETl ) of
      true  -> ETl;
      false -> {stalled, ETl}
    end,

  EAcc1 = bind_all( Info, [{XAcc, EAcc}, {XLst, EHd1}], EBody ),

  {{ETl1, Env},
   [{fold_arg, Info, {XAcc, EAcc1}, XLst, EBody, Env}|K],
   Outbox};

try_ascend( {{{stalled, ELst}, _},
             [{fold_arg, Info, {XAcc, EAcc}, XLst, EBody, _}|K],
             Outbox} ) ->
  {{{stalled, {fold, Info, {XAcc, EAcc}, {XLst, ELst}, EBody}}, #{}},
   K,
   Outbox};

try_ascend( {{{cons, _, EHd, ETl}, _},
             [{fold_arg, Info, {XAcc, EAcc}, XLst, EBody, Env}|K],
             Outbox} ) ->
  EAcc1 = bind_all( Info, [{XAcc, EAcc}, {XLst, EHd}], EBody ),
  {{ETl, Env},
   [{fold_arg, Info, {XAcc, EAcc1}, XLst, EBody, Env}|K],
   Outbox};

try_ascend( {{{null, _, _}, _},
             [{fold_arg, _, {_, EAcc}, _, _, Env}|K],
             Outbox} ) ->
  {{EAcc, Env}, K, Outbox};



try_ascend( _ ) ->
  norule.















-spec split_zip( EBindLst ) -> Result
when EBindLst :: [{x(), e()}],
     Result   :: {[{x(), e()}], []}
               | stalled
               | null
               | stuck.

split_zip( EBindLst ) ->

  F =
    fun
      ( {X, {stalled, {cons, _, Hd, Tl}}}, {L1, L2} ) ->
        Hd1 =
          case is_value( Hd ) of
            true  -> Hd;
            false -> {stalled, Hd}
          end,
        Tl1 =
          case is_value( Tl ) of
            true  -> Tl;
            false -> {stalled, Tl}
          end,
        {[{X, Hd1}|L1], [{X, Tl1}|L2]};
      ( {_, {stalled, _}}, _ ) ->
        throw( stalled );
      ( {X, {cons, _, Hd, Tl}}, {L1, L2} ) ->
        {[{X, Hd}|L1], [{X, Tl}|L2]};
      ( {_, {null, _, _}}, _ ) ->
        throw( null );
      ( _, _ ) ->
        throw( stuck )
    end,

  try lists:foldr( F, {[], []}, EBindLst ) of
    {L1, L2} -> {L1, L2}
  catch
    throw:stalled -> stalled;
    throw:null    -> null;
    throw:stuck   -> stuck
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



-spec is_stalled( [e_bind()] ) -> boolean().

is_stalled( EBindLst ) when is_list( EBindLst ) ->

  F =
    fun
      ( {_, {stalled, _}} ) -> true;
      ( {_, _} )            -> false
    end,

  lists:any( F, EBindLst ).


-spec unstall( [e_bind()] ) -> [e_bind()].

unstall( EBindLst ) when is_list( EBindLst ) ->

  F =
    fun
      ( {X, {stalled, E}} ) -> {X, E};
      ( {X, E} )            -> {X, E}
    end,

  lists:map( F, EBindLst ).