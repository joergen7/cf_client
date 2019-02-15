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

-module( cuneiform_sem_cek ).
-behavior( cuneiform_sem ).


%%====================================================================
%% Exports
%%====================================================================

-export( [step/1, split_zip/1, bind_all/3] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform_sem_cek.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_sem, [is_value/1] ).


%%====================================================================
%% cuneiform_sem callback implementation
%%====================================================================

-spec step( E ) -> Result
when E      :: e(),
     Result :: {ok, e(), [e()]}.

step( E ) ->

  % load expression
  P = {{E, #{}}, [], []},

  % evaluate expression
  {{E1, _}, [], Outbox} = eval_cek( P ),

  % remove stalled flag if necessary
  E2 =
    case E1 of
      {stalled, E11} -> E11;
      _              -> E1
    end,

  {ok, E2, Outbox}.


%%====================================================================
%% CEK machine
%%====================================================================

-spec eval_cek( P :: prog() ) -> prog().

eval_cek( P ) ->
  case step_cek( P ) of
    norule -> P;
    P      -> error( no_change );
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

try_descend( {{E1 = {fut, _, _, _}, _}, K, Output} ) ->
  {{{stalled, E1}, #{}}, K, Output};

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

try_descend( {{{for, Info, Type, [{X1, T1, E1}|TypedBindLst], EBody}, Env},
              K,
              Outbox} ) ->
  {{E1, Env},
   [{for_arg, Info, Type, [], X1, T1, TypedBindLst, EBody, Env}|K],
   Outbox};

try_descend( {{{fold, Info, {XAcc, TAcc, EAcc}, {XArg, TArg, EArg}, EBody}, Env},
              K,
              Outbox} ) ->
  {{EArg, Env}, [{fold_arg, Info, {XAcc, TAcc, EAcc}, XArg, TArg, EBody, Env}|K], Outbox};

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
    false -> error( stuck );
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
    false -> error( stuck );
    true  -> {{{stalled, {cmp, Info, E1, E2}}, #{}}, K, Outbox}
  end;

% neither is stalled and comparing strings
try_ascend( {{{str, _, S2}, _}, [{cmp_rhs, Info, {str, _, S1}}|K], Outbox} ) ->
  {{{S1 =:= S2, Info}, #{}}, K, Outbox};

% neither is stalled and comparing Booleans
try_ascend( {{{B2, _}, _}, [{cmp_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 =:= B2, Info}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{cmp_rhs, _, _}|_], _} ) ->
  error( stuck );


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
  error( stuck );


% negation operand

try_ascend( {{{stalled, E1}, _}, [{neg_op, Info}|K], Outbox} ) ->
    {{{stalled, {neg, Info, E1}}, #{}}, K, Outbox};

try_ascend( {{{B1, _}, _}, [{neg_op, Info}|K], Outbox} )
when is_boolean( B1 ) ->
  {{{not B1, Info}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{neg_op, _}|_], _} ) ->
  error( stuck );



% conjunction lhs

try_ascend( {{{stalled, E1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{conj_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{{B1, Info1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} )
when is_boolean( B1 ) ->
  {{E2, Env}, [{conj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{_, _}, [{conj_lhs, _, _, _}|_], _} ) ->
  error( stuck );


% conjunction rhs

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
  error( stuck );


% disjunction lhs

try_ascend( {{{stalled, E1}, _}, [{disj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{disj_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{{B1, Info1}, _}, [{disj_lhs, Info, E2, Env}|K], Outbox} )
when is_boolean( B1 ) ->
  {{E2, Env}, [{disj_rhs, Info, {B1, Info1}}|K], Outbox};

try_ascend( {{_, _}, [{disj_lhs, _, _, _}|_], _} ) ->
  error( stuck );


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
  error( stuck );


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
  error( stuck );


% application function body

try_ascend( {{{stalled, EBody}, _},
             [{app_body, Info, {lam_ntv, LamInfo, ArgLst, _}, EBindLst}|K],
             Outbox} ) ->
  Lam = {lam_ntv, LamInfo, ArgLst, EBody},
  EBindLst1 = unstall( EBindLst ),
  {{{stalled, {app, Info, Lam, EBindLst1}}, #{}}, K, Outbox};

try_ascend( {{EBody, _}, [{app_body, _, _, _}|K], Outbox} ) ->
  case is_value( EBody ) of
    false -> error( stuck );
    true  -> {{EBody, #{}}, K, Outbox}
  end;


% application argument

try_ascend( {{{stalled, E1}, _},
             [{app_arg, Info, Lam, PreBindLst, X1, [], _}|K],
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
    false -> error( stuck );
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
    false -> error( stuck );
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
    false -> error( stuck );
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
    false -> error( stuck );
    true  -> {{{stalled, {cons, Info, E1, E2}}, #{}}, K, Outbox}
  end;

% only cons lhs stalled
try_ascend( {{E2, _}, [{cons_tl, Info, {stalled, E1}}|K], Outbox} ) ->
  case is_value( E2 ) of
    false -> error( stuck );
    true  -> {{{stalled, {cons, Info, E1, E2}}, #{}}, K, Outbox}
  end;

try_ascend( {{E2, _}, [{cons_tl, Info, E1}|K], Outbox} ) ->
  case is_value( E1 ) andalso is_value( E2 ) of
    false -> error( stuck );
    true  -> {{{cons, Info, E1, E2}, #{}}, K, Outbox}
  end;


% append lhs

try_ascend( {{{stalled, {cons, ConsInfo, E11, E12}}, _},
             [{append_lhs, Info, E2, Env}|K],
             Outbox} ) ->

  
  F11 =
    case is_value( E11 ) of
      true  -> E11;
      false -> {stalled, E11}
    end,

  F12 =
    case is_value( E12 ) of
      true  -> E12;
      false -> {stalled, E12}
    end,

  {{F12, #{}},
   [{append_lhs, Info, E2, Env}, {cons_tl, ConsInfo, F11}|K],
   Outbox};

try_ascend( {{{stalled, E1}, _}, [{append_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{append_rhs, Info, {stalled, E1}}|K], Outbox};

try_ascend( {{{cons, ConsInfo, E11, E12}, _},
             [{append_lhs, Info, E2, Env}|K], Outbox} ) ->
  case is_value( E11 ) andalso is_value( E12 ) of
    false -> error( stuck );
    true  ->
      {{E12, #{}},
       [{append_lhs, Info, E2, Env}, {cons_tl, ConsInfo, E11}|K],
       Outbox}
  end;

try_ascend( {{{null, _, _}, _}, [{append_lhs, _, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, K, Outbox};



% append rhs

try_ascend( {{{stalled, E2}, _}, [{append_rhs, Info, {stalled, E1}}|K], Outbox} ) ->
  {{{stalled, {append, Info, E1, E2}}, #{}}, K, Outbox};

try_ascend( {{E2, _}, [{append_rhs, Info, {stalled, E1}}|K], Outbox} ) ->
  {{{stalled, {append, Info, E1, E2}}, #{}}, K, Outbox};


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
  error( stuck );


% record field

try_ascend( {{{stalled, E1}, _},
             [{rcd_field, Info, PreLst, X1, [], _}|K],
             Outbox} ) ->
  EBindLst1 = lists:reverse( [{X1, E1}|PreLst] ),
  EBindLst2 = unstall( EBindLst1 ),
  {{{stalled, {rcd, Info, EBindLst2}}, #{}}, K, Outbox};

try_ascend( {{E1, _}, [{rcd_field, Info, PreLst, X1, [], _}|K], Outbox} ) ->
  case is_value( E1 ) of
    false -> error( stuck );
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
    false -> error( stuck );
    true  ->
      {{E2, Env},
       [{rcd_field, Info, [{X1, E1}|PreLst], X2, PostLst, Env}|K],
       Outbox}
  end;


% record projection

try_ascend( {{{stalled, {rcd, _, EBindLst}}, _},
             [{proj_op, _, X}|K],
             Outbox} ) ->
    {_, EX} = lists:keyfind( X, 1, EBindLst ),
  EX1 =
    case is_value( EX ) of
      true  -> EX;
      false -> {stalled, EX}
    end,
  {{EX1, #{}}, K, Outbox};
  

try_ascend( {{{stalled, E1}, _}, [{proj_op, Info, X}|K], Outbox} ) ->
  {{{stalled, {proj, Info, X, E1}}, #{}}, K, Outbox};

try_ascend( {{{rcd, _, EBindLst}, _}, [{proj_op, _, X}|K], Outbox} ) ->
  {_, EX} = lists:keyfind( X, 1, EBindLst ),
  {{EX, #{}}, K, Outbox};

try_ascend( {{_, _}, [{proj_op, _, _}|_], _} ) ->
  error( stuck );


% fixpoint operator

try_ascend( {{{stalled, E1}, _}, [{fix_op, Info}|K], Outbox} ) ->
    {{{stalled, {fix, Info, E1}}, #{}}, K, Outbox};

try_ascend( {{E = {lam_ntv, LamInfo, [FArg = {X, _}|ArgLst], EBody}, _},
            [{fix_op, Info}|K],
            Outbox} ) ->
  EBody1 = {app, Info,
                 {lam_ntv, Info, [FArg], EBody},
                 [{X, {fix, Info, E}}]},
  {{{lam_ntv, LamInfo, ArgLst, EBody1}, #{}}, K, Outbox};

try_ascend( {{_, _}, [{fix_op, _}|_], _} ) ->
  error( stuck );


% for argument

try_ascend( {{E1, _},
            [{for_arg, Info, Type, PreLst, X1, T1, [], EBody, Env}|K],
            Outbox} ) ->


  TypedBindLst1 = lists:reverse( [{X1, T1, E1}|PreLst] ),

  case split_zip( TypedBindLst1 ) of
    stuck    -> error( stuck );
    null     -> {{{null, Info, Type}, #{}}, K, Outbox};
    {L1, L2} ->
      EBody1 = bind_all( Info, L1, EBody ),
      EFor1 = {for, Info, Type, L2, EBody},
      {{EBody1, Env}, [{cons_hd, Info, EFor1, Env}|K], Outbox};
    stalled  ->
      TypedBindLst2 = unstall_typed( TypedBindLst1 ),
      {{{stalled, {for, Info, Type, TypedBindLst2, EBody}}, #{}}, K, Outbox}
  end;

try_ascend( {{{stalled, E1}, _},
             [{for_arg, Info,
                        Type,
                        PreLst,
                        X1,
                        T1,
                        [{X2, T2, E2}|PostLst],
                        EBody,
                        Env}|K],
             Outbox} ) ->
    {{E2, Env},
   [{for_arg, Info,
              Type,
              [{X1, T1, {stalled, E1}}|PreLst],
              X2,
              T2,
              PostLst,
              EBody,
              Env}|K],
   Outbox};

try_ascend( {{E1, _},
            [{for_arg, Info,
                       Type,
                       PreLst,
                       X1,
                       T1,
                       [{X2, T2, E2}|PostLst],
                       EBody,
                       Env}|K],
            Outbox} ) ->
  case is_value( E1 ) of
    false -> error( stuck );
    true  ->
      {{E2, Env},
       [{for_arg, Info,
                  Type,
                  [{X1, T1, E1}|PreLst],
                  X2,
                  T2,
                  PostLst,
                  EBody,
                  Env}|K],
       Outbox}
  end;


% fold argument

% try_ascend( {{{stalled, {cons, _, EHd, ETl}}, _},
%              [{fold_arg, Info, {XAcc, TAcc, EAcc}, XArg, TArg, EBody, Env}|K],
%              Outbox} ) ->
%     EHd1 =
%     case is_value( EHd ) of
%       true  -> EHd;
%       false -> {stalled, EHd}
%     end,

%   ETl1 =
%     case is_value( ETl ) of
%       true  -> ETl;
%       false -> {stalled, ETl}
%     end,

%   EAcc1 = bind_all( Info, [{XAcc, TAcc, EAcc}, {XArg, TArg, EHd1}], EBody ),

%   {{ETl1, #{}},
%    [{fold_arg, Info, {XAcc, TAcc, EAcc1}, XArg, TArg, EBody, Env}|K],
%    Outbox};

try_ascend( {{{stalled, EArg}, _},
             [{fold_arg, Info, {XAcc, TAcc, EAcc}, XArg, TArg, EBody, _}|K],
             Outbox} ) ->
  {{{stalled, {fold, Info, {XAcc, TAcc, EAcc}, {XArg, TArg, EArg}, EBody}}, #{}},
   K,
   Outbox};

try_ascend( {{{cons, _, EHd, ETl}, _},
             [{fold_arg, Info, {XAcc, TAcc, EAcc}, XArg, TArg, EBody, Env}|K],
             Outbox} ) ->


  EAcc1 = bind_all( Info, [{XAcc, TAcc, EAcc}, {XArg, TArg, EHd}], EBody ),

  {{ETl, #{}},
   [{fold_arg, Info, {XAcc, TAcc, EAcc1}, XArg, TArg, EBody, Env}|K],
   Outbox};

try_ascend( {{{null, _, _}, _},
             [{fold_arg, _, {_, _, EAcc}, _, _, _, Env}|K],
             Outbox} ) ->
  {{EAcc, Env}, K, Outbox};



try_ascend( _ ) ->
  norule.















-spec split_zip( TypedBindLst ) -> Result
when TypedBindLst :: [typed_bind()],
     Result       :: {[typed_bind()], [typed_bind()]}
                   | stalled
                   | null
                   | stuck.

split_zip( TypedBindLst ) ->

  F =
    fun
      ( {X, T, {stalled, {cons, _, Hd, Tl}}}, {L1, L2} ) ->
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
        {[{X, T, Hd1}|L1], [{X, T, Tl1}|L2]};

      ( {_, _, {stalled, _}}, _ ) ->
        throw( stalled );

      ( {X, T, {cons, _, Hd, Tl}}, {L1, L2} ) ->
        {[{X, T, Hd}|L1], [{X, T, Tl}|L2]};

      ( {_, _, {null, _, _}}, _ ) ->
        throw( null );

      ( _, _ ) ->
        throw( stuck )
    end,

  try lists:foldr( F, {[], []}, TypedBindLst ) of
    {L1, L2} -> {L1, L2}
  catch
    throw:stalled -> stalled;
    throw:null    -> null;
    throw:stuck   -> stuck
  end.


-spec bind_all( Info, TypedBindLst, EBody ) -> e()
when Info         :: info(),
     TypedBindLst :: [typed_bind()],
     EBody        :: e().

bind_all( _, [], EBody ) ->
  EBody;

bind_all( Info, [{X1, T1, E1}|EBindLst], EBody ) ->
  Lam = {lam_ntv, Info, [{X1, T1}], EBody},
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


-spec unstall_typed( [typed_bind()] ) -> [typed_bind()].

unstall_typed( EBindLst ) when is_list( EBindLst ) ->

  F =
    fun
      ( {X, T, {stalled, E}} ) -> {X, T, E};
      ( {X, T, E} )            -> {X, T, E}
    end,

  lists:map( F, EBindLst ).