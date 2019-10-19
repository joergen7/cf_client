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

-module( cuneiform_cek ).

-export( [] ).

-include( "cuneiform.hrl" ).

-type k() :: {app_arg, info(), e(), [{x(), e()}], x(), [{x(), e()}]}
           | {app_fn, info(), [x(), e()]}
           | {fix_op, info()}
           | {cmp_lhs, info(), e(), env()}
           | {cmp_rhs, info(), e()}
           | {conj_lhs, info(), e(), env()}
           | {conj_rhs, info(), e()}
           | {disj_lhs, info(), e(), env()}
           | {disj_rhs, info(), e()}
           | {neg_op, info()}
           | {isnil_op, info()}
           | {cnd_pred, info(), e(), e()}
           | {cons_lhs, info(), e(), env()}
           | {cons_rhs, info(), e()}
           | {hd_op, info(), e()}
           | {tl_op, info(), e()}
           | {append_lhs, info(), e(), env()}
           | {append_rhs, info(), e()}

-type env() :: #{ x() => {e(), env()} }.

-type dir() :: up | down.

-type p() :: {[e()], #{e() => e()}, dir(), e(), env(), [k()]}.

-spec step( p() ) -> p().



%% Notion of Reduction

step( {OutBox, InBox, down, {app, _, {lam, _, XtLst, {ntv, EBody}}, XeLst}, Env, K} ) ->
  F =
    fun( {X, _}, {_, E} ) ->
      {X, {E, Env}}
    end,
  Env1 = maps:merge( Env, maps:from_list( maps:zipwith( F, XtLst, XeLst ) ) ),
  {OutBox, InBox, down, EBody, Env1, K};

step( {OutBox, InBox, down, E = {fix, IFix, {lam, ILam, [{X, T}|XtLst], {ntv, EBody}}}, Env, K} ) ->
  EBody1 = {app, IFix, {lam, IFix, [{X, T}], {ntv, EBody}}, [{X, E}]}, %%%% TODO: substitute environment in EBody %%%%%%%
  E1 = {lam, ILam, XtLst, {ntv, EBody1}},
  {OutBox, InBox, up, E1, #{}, K}; %% because here we drop the environment but the body expression might take from the closure

step( {OutBox, InBox, down, {cmp, I, {str, _, S1}, {str, _, S2}}, Env, K} ) ->
  E1 = {S1 =:= S2, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 =:= A2, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {null, _, _}, {null, _, _}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {null, _, _}, {cons, _, _, _}}, Env, K} ) ->
  E1 = {false, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {cons, _, _, _}, {null, _, _}}, Env, K} ) ->
  E1 = {false, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {cons, _, E11, E12}, {cons, _, E21, E22}}, Env, K} ) ->
  E1 = {conj, I, {cmp, I, E11, E21}, {cmp, I, E12, E22}},
  {OutBox, InBox, down, E1, Env, K};

step( {OutBox, InBox, down, {cmp, I, {rcd, _, []}, {rcd, _, []}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cmp, I, {rcd, I1, [{X, E1}|XeLst1]}, {rcd, I2, XeLst2}}, Env, K} ) ->
  {_, E2} = lists:keyfind( X, 1, XeLst2 ),
  E3 = {conj, I, {cmp, I, E1, E2},
                 {cmp, I, {rcd, I1, XeLst1},
                          {rcd, I2, lists:remove( {X, E2}, XeLst2 )}}},
  {OutBox, InBox, down, E3, Env, K};

step( {OutBox, InBox, down, {conj, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 and A2, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {disj, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 or A2, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {neg, I, {A1, _}}, Env, K} ) ->
  E1 = {not A1, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {isnil, I, {null, _, _}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, up, E1, #{}, K};

step( {OutBox, InBox, down, {cnd, _, {true, _}, E2, E3}, Env, K} ) ->
  {OutBox, InBox, down, E2, Env, K};

step( {OutBox, InBox, down, {cnd, _, {false, _}, E2, E3}, Env, K} ) ->
  {OutBox, InBox, down, E3, Env, K};

step( {OutBox, InBox, down, {hd, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, down, E2, Env, K};

step( {OutBox, InBox, down, {hd, I, {cons, _, E11, _}, _}, Env, K} ) ->
  {OutBox, InBox, down, E11, Env, K};

step( {OutBox, InBox, down, {tl, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, down, E2, Env, K};

step( {OutBox, InBox, down, {tl, I, {cons, _, _, E12}, _}, Env, K} ) ->
  {OutBox, InBox, down, E12, Env, K};

step( {OutBox, InBox, down, {append, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, down, E2, Env, K};

step( {OutBox, InBox, down, {append, I, {cons, E11, E12}, E2}, Env, K} ) ->
  E1 = {cons, I, E11, {append, I, E12, E2}},
  {OutBox, InBox, down, E1, Env, K};

step( {OutBox, InBox, down, {for, I, TBody, XteLst, EBody}, Env, K} ) ->

  IsNil =
    fun
      ( {null, _, _} ) -> true;
      ( _ )            -> false
    end,

  IsCons =
    fun
      ( {cons, _, _, _} ) -> true;
      ( _ )               -> false
    end,

  AnyNil =
    lists:any( [IsNil( E ) || {_, _, E} <- XteLst] ),

  case AnyNil of

    true ->
      {OutBox, InBox, up, {null, I, TBody}, #{}, K};

    false ->

      AllCons =
        lists:all( [IsCons( E ) || {_, _, E} <- XteLst] ),

      case AllCons of

        false ->
          {OutBox, InBox, down, } %% TODO

        true ->
          XteLst1 = [{X, T, E1} || {X, T, {cons, _, E1, E2}} <- XteLst],
          XteLst2 = [{X, T, E2} || {X, T, {cons, _, E1, E2}} <- XteLst],
          E3 = {cons, I, {app, I, {lam, I, [{X, T} || {X, T, _} <- XteLst1],
                                           {ntv, EBody}},
                                  [{X, E} || {X, _, E} <- XteLst1]},
                         {for, I, TBody, XteLst2, EBody}},
          {OutBox, InBox, down, E3, Env, K}

      end

  end;

step( {OutBox, InBox, down, {fold, _, {_, _, E1}, {_, _, {null, _, _}}, _}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, K};

step( {OutBox, InBox, down, {fold, I, {XAcc, TAcc, EAcc}, {XLst, TLst, {cons, _, E1, E2}}, EBody}, Env, K} ) ->
  EAcc1 = {app, I, {lam, I, [{XAcc, TAcc}, {XLst, TLst}], {ntv, EBody}}, [{XAcc, EAcc}, {XLst, E1}]},
  E3 = {fold, I, {XAcc, TAcc, EAcc1}, {XLst, TLst, E2}, EBody}, 
  {OutBox, InBox, down, E3, Env, K};

step( {OutBox, InBox, down, {proj, _, X, {rcd, _, XeLst}}, Env, K} ) ->
  {_, E1} = lists:find( X, 1, XeLst ),
  {OutBox, InBox, down, E1, Env, K};


%% Send and Receive

step( {OutBox,
       InBox,
       down,
       E = {app, I, Lam = {lam, _, _, {frn, _, _, _, _}}, XeLst},
       Env,
       K} ) ->

  AllValue =
    lists:all( [is_value( E ) || {_, E} <- XeLst] ),

  case AllValue of
    true  ->
      {[E|OutBox], InBox, up, {fut, I, E}, #{}, K};
    false ->
      [{X1, E1}|R] = XeLst,
      {OutBox, InBox, down, E1, Env, [{app_arg, I, Lam, [], X1, R}|K]}
  end;

step( {OutBox, InBox, down, {fut, I, E}, Env, K} ) ->
  case maps:get( E, InBox, no_entry ) of
    no_entry -> {OutBox, InBox, up, {fut, I, E}, #{}, K};
    E1       -> {OutBox, InBox, down, E1, #{}, K}
  end;


%% Errors

step( {OutBox, InBox, down, E = {err, _, _, _}, Env, [_|_]} ) ->
  {OutBox, InBox, up, E, #{}, []};


%% Descend into Evaluation Context

step( {OutBox, InBox, down, {app, I, EFn, XeLst}, Env, K} ) ->
  {OutBox, InBox, down, EFn, Env, [{app_fn, I, XeLst}|K]};

step( {OutBox, InBox, down, {fix, I, EOp}, Env, K} ) ->
  {OutBox, InBox, down, EOp, Env, [{fix_op, I}|K]};

step( {OutBox, InBox, down, {cmp, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{cmp_lhs, I, E2, Env}|K]};

step( {OutBox, InBox, up, E1, _, [{cmp_lhs, I, E2, Env}|K]} ) ->
  {OutBox, InBox, down, E2, Env, [{cmp_rhs, I, E1}|K]};

step( {OutBox, InBox, down, {conj, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{conj_lhs, I, E2, Env}|K]};

step( {OutBox, InBox, up, E1, _, [{conj_lhs, I, E2, Env}|K]} ) ->
  {OutBox, InBox, down, E2, Env, [{conj_rhs, I, E1}|K]};

step( {OutBox, InBox, down, {disj, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{disj_lhs, I, E2, Env}|K]};

step( {OutBox, InBox, up, E1, _, [{disj_lhs, I, E2, Env}|K]} ) ->
  {OutBox, InBox, down, E2, Env, [{disj_rhs, I, E1}|K]};

step( {OutBox, InBox, down, {neg, I, EOp}, Env, K} ) ->
  {OutBox, InBox, down, EOp, Env, [{neg_op, I}|K]};

step( {OutBox, InBox, down, {isnil, I, EOp}, Env, K} ) ->
  {OutBox, InBox, down, EOp, Env, [{isnil_op, I}|K]};

step( {OutBox, InBox, down, {cnd, I, E1, E2, E3}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{cnd_pred, I, E2, E3}|K]};

step( {OutBox, InBox, down, {cons, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{cons_lhs, I, E2, Env}|K]};

step( {OutBox, InBox, up, E1, _, [{cons_lhs, I, E2, Env}|K]} ) ->
  {OutBox, InBox, down, E2, Env, [{cons_rhs, I, E1}|K]};

step( {OutBox, InBox, down, {hd, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{hd_op, I, E2}|K]};

step( {OutBox, InBox, down, {tl, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{tl_op, I, E2}|K]};

step( {OutBox, InBox, down, {append, I, E1, E2}, Env, K} ) ->
  {OutBox, InBox, down, E1, Env, [{append_lhs, I, E2, Env}|K]};

step( {OutBox, InBox, up, E1, _, [{append_lhs, I, E2, Env}|K]} ) ->
  {OutBox, InBox, down, E2, Env, [{append_rhs, I, E1}|K]};

%% TODO: continue going down


%% Acend from Evaluation Context

%% TODO: go up again




step( OutBox, InBox, up, _, _, [] ) ->
  throw( norule ).