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

-type k() :: mt.

-type env() :: #{ x() => {e(), env()} }.

-type p() :: {[e()], [{e(), e()}], e(), env(), k()}.

-spec step( p() ) -> p().



%% Notion of Reduction

step( {OutBox, InBox, {app, _, {lam, _, XtLst, {ntv, EBody}}, XeLst}, Env, K} ) ->
  F =
    fun( {X, _}, {_, E} ) ->
      {X, {E, Env}}
    end,
  Env1 = maps:merge( Env, maps:from_list( maps:zipwith( F, XtLst, XeLst ) ) ),
  {OutBox, InBox, EBody, Env1, K};

step( {OutBox, InBox, E = {fix, IFix, {lam, ILam, [{X, T}|XtLst], {ntv, EBody}}}, Env, K} ) ->
  EBody1 = {app, IFix, {lam, IFix, [{X, T}], {ntv, EBody}}, [{X, E}]},
  E1 = {lam, ILam, XtLst, {ntv, EBody1}},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {str, _, S1}, {str, _, S2}}, Env, K} ) ->
  E1 = {S1 =:= S2, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 =:= A2, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {null, _, _}, {null, _, _}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {null, _, _}, {cons, _, _, _}}, Env, K} ) ->
  E1 = {false, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {cons, _, _, _}, {null, _, _}}, Env, K} ) ->
  E1 = {false, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {cons, _, E11, E12}, {cons, _, E21, E22}}, Env, K} ) ->
  E1 = {conj, I, {cmp, I, E11, E21}, {cmp, I, E12, E22}},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {rcd, _, []}, {rcd, _, []}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cmp, I, {rcd, I1, [{X, E1}|XeLst1]}, {rcd, I2, XeLst2}, Env, K} ) ->
  {_, E2} = lists:keyfind( X, 1, XeLst2 ),
  E3 = {conj, I, {cmp, I, E1, E2},
                 {cmp, I, {rcd, I1, XeLst1},
                          {rcd, I2, lists:remove( {X, E2}, XeLst2 )}}},
  {OutBox, InBox, E3, Env, K};

step( {OutBox, InBox, {conj, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 and A2, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {disj, I, {A1, _}, {A2, _}}, Env, K} ) ->
  E1 = {A1 or A2, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {neg, I, {A1, _}}, Env, K} ) ->
  E1 = {not A1, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {isnil, I, {null, _, _}}, Env, K} ) ->
  E1 = {true, I},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {cnd, _, {true, _}, E2, E3}, Env, K} ) ->
  {OutBox, InBox, E2, Env, K};

step( {OutBox, InBox, {cnd, _, {false, _}, E2, E3}, Env, K} ) ->
  {OutBox, InBox, E3, Env, K};

step( {OutBox, InBox, {hd, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, E2, Env, K};

step( {OutBox, InBox, {hd, I, {cons, _, E11, _}, _}, Env, K} ) ->
  {OutBox, InBox, E11, Env, K};

step( {OutBox, InBox, {tl, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, E2, Env, K};

step( {OutBox, InBox, {tl, I, {cons, _, _, E12}, _}, Env, K} ) ->
  {OutBox, InBox, E12, Env, K};

step( {OutBox, InBox, {append, I, {null, _, _}, E2}, Env, K} ) ->
  {OutBox, InBox, E2, Env, K};

step( {OutBox, InBox, {append, I, {cons, E11, E12}, E2}, Env, K} ) ->
  E1 = {cons, I, E11, {append, I, E12, E2}},
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {for, I, TBody, XteLst, EBody}, Env, K} ) ->

  AnyNil =
    lists:any( [case E of {null, _, _} -> true; {cons, _, _, _} -> false end || {_, _, E} <- XteLst] ),

  E3 =
    case AnyNil of
      true  -> {null, I, TBody}
      false ->
        XteLst1 = [{X, T, E1} || {X, T, {cons, _, E1, E2}} <- XteLst],
        XteLst2 = [{X, T, E2} || {X, T, {cons, _, E1, E2}} <- XteLst],
        {cons, I, {app, I, {lam, I, [{X, T} || {X, T, _} <- XteLst1],
                                    {ntv, EBody}},
                           [{X, E} || {X, _, E} <- XteLst1]},
                  {for, I, TBody, XteLst2, EBody}}
    end,

  {OutBox, InBox, E3, Env, K};

step( {OutBox, InBox, {fold, _, {_, _, E1}, {_, _, {null, _, _}}, _}, Env, K} ) ->
  {OutBox, InBox, E1, Env, K};

step( {OutBox, InBox, {fold, I, {XAcc, TAcc, EAcc}, {XLst, TLst, {cons, _, E1, E2}}, EBody}, Env, K} ) ->
  EAcc1 = {app, I, {lam, I, [{XAcc, TAcc}, {XLst, TLst}], {ntv, EBody}}, [{XAcc, EAcc}, {XLst, E1}]},
  E3 = {fold, I, {XAcc, TAcc, EAcc1}, {XLst, TLst, E2}, EBody}, 
  {OutBox, InBox, E3, Env, K};

step( {OutBox, InBox, {proj, _, X, {rcd, _, XeLst}}, Env, K} ) ->
  {_, E1} = lists:find( X, 1, XeLst ),
  {OutBox, InBox, E1, Env, K};


%% Send and Receive

step( {OutBox, InBox, E = {app, I, Lam = {lam, _, _, {frn, _, _, _, _}}, XeLst}, Env, K} ) ->

  AllValue =
    lists:all( [is_value( E ) || {_, E} <- XeLst] ),

  case AllValue of
    true  -> {[E|OutBox], InBox, {fut, I, E}, Env, K};
    false ->
      [{X1, E1}|R] = XeLst,
      {OutBox, InBox, E1, Env, {arg, I, Lam, [], X1, R, K}}


%% Evaluation Context

step( {OutBox, InBox, } )
