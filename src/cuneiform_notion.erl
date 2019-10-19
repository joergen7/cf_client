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

-module( cuneiform_notion ).

-export( [notion/1] ).

-include( "cuneiform.hrl" ).

-import( cuneiform_lang, [subst/3] ).

-spec notion( E :: e() ) -> e().

notion( {app, _, {lam, _, [], {ntv, EBody}}, []} ) ->                           % E-beta-base
  EBody;

notion( {app, I1, {lam, I2, [{X, _}|XtLst], {ntv, EBody}}, [{_, E}|XeLst]} ) -> % E-beta-ind
  EBody1 = subst( EBody, X, E ),
  {app, I1, {lam, I2, XtLst, {ntv, EBody1}}, XeLst};

notion( E = {fix, I1, {lam, I2, [{X, T}|XtLst], {ntv, EBody}}} ) ->             % E-fix
  EBody1 = {app, I1, {lam, I1, [{X, T}], {ntv, EBody}}, [{X, E}]},
  {lam, I2, XtLst, {ntv, EBody1}};

notion( {cmp, I, {str, _, S1}, {str, _, S2}} ) ->
  {S1 =:= S2, I};

notion( {cmp, I, {A1, _}, {A2, _}} ) ->
  {A1 =:= A2, I};

notion( {cmp, I, {null, _, _}, {null, _, _}} ) ->
  {true, I};

notion( {cmp, I, {null, _, _}, {cons, _, _, _}} ) ->
  {false, I};

notion( {cmp, I, {cons, _, _, _}, {null, _, _}} ) ->
  {false, I};

notion( {cmp, I, {cons, _, E11, E12}, {cons, _, E21, E22}} ) ->
  {conj, I, {cmp, I, E11, E21}, {cmp, I, E12, E22}};

notion( {cmp, I, {rcd, _, []}, {rcd, _, []}} ) ->
  {true, I};

notion( E = {cmp, I, {rcd, I1, [{X, E1}|XeLst1]}, {rcd, I2, XeLst2}} ) ->
  case lists:keyfind( X, 1, XeLst2 ) of
    {_, E2} ->
      {conj, I, {cmp, I, E1, E2},
                {cmp, I, {rcd, I1, XeLst1},
                         {rcd, I2, lists:delete( {X, E2}, XeLst2 )}}};
    _ -> error( {norule, E} )
  end;

notion( {conj, I, {A1, _}, {A2, _}} ) ->
  {A1 and A2, I};

notion( {disj, I, {A1, _}, {A2, _}} ) ->
  {A1 or A2, I};


notion( {neg, I, {A, _}} ) ->
  {not A, I};

notion( {isnil, I, {null, _, _}} ) ->
  {true, I};

notion( {isnil, I, {cons, _, _, _}} ) ->
  {false, I};

notion( {cnd, _, {true, _}, E2, E3} ) ->
  E2;

notion( {cnd, _, {false, _}, E2, E3} ) ->
  E3;

notion( {hd, I, {null, _, _}, E2} ) ->
  E2;

notion( {hd, I, {cons, _, E11, _}, _} ) ->
  E11;

notion( {tl, I, {null, _, _}, E2} ) ->
  E2;

notion( {tl, I, {cons, _, _, E12}, _} ) ->
  E12;

notion( {append, I, {null, _, _}, E2} ) ->
  E2;

notion( {append, I, {cons, E11, E12}, E2} ) ->
  {cons, I, E11, {append, I, E12, E2}};

notion( {for, I, TBody, XteLst, EBody} ) ->

  AnyNil =
    lists:any( [case E of {null, _, _} -> true; {cons, _, _, _} -> false end || {_, _, E} <- XteLst] ),

  case AnyNil of
    true  -> {null, I, TBody}
    false ->
      XteLst1 = [{X, T, E1} || {X, T, {cons, _, E1, E2}} <- XteLst],
      XteLst2 = [{X, T, E2} || {X, T, {cons, _, E1, E2}} <- XteLst],
      {cons, I, {app, I, {lam, I, [{X, T} || {X, T, _} <- XteLst1],
                                  {ntv, EBody}},
                         [{X, E} || {X, _, E} <- XteLst1]},
                {for, I, TBody, XteLst2, EBody}}
  end;


notion( {fold, _, {_, _, E1}, {_, _, {null, _, _}}, _} ) ->
  E1;

notion( {fold, I, {X1, T1, E1}, {X2, T2, {cons, _, E21, E22}}, EBody} ) ->
  Acc1 = {app, I, {lam, I, [{X1, T1}, {X2, T2}], EBody}, [{X1, E1}, {X2, E21}]},
  {fold, I, {X1, T1, Acc1}, {X2, T2, E22}, EBody};



notion( E ) -> error( {norule, E} ).