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

-module(cuneiform_preproc).

%%====================================================================
%% Exports
%%====================================================================

-export([join_stat/2,
         visit_from/3,
         visit_fold/7,
         visit_cnd/6,
         visit_for/5,
         visit_import/1,
         visit_r_var/2,
         visit_var/1,
         visit_file/1,
         visit_str/1,
         visit_assign/3,
         visit_def_frn/6,
         visit_def_ntv/6,
         visit_t_arg/2,
         visit_true/1,
         visit_false/1,
         visit_cmp/3,
         visit_conj/3,
         visit_disj/3,
         visit_neg/2,
         visit_app/2,
         visit_rcd/2,
         visit_proj/2,
         visit_append/3,
         visit_lst/3,
         visit_cons/3,
         visit_isnil/2,
         visit_err/3,
         visit_e_bind/2,
         visit_r_bind/2,
         visit_hd/3,
         visit_tl/3]).

%%====================================================================
%% Includes
%%====================================================================

-include("cuneiform_lang.hrl").

%%====================================================================
%% Imports
%%====================================================================

-import(cuneiform_lang, [t_bool/0, t_fn/2, t_rcd/1]).

-import(cuneiform_lang, [r_var/2, r_rcd/1]).

-import(cuneiform_lang,
        [var/2,
         file/2,
         str/2,
         assign/3,
         true/1,
         false/1,
         expand_closure/2,
         cmp/3,
         conj/3,
         disj/3,
         neg/2,
         cnd/4,
         fix/2,
         lam/3,
         app/3,
         rcd/2,
         proj/3,
         append/3,
         lst/3,
         isnil/2,
         for/4,
         fold/4,
         cons/3,
         err/3,
         hd/3,
         tl/3]).

-import(cuneiform_lang, [ambiguous_names/1, pattern_names/1]).

%%====================================================================
%% Preprocessor functions
%%====================================================================


-spec join_stat(T1, T2) -> {[_], [_], [_]}
              when T1 :: {[_], [_], [_]},
                   T2 :: {[_], [_], [_]}.

join_stat({A1, B1, C1}, {A2, B2, C2}) ->
    {A1 ++ A2, B1 ++ B2, C1 ++ C2}.


-spec visit_from(Id, T, E) -> {x(), t(), e()}
              when Id :: {id, _, S :: string()},
                   T :: t(),
                   E :: e().

visit_from({id, _, S}, T, E) ->
    {list_to_atom(S), T, E}.


-spec visit_fold(Fold, Id, T, E, From, AssignLst, EBody) -> e()
              when Fold :: {fold, L :: pos_integer(), _},
                   Id :: {id, _, S :: string()},
                   T :: t(),
                   E :: e(),
                   From :: {x(), t(), e()},
                   AssignLst :: [assign()],
                   EBody :: e().

visit_fold({fold, L, _}, {id, _, S}, T, E, From, AssignLst, EBody) ->

    AccBind = {list_to_atom(S), T, E},

    C = expand_closure(AssignLst, EBody),
    fold(L, AccBind, From, C).


-spec visit_cnd(Cnd, EIf, DefLstThen, EThen, DefLstElse, EElse) -> e()
              when Cnd :: {cnd, L :: pos_integer(), _},
                   EIf :: e(),
                   DefLstThen :: [assign()],
                   EThen :: e(),
                   DefLstElse :: [assign()],
                   EElse :: e().

visit_cnd({cnd, L, _}, EIf, DefLstThen, EThen, DefLstElse, EElse) ->
    E2 = expand_closure(DefLstThen, EThen),
    E3 = expand_closure(DefLstElse, EElse),
    cnd(L, EIf, E2, E3).


-spec visit_for(For, FromLst, DefLst, E, TRet) -> e()
              when For :: {for, L :: pos_integer(), _},
                   FromLst :: [{x(), t(), e()}],
                   DefLst :: [assign()],
                   E :: e(),
                   TRet :: t().

visit_for({for, L, _}, FromLst, DefLst, E, TRet) ->
    C = expand_closure(DefLst, E),
    for(L, TRet, FromLst, C).


-spec visit_import({filelit, L :: _, S :: string()}) -> {import, _, string()}.

visit_import({filelit, L, S}) ->
    {import, L, S}.


-spec visit_r_var({id, L :: _, S :: string()}, T :: t()) -> r().

visit_r_var({id, _, S}, T) ->
    r_var(list_to_atom(S), T).


-spec visit_var({id, L :: _, Varname :: string()}) -> e().

visit_var({id, L, Varname}) ->
    var(L, list_to_atom(Varname)).


-spec visit_file({filelit, L :: _, S :: string()}) -> e().

visit_file({filelit, L, S}) ->
    file(L, list_to_binary(S)).


-spec visit_str({_, L :: _, S :: string()}) -> e().

visit_str({_, L, S}) ->
    str(L, list_to_binary(S)).


-spec visit_true({true, L :: _, _}) -> e().

visit_true({true, L, _}) ->
    true(L).


-spec visit_false({false, L :: _, _}) -> e().

visit_false({false, L, _}) ->
    false(L).


-spec visit_cmp(E1 :: e(), {cmp, L :: _, _}, E2 :: e()) -> e().

visit_cmp(E1, {cmp, L, _}, E2) ->
    cmp(L, E1, E2).


-spec visit_conj(E1 :: e(), {wedge, L :: _, _}, E2 :: e()) -> e().

visit_conj(E1, {wedge, L, _}, E2) ->
    conj(L, E1, E2).


-spec visit_disj(E1 :: e(), {vee, L :: _, _}, E2 :: e()) -> e().

visit_disj(E1, {vee, L, _}, E2) ->
    disj(L, E1, E2).


-spec visit_neg({neg, L :: _, _}, E :: e()) -> e().

visit_neg({neg, L, _}, E) ->
    neg(L, E).


-spec visit_def_frn(Def, Id, ArgLst, UArgLst, Lang, Body) -> assign()
              when Def :: {def, _, _},
                   Id :: {id, _, string()},
                   ArgLst :: [{x(), t()}],
                   UArgLst :: [{x(), t()}],
                   Lang :: l(),
                   Body :: {body, _, string()}.

visit_def_frn({def, L, _}, {id, _, SName}, ArgLst, UArgLst, Lang, {body, _, SBody}) ->
    BBody = list_to_binary(SBody),
    FName = list_to_atom(SName),
    RetType = t_rcd(UArgLst),
    T = t_fn(ArgLst, RetType),
    R = r_var(FName, T),
    Lam = lam(L, ArgLst, {frn, FName, RetType, Lang, BBody}),
    assign(L, R, Lam).


-spec visit_def_ntv(Def, Id, ArgLst, RetType, AssignLst, EBody) -> assign()
              when Def :: {def, _, _},
                   Id :: {id, _, string()},
                   ArgLst :: [{x(), t()}],
                   RetType :: t(),
                   AssignLst :: [assign()],
                   EBody :: e().

visit_def_ntv({def, L, _},
              {id, _, SName},
              ArgLst,
              RetType,
              AssignLst,
              EBody)
  when is_integer(L),
       is_list(SName),
       is_list(ArgLst),
       is_list(AssignLst) ->

    C = expand_closure(AssignLst, EBody),
    FName = list_to_atom(SName),
    TFn = t_fn(ArgLst, RetType),
    Lam = fix(
            L,
            lam(
              L,
              [{FName, TFn} | ArgLst],
              {ntv, C})),
    R = r_var(FName, TFn),
    assign(L, R, Lam).


-spec visit_t_arg({id, _, S :: string()}, T :: t()) -> {x(), t()}.

visit_t_arg({id, _, S}, T) ->
    {list_to_atom(S), T}.


-spec visit_app({id, L :: _, S :: string()}, EBindLst :: [{x(), e()}]) -> e().

visit_app({id, L, S}, EBindLst) ->
    app(L, var(L, list_to_atom(S)), EBindLst).


-spec visit_e_bind({id, _, S :: string()}, E :: e()) -> {x(), e()}.

visit_e_bind({id, _, S}, E) ->
    {list_to_atom(S), E}.


-spec visit_r_bind({id, _, S :: string()}, R :: r()) -> {x(), r()}.

visit_r_bind({id, _, S}, R) ->
    {list_to_atom(S), R}.


-spec visit_rcd({ltag, L :: _, _}, EBindLst :: [{x(), e()}]) -> e().

visit_rcd({ltag, L, _}, EBindLst) ->
    rcd(L, EBindLst).


-spec visit_proj(E :: e(), {id, L :: _, S :: string()}) -> e().

visit_proj(E, {id, L, S}) ->
    proj(L, list_to_atom(S), E).


-spec visit_append(E :: e(), {plus, L :: _, _}, E :: e()) -> e().

visit_append(E1, {plus, L, _}, E2) ->
    append(L, E1, E2).


-spec visit_lst(ELst :: [e()], {colon, L :: _, _}, T :: t()) -> e().

visit_lst(ELst, {colon, L, _}, T) ->
    lst(L, T, ELst).


-spec visit_isnil({isnil, L :: _, _}, E :: e()) -> e().

visit_isnil({isnil, L, _}, E) ->
    isnil(L, E).


-spec visit_assign({assign, L :: _, _}, R :: r(), E :: e()) -> assign().

visit_assign({assign, L, _}, R, E) ->
    assign(L, R, E).


-spec visit_cons({doublertag, L :: _, _}, E1 :: e(), E2 :: e()) -> e().

visit_cons({doublertag, L, _}, E1, E2) ->
    cons(L, E1, E2).


-spec visit_err({err, L :: _, _}, Msg :: {strlit, _, S :: string()}, T :: t()) -> e().

visit_err({err, L, _}, {strlit, _, S}, T) ->
    Reason = {user, list_to_binary(S)},
    err(L, T, Reason).


-spec visit_hd({hd, L :: _, _}, E1 :: e(), E2 :: e()) -> e().

visit_hd({hd, L, _}, E1, E2) ->
    hd(L, E1, E2).


-spec visit_tl({tl, L :: _, _}, E1 :: e(), E2 :: e()) -> e().

visit_tl({tl, L, _}, E1, E2) ->
    tl(L, E1, E2).
