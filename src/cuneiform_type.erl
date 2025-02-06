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

-module(cuneiform_type).

-include("cuneiform_lang.hrl").
-include("cuneiform_type.hrl").

-import(cuneiform_lang,
        [ambiguous_names/1,
         xt_names/1,
         xte_names/1,
         xe_names/1]).

-export([type/1, type/2, is_type_comparable/1, is_type_equivalent/2]).

%%====================================================================
%% Typing relation
%%====================================================================


-spec type(E :: e()) -> {ok, t()} | {error, type_error()}.

type(E) ->
    type(#{}, E).


-spec type(Gamma, E) -> {ok, t()} | {error, type_error()}
              when Gamma :: gamma(),
                   E :: e().

type(Gamma, {var, Info, X}) ->  % T-var
    case maps:is_key(X, Gamma) of
        true -> {ok, maps:get(X, Gamma)};
        false -> {error, {unbound_var, Info, X}}
    end;

type(Gamma, {lam, Info, ArgLst, {ntv, EBody}}) ->  % T-lambda-ntv

    case ambiguous_names(xt_names(ArgLst)) of

        [] ->
            Gamma1 = maps:from_list(ArgLst),
            Gamma2 = maps:merge(Gamma, Gamma1),
            case type(Gamma2, EBody) of
                {ok, TBody} -> {ok, {'Fn', ArgLst, TBody}};
                {error, R} -> {error, R}
            end;

        Lst ->
            {error, {ntv_fn_ambiguous_arg_name, Info, Lst}}

    end;

type(_Gamma, {lam, Info, ArgLst, {frn, _FName, TRet, Lang, _SBody}}) ->  % t-lambda-frn

    try

        FieldLst =
            case TRet of
                {'Rcd', L} -> L;
                _ -> throw({frn_fn_returns_no_rcd, Info, TRet})
            end,

        NameLst = xt_names(ArgLst) ++ xt_names(FieldLst),

        ok =
            case ambiguous_names(NameLst) of
                [] -> ok;
                Lst -> throw({frn_fn_ambiguous_arg_or_return_field_name, Info, Lst})
            end,

        ok =
            case Lang of

                'Awk' ->

                    ok =
                        case ArgLst of
                            [{_, 'File'} | _] -> ok;
                            [{X1, T1} | _] -> throw({awk_frn_fn_first_arg_no_file, Info, {X1, T1}});
                            [] -> throw({awk_frn_fn_no_arg, Info})
                        end,

                    case lists:keyfind('result', 1, FieldLst) of
                        {_, 'File'} -> ok;
                        {_, T2} -> throw({awk_frn_fn_result_field_no_file, Info, T2});
                        false -> throw({awk_frn_fn_no_result_field, Info})
                    end;

                _ ->
                    ok

            end,

        {ok, {'Fn', ArgLst, TRet}}

    catch
        throw:R -> {error, R}
    end;

type(Gamma, {app, Info, F, BindLst}) ->  % T-app

    case type(Gamma, F) of

        {ok, {'Fn', ArgLst, TRet}} ->
            case check_argument_binding(Gamma, Info, ArgLst, BindLst) of
                {error, Reason2} -> {error, Reason2};
                ok -> {ok, TRet}
            end;

        {ok, T} ->
            {error, {app_lhs_no_function, Info, {F, T}}};

        {error, Reason1} ->
            {error, Reason1}

    end;

type(Gamma, {fix, Info, E}) ->  % T-fix
    case type(Gamma, E) of

        {error, Reason} ->
            {error, Reason};

        {ok, T4 = {'Fn', [], _}} ->
            {error, {fix_fn_no_arg, Info, {E, T4}}};

        {ok, {'Fn', [Xt | XtLst1], TRet1}} ->
            TFn1 = {'Fn', XtLst1, TRet1},
            {X, TFn2} = Xt,

            case TFn2 of
                {'Fn', _, TRet2} ->
                    case is_type_equivalent(TRet1, TRet2) of
                        false -> {error, {fix_return_type_mismatch, Info, {TRet2, TRet1}}};
                        true ->
                            case is_type_equivalent(TFn1, TFn2) of
                                true -> {ok, TFn1};
                                false -> {error, {fix_fn_arg_type_mismatch, Info, {X, TFn1, TFn2}}}
                            end
                    end;
                _ ->
                    {error, {fix_fn_arg_no_fn, Info, {X, TFn2}}}
            end;

        {ok, T5} ->
            {error, {fix_no_fn, Info, {E, T5}}}

    end;

type(_Gamma, {fut, _Info, T, _H}) ->
    {ok, T};

type(_Gamma, {err, _Info, T, _Reason}) ->
    {ok, T};

type(_Gamma, {str, _Info, _S}) ->  % T-str
    {ok, 'Str'};

type(_Gamma, {file, _Info, _S}) ->  % T-file
    {ok, 'File'};

type(_Gamma, {true, _Info}) ->  % T-true
    {ok, 'Bool'};

type(_Gamma, {false, _Info}) ->  % T-false
    {ok, 'Bool'};

type(Gamma, {cmp, Info, E1, E2}) ->  % T-cmp
    case type(Gamma, E1) of
        {error, Reason1} -> {error, Reason1};
        {ok, T1} ->
            case is_type_comparable(T1) of
                false -> {error, {cmp_no_comparable_type, Info, {E1, T1}}};
                true ->
                    case type(Gamma, E2) of
                        {error, Reason2} -> {error, Reason2};
                        {ok, T2} ->
                            case is_type_equivalent(T1, T2) of
                                true -> {ok, 'Bool'};
                                false -> {error, {cmp_incomparable, Info, {E1, T1, E2, T2}}}
                            end
                    end
            end
    end;

type(Gamma, {conj, Info, E1, E2}) ->  % T-conj

    case type(Gamma, E1) of

        {ok, 'Bool'} ->
            case type(Gamma, E2) of
                {ok, 'Bool'} -> {ok, 'Bool'};
                {ok, T2} -> {error, {conj_rhs_no_bool, Info, {E2, T2}}};
                {error, Reason2} -> {error, Reason2}
            end;

        {ok, T1} ->
            {error, {conj_lhs_no_bool, Info, {E1, T1}}};

        {error, Reason1} ->
            {error, Reason1}

    end;

type(Gamma, {disj, Info, E1, E2}) ->  % T-disj

    case type(Gamma, E1) of

        {ok, 'Bool'} ->
            case type(Gamma, E2) of
                {ok, 'Bool'} -> {ok, 'Bool'};
                {ok, T2} -> {error, {disj_rhs_no_bool, Info, {E2, T2}}};
                {error, Reason2} -> {error, Reason2}
            end;

        {ok, T1} ->
            {error, {disj_lhs_no_bool, Info, {E1, T1}}};

        {error, Reason1} ->
            {error, Reason1}

    end;

type(Gamma, {neg, Info, E}) ->  % T-neg
    case type(Gamma, E) of

        {ok, 'Bool'} ->
            {ok, 'Bool'};

        {ok, T} ->
            {error, {neg_no_bool, Info, {E, T}}};

        {error, Reason} ->
            {error, Reason}

    end;

type(Gamma, {isnil, Info, E}) ->  % T-isnil
    case type(Gamma, E) of

        {ok, {'Lst', _}} ->
            {ok, 'Bool'};

        {ok, T} ->
            {error, {isnil_no_list, Info, {E, T}}};

        {error, Reason} ->
            {error, Reason}

    end;

type(Gamma, {cnd, Info, E1, E2, E3}) ->  % T-if
    case type(Gamma, E1) of
        {ok, 'Bool'} ->
            case type(Gamma, E2) of
                {error, Reason2} -> {error, Reason2};
                {ok, T2} ->
                    case type(Gamma, E3) of
                        {error, Reason3} -> {error, Reason3};
                        {ok, T3} ->
                            case is_type_equivalent(T2, T3) of
                                false -> {error, {cnd_result_type_mismatch, Info, {E2, T2, E3, T3}}};
                                true -> {ok, T2}
                            end
                    end
            end;

        {ok, T1} ->
            {error, {cnd_test_no_bool, Info, {E1, T1}}};

        {error, Reason1} ->
            {error, Reason1}

    end;

type(_Gamma, {null, _Info, T}) ->  % T-nil
    {ok, {'Lst', T}};

type(Gamma, {cons, Info, E1, E2}) ->  % T-cons

    case type(Gamma, E1) of
        {error, Reason} -> {error, Reason};
        {ok, T1} ->
            case type(Gamma, E2) of
                {ok, {'Lst', T2}} ->
                    case is_type_equivalent(T1, T2) of
                        false -> {error, {cons_element_type_mismatch, Info, {T2, E1, T1}}};
                        true -> {ok, {'Lst', T2}}
                    end;
                {ok, T2} -> {error, {cons_no_list, Info, {E2, T2}}};
                {error, Reason1} -> {error, Reason1}
            end
    end;

type(Gamma, {hd, Info, E1, E2}) ->  % T-hd

    case type(Gamma, E1) of
        {ok, {'Lst', T1}} ->
            case type(Gamma, E2) of
                {error, Reason2} -> {error, Reason2};
                {ok, T2} ->
                    case is_type_equivalent(T1, T2) of
                        false -> {error, {hd_type_mismatch, Info, {T1, E2, T2}}};
                        true -> {ok, T1}
                    end
            end;
        {ok, T1} -> {error, {hd_no_list, Info, {E1, T1}}};
        {error, Reason1} -> {error, Reason1}
    end;

type(Gamma, {tl, Info, E1, E2}) ->  % T-tl

    case type(Gamma, E1) of
        {ok, {'Lst', T1}} ->
            case type(Gamma, E2) of
                {error, Reason2} -> {error, Reason2};
                {ok, T2} ->
                    case is_type_equivalent(T1, T2) of
                        false -> {error, {tl_type_mismatch, Info, {T1, E2, T2}}};
                        true -> {ok, T1}
                    end
            end;
        {ok, T5} -> {error, {tl_no_list, Info, {E1, T5}}};
        {error, Reason1} -> {error, Reason1}
    end;

type(Gamma, {append, Info, E1, E2}) ->  % T-append

    case type(Gamma, E1) of

        {ok, {'Lst', T1}} ->
            case type(Gamma, E2) of
                {ok, {'Lst', T2}} ->
                    case is_type_equivalent(T1, T2) of
                        false -> {error, {append_element_type_mismatch, Info, {T1, T2}}};
                        true -> {ok, {'Lst', T1}}
                    end;
                {ok, T2} -> {error, {append_rhs_no_list, Info, {E2, T2}}};
                {error, Reason2} -> {error, Reason2}
            end;

        {ok, T1} ->
            {error, {append_lhs_no_list, Info, {E1, T1}}};

        {error, Reason1} ->
            {error, Reason1}

    end;

type(Gamma, {for, Info, TRet, XteLst, EBody}) ->  % T-for

    NameLst = xte_names(XteLst),

    case ambiguous_names(NameLst) of

        [] ->
            case check_for_binding(Gamma, Info, XteLst) of
                {error, Reason1} -> {error, Reason1};
                ok ->
                    Gamma1 = maps:from_list([ {X, T} || {X, T, _E} <- XteLst ]),
                    Gamma2 = maps:merge(Gamma, Gamma1),
                    case type(Gamma2, EBody) of
                        {error, Reason2} -> {error, Reason2};
                        {ok, TBody} ->
                            case is_type_equivalent(TRet, TBody) of
                                false -> {error, {for_body_type_mismatch, Info, {TRet, EBody, TBody}}};
                                true -> {ok, {'Lst', TRet}}
                            end
                    end
            end;

        Lst ->
            {error, {for_ambiguous_bind_name, Info, Lst}}

    end;

type(_Gamma, {fold, Info, {X, _TAcc, _EAcc}, {X, _TLst, _ELst}, _EBody}) ->  % T-fold
    {error, {fold_ambiguous_bind_name, Info, X}};

type(Gamma, {fold, Info, {X1, T1, E1}, {X2, T2, E2}, EBody}) ->

    case type(Gamma, E1) of
        {error, Reason1} -> {error, Reason1};
        {ok, T3} ->
            case is_type_equivalent(T1, T3) of
                false -> {error, {fold_acc_bind_type_mismatch, Info, {X1, T1, E1, T3}}};
                true ->
                    case type(Gamma, E2) of

                        {ok, {'Lst', T4}} ->
                            case is_type_equivalent(T2, T4) of
                                false -> {error, {fold_list_bind_type_mismatch, Info, {X2, T2, E2, T4}}};
                                true ->
                                    case type(Gamma#{X1 => T1, X2 => T2}, EBody) of
                                        {error, ReasonBody} -> {error, ReasonBody};
                                        {ok, TBody} ->
                                            case is_type_equivalent(T1, TBody) of
                                                false -> {error, {fold_body_type_mismatch, Info, {T1, EBody, TBody}}};
                                                true -> {ok, T1}
                                            end
                                    end
                            end;

                        {ok, T5} ->
                            {error, {fold_list_bind_no_list, Info, {{'Lst', T2}, E2, T5}}};

                        {error, Reason2} ->
                            {error, Reason2}

                    end
            end
    end;

type(Gamma, {rcd, Info, BindLst}) ->  % T-rcd
    case ambiguous_names(xe_names(BindLst)) of
        [] -> type_unambiguous_rcd_binding(Gamma, BindLst);
        Lst -> {error, {rcd_ambiguous_field_name, Info, Lst}}
    end;

type(Gamma, {proj, Info, X, E}) ->  % T-pi

    case type(Gamma, E) of

        {ok, {'Rcd', TArgLst}} ->
            case lists:keyfind(X, 1, TArgLst) of
                {X, T} -> {ok, T};
                false -> {error, {proj_field_missing, Info, X}}
            end;

        {ok, T} ->
            {error, {proj_no_record, Info, {E, T}}};

        {error, Reason} ->
            {error, Reason}

    end;

type(_Gamma, {close, _, E, Env}) ->
    type(env_to_gamma(Env), E).


%%====================================================================
%% Internal functions
%%====================================================================


-spec type_unambiguous_rcd_binding(Gamma, XeLst) -> {ok, t()} | {error, type_error()}
              when Gamma :: #{x() => t()},
                   XeLst :: [{x(), e()}].

type_unambiguous_rcd_binding(_Gamma, []) ->
    {ok, {'Rcd', []}};

type_unambiguous_rcd_binding(Gamma, [{X, E} | Tl]) ->
    case type_unambiguous_rcd_binding(Gamma, Tl) of
        {ok, {'Rcd', XtLst}} ->
            case type(Gamma, E) of
                {ok, T} -> {ok, {'Rcd', [{X, T} | XtLst]}};
                {error, Reason2} -> {error, Reason2}
            end;
        {error, Reason1} -> {error, Reason1}
    end.


-spec check_for_binding(Gamma, Info, XteLst) -> ok | {error, type_error()}
              when Gamma :: #{x() => t()},
                   Info :: info(),
                   XteLst :: [{x(), t(), e()}].

check_for_binding(_Gamma, _Info, []) ->
    ok;

check_for_binding(Gamma, Info, [{X, T, E} | Tl]) ->

    case type(Gamma, E) of

        {ok, {'Lst', T1}} ->
            case is_type_equivalent(T, T1) of
                false -> {error, {for_bind_type_mismatch, Info, {X, T, E, T1}}};
                true -> check_for_binding(Gamma, Info, Tl)
            end;

        {ok, T2} ->
            {error, {for_bind_no_list, Info, {{'Lst', T}, E, T2}}};

        {error, Reason} ->
            {error, Reason}

    end.


-spec check_argument_binding(Gamma, Info, TArgLst, EBindLst) -> Result
              when Gamma :: #{x() => t()},
                   Info :: info(),
                   TArgLst :: [{x(), t()}],
                   EBindLst :: [{x(), e()}],
                   Result :: ok | {error, type_error()}.

check_argument_binding(_Gamma, _Info, [], []) ->
    ok;

check_argument_binding(_Gamma, Info, XtLst = [_ | _], []) ->
    {error, {app_missing_bind, Info, XtLst}};

check_argument_binding(_Gamma, Info, [], XeLst = [_ | _]) ->
    {error, {app_dangling_bind, Info, XeLst}};

check_argument_binding(Gamma, Info, [{X1, T1} | Tl1], [{X1, E1} | Tl2]) ->

    case type(Gamma, E1) of

        {ok, T2} ->
            case is_type_equivalent(T1, T2) of
                true -> check_argument_binding(Gamma, Info, Tl1, Tl2);
                false -> {error, {app_bind_type_mismatch, Info, {X1, T1, E1, T2}}}
            end;

        {error, Reason} ->
            {error, Reason}

    end;

check_argument_binding(_Gamma, Info, [{X, _} | _], [{Y, _} | _]) ->
    {error, {app_arg_name_mismatch, Info, {X, Y}}}.


-spec is_type_comparable(T :: t()) -> boolean().

is_type_comparable('Str') -> true;
is_type_comparable('Bool') -> true;
is_type_comparable({'Lst', T}) -> is_type_comparable(T);
is_type_comparable({'Rcd', XtLst}) -> lists:all(fun({_, T}) -> is_type_comparable(T) end, XtLst);
is_type_comparable(_) -> false.


-spec is_type_equivalent(T1 :: t(), T2 :: t()) -> boolean().

is_type_equivalent('Str', 'Str') -> true;
is_type_equivalent('File', 'File') -> true;
is_type_equivalent('Bool', 'Bool') -> true;

is_type_equivalent({'Fn', XtLst1, TRet1}, {'Fn', XtLst2, TRet2}) ->

    F =
        fun({X1, T1}, {X1, T2}) -> is_type_equivalent(T1, T2);
           (_, _) -> false
        end,

    case length(XtLst1) =:= length(XtLst2) of
        false -> false;
        true ->

            L = lists:zipwith(F, XtLst1, XtLst2),

            lists:foldl(fun(Acc, E) -> Acc and E end, true, L) andalso
            is_type_equivalent(TRet1, TRet2)

    end;

is_type_equivalent({'Lst', T1}, {'Lst', T2}) ->
    is_type_equivalent(T1, T2);

is_type_equivalent({'Rcd', []}, {'Rcd', []}) ->
    true;

is_type_equivalent({'Rcd', [{X1, T1} | XtLst1]}, {'Rcd', XtLst2}) ->
    case lists:keyfind(X1, 1, XtLst2) of
        Xt = {_, T2} ->
            case is_type_equivalent(T1, T2) of
                true ->
                    is_type_equivalent({'Rcd', XtLst1},
                                       {'Rcd', lists:delete(Xt, XtLst2)});
                false -> false
            end;
        false -> false
    end;

is_type_equivalent(_, _) ->
    false.


-spec env_to_gamma(Env :: env()) -> gamma().

env_to_gamma(Env) ->

    F =
        fun(_X, {E1, Env1}) ->
                case type(env_to_gamma(Env1), E1) of
                    {ok, T} -> T;
                    {error, Reason} -> {error, Reason}
                end
        end,

    maps:map(F, Env).
