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

-module( cuneiform_type ).

-include( "cuneiform.hrl" ).

-import( cuneiform_lang, [t_bool/0,
                          t_file/0,
                          t_fn/2,
                          t_lst/1,
                          t_rcd/1,
                          t_str/0] ).
-import( cuneiform_lang, [ambiguous_names/1,
                          xt_names/1,
                          xte_names/1,
                          xe_names/1] ).

-export( [type/1, type/2] ).



%%====================================================================
%% Typing relation
%%====================================================================

-spec type( E :: e() ) -> {ok, t()} | {error, type_error()}.

type( E ) ->
  type( #{}, E ).


-spec type( Gamma, E ) -> {ok, t()} | {error, type_error()}
when Gamma :: #{ x() => t() },
     E     :: e().

type( Gamma, {var, Info, X} ) ->                                                % T-var
  case maps:is_key( X, Gamma ) of
    true  -> {ok, maps:get( X, Gamma )};
    false -> {error, {unbound_var, Info, X}}
  end;

type( Gamma, {lam, Info, ArgLst, {ntv, EBody}} ) ->                             % T-lambda-ntv

  case ambiguous_names( xt_names( ArgLst ) ) of

    [] ->
      Gamma1 = maps:from_list( ArgLst ),
      Gamma2 = maps:merge( Gamma, Gamma1 ),
      type( Gamma2, EBody );

    Lst ->
      {error, {ambiguous_name, Info, Lst}}

  end;

type( _Gamma, {lam_frn, Info, ArgLst, {frn, _FName, TRet, Lang, _SBody}} ) ->   % t-lambda-frn

  {'Rcd', FieldLst} = TRet,
  NameLst = xt_names( ArgLst )++xt_names( FieldLst ),

  try

    ok =
      case ambiguous_names( NameLst ) of
        []    -> ok;
        Lst -> throw( {frn_fn_ambiguous_arg_or_return_field_name, Info, Lst} )
      end,

    ok =
      case Lang of

        'Awk' ->

          ok =
            case ArgLst of
              [{_, 'File'}|_] -> ok;
              [{X1, T1}|_]    -> throw( {awk_frn_fn_first_arg_no_file, Info, {X1, T1}} );
              []              -> throw( {awk_frn_fn_no_arg, Info} )
            end,

          case lists:keyfind( result, 1, FieldLst ) of
            {result, 'File'} -> ok;
            {result, T2}     -> throw( {awk_frn_fn_result_field_no_file, Info, T2} );
            false            -> throw( {awk_frn_fn_no_result_field, Info} )
          end;
            
        _ ->
          ok

      end,

    {ok, t_fn( ArgLst, TRet )}

  catch
    throw:R -> {error, R}
  end;

type( Gamma, {app, Info, F, BindLst} ) ->                                       % T-app

  case type( Gamma, F ) of
    
    {error, Reason1} ->
      {error, Reason1};
    
    {ok, {'Fn', ArgLst, TRet}} ->
      case check_argument_binding( Gamma, Info, ArgLst, BindLst ) of
        {error, Reason2} -> {error, Reason2};
        ok               -> {ok, TRet}
      end

  end;

type( Gamma, {fix, Info, E} ) ->                                                % T-fix
  case type( Gamma, E ) of

    {error, Reason} ->
      {error, Reason};

    {ok, T4={'Fn', [], _}} ->
      {error, {fix_fn_no_arg, Info, {E, T4}}};

    {ok, {'Fn', [Arg1|ArgLst], TRet}} ->
      TFix = t_fn( ArgLst, TRet ),
      case Arg1 of
        {_, TFix} -> {ok, TFix};
        {X, T1}   -> {error, {fix_fn_arg_type_mismatch, Info, {X, TFix, T1}}}
      end;

    {ok, T5} ->
      {error, {fix_no_fn, Info, {E, T5}}}

  end;

type( _Gamma, {fut, _Info, {app, _, {lam, _, _, {frn, _, TRet, _, _}}, _}} ) ->
  {ok, TRet};

type( _Gamma, {err, _Info, T, _Reason} ) ->
  {ok, T};

type( _Gamma, {str, _Info, _S} ) ->                                             % T-str
  {ok, t_str()};

type( _Gamma, {file, _Info, _S} ) ->                                            % T-file
  {ok, t_file()};

type( _Gamma, {true, _Info} ) ->                                                % T-true
  {ok, t_bool()};

type( _Gamma, {false, _Info} ) ->                                               % T-false
  {ok, t_bool()};

type( Gamma, {cmp, Info, E1, E2} ) ->                                           % T-cmp

  case type( Gamma, E1 ) of

    {ok, T1} ->
      case type( Gamma, E2 ) of

        {ok, T2} ->
          case is_comparable( T1, T2 ) of
            true  -> {ok, t_bool()};
            false -> {error, {cmp_incomparable, Info, {E1, T1, E2, T2}}}
          end;

        {error, Reason2} ->
          {error, Reason2}

      end;

    {error, Reason1} ->
      {error, Reason1}
  end;

type( Gamma, {conj, Info, E1, E2} ) ->                                          % T-conj

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {conj_rhs_no_bool, Info, {E2, T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {conj_lhs_no_bool, Info, {E1, T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {disj, Info, E1, E2} ) ->                                          % T-disj

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {disj_rhs_no_bool, Info, {E2, T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {disj_lhs_no_bool, Info, {E1, T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {neg, Info, E} ) ->                                                % T-neg
  case type( Gamma, E ) of

    {ok, 'Bool'} ->
      {ok, t_bool()};

    {ok, T} ->
      {error, {neg_no_bool, Info, {E, T}}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {isnil, Info, E} ) ->                                              % T-isnil
  case type( Gamma, E ) of

    {ok, {'Lst', _}} ->
      {ok, t_bool()};

    {ok, T} ->
      {error, {isnil_no_list, Info, {E, T}}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {cnd, Info, E1, E2, E3} ) ->                                       % T-if

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of

        {ok, T2} ->
          case type( Gamma, E3 ) of
            {ok, T2}         -> {ok, T2};
            {ok, T3}         -> {error, {cnd_result_type_mismatch, Info, {T2, T3}}};
            {error, Reason3} -> {error, Reason3}
          end;
        
        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T1} ->
      {error, {cnd_case_no_bool, Info, {E1, T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( _Gamma, {null, _Info, T} ) ->                                             % T-nil
  {ok, t_lst( T )};

type( Gamma, {cons, Info, E1, E2} ) ->                                          % T-cons

  case type( Gamma, E1 ) of

    {ok, T1} ->
      case type( Gamma, E2 ) of
        {ok, {'Lst', T1}} -> {ok, {'Lst', T1}};
        {ok, {'Lst', T2}} -> {error, {cons_element_type_mismatch, Info, {T2, E1, T1}}};
        {ok, T2}          -> {error, {cons_no_list, Info, {E2, T2}}};
        {error, Reason1}  -> {error, Reason1}
      end;

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {hd, Info, E1, E2} ) ->                                            % T-hd

  case type( Gamma, E1 ) of
    {ok, {'Lst', T1}} ->
      case type( Gamma, E2 ) of
        {ok, T1}         -> {ok, T1};
        {ok, T2}         -> {error, {hd_type_mismatch, Info, {T1, E2, T2}}};
        {error, Reason2} -> {error, Reason2}
      end;
    {ok, T1}          -> {error, {hd_no_list, Info, {E1, T1}}};
    {error, Reason1}  -> {error, Reason1}
  end;

type( Gamma, {tl, Info, E1, E2} ) ->                                            % T-tl

  case type( Gamma, E1 ) of
    {ok, {'Lst', T1}} ->
      case type( Gamma, E2 ) of
        {ok, T1}         -> {ok, T1};
        {ok, T2}         -> {error, {tl_type_mismatch, Info, {T1, E2, T2}}};
        {error, Reason2} -> {error, Reason2}
      end;
    {ok, T5}          -> {error, {tl_no_list, Info, {E1, T5}}};
    {error, Reason1}  -> {error, Reason1}
  end;

type( Gamma, {append, Info, E1, E2} ) ->                                        % T-append
  
  case type( Gamma, E1 ) of

    {ok, {'Lst', T1}} ->
      case type( Gamma, E2 ) of
        {ok, {'Lst', T1}} -> {ok, t_lst( T1 )};
        {ok, {'Lst', T2}} -> {error, {append_element_type_mismatch, Info, {T1, T2}}};
        {ok, T2}          -> {error, {append_rhs_no_list, Info, {E2, T2}}};
        {error, Reason2}  -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {append_lhs_no_list, Info, {E1, T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {for, Info, TRet, XteLst, EBody} ) ->                              % T-for

  NameLst = xte_names( XteLst ),

  case ambiguous_names( NameLst ) of

    [] ->
      case check_for_binding( Gamma, Info, XteLst ) of

        {error, Reason1} ->
          {error, Reason1};

        ok ->
          Gamma1 = maps:from_list( [{X, T} || {X, T, _E} <- XteLst] ),
          Gamma2 = maps:merge( Gamma, Gamma1 ),
          case type( Gamma2, EBody ) of
            {ok, TRet}       -> {ok, t_lst( TRet )};
            {ok, T1}         -> {error, {for_body_type_mismatch, Info, {TRet, EBody, T1}}};
            {error, Reason2} -> {error, Reason2}
          end
      end;

    Lst ->
      {error, {for_ambiguous_bind_name, Info, Lst}}

  end;

type( _Gamma, {fold, Info, {X, _T, _EAcc}, {X, _T, _ELst}, _EBody} ) ->         % T-fold
  {error, {fold_ambiguous_bind_name, Info, X}};

type( Gamma, {fold, Info, {X1, T1, E1}, {X2, T2, E2}, EBody} ) ->

  case type( Gamma, E1 ) of

    {ok, T1} ->
      case type( Gamma, E2 ) of

        {ok, {'Lst', T2}} ->
          case type( Gamma#{ X1 => T1, X2 => T2 }, EBody ) of
            {ok, T1}            -> {ok, T1};
            {ok, TBody}         -> {error, {fold_body_type_mismatch, Info, {T1, EBody, TBody}}};
            {error, ReasonBody} -> {error, ReasonBody}
          end;

        {ok, {'Lst', T4}} ->
          {error, {fold_list_bind_type_mismatch, Info, {X2, T2, E2, T4}}};

        {ok, T5} ->
          {error, {fold_no_list_type, Info, {X2, T2, E2, T5}}};
        
        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T3} ->
      {error, {fold_acc_bind_type_mismatch, Info, {X1, T1, E1, T3}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {rcd, Info, BindLst} ) ->                                          % T-rcd
  case ambiguous_names( xe_names( BindLst ) ) of
    []  -> check_rcd_binding( Gamma, BindLst );
    Lst -> {error, {rcd_ambiguous_field_name, Info, Lst}}
  end;



type( Gamma, {proj, Info, X, E} ) ->                                            % T-pi

  case type( Gamma, E ) of

    {ok, {'Rcd', TArgLst}} ->
      case lists:keyfind( X, 1, TArgLst ) of
        {X, T} -> {ok, T};
        false  -> {error, {proj_field_missing, Info, X}}
      end;

    {ok, T} ->
      {error, {proj_no_record, Info, {X, T, E}}};

    {error, Reason} ->
      {error, Reason}

  end;

type( _Gamma, E ) -> error( {bad_expr, E} ).                                    % T-error


%%====================================================================
%% Internal functions
%%====================================================================

-spec check_rcd_binding( Gamma, XeLst ) -> {ok, t()} | {error, type_error()}
when Gamma :: #{ x() => t() },
     XeLst :: [{x(), e()}].

check_rcd_binding( _Gamma, [] ) ->
  {ok, t_rcd( [] )};

check_rcd_binding( Gamma, [{X, E}|Tl] ) ->
  case check_rcd_binding( Gamma, Tl ) of
    {ok, {'Rcd', XtLst}} ->
      case type( Gamma, E ) of
        {ok, T}          -> {ok, t_rcd( [{X, T}|XtLst] )};
        {error, Reason2} -> {error, Reason2}
      end;
    {error, Reason1}     -> {error, Reason1}
  end.

-spec check_for_binding( Gamma, Info, XteLst ) -> ok | {error, type_error()}
when Gamma  :: #{ x() => t() },
     Info   :: info(),
     XteLst :: [{x(), t(), e()}].

check_for_binding( _Gamma, _Info, [] ) ->
  ok;

check_for_binding( Gamma, Info, [{X, T, E}|Tl] ) ->

  case type( Gamma, E ) of

    {ok, {'Lst', T}} ->
      check_for_binding( Gamma, Info, Tl );

    {ok, {'Lst', T1}} ->
      {error, {for_bind_type_mismatch, Info, {X, T, E, T1}}};

    {ok, T2} ->
      {error, {for_bind_no_list, Info, {X, T, E, T2}}};

    {error, Reason} ->
      {error, Reason}

  end.


-spec check_argument_binding( Gamma, Info, TArgLst, EBindLst ) -> Result
when Gamma    :: #{ x() => t() },
     Info     :: info(),
     TArgLst  :: [{x(), t()}],
     EBindLst :: [{x(), e()}],
     Result   :: ok | {error, type_error()}.

check_argument_binding( _Gamma, _Info, [], [] ) ->
  ok;

check_argument_binding( _Gamma, Info, [{X, T}|_], [] ) ->
  {error, {app_missing_bind, Info, {X, T}}};

check_argument_binding( _Gamma, Info, [], [{X, E}] ) ->
  {error, {app_dangling_bind, Info, {X, E}}};

check_argument_binding( Gamma, Info, [{X, TArg}|T1], [{X, EArg}|T2] ) ->

  case type( Gamma, EArg ) of

    {ok, TArg} ->
      check_argument_binding( Gamma, Info, T1, T2 );

    {ok, T} ->
      {error, {app_bind_type_mismatch, Info, {X, TArg, EArg, T}}};

    {error, Reason} ->
      {error, Reason}

  end;

check_argument_binding( _Gamma, Info, [{X, _}|_], [{Y, _}|_] ) ->
  {error, {app_arg_name_mismatch, Info, {X, Y}}}.


-spec is_comparable( T1 :: t(), T2 :: t() ) -> boolean().

is_comparable( 'Bool', 'Bool' )           -> true;                    % C-bool
is_comparable( 'Str', 'Str' )             -> true;                    % C-str
is_comparable( {'Lst', T1}, {'Lst', T2} ) -> is_comparable( T1, T2 ); % C-lst
is_comparable( {'Rcd', []}, {'Rcd', []} ) -> true;                    % C-rcd-base
is_comparable( {'Rcd', XtLst1}, {'Rcd', XtLst2} ) ->                  % C-rcd-ind
  [{X1, T1}|Tl1] = lists:keysort( 1, XtLst1 ),
  [{X2, T2}|Tl2] = lists:keysort( 1, XtLst2 ),
  case X1 of
    X2 ->
              is_comparable( T1, T2 )
      andalso is_comparable( t_rcd( Tl1 ), t_rcd( Tl2 ) );
    _  ->
      false
  end;
is_comparable( _, _ )                     -> false.
