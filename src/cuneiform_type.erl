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

-import( cuneiform_lang, [lam_ntv/3, rcd/2, for/4, assign/4, proj/3] ).
-import( cuneiform_lang, [t_str/0, t_file/0, t_bool/0, t_fn/3, t_arg/2,
                          t_rcd/1, t_lst/1] ).
-import( cuneiform_lang, [r_rcd/1] ).
-import( cuneiform_lang, [find_ambiguous/1] ).

-export( [type/1] ).


%%====================================================================
%% Typing relation
%%====================================================================

-spec type( E :: e() ) -> {ok, t()} | {error, type_error()}.

type( E ) ->
  type( #{}, E ).


-spec type( Gamma :: #{ x() => t() }, E :: e() ) -> {ok, t()} | {error, type_error()}.

type( _Gamma, {str, _Info, _S} ) ->
  {ok, t_str()};

type( _Gamma, {file, _Info, _S, _Hash} ) ->
  {ok, t_file()};

type( _Gamma, {true, _Info} ) ->
  {ok, t_bool()};

type( _Gamma, {false, _Info} ) ->
  {ok, t_bool()};

type( Gamma, {cmp, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, T1} ->
      case type( Gamma, E2 ) of

        {ok, T1} ->

          case T1 of
            'Str'  -> {ok, t_bool()};
            'Bool' -> {ok, t_bool()};
            _      -> {error, {no_comparable_type, Info, T1}}
          end;

        {ok, T2} ->
          {error, {type_mismatch, Info, {T1, T2}}};

        {error, Reason2} ->
          {error, Reason2}

      end;

    {error, Reason1} ->
      {error, Reason1}
  end;

type( Gamma, {lam_ntv, _Info, [], EBody} ) ->
  case type( Gamma, EBody ) of

    {ok, TBody} ->
      {ok, t_fn( ntv, [], TBody )};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {lam_ntv, Info, [{X1, T1}|ArgLst], EBody} ) ->

  case find_ambiguous( [X1|[X || {X, _} <- ArgLst]] ) of

    {ambiguous, X} ->
      {error, {ambiguous_name, Info, X}};

    unambiguous ->
      case type( Gamma#{ X1 => T1 }, lam_ntv( Info, ArgLst, EBody ) ) of

        {ok, {'Fn', ntv, _, TRet}} ->
          {ok, t_fn( ntv, [{X1, T1}|ArgLst], TRet )};
        
        {error, Reason} ->
          {error, Reason}

      end

  end;

type( Gamma, {var, Info, X} ) ->
  case maps:is_key( X, Gamma ) of
    true  -> {ok, maps:get( X, Gamma )};
    false -> {error, {unbound_var, Info, X}}
  end;

type( Gamma, {neg, Info, E} ) ->
  case type( Gamma, E ) of

    {ok, 'Bool'} ->
      {ok, t_bool()};

    {ok, T} ->
      {error, {type_mismatch, Info, {t_bool(), T}}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {cnd, Info, E1, E2, E3} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of

        {ok, T2} ->
          case type( Gamma, E3 ) of
            {ok, T2}         -> {ok, T2};
            {ok, T3}         -> {error, {type_mismatch, Info, {T2, T3}}};
            {error, Reason3} -> {error, Reason3}
          end;
        
        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {conj, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {type_mismatch, Info, {t_bool(), T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {disj, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {type_mismatch, Info, {t_bool(), T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( _Gamma, {rcd, _Info, []} ) ->
  {ok, t_rcd( [] )};

type( Gamma, {rcd, Info, [{X, E}|EBindLst]} ) ->

  case find_ambiguous( [X|[Y || {Y, _} <- EBindLst]] ) of

    {ambiguous, Z} ->
      {error, {ambiguous_name, Info, Z}};

    unambiguous ->
      case type( Gamma, E ) of

        {error, Reason1} ->
          {error, Reason1};

        {ok, T} ->
          case type( Gamma, rcd( Info, EBindLst ) ) of
            {error, Reason2}       -> {error, Reason2};
            {ok, {'Rcd', TArgLst}} -> {ok, t_rcd( [t_arg( X, T )|TArgLst])}
          end

      end

  end;

type( _Gamma, {lam_frn, Info, _FName, TArgLst, TRet, _Lang, _SBody} ) ->

  {'Rcd', RetFieldLst} = TRet,

  try

    NameLst = [X || {X, _} <- TArgLst]++[X || {X, _} <- RetFieldLst],

    case find_ambiguous( NameLst ) of
      {ambiguous, Y} -> {error, {ambiguous_name, Info, Y}};
      unambiguous    -> {ok, t_fn( frn, TArgLst, TRet )}
    end

  catch
    throw:{error, Reason} -> {error, Reason}
  end;

type( Gamma, {app, Info, F, EBindLst} ) ->

  case type( Gamma, F ) of
    
    {error, Reason1} ->
      {error, Reason1};
    
    {ok, {'Fn', _Tau, TArgLst, TRet}} ->
      case check_argument_binding( Gamma, Info, TArgLst, EBindLst ) of
        {error, Reason2} -> {error, Reason2};
        ok               -> {ok, TRet}
      end

  end;

type( Gamma, {proj, Info, X, E} ) ->

  case type( Gamma, E ) of

    {ok, {'Rcd', TArgLst}} ->
      case lists:keyfind( X, 1, TArgLst ) of
        {X, T} -> {ok, T};
        false  -> {error, {key_missing, Info, X}}
      end;

    {ok, T} ->
      {error, {no_record_type, Info, T}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {fix, Info, E} ) ->
  case type( Gamma, E ) of

    {error, Reason} ->
      {error, Reason};

    {ok, T={'Fn', ntv, [], _}} ->
      {error, {no_argument, Info, T}};

    {ok, {'Fn', ntv, [TArg1|TArgLst], TRet}} ->
      TFix = t_fn( ntv, TArgLst, TRet ),
      case TArg1 of
        {_, TFix} -> {ok, TFix};
        {_, T1}   -> {error, {type_mismatch, Info, {TFix, T1}}}
      end;

    {ok, T} ->
      {error, {no_native_function_type, Info, T}}

  end;

type( _Gamma, {null, _Info, T} ) ->
  {ok, t_lst( T )};

type( Gamma, {cons, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, T1} ->
      case type( Gamma, E2 ) of
        {ok, {'Lst', T1}} -> {ok, {'Lst', T1}};
        {ok, {'Lst', T2}} -> {error, {type_mismatch, Info, {T2, T1}}};
        {ok, T2}          -> {error, {type_mismatch, Info, {T2, {'Lst', T1}}}};
        {error, Reason1}  -> {error, Reason1}
      end;

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {append, Info, E1, E2} ) ->
  
  case type( Gamma, E1 ) of

    {ok, {'Lst', T1}} ->
      case type( Gamma, E2 ) of
        {ok, {'Lst', T1}} -> {ok, t_lst( T1 )};
        {ok, {'Lst', T2}} -> {error, {type_mismatch, Info, {t_lst( T1 ), t_lst( T2 )}}};
        {ok, T2}          -> {error, {no_list_type, Info, T2}};
        {error, Reason2}  -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {no_list_type, Info, T1}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {isnil, Info, E} ) ->
  case type( Gamma, E ) of

    {ok, {'Lst', _}} ->
      {ok, t_bool()};

    {ok, T} ->
      {error, {no_list_type, Info, T}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {for, Info, TRet, [], EBody} ) ->
  case type( Gamma, EBody ) of
    {ok, TRet}         -> {ok, t_lst( TRet )};
    {ok, T}            -> {error, {type_mismatch, Info, {TRet, T}}};
    {error, Reason}    -> {error, Reason}
  end;

type( Gamma, {for, Info, TRet, [{X1, T1, E1}|TypedBindLst], EBody} ) ->

  NameLst = [X1|[X || {X, _, _} <- TypedBindLst]],
  case cuneiform_lang:find_ambiguous( NameLst ) of

    {ambiguous, Xa} ->
      {error, {ambiguous_name, Info, Xa}};

    unambiguous ->
      case type( Gamma, E1 ) of

        {error, Reason} ->
          {error, Reason};

        {ok, {'Lst', T1}} ->
          type( Gamma#{ X1 => T1 }, for( Info, TRet, TypedBindLst, EBody ) );

        {ok, {'Lst', T2}} ->
          {error, {type_mismatch, Info, {{'Lst', T1}, {'Lst', T2}}}};

        {ok, T3} ->
          {error, {no_list_type, Info, T3}}

      end

  end;

type( _Gamma, {fold, Info, {X, _T, _EAcc}, {X, _T, _ELst}, _EBody} ) ->
  {error, {ambiguous_name, Info, X}};

type( Gamma, {fold, Info, {X1, T1, E1}, {X2, T2, E2}, EBody} ) ->
  case type( Gamma, E1 ) of

    {error, Reason1} ->
      {error, Reason1};

    {ok, T1} ->
      case type( Gamma, E2 ) of

        {ok, {'Lst', T2}} ->
          case type( Gamma#{ X1 => T1, X2 => T2 }, EBody ) of
            {ok, T1}            -> {ok, T1};
            {ok, TBody}         -> {error, {type_mismatch, Info, {T1, TBody}}};
            {error, ReasonBody} -> {error, ReasonBody}
          end;

        {ok, {'Lst', T4}} ->
          {error, {type_mismatch, Info, {{'Lst', T2}, {'Lst', T4}}}};

        {ok, T2} ->
          {error, {no_list_type, Info, T2}};
        
        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T3} ->
      {error, {type_mismatch, Info, {T1, T3}}}

  end;

type( _Gamma, {fut, _Info, T, _Hash} ) ->
  {ok, T};

type( _Gamma, {err, _Info, T, _Reason} ) ->
  {ok, T};

type( _Gamma, E ) -> error( {bad_expr, E} ).


%%====================================================================
%% Internal functions
%%====================================================================

-spec check_argument_binding( Gamma, Info, TArgLst, EBindLst ) -> Result
when Gamma    :: #{ x() => t() },
     Info     :: info(),
     TArgLst  :: [t_arg()],
     EBindLst :: [e_bind()],
     Result   :: ok | {error, type_error()}.

check_argument_binding( _Gamma, _Info, [], [] ) ->
  ok;

check_argument_binding( _Gamma, Info, [{X, _}|_], [] ) ->
  {error, {key_missing, Info, X}};

check_argument_binding( _Gamma, Info, [], [{X, _}] ) ->
  {error, {superfluous_key, Info, X}};

check_argument_binding( Gamma, Info, [{X, TLam}|T1], [{X, EArg}|T2] ) ->

  case type( Gamma, EArg ) of

    {ok, TLam} ->
      check_argument_binding( Gamma, Info, T1, T2 );

    {ok, TArg} ->
      {error, {type_mismatch, Info, {TLam, TArg}}};

    {error, Reason} ->
      {error, Reason}

  end;

check_argument_binding( _Gamma, Info, [{X, _}|_], [{Y, _}|_] ) ->
  {error, {key_mismatch, Info, {X, Y}}}.