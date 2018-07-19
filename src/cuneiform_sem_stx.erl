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

-module( cuneiform_sem_stx ).
-behavior( cuneiform_sem ).


-include_lib( "cuneiform.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

-export( [step/1] ).
-export( [reduce/1, rename/3, subst/3, gensym/1, in_hole/2, find_context/1] ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_lang, [e_bind/2, lam_ntv_arg/2] ).

-import( cuneiform_lang, [
                          true/1, false/1, app/3, cmp/3, cnd/4, neg/2, conj/3,
                          disj/3, var/2, lam_ntv/3, lst/3, append/3, isnil/2,
                          for/4, fold/4, rcd/2, proj/3, fix/2, cons/3, null/2,
                          str/2, file/3
                         ] ).

-import( cuneiform_lang, [is_lst_literal/1] ).


%%====================================================================
%% Reduction relation
%%====================================================================

-spec step( E ) -> Result
when E       :: e(),
     Result  :: {ok, e(), [e()]}
              | norule.

step( E ) ->
  case find_context( E ) of

    {ok, E1, Ctx} ->
      io:format( "e: ~p    ctx: ~p~n", [E1, Ctx] ),
      case E1 of

        % when the redex is a foreign application invoke the E-send
        {app, Info, {lam_frn, _, _, _, RetType, _, _}, _} ->

          EffiRequest = cf_client_effi:app_to_effi_request( E1 ),
          #{ app_id := AppId } = EffiRequest,

          E2 = {fut, Info, RetType, AppId},
          E3 = in_hole( E2, Ctx ),

          {ok, E3, [EffiRequest]};

        % when the redex is an error drop the context
        {err, _, _, _} ->
          {ok, E1, []};

        % in all other cases reduce
        _ ->
          E2 = reduce( E1 ),
          E3 = in_hole( E2, Ctx ),
          {ok, E3, []}

      end;

    no_ctx ->
      norule

  end.



%%====================================================================
%% Notion of reduction
%%====================================================================

-spec reduce( E :: e() ) -> e().

reduce( {cmp, Info, {str, _, S1}, {str, _, S2}} ) ->
  case S1 =:= S2 of
    true  -> true( Info );                                             % E-cmp-str-equal
    false -> false( Info )                                             % E-cmp-str-unequal
  end;

reduce( {cmp, Info, {X, _}, {X, _}} ) ->
  true( Info );                                                        % E-cmp-bool-equal

reduce( {cmp, Info, {_, _}, {_, _}} ) ->
  false( Info );                                                       % E-cmp-bool-unequal

reduce( {cnd, _, {true, _}, EThen, _} ) ->                             % E-cnd-true
  EThen;

reduce( {cnd, _, {false, _}, _, EElse} ) ->                            % E-cnd-false
  EElse;

reduce( {neg, Info, {true, _}} ) ->                                    % E-neg-true
  false( Info );

reduce( {neg, Info, {false, _}} ) ->                                   % E-neg-false
  true( Info );

reduce( {conj, _, {true, _}, E} ) ->                                   % E-conj-true
  E;

reduce( {conj, Info, {false, _}, _} ) ->                               % E-conj-false
  false( Info );

reduce( {disj, Info, {true, _}, _} ) ->                                % E-disj-true
  true( Info );

reduce( {disj, _, {false, _}, E} ) ->                                  % E-disj-false
  E;

reduce( {app, _, {lam_ntv, _, [], EBody}, []} ) ->                     % E-beta-base
  EBody;

reduce( {app, AppInfo,                                                 % E-beta
              {lam_ntv, LamInfo, [{XIn, XOut, _}|LamArgTl], EBody},
              [{XOut, E}|AppArgTl]} ) ->
  EBody1 = subst( EBody, XIn, E ),
  EFn1 = {lam_ntv, LamInfo, LamArgTl, EBody1},
  app( AppInfo, EFn1, AppArgTl );

reduce( {append, _, {null, _, _}, E2} ) ->                             % E-append-null
  E2;

reduce( {append, InfoAppend, {cons, InfoCons, Hd, Tl}, E2} ) ->        % E-append-cons
  cons( InfoCons, Hd, append( InfoAppend, Tl, E2 ) );

reduce( {isnil, Info, {null, _, _}} ) ->                               % E-isnil-null
  true( Info );

reduce( {isnil, Info, {cons, _, _, _}} ) ->                            % E-isnil-cons
  false( Info );

reduce( {proj, _, X, {rcd, _, EBindLst}} ) ->                          % E-proj
  {X, E} = lists:keyfind( X, 1, EBindLst ),
  E;

reduce( {for, Info, TRet, EBindLst, EBody} ) ->

  IsCons =
    fun
      ( {_, {cons, _, _, _}} ) -> true;
      ( {_, {null, _, _}} )    -> false
    end,

  Gobble =
    fun( {X, {cons, _, E1, E2}}, {EBody1, EBindLst2} ) ->
      {subst( EBody1, X, E1 ), [e_bind( X, E2 )|EBindLst2]}
    end,

  case lists:all( IsCons, EBindLst ) of

    false ->                                                           % E-for-null
      null( Info, TRet );

    true ->                                                            % E-for-cons

      {EBody1, EBindLst1} = lists:foldr( Gobble, {EBody, []}, EBindLst ),

      cons( Info,
            EBody1,
            for( Info, TRet, EBindLst1, EBody) )

  end;

reduce( {fold, _Info, {_XAcc, EAcc}, {_X, {null, _, _}}, _EBody} ) ->  % E-fold-null
  EAcc;

reduce( {fold, Info, {XAcc, EAcc}, {X, {cons, _, E1, E2}}, EBody} ) -> % E-fold-cons
  EAcc1 = subst( subst( EBody, XAcc, EAcc ), X, E1 ),
  fold( Info, e_bind( XAcc, EAcc1), e_bind( X, E2 ), EBody );

reduce( E = {fix, _, {lam_ntv, LamInfo, [{F, _, _T}|R], EBody}} ) ->   % E-fix
  EBody1 = subst( EBody, F, E ),
  lam_ntv( LamInfo, R, EBody1 ).

  



%%====================================================================
%% Substitution and renaming
%%====================================================================

%% @doc Consistently renames in E every occurrence of the name X1 to X2.

-spec rename( E, X1, X2 ) -> e()
when E  :: e(),
     X1 :: x(),
     X2 :: x().

rename( E = {str, _, _}, _, _ )                 -> E;
rename( E = {file, _, _, _}, _, _ )             -> E;
rename( E = {true, _}, _, _ )                   -> E;
rename( E = {false, _}, _, _ )                  -> E;
rename( E = {lam_frn, _, _, _, _, _, _}, _, _ ) -> E;
rename( E = {fut, _, _, _}, _, _ )              -> E;
rename( E = {err, _, _, _}, _, _ )              -> E;
rename( E = {null, _, _}, _, _ )                -> E;

rename( {cmp, Info, E1, E2}, X1, X2 ) ->
  cmp( Info, rename( E1, X1, X2 ),
                            rename( E2, X1, X2 ) );

rename( {cnd, Info, EIf, EThen, EElse}, X, Y ) ->
  cnd( Info, rename( EIf, X, Y ),
                            rename( EThen, X, Y ),
                            rename( EElse, X, Y ) );

rename( {neg, Info, E}, X1, X2 ) ->
  neg( Info, rename( E, X1, X2 ) );

rename( {conj, Info, E1, E2}, X1, X2 ) ->
  conj( Info, rename( E1, X1, X2 ),
                             rename( E2, X1, X2 ) );

rename( {disj, Info, E1, E2}, X1, X2 ) ->
  disj( Info, rename( E1, X1, X2 ),
                             rename( E2, X1, X2 ) );

rename( {var, Info, X}, X, Y ) ->
  var( Info, Y );

rename( E = {var, _, _}, _, _ )  -> E;

rename( {lam_ntv, Info, ArgLst, EBody}, X, Y ) ->

  F = fun
        ( {X1, S1, T1} ) when X1 =:= X -> {Y, S1, T1};
        ( Arg )                        -> Arg
      end,

  ArgLst1 = [F( Arg ) || Arg <- ArgLst],
  EBody1 = rename( EBody, X, Y ),

  lam_ntv( Info, ArgLst1, EBody1 );

rename( {app, Info, EFn, ArgLst}, X, Y ) ->

  EFn1 = rename( EFn, X, Y ),
  ArgLst1 = [e_bind( S, rename( E, X, Y ) ) || {S, E} <- ArgLst],

  app( Info, EFn1, ArgLst1 );

rename( {cons, Info, E1, E2}, X1, X2 ) ->
  cons( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {append, Info, E1, E2}, X1, X2 ) ->
  append( Info, rename( E1, X1, X2 ),
                               rename( E2, X1, X2 ) );

rename( {isnil, Info, E}, X1, X2 ) ->
  isnil( Info, rename( E, X1, X2 ) );

rename( {for, Info, TRet, EBindLst, EBody}, X1, X2 ) ->
  EBindLst1 = [e_bind( case X of X1 -> X2; _ -> X end,
                                      rename( E, X1, X2 ) )
               || {X, E} <- EBindLst],
  EBody1 = rename( EBody, X1, X2 ),
  for( Info, TRet, EBindLst1, EBody1 );

rename( {fold, Info, {XAcc, EAcc}, {XLst, ELst}, EBody}, X1, X2 ) ->
  AccBind1 = e_bind( case XAcc of X1 -> X2; _ -> XAcc end,
                                    rename( EAcc, X1, X2 ) ),
  LstBind1 = e_bind( case XLst of X1 -> X2; _ -> XLst end,
                                    rename( ELst, X1, X2 ) ),
  EBody1 = rename( EBody, X1, X2 ),
  fold( Info, AccBind1, LstBind1, EBody1 );

rename( {rcd, Info, EBindLst}, X1, X2 ) ->
  rcd( Info, [e_bind( X, rename( E, X1, X2 ) )
                             || {X, E} <- EBindLst] );

rename( {proj, Info, X, E}, X1, X2 ) ->
  proj( Info, X, rename( E, X1, X2 ) );

rename( {fix, Info, E}, X1, X2 ) ->
  fix( Info, rename( E, X1, X2 ) ).




%% @doc Substitutes in E1 every occurrence of the variable X with E2.

-spec subst( E1, X, E2 ) -> e()
when E1 :: e(),
     X  :: x(),
     E2 :: e().

subst( {var, _, X}, X, ES )                    -> ES;
subst( E = {var, _, _}, _, _ )                 -> E;
subst( E = {str, _, _}, _, _ )                 -> E;
subst( E = {file, _, _, _}, _, _ )             -> E;
subst( E = {true, _}, _, _ )                   -> E;
subst( E = {false, _}, _, _ )                  -> E;
subst( E = {fut, _, _, _}, _, _ )              -> E;
subst( E = {err, _, _, _}, _, _ )              -> E;
subst( E = {null, _, _}, _, _ )                -> E;
subst( E = {lam_frn, _, _, _, _, _, _}, _, _ ) -> E;

subst( {cmp, Info, E1, E2}, X, ES ) ->
  cmp( Info, subst( E1, X, ES ),
                            subst( E2, X, ES ) );

subst( {cnd, Info, EIf, EThen, EElse}, X, E2 ) ->
  cnd( Info, subst( EIf, X, E2 ),
                            subst( EThen, X, E2 ),
                            subst( EElse, X, E2 ) );

subst( {neg, Info, E}, X, ES ) ->
  neg( Info, subst( E, X, ES ) );

subst( {conj, Info, E1, E2}, X, ES ) ->
  conj( Info, subst( E1, X, ES ),
              subst( E2, X, ES ) );

subst( {disj, Info, E1, E2}, X, ES ) ->
  disj( Info, subst( E1, X, ES ),
              subst( E2, X, ES ) );

subst( {lam_ntv, Info, ArgLst, EBody}, X, E2 ) ->

  F = fun( {X1, S, T}, {lam_ntv, Info1, ArgLst1, EBody1} ) ->
        X2 = gensym( X1 ),
        EBody2 = rename( EBody1, X1, X2 ),
        lam_ntv( Info1, [{X2, S, T}|ArgLst1], EBody2 )
      end,

  Lam0 = lam_ntv( Info, [], EBody ),
  {lam_ntv, Info, NewArgLst, NewEBody} = lists:foldr( F, Lam0, ArgLst ),

  lam_ntv( Info, NewArgLst, subst( NewEBody, X, E2 ) );

subst( {app, Info, EFn, ArgLst}, X, E2 ) ->

  EFn1 = subst( EFn, X, E2 ),
  ArgLst1 = [e_bind( S, subst( E, X, E2 ) ) || {S, E} <- ArgLst],

  app( Info, EFn1, ArgLst1 );

subst( {cons, Info, E1, E2}, X, ES ) ->
  cons( Info, subst( E1, X, ES ), subst( E2, X, ES ) );

subst( {append, Info, E1, E2}, X, ES ) ->
  append( Info, subst( E1, X, ES ),
                subst( E2, X, ES ) );

subst( {isnil, Info, E1}, X, ES ) ->
  isnil( Info, subst( E1, X, ES ) );

subst( {rcd, Info, EBindLst}, X, ES ) ->
  rcd( Info, [{XField, subst( EField, X, ES )}
              || {XField, EField} <- EBindLst] );

subst( {proj, Info, XField, E1}, X, ES ) ->
  proj( Info, XField, subst( E1, X, ES ) );

subst( {fix, Info, E1}, X, ES ) ->
  fix( Info, subst( E1, X, ES ) );

subst( {for, Info, TRet, EBindLst, EBody}, XS, ES ) ->

  F = fun( {X1, E1}, {for, Info1, TRet1, EBindLst1, EBody1} ) ->
        X2 = gensym( X1 ),
        EBody2 = rename( EBody1, X1, X2 ),
        E2 = subst( E1, XS, ES ),
        for( Info1, TRet1, [e_bind( X2, E2 )|EBindLst1], EBody2 )
      end,

  For0 = for( Info, TRet, [], EBody ),
  {for, _Info, _TRet, NewEBindLst, NewEBody} = lists:foldr( F, For0, EBindLst ),

  for( Info, TRet, NewEBindLst, subst( NewEBody, XS, ES ) );

subst( {fold, Info, {XInit, EInit}, {XLst, ELst}, EBody}, XS, ES ) ->

  XInit1 = gensym( XInit ),
  EInit1 = subst( EInit, XS, ES ),

  XLst1 = gensym( XLst ),
  ELst1 = subst( ELst, XS, ES ),

  EBody1 = rename( EBody, XInit, XInit1 ),
  EBody2 = rename( EBody1, XLst, XLst1 ),
  EBody3 = subst( EBody2, XS, ES ),

  fold( Info, e_bind( XInit1, EInit1 ), e_bind( XLst1, ELst1 ), EBody3 ).


-spec gensym( X :: atom() ) -> atom().

gensym( X ) when is_atom( X ) ->
  [S1|_] = string:tokens( atom_to_list( X ), "$" ),
  N = erlang:unique_integer( [positive, monotonic] ),
  S2 = [$$|integer_to_list( N )],
  list_to_atom( S1++S2 ).

%%====================================================================
%% Evaluation contexts
%%====================================================================

-spec in_hole( E , Ctx ) -> e() | ctx()
when E   :: e() | ctx(),
     Ctx :: e() | ctx().

in_hole( E, Ctx ) ->
  case try_hole( E, Ctx ) of
    no_hole -> error( no_hole );
    {ok, E1} -> E1
  end.

try_hole( E, hole )                         -> {ok, E};
try_hole( _E, {str, _, _} )                 -> no_hole;
try_hole( _E, {file, _, _, _} )             -> no_hole;
try_hole( _E, {true, _} )                   -> no_hole;
try_hole( _E, {false, _} )                  -> no_hole;
try_hole( _E, {var, _, _} )                 -> no_hole;
try_hole( _E, {lam_ntv, _, _, _} )          -> no_hole;
try_hole( _E, {lam_frn, _, _, _, _, _, _} ) -> no_hole;
try_hole( _E, {fut, _, _, _} )              -> no_hole;
try_hole( _E, {null, _, _} )                -> no_hole;
try_hole( _E, {err, _, _, _} )              -> no_hole;

try_hole( E, {cmp, Info, E1, E2} ) ->
  case try_hole( E, E1 ) of
    no_hole ->
      case try_hole( E, E2 ) of
        no_hole -> no_hole;
        {ok, X} -> {ok, {cmp, Info, E1, X}}
      end;
    {ok, X} -> {ok, {cmp, Info, X, E2}}
  end;

try_hole( E, {cnd, Info, EIf, EThen, EElse} ) ->
  % note that we do not traverse the then- and else expressions because there
  % can never be a hole down these two roads
  case try_hole( E, EIf ) of
    no_hole -> no_hole;
    {ok, X} -> {ok, {cnd, Info, X, EThen, EElse}}
  end;

try_hole( E, {neg, Info, E1} ) ->
  case try_hole( E, E1 ) of
    no_hole -> no_hole;
    {ok, X} -> {ok, {neg, Info, X}}
  end;

try_hole( E, {conj, Info, E1, E2} ) ->
  case try_hole( E, E1 ) of
    no_hole ->
      case try_hole( E, E2 ) of
        no_hole -> no_hole;
        {ok, X} -> {ok, {conj, Info, E1, X}}
      end;
    {ok, X} -> {ok, {conj, Info, X, E2}}
  end;

try_hole( E, {disj, Info, E1, E2} ) ->
  case try_hole( E, E1 ) of
    no_hole ->
      case try_hole( E, E2 ) of
        no_hole -> no_hole;
        {ok, X} -> {ok, {disj, Info, E1, X}}
      end;
    {ok, X} -> {ok, {disj, Info, X, E2}}
  end;

try_hole( E, {app, Info, EFn, EBindLst} ) ->
  case try_hole( E, EFn ) of
    no_hole ->
      case try_hole_e_bind_lst( E, EBindLst, [] ) of
        no_hole         -> no_hole;
        {ok, EBindLst1} -> {ok, {app, Info, EFn, EBindLst1}}
      end;
    {ok, EFn1} -> {ok, {app, Info, EFn1, EBindLst}}
  end;

try_hole( E, {cons, Info, E1, E2} ) ->
  case try_hole( E, E1 ) of
    no_hole ->
      case try_hole( E, E2 ) of
        no_hole -> no_hole;
        {ok, X} -> {ok, {cons, Info, E1, X}}
      end;
    {ok, X} -> {ok, {cons, Info, X, E2}}
  end;

try_hole( E, {append, Info, E1, E2} ) ->
  case try_hole( E, E1 ) of
    no_hole ->
      case try_hole( E, E2 ) of
        no_hole -> no_hole;
        {ok, X} -> {ok, {append, Info, E1, X}}
      end;
    {ok, X} -> {ok, {append, Info, X, E2}}
  end;

try_hole( E, {isnil, Info, E1} ) ->
  case try_hole( E, E1 ) of
    no_hole -> no_hole;
    {ok, X} -> {ok, {isnil, Info, X}}
  end;

try_hole( E, {for, Info, TRet, EBindLst, EBody} ) ->
  % note that we do not traverse the body expression because there can never be
  % a hole down that road
  case try_hole_e_bind_lst( E, EBindLst, [] ) of
    no_hole         -> no_hole;
    {ok, EBindLst1} -> {ok, {for, Info, TRet, EBindLst1, EBody}}
  end;

try_hole( E, {fold, Info, AccBind, {X, ELst}, EBody} ) ->
  % note that we traverse neither accumulator initialization expression nor body
  % expression because there can never be a hole down that road
  case try_hole( E, ELst ) of
    no_hole     -> no_hole;
    {ok, ELst1} -> {ok, {fold, Info, AccBind, {X, ELst1}, EBody}}
  end;

try_hole( E, {rcd, Info, EBindLst} ) ->
  case try_hole_e_bind_lst( E, EBindLst, [] ) of
    no_hole         -> no_hole;
    {ok, EBindLst1} -> {ok, {rcd, Info, EBindLst1}}
  end;

try_hole( E, {proj, Info, X, E1} ) ->
  case try_hole( E, E1 ) of
    no_hole -> no_hole;
    {ok, Y} -> {ok, {proj, Info, X, Y}}
  end;

try_hole( E, {fix, Info, E1} ) ->
  case try_hole( E, E1 ) of
    no_hole -> no_hole;
    {ok, X} -> {ok, {fix, Info, X}}
  end.


try_hole_e_bind_lst( _E, [], _NoHoleBindLst ) ->
  no_hole;

try_hole_e_bind_lst( E, [{X, E1}|EBindLst], NoHoleBindLst ) ->
  case try_hole( E, E1 ) of

    no_hole ->
      try_hole_e_bind_lst( E, EBindLst, [{X, E1}|NoHoleBindLst] );

    {ok, Y} ->
      {ok, lists:reverse( NoHoleBindLst )++[{X, Y}|EBindLst]}

  end.


-spec find_context( E :: e() ) -> {ok, e(), ctx()} | no_ctx.

find_context( E ) ->
  try try_context( E, hole ) of
    no_ctx -> no_ctx
  catch
    throw:{E1, Ctx1} -> {ok, E1, Ctx1}
  end.


-spec try_context( E, Ctx ) -> no_ctx
when E   :: e(),
     Ctx :: ctx().

try_context( {str, _, _}, _ )                 -> no_ctx;
try_context( {file, _, _, _}, _ )             -> no_ctx;
try_context( {true, _}, _ )                   -> no_ctx;
try_context( {false, _}, _ )                  -> no_ctx;
try_context( {var, _, _}, _ )                 -> no_ctx;
try_context( {lam_ntv, _, _, _}, _ )          -> no_ctx;
try_context( {lam_frn, _, _, _, _, _, _}, _ ) -> no_ctx;
try_context( {fut, _, _, _}, _ )              -> no_ctx;
try_context( {null, _, _}, _ )                -> no_ctx;

try_context( E = {cmp, Info, E1, E2}, Ctx ) ->
  case cuneiform_sem:is_value( E1 ) andalso cuneiform_sem:is_value( E2 ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context( E1, in_hole( {cmp, Info, hole, E2}, Ctx ) ),
      try_context( E2, in_hole( {cmp, Info, E1, hole}, Ctx ) )
  end;

try_context( E = {cnd, Info, EIf, EThen, EElse}, Ctx ) ->
  case cuneiform_sem:is_value( EIf ) of
    true  -> throw( {E, Ctx} );
    false ->
      Ctx1 = in_hole( {cnd, Info, hole, EThen, EElse}, Ctx ),
      try_context( EIf, Ctx1 )
  end;

try_context( E = {neg, Info, E1}, Ctx ) ->
  case cuneiform_sem:is_value( E1 ) of
    true  -> throw( {E, Ctx} );
    false -> try_context( E1, in_hole( {neg, Info, hole}, Ctx ) )
  end;

try_context( E = {conj, Info, E1, E2}, Ctx ) ->
  case cuneiform_sem:is_value( E1 ) andalso cuneiform_sem:is_value( E2 ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context( E1, in_hole( {conj, Info, hole, E2}, Ctx ) ),
      try_context( E2, in_hole( {conj, Info, E1, hole}, Ctx ) )
  end;

try_context( E = {disj, Info, E1, E2}, Ctx ) ->
  case cuneiform_sem:is_value( E1 ) andalso cuneiform_sem:is_value( E2 ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context( E1, in_hole( {disj, Info, hole, E2}, Ctx ) ),
      try_context( E2, in_hole( {disj, Info, E1, hole}, Ctx ) )
  end;

try_context( E = {app, _, {lam_ntv, _, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( E = {app, Info, LamFrn = {lam_frn, _, _, _, _, _, _}, EBindLst},
             Ctx ) ->

  F =
    fun

      F( _Prefix, [] ) ->
        no_ctx;

      F( Prefix, [Pivot|Suffix] ) ->
        {X, E1} = Pivot,
        try_context( E1,
                     in_hole( {app, Info, LamFrn, Prefix++[{X, hole}|Suffix]},
                              Ctx ) ),
        F( Prefix++[Pivot], Suffix )

    end,

  case lists:all( fun cuneiform_sem:is_value/1, [E1 || {_X, E1} <- EBindLst] ) of
    true  -> throw( {E, Ctx} );
    false -> F( [], EBindLst )
  end;

try_context( {app, Info, EFn, ArgLst}, Ctx ) ->
  try_context( EFn, in_hole( {app, Info, hole, ArgLst}, Ctx ) );

try_context( {cons, Info, E1, E2}, Ctx ) ->
  try_context( E1, in_hole( {cons, Info, hole, E2}, Ctx ) ),
  try_context( E2, in_hole( {cons, Info, E1, hole}, Ctx ) );

try_context( E = {append, _, {null, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( E = {append, _, {cons, _, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {append, Info, E1, E2}, Ctx ) ->
  try_context( E1, in_hole( {append, Info, hole, E2}, Ctx ) ),
  try_context( E2, in_hole( {append, Info, E1, hole}, Ctx ) );

try_context( E = {isnil, _, {null, _, _}}, Ctx ) ->
  throw( {E, Ctx} );

try_context( E = {isnil, _, {cons, _, _, _}}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {isnil, Info, E1}, Ctx ) ->
  try_context( E1, in_hole( {isnil, Info, hole}, Ctx ) );

try_context( {rcd, Info, EBindLst}, Ctx ) ->

  F =
    fun

      F( _Prefix, [] ) ->
        no_ctx;

      F( Prefix, [Pivot|Suffix] ) ->
        {X, E1} = Pivot,
        try_context( E1,
                     in_hole( {rcd, Info, Prefix++[{X, hole}|Suffix]}, Ctx ) ),
        F( Prefix++[Pivot], Suffix )

    end,

  F( [], EBindLst );

try_context( E = {proj, _, _, {rcd, _, _}}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {proj, Info, X, E1}, Ctx ) ->
  try_context( E1, in_hole( {proj, Info, X, hole}, Ctx ) );

try_context( E = {fix, Info, E1}, Ctx ) ->
  case cuneiform_sem:is_value( E1 ) of
    true  -> throw( {E, Ctx} );
    false -> try_context( E1, in_hole( {fix, Info, hole}, Ctx ) )
  end;

try_context( E = {for, Info, TRet, EBindLst, EBody}, Ctx ) ->

  F =
    fun

      F( _Prefix, [] ) ->
        no_ctx;

      F( Prefix, [Pivot|Suffix] ) ->
        {X, E1} = Pivot,
        try_context(
          E1,
          in_hole( {for, Info, TRet, Prefix++[{X, hole}|Suffix], EBody}, Ctx ) ),
        F( Prefix++[Pivot], Suffix )

    end,

  Pred =
    fun( {_X, E1} ) ->
      is_lst_literal( E1 )
    end,

  case lists:all( Pred, EBindLst ) of
    true  -> throw( {E, Ctx} );
    false -> F( [], EBindLst )
  end;

try_context( E = {fold, Info, AccBind, {X, ELst}, EBody}, Ctx ) ->

  case is_lst_literal( ELst ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context(
        ELst,
        in_hole( {fold, Info, AccBind, {X, hole}, EBody}, Ctx ) )
  end;

try_context( E = {err, _Info, _Type, _Reason}, Ctx ) ->
  throw( {E, Ctx} ).


