-module( cuneiform_sem ).
-include_lib( "cuneiform.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

-export( [reduce/1] ).
-export( [is_value/1] ).
-export( [rename/3, subst/3, subst_fut/3, gensym/1] ).
-export( [in_hole/2, find_context/1] ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_lang, [e_bind/2, lam_ntv_arg/2] ).

-import( cuneiform_lang, [
                          true/1, false/1, app/3, cmp/3, cnd/4, neg/2, conj/3,
                          disj/3, var/2, lam_ntv/3, lst/3, append/3, isnil/2,
                          for/4, fold/4, rcd/2, proj/3, fix/2, lst/2
                         ] ).


%%====================================================================
%% Notion of reduction
%%====================================================================

-spec reduce( E :: e() ) -> e().

reduce( {cmp, Info, {str, _, S1}, {str, _, S2}} ) ->
  case S1 =:= S2 of
    true  -> true( Info );                                     % E-cmp-str-equal
    false -> false( Info )                                     % E-cmp-str-unequal
  end;

reduce( {cmp, Info, {X, _}, {X, _}} ) ->
  true( Info );                                                % E-cmp-str-equal

reduce( {cmp, Info, {_, _}, {_, _}} ) ->
  false( Info );                                               % E-cmp-str-unequal

reduce( {cnd, _, {true, _}, EThen, _} ) ->                     % E-cnd-true
  EThen;

reduce( {cnd, _, {false, _}, _, EElse} ) ->                    % E-cnd-false
  EElse;

reduce( {neg, Info, {true, _}} ) ->                            % E-neg-true
  false( Info );

reduce( {neg, Info, {false, _}} ) ->                           % E-neg-false
  true( Info );

reduce( {conj, _, {true, _}, E} ) ->                           % E-conj-true
  E;

reduce( {conj, Info, {false, _}, _} ) ->                       % E-conj-false
  false( Info );

reduce( {disj, Info, {true, _}, _} ) ->                        % E-disj-true
  true( Info );

reduce( {disj, _, {false, _}, E} ) ->                          % E-disj-false
  E;

reduce( {app, _, {lam_ntv, _, [], EBody}, []} ) ->             % E-beta-base
  EBody;

reduce( {app, AppInfo,                                         % E-beta
              {lam_ntv, LamInfo, [{XIn, XOut, _}|LamArgTl], EBody},
              [{XOut, E}|AppArgTl]} ) ->
  EBody1 = subst( EBody, XIn, E ),
  EFn1 = {lam_ntv, LamInfo, LamArgTl, EBody1},
  app( AppInfo, EFn1, AppArgTl );

reduce( {append, Info, {lst, _, T, L1}, {lst, _, _, L2}} ) ->  % E-append
  lst( Info, T, L1++L2 );

reduce( {isnil, Info, {lst, _, _, []}} ) ->                    % E-isnil-empty
  true( Info );

reduce( {isnil, Info, {lst, _, _, [_|_]}} ) ->                 % E-isnil-nonempty
  false( Info );

reduce( {proj, _, X, {rcd, _, EBindLst}} ) ->                  % E-proj
  {X, E} = lists:keyfind( X, 1, EBindLst ),
  E;

reduce( {for, Info, TRet, [{X, {lst, _, T, ELst}}], EBody} ) ->

  F =
    fun( E ) ->
      app( Info,
           lam_ntv( Info, [lam_ntv_arg( X, T )], EBody ),
           [e_bind( X, E )] )
    end,

  ELst1 = [F( E ) || E <- ELst],
  lst( Info, TRet, ELst1 ).

  

%%====================================================================
%% Determining values
%%====================================================================

-spec is_value( E :: e() ) -> boolean().

is_value( {str, _, _} )                 -> true;
is_value( {cmp, _, _, _} )              -> false;
is_value( {file, _, _, _} )             -> true;
is_value( {true, _} )                   -> true;
is_value( {false, _} )                  -> true;
is_value( {cnd, _, _, _, _} )           -> false;
is_value( {neg, _, _} )                 -> false;
is_value( {conj, _, _, _} )             -> false;
is_value( {disj, _, _, _} )             -> false;
is_value( {var, _, _} )                 -> false;
is_value( {lam_ntv, _, _, _} )          -> true;
is_value( {lam_frn, _, _, _, _, _, _} ) -> true;
is_value( {app, _, _, _} )              -> false;
is_value( {fut, _, _} )                 -> false;
is_value( {lst, _, _, ELst} )           -> lists:all( fun is_value/1, ELst );
is_value( {append, _, _, _} )           -> false;
is_value( {isnil, _, _} )               -> false;
is_value( {for, _, _, _, _} )           -> false;
is_value( {fold, _, _, _, _} )          -> false;
is_value( {rcd, _, EBindLst} )          -> lists:all( fun is_value/1, [E || {_, E} <- EBindLst] );
is_value( {proj, _, _, _} )             -> false;
is_value( {fix, _, _} )                 -> false;
is_value( {err, _, _, _} )              -> true.


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
rename( E = {fut, _, _}, _, _ )                 -> E;
rename( E = {err, _, _, _}, _, _ )              -> E;

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

rename( {lst, Info, T, ELst}, X1, X2 ) ->
  lst( Info, T, [rename( E, X1, X2 ) || E <- ELst] );

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

subst( {var, _, X}, X, E2 )         -> E2;
subst( E1 = {var, _, _}, _, _ )     -> E1;
subst( E1 = {str, _, _}, _, _ )     -> E1;
subst( E1 = {file, _, _, _}, _, _ ) -> E1;
subst( E1 = {true, _}, _, _ )       -> E1;
subst( E1 = {false, _}, _, _ )      -> E1;
subst( E1 = {fut, _, _}, _, _ )     -> E1;
subst( E1 = {err, _, _, _}, _, _ )  -> E1;

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

subst( {lst, Info, T, ELst}, X, ES ) ->
  lst( Info, T, [subst( E, X, ES ) || E <- ELst] );

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


-spec subst_fut( E, A, Delta ) -> e()
when E     :: e(),
     A     :: e(),
     Delta :: e().

subst_fut( _, _, _ ) -> error( nyi ).


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

in_hole( E, hole )                               -> E;
in_hole( _E, Ctx = {str, _, _} )                 -> Ctx;
in_hole( _E, Ctx = {file, _, _, _} )             -> Ctx;
in_hole( _E, Ctx = {true, _} )                   -> Ctx;
in_hole( _E, Ctx = {false, _} )                  -> Ctx;
in_hole( _E, Ctx = {var, _, _} )                 -> Ctx;
in_hole( _E, Ctx = {lam_ntv, _, _, _} )          -> Ctx;
in_hole( _E, Ctx = {lam_frn, _, _, _, _, _, _} ) -> Ctx;
in_hole( _E, Ctx = {fut, _, _} )                 -> Ctx;

in_hole( E, {cmp, Info, E1, E2} ) ->
  {cmp, Info, in_hole( E, E1 ), in_hole( E, E2 )};

in_hole( E, {cnd, Info, EIf, EThen, EElse} ) ->
  % note that we do not traverse the then- and else expressions because there
  % can never be a hole down these two roads
  {cnd, Info, in_hole( E, EIf ), EThen, EElse};

in_hole( E, {neg, Info, E1} ) ->
  {neg, Info, in_hole( E, E1 )};

in_hole( E, {conj, Info, E1, E2} ) ->
  {conj, Info, in_hole( E, E1 ), in_hole( E, E2 )};

in_hole( E, {disj, Info, E1, E2} ) ->
  {disj, Info, in_hole( E, E1 ), in_hole( E, E2 )};

in_hole( E, {app, Info, EFn, EBindLst} ) ->
  {app, Info,
        in_hole( E, EFn ),
        [{X, in_hole( E, E1 )} || {X, E1} <- EBindLst]};

in_hole( E, {lst, Info, T, ELst} ) ->
  {lst, Info, T, [in_hole( E, E1 ) || E1 <- ELst]};

in_hole( E, {append, Info, E1, E2} ) ->
  {append, Info, in_hole( E, E1 ), in_hole( E, E2 )};

in_hole( E, {isnil, Info, E1} ) ->
  {isnil, Info, in_hole( E, E1 )};

in_hole( E, {for, Info, TRet, EBindLst, EBody} ) ->
  % note that we do not traverse the body expression because there can never be
  % a hole down that road
  {for, Info, TRet, [{X1, in_hole( E, E1 )} || {X1, E1} <- EBindLst], EBody};

in_hole( E, {fold, Info, AccBind, {X, ELst}, EBody} ) ->
  % note that we traverse neither accumulator initialization expression nor body
  % expression because there can never be a hole down that road
  {fold, Info, AccBind, {X, in_hole( E, ELst )}, EBody};

in_hole( E, {rcd, Info, EBindLst} ) ->
  {rcd, Info, [{X, in_hole( E, E1 )} || {X, E1} <- EBindLst]};

in_hole( E, {proj, Info, X, E1} ) ->
  {proj, Info, X, in_hole( E, E1 )};

in_hole( E, {fix, Info, E1} ) ->
  {fix, Info, in_hole( E, E1 )}.


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
try_context( {fut, _, _}, _ )                 -> no_ctx;

try_context( E = {cmp, Info, E1, E2}, Ctx ) ->
  case is_value( E1 ) andalso is_value( E2 ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context( E1, in_hole( {cmp, Info, hole, E2}, Ctx ) ),
      try_context( E2, in_hole( {cmp, Info, E1, hole}, Ctx ) )
  end;

try_context( E = {cnd, Info, EIf, EThen, EElse}, Ctx ) ->
  case is_value( EIf ) of
    true  -> throw( {E, Ctx} );
    false ->
      Ctx1 = in_hole( {cnd, Info, hole, EThen, EElse}, Ctx ),
      try_context( EIf, Ctx1 )
  end;

try_context( E = {neg, Info, E1}, Ctx ) ->
  case is_value( E1 ) of
    true  -> throw( {E, Ctx} );
    false -> try_context( E1, in_hole( {neg, Info, hole}, Ctx ) )
  end;

try_context( E = {conj, Info, E1, E2}, Ctx ) ->
  case is_value( E1 ) andalso is_value( E2 ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context( E1, in_hole( {conj, Info, hole, E2}, Ctx ) ),
      try_context( E2, in_hole( {conj, Info, E1, hole}, Ctx ) )
  end;

try_context( E = {disj, Info, E1, E2}, Ctx ) ->
  case is_value( E1 ) andalso is_value( E2 ) of
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

  case lists:all( fun is_value/1, [E1 || {_X, E1} <- EBindLst] ) of
    true  -> throw( {E, Ctx} );
    false -> F( [], EBindLst )
  end;

try_context( {app, Info, EFn, ArgLst}, Ctx ) ->
  try_context( EFn, in_hole( {app, Info, hole, ArgLst}, Ctx ) );

try_context( {lst, Info, T, ELst}, Ctx ) ->
  
  F =
    fun

      F( _Prefix, [] ) ->
        no_ctx;

      F( Prefix, [Pivot|Suffix] ) ->
        try_context( Pivot,
                     in_hole( {lst, Info, T, Prefix++[hole|Suffix]}, Ctx ) ),
        F( Prefix++[Pivot], Suffix )

    end,

  F( [], ELst );

try_context( E = {append, _, {lst, _, _, _}, {lst, _, _, _}}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {append, Info, E1, E2}, Ctx ) ->
  try_context( E1, in_hole( {append, Info, hole, E2}, Ctx ) ),
  try_context( E2, in_hole( {append, Info, E1, hole}, Ctx ) );

try_context( E = {isnil, _, {lst, _, _, _}}, Ctx ) ->
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
  case is_value( E1 ) of
    true  -> throw( {E, Ctx} );
    false -> try_context( E1, in_hole( {fix, Info, hole}, Ctx ) )
  end;

try_context( E = {for, Info, TRet, EBindLst, EBody}, Ctx ) ->

  IsLst =
    fun
      ( {lst, _, _, _} ) -> true;
      ( _ )              -> false
    end,

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

  case lists:all( IsLst, [E1 || {_, E1} <- EBindLst] ) of
    true  -> throw( {E, Ctx} );
    false -> F( [], EBindLst )
  end;

try_context( E = {fold, Info, AccBind, {X, ELst}, EBody}, Ctx ) ->

  IsLst =
    fun
      ( {lst, _, _, _} ) -> true;
      ( _ )              -> false
    end,

  case IsLst( ELst ) of
    true  -> throw( {E, Ctx} );
    false ->
      try_context(
        ELst,
        in_hole( {fold, Info, AccBind, {X, hole}, EBody}, Ctx ) )
  end;

try_context( E = {err, _Info, _Script, _Output}, Ctx ) ->
  throw( {E, Ctx} ).