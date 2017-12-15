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
%% Notion of reduction
%%====================================================================

-spec reduce( E :: e() ) -> e().

reduce( {cmp, Info, {str, _, S1}, {str, _, S2}} ) ->
  case S1 =:= S2 of
    true  -> cuneiform_lang:true( Info );                      % E-cmp-equal
    false -> cuneiform_lang:false( Info )                      % E-cmp-unequal
  end;

reduce( {cnd, _, {true, _}, EThen, _} ) ->                     % E-cnd-true
  EThen;

reduce( {cnd, _, {false, _}, _, EElse} ) ->                    % E-cnd-false
  EElse;

reduce( {neg, Info, {true, _}} ) ->                            % E-neg-true
  cuneiform_lang:false( Info );

reduce( {neg, Info, {false, _}} ) ->                           % E-neg-false
  cuneiform_lang:true( Info );

reduce( {conj, _, {true, _}, E} ) ->                           % E-conj-true
  E;

reduce( {conj, Info, {false, _}, _} ) ->                       % E-conj-false
  cuneiform_lang:false( Info );

reduce( {disj, Info, {true, _}, _} ) ->                        % E-disj-true
  cuneiform_lang:true( Info );

reduce( {disj, _, {false, _}, E} ) ->                          % E-disj-false
  E;

reduce( {app, _, {lam_ntv, _, [], EBody}, []} ) ->             % E-beta-base
  EBody;

reduce( {app, AppInfo,                                         % E-beta
              {lam_ntv, LamInfo, [{XIn, XOut, _}|LamArgTl], EBody},
              [{XOut, E}|AppArgTl]} ) ->
  EBody1 = subst( EBody, XIn, E ),
  EFn1 = {lam_ntv, LamInfo, LamArgTl, EBody1},
  cuneiform_lang:app( AppInfo, EFn1, AppArgTl ).

  

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
is_value( {isnil, _, _} )               -> false.


%%====================================================================
%% Substitution and renaming
%%====================================================================

-spec rename( E, X1, X2 ) -> e()
when E  :: e(),
     X1 :: x(),
     X2 :: x().

rename( E = {str, _, _}, _, _ )     -> E;
rename( E = {file, _, _, _}, _, _ ) -> E;
rename( E = {true, _}, _, _ )       -> E;
rename( E = {false, _}, _, _ )      -> E;

rename( {cnd, Info, EIf, EThen, EElse}, X, Y ) ->
  {cnd, Info, rename( EIf, X, Y ),
              rename( EThen, X, Y ),
              rename( EElse, X, Y )};

rename( {var, Info, X}, X, Y ) ->
  {var, Info, Y};

rename( E = {var, _, _}, _, _ )  -> E;

rename( {lam_ntv, Info, ArgLst, EBody}, X, Y ) ->

  F = fun
        ( {X1, S1, T1} ) when X1 =:= X -> {Y, S1, T1};
        ( Arg )                        -> Arg
      end,

  ArgLst1 = [F( Arg ) || Arg <- ArgLst],
  EBody1 = rename( EBody, X, Y ),

  {lam_ntv, Info, ArgLst1, EBody1};

rename( {app, Info, EFn, ArgLst}, X, Y ) ->

  EFn1 = rename( EFn, X, Y ),
  ArgLst1 = [{S, rename( E, X, Y )} || {S, E} <- ArgLst],

  {app, Info, EFn1, ArgLst1}.


-spec subst( E1, X, E2 ) -> e()
when E1 :: e(),
     X  :: x(),
     E2 :: e().

subst( E1 = {str, _, _}, _, _ )     -> E1;
subst( E1 = {file, _, _, _}, _, _ ) -> E1;
subst( E1 = {true, _}, _, _ )       -> E1;
subst( E1 = {false, _}, _, _ )      -> E1;

subst( {cnd, Info, EIf, EThen, EElse}, X, E2 ) ->
  {cnd, Info, subst( EIf, X, E2 ),
              subst( EThen, X, E2 ),
              subst( EElse, X, E2 )};

subst( {var, _, X}, X, E2 )      -> E2;
subst( E1 = {var, _, _}, _, _ )  -> E1;

subst( {lam_ntv, Info, ArgLst, EBody}, X, E2 ) ->

  F = fun( {X1, S, T}, {lam_ntv, Info1, ArgLst1, EBody1} ) ->
        X2 = gensym( X1 ),
        EBody2 = rename( EBody1, X1, X2 ),
        {lam_ntv, Info1, [{X2, S, T}|ArgLst1], EBody2}
      end,

  Lam0 = {lam_ntv, Info, [], EBody},
  {lam_ntv, Info, NewArgLst, NewEBody} = lists:foldr( F, Lam0, ArgLst ),

  {lam_ntv, Info, NewArgLst, subst( NewEBody, X, E2 )};

subst( {app, Info, EFn, ArgLst}, X, E2 ) ->

  EFn1 = subst( EFn, X, E2 ),
  ArgLst1 = [{S, subst( E, X, E2 )} || {S, E} <- ArgLst],

  {app, Info, EFn1, ArgLst1}.

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

-spec in_hole( E , Ctx ) -> e()
when E   :: e(),
     Ctx :: ctx().

in_hole( E, hole ) ->
  E;

in_hole( E, {cnd, Info, EIf, EThen, EElse} ) ->
  % note that we do not traverse the then- and else expressions because there
  % can never be a hole down these two roads
  {cnd, Info, in_hole( E, EIf ), EThen, EElse};

% TODO: first check for foreignness
% in_hole( E, {app, Info, {lam_frn, ...}, ArgLst} ) -> ...

in_hole( E, {app, Info, EFn, ArgLst} ) ->
  % note that we do not traverse the argument list because unless the function
  % expression is a foreign function, the hole must be left hand
  {app, Info, in_hole( E, EFn ), ArgLst}.


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

try_context( {str, _, _}, _ )        -> no_ctx;
try_context( {file, _, _, _}, _ )    -> no_ctx;
try_context( {true, _}, _ )          -> no_ctx;
try_context( {false, _}, _ )         -> no_ctx;

try_context( E = {cnd, Info, EIf, EThen, EElse}, Ctx ) ->
  case is_value( EIf ) of
    true  -> throw( {E, Ctx} );
    false ->
      Ctx1 = in_hole( {cnd, Info, hole, EThen, EElse}, Ctx ),
      try_context( EIf, Ctx1 )
  end;

try_context( {var, _, _}, _ )        -> no_ctx;
try_context( {lam_ntv, _, _, _}, _ ) -> no_ctx;

try_context( E = {app, _, {lam_ntv, _, _, _}, _}, Ctx ) ->
  throw( {E, Ctx} );

try_context( {app, Info, EFn, ArgLst}, Ctx ) ->
  Ctx1 = in_hole( {app, Info, hole, ArgLst}, Ctx ),
  try_context( EFn, Ctx1 ).


