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

-module( cuneiform_lang ).

%%====================================================================
%% Exports
%%====================================================================

%% Language constructors
-export( [l_awk/0, l_bash/0, l_elixir/0, l_erlang/0, l_gnuplot/0, l_java/0,
          l_javascript/0, l_matlab/0, l_octave/0, l_perl/0, l_python/0, l_r/0,
          l_racket/0] ).

%% Type constructors
-export( [t_str/0, t_file/0, t_bool/0, t_fn/2, t_lst/1, t_rcd/1] ).

%% Expression constructors
-export( [
          var/1,    var/2,
          lam/2,    lam/3,
          app/2,    app/3,
          fix/1,    fix/2,
          fut/2,    fut/3,
          str/1,    str/2,
          file/1,   file/2,
          true/0,   true/1,
          false/0,  false/1,
          cmp/2,    cmp/3,
          conj/2,   conj/3,
          disj/2,   disj/3,
          neg/1,    neg/2,
          isnil/1,  isnil/2,          
          cnd/3,    cnd/4,
          null/1,   null/2,
          cons/2,   cons/3,
          hd/2,     hd/3,
          tl/2,     tl/3,
          append/2, append/3,
          for/3,    for/4,
          fold/3,   fold/4,
          rcd/1,    rcd/2,
          proj/2,   proj/3,
          err/2,    err/3] ).

%% Syntactic Sugar
-export( [lst/2, lst/3,
          alet/2, alet/3] ).

%% Patterns, Assignments, and Expansion
-export( [r_var/2, r_rcd/1, assign/2, assign/3,
          expand_closure/2] ).

%% Name Helpers
-export( [ambiguous_names/1,
          pattern_names/1,
          xt_names/1,
          xe_names/1,
          xte_names/1] ).

%% Validators
-export( [validate_pattern/1,
          validate_info/1,
          validate_expr/1,
          validate_type/1,
          validate_lang/1,
          validate_reason/1,
          validate_assign/1] ).

-export( [drop_info/1, impose_info/2] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform_lang.hrl" ).

%%====================================================================
%% Language constructors
%%====================================================================

% @doc Generates Awk language designation.
% @return Awk language designation
-spec l_awk()        -> l(). l_awk()        -> 'Awk'.
% @doc Generates Bash language designation.
% @return Bash language designation
-spec l_bash()       -> l(). l_bash()       -> 'Bash'.
% @doc Generates Elixir language designation.
% @return Elixir language designation
-spec l_elixir()     -> l(). l_elixir()     -> 'Elixir'.
% @doc Generates Erlang language designation.
% @return Erlang language designation
-spec l_erlang()     -> l(). l_erlang()     -> 'Erlang'.
% @doc Generates Gnuplot language designation.
% @return Gnuplot language designation
-spec l_gnuplot()    -> l(). l_gnuplot()    -> 'Gnuplot'.
% @doc Generates Java language designation.
% @return Java language designation
-spec l_java()       -> l(). l_java()       -> 'Java'.
% @doc Generates Javascript language designation.
% @return Javascript language designation
-spec l_javascript() -> l(). l_javascript() -> 'Javascript'.
% @doc Generates Matlab language designation.
% @return Matlab language designation
-spec l_matlab()     -> l(). l_matlab()     -> 'Matlab'.
% @doc Generates Octave language designation.
% @return Octave language designation
-spec l_octave()     -> l(). l_octave()     -> 'Octave'.
% @doc Generates Perl language designation.
% @return Perl language designation
-spec l_perl()       -> l(). l_perl()       -> 'Perl'.
% @doc Generates Python language designation.
% @return Python language designation
-spec l_python()     -> l(). l_python()     -> 'Python'.
% @doc Generates R language designation.
% @return R language designation
-spec l_r()          -> l(). l_r()          -> 'R'.
% @doc Generates Racket language designation.
% @return Racket language designation
-spec l_racket()     -> l(). l_racket()     -> 'Racket'.


%%====================================================================
%% Type constructors
%%====================================================================

% @doc Generates string type.
% @return string type
-spec t_str() -> t().
      t_str() -> 'Str'.

% @doc Generates file type.
% @return file type
-spec t_file() -> t().
      t_file() -> 'File'.

% @doc Generates Boolean type.
% @return Boolean type
-spec t_bool() -> t().
      t_bool() -> 'Bool'.

% @doc Generates function type.
% @param XtLst list of pairs holding a function argument's name and type
% @param TRet  function's return type
% @return function type
-spec t_fn( XtLst :: [{x(), t()}], TRet :: t() ) -> t().

t_fn( XtLst, TRet ) ->
  validate_type( {'Fn', XtLst, TRet} ).

% @doc Generates list type.
% @param T list element's type
% @return list type
-spec t_lst( T :: t() ) -> t().
      t_lst( T )        -> validate_type( {'Lst', T} ).

% @doc Generates record type.
% @param XtLst list of pairs holding a record field's name and type
% @return record type
-spec t_rcd( XtLst :: [{x(), t()}] ) -> t().
      t_rcd( XtLst )                 -> validate_type( {'Rcd', XtLst} ).


%%====================================================================
%% Expression constructors
%%====================================================================

% @doc Generates anonymous variable expression.
% @param X variable name
% @return variable expression
-spec var( X :: x() ) -> e().
      var( X )        -> var( na, X ).

% @doc Generates variable expression.
% @param Info source location
% @param X variable name
% @return variable expression
-spec var( Info :: info(), X :: x() ) -> e().
      var( Info, X )                  -> validate_expr( {var, Info, X} ).

% @doc Generates anonymous lambda expression.
% @param XtLst list of pairs holding a function argument's name and type
% @param Body function body, either native or foreign body.
% @return lambda expression
-spec lam( XtLst, Body ) -> e()
when XtLst :: [{x(), t()}],
     Body  :: {ntv, e()}
            | {frn, x(), t(), l(), s()}.

lam( XtLst, Body ) ->
  lam( na, XtLst, Body ).

% @doc Generates lambda expression.
% @param Info source location
% @param XtLst list of pairs holding a function argument's name and type
% @param Body function body, either native or foreign body.
% @return lambda expression
-spec lam( Info, XtLst, Body ) -> e()
when Info  :: info(),
     XtLst :: [{x(), t()}],
     Body  :: {ntv, e()}
            | {frn, x(), t(), l(), s()}.

lam( Info, XtLst, Body ) ->
  validate_expr( {lam, Info, XtLst, Body} ).

% @doc Generates anonymous application expression.
% @param EFn   application's function expression
% @param XeLst list of pairs holding a function argument's name and expression
%              to bind
% @return application expression
-spec app( EFn :: e(), XeLst :: [{x(), e()}] ) -> e().
      app( EFn, XeLst )                        -> app( na, EFn, XeLst ).

% @doc Generates application expression.
% @param Info source location
% @param EFn   application's function expression
% @param XeLst list of pairs holding a function argument's name and expression
%              to bind
% @return application expression
-spec app( Info :: info(), EFn :: e(), XeLst :: [{x(), e()}] ) -> e().

app( Info, EFn, XeLst ) ->
  validate_expr( {app, Info, EFn, XeLst} ).

-spec fix( EOp :: e() ) -> e().
      fix( EOp )        -> fix( na, EOp ).

-spec fix( Info :: info(), EOp :: e() ) -> e().

fix( Info, EOp ) ->
  validate_expr( {fix, Info, EOp} ).

-spec fut( T :: t(), Hash :: binary() ) -> e().
      fut( T, Hash )                    -> fut( na, T, Hash ).

-spec fut( Info :: info(), T :: t(), Hash :: binary() ) -> e().

fut( Info, T, Hash ) ->
  validate_expr( {fut, Info, T, Hash} ).

-spec str( S :: s() ) -> e().
      str( S )        -> str( na, S ).

-spec str( Info :: info(), S :: s() )    -> e().

str( Info, S ) ->
  validate_expr( {str, Info, S} ).

-spec file( S :: s() ) -> e().
      file( S ) -> file( na, S ).

-spec file( Info :: info(), S :: s() ) -> e().

file( Info, S ) ->
  validate_expr( {file, Info, S} ).

-spec true() -> e().
      true() -> true( na ).

-spec true( Info :: info() ) -> e().
      true( Info )           -> validate_expr( {true, Info} ).

-spec false() -> e().
      false() -> false( na ).

-spec false( Info :: info() ) -> e().
      false( Info )           -> validate_expr( {false, Info} ).

-spec cmp( E1 :: e(), E2 :: e() ) -> e().
      cmp( E1, E2 )               -> cmp( na, E1, E2 ).

-spec cmp( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

cmp( Info, E1, E2 ) ->
  validate_expr( {cmp, Info, E1, E2} ).

-spec conj( E1 :: e(), E2 :: e() ) -> e().
      conj( E1, E2 )               -> conj( na, E1, E2 ).

-spec conj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

conj( Info, E1, E2 ) ->
  validate_expr( {conj, Info, E1, E2} ).

-spec disj( E1 :: e(), E2 :: e() ) -> e().
      disj( E1, E2 )               -> disj( na, E1, E2 ).

-spec disj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

disj( Info, E1, E2 ) ->
  validate_expr( {disj, Info, E1, E2} ).

-spec neg( E :: e() ) -> e().
      neg( E )        -> neg( na, E ).

-spec neg( Info :: info(), E :: e() ) -> e().

neg( Info, E ) ->
  validate_expr( {neg, Info, E} ).

-spec isnil( E :: e() ) -> e().
      isnil( E )        -> isnil( na, E ).

-spec isnil( Info :: info(), E :: e() )   -> e().

isnil( Info, E ) ->
  validate_expr( {isnil, Info, E} ).

-spec cnd( E1 :: e(), E2 :: e(), E3 :: e() ) -> e().
      cnd( E1, E2, E3 )                      -> cnd( na, E1, E2, E3 ).

-spec cnd( Info :: info(), E1 :: e(), E2 :: e(), E3 :: e() ) -> e().

cnd( Info, E1, E2, E3 ) ->
  validate_expr( {cnd, Info, E1, E2, E3} ).
  
-spec null( T :: t() ) -> e().
      null( T )        -> null( na, T ).

-spec null( Info :: info(), T :: t() ) -> e().

null( Info, T ) ->
  validate_expr( {null, Info, T} ).

-spec cons( E1 :: e(), E2 :: e() ) -> e().
      cons( E1, E2 )               -> cons( na, E1, E2 ).

-spec cons( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

cons( Info, E1, E2 ) ->
  validate_expr( {cons, Info, E1, E2} ).

-spec hd( E1 :: e(), E2 :: e() ) -> e().
      hd( E1, E2 )               -> hd( na, E1, E2 ).

-spec hd( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

hd( Info, E1, E2 ) ->
  validate_expr( {hd, Info, E1, E2} ).

-spec tl( E1 :: e(), E2 :: e() ) -> e().
      tl( E1, E2 )               -> tl( na, E1, E2 ).

-spec tl( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

tl( Info, E1, E2 ) ->
  validate_expr( {tl, Info, E1, E2} ).

-spec append( E1 :: e(), E2 :: e() ) -> e().
      append( E1, E2 )               -> append( na, E1, E2 ).

-spec append( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

append( Info, E1, E2 ) ->
  validate_expr( {append, Info, E1, E2} ).

-spec for( TRet :: t(), ArgLst :: [{x(), t(), e()}], E :: e() ) -> e().

for( TRet, ArgLst, E ) ->
  for( na, TRet, ArgLst, E ).

-spec for( Info, TRet, XteLst, EBody ) -> e()
when Info   :: info(),
     TRet   :: t(),
     XteLst :: [{x(), t(), e()}],
     EBody  :: e().

for( Info, TRet, XteLst, EBody ) ->
  validate_expr( {for, Info, TRet, XteLst, EBody} ).


-spec fold( AccBind, ArgBind, E ) -> e()
when AccBind :: {x(), t(), e()},
     ArgBind :: {x(), t(), e()},
     E       :: e().

fold( AccBind, ArgBind, E ) ->
  fold( na, AccBind, ArgBind, E ).


-spec fold( Info, AccBind, ArgBind, E ) -> e()
when Info    :: info(),
     AccBind :: {x(), t(), e()},
     ArgBind :: {x(), t(), e()},
     E       :: e().

fold( Info, AccBind, LstBind, E ) ->
  validate_expr( {fold, Info, AccBind, LstBind, E} ).


-spec rcd( ArgLst :: [{x(), e()}] ) -> e().
      rcd( ArgLst ) -> rcd( na, ArgLst ).

-spec rcd( Info :: info(), BindLst :: [{x(), e()}] ) -> e().

rcd( Info, BindLst ) ->
  validate_expr( {rcd, Info, BindLst} ).

-spec proj( X :: x(), E :: e() ) -> e().
      proj( X, E )               -> proj( na, X, E ).


-spec proj( Info :: info(), X :: x(), E :: e() ) -> e().

proj( Info, X, E ) ->
  validate_expr( {proj, Info, X, E} ).

-spec err( T :: t(), Reason :: reason() ) -> e().
      err( T, Msg )                         -> err( na, T, Msg ).

-spec err( Info :: info(), T :: t(), Reason :: reason() ) -> e().

err( Info, T, Reason ) ->
  validate_expr( {err, Info, T, Reason} ).


%%====================================================================
%% Syntactic Sugar
%%====================================================================

-spec lst( T :: t(), ELst :: [e()] ) -> e().
      lst( T, ELst )                 -> lst( na, T, ELst ).


-spec lst( Info :: info(), T :: t(), ELst :: [e()] ) -> e().

lst( Info, T, [] )      -> null( Info, T );
lst( Info, T, [Hd|Tl] ) -> cons( Info, Hd, lst( Info, T, Tl ) );
lst( _, _, Z )          -> error( {bad_element_lst, Z} ).


-spec alet( XteLst :: [{x(), t(), e()}], EBody :: e() ) -> e().

alet( XteLst, EBody ) ->
  alet( na, XteLst, EBody ).


-spec alet( Info :: info(), XteLst :: [{x(), t(), e()}], EBody :: e() ) -> e().

alet( _Info, [], EBody ) ->
  validate_expr( EBody );

alet( Info, XteLst, EBody ) ->
  validate_xte_lst( XteLst ),
  app( Info,
       {lam, Info, [{X, T} || {X, T, _} <- XteLst],
                   {ntv, EBody}},
       [{X, E} || {X, _, E} <- XteLst] ).


%%====================================================================
%% Pattern Constructors, Assignments, and Expansion
%%====================================================================

-spec r_var( X :: x(), T :: t() ) -> r().
      r_var( X, T )               -> validate_pattern( {r_var, X, T} ).


-spec r_rcd( RLst :: [{x(), r()}] ) -> r().
      r_rcd( RLst )                 -> validate_pattern( {r_rcd, RLst} ).

-spec assign( R :: r(), E :: e() ) -> assign().
      assign( R, E )               -> assign( na, R, E ).


-spec assign( Info :: info(), R :: r(), E :: e() ) -> assign().

assign( Info, R, E ) ->                           
  validate_assign( {assign, Info, R, E} ).


-spec expand_closure( AssignLst, EBody ) -> e()
when AssignLst :: [assign()],
     EBody     :: e().

expand_closure( [], EBody ) ->
  EBody;

expand_closure( [Hd = {assign, Info, _, _}|Tl], EBody ) ->
  XteLst = expand_assign( Hd ),
  alet( Info, XteLst, expand_closure( Tl, EBody ) ).


-spec expand_assign( assign() ) -> [{x(), t(), e()}].

expand_assign( {assign, _Info, {r_var, X, T}, E} ) ->
  [{X, T, E}];

expand_assign( {assign, _Info, {r_rcd, []}, _E} ) ->
  [];

expand_assign( {assign, Info, {r_rcd, [{X, R}|T]}, E} ) ->
    expand_assign( assign( Info, R, proj( Info, X, E ) ) )
  ++expand_assign( assign( Info, {r_rcd, T}, E ) ).


%%====================================================================
%% Name Helpers
%%====================================================================

-spec ambiguous_names( NameLst :: [x()] ) -> [x()].

ambiguous_names( NameLst ) when is_list( NameLst ) ->
  lists:usort( NameLst--lists:usort( NameLst ) ).


-spec pattern_names( Pattern :: r() ) -> [x()].

pattern_names( {r_var, X, _T} ) when is_atom( X ) ->
  [X];

pattern_names( {r_rcd, BindLst} ) when is_list( BindLst ) ->
  lists:flatmap( fun( {_X, R} ) -> pattern_names( R ) end,
                 BindLst ).


-spec xt_names( BindLst :: [{x(), t()}] ) -> [x()].

xt_names( BindLst ) when is_list( BindLst ) ->
  [X || {X, _T} <- BindLst].


-spec xe_names( BindLst :: [{x(), e()}] ) -> [x()].

xe_names( BindLst ) when is_list( BindLst ) ->
  [X || {X, _E} <- BindLst].


-spec xte_names( BindLst :: [{x(), t(), e()}] ) -> [x()].

xte_names( BindLst ) when is_list( BindLst ) ->
  [X || {X, _T, _E} <- BindLst].


%%====================================================================
%% Validators
%%====================================================================

-spec validate_x( Z :: _ ) -> x().

validate_x( Z ) when is_atom( Z ) -> Z;
validate_x( Z )                   -> error( {bad_x, Z} ).

-spec validate_s( Z :: _ ) -> s().

validate_s( Z ) when is_binary( Z ) -> Z;
validate_s( Z )                     -> error( {bad_s, Z} ).

-spec validate_assign( Z :: _ ) -> assign().

validate_assign( A = {assign, Info, R, E} ) ->
  validate_info( Info ),
  validate_pattern( R ),
  validate_expr( E ),
  A.

-spec validate_reason( R :: _ ) -> reason().

validate_reason( R = {run, Node, AppId, LamName, ExtendedScript, Output} )
when is_binary( Node ),
     is_binary( AppId ),
     is_binary( ExtendedScript ),
     is_binary( Output ) ->
  validate_x( LamName ),
  R;

validate_reason( {run, Z, _, _, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {run, _, Z, _, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {run, _, _, _, Z, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {run, _, _, _, _, Z} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {stagein, _, _, _, []} ) ->
  error( stagein_empty_file_list );

validate_reason( R = {stagein, Node, AppId, LamName, FileLst} )
when is_binary( Node ),
     is_binary( AppId ),
     is_list( FileLst ) ->
  validate_x( LamName ),
  lists:foreach( fun validate_s/1, FileLst ),
  R;

validate_reason( {stagein, Z, _, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {stagein, _, Z, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {stagein, _, _, _, Z} )
when not is_list( Z ) ->
  error( {bad_file_lst, Z} );

validate_reason( {stageout, _, _, _, []} ) ->
  error( stageout_empty_file_list );

validate_reason( R = {stageout, Node, AppId, LamName, FileLst} )
when is_binary( Node ),
     is_binary( AppId ),
     is_list( FileLst ) ->
  validate_x( LamName ),
  lists:foreach( fun validate_s/1, FileLst ),
  R;

validate_reason( {stageout, Z, _, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {stageout, _, Z, _, _} )
when not is_binary( Z ) ->
  error( {bad_binary, Z} );

validate_reason( {stageout, _, _, _, Z} )
when not is_list( Z ) ->
  error( {bad_file_lst, Z} );

validate_reason( R = {user, Msg} ) ->
  validate_s( Msg ),
  R;

validate_reason( Z ) ->
  error( {bad_reason, Z} ).


-spec validate_lang( Z :: _ ) -> l().

validate_lang( Z = 'Awk' )        -> Z;
validate_lang( Z = 'Bash' )       -> Z;
validate_lang( Z = 'Elixir' )     -> Z;
validate_lang( Z = 'Erlang' )     -> Z;
validate_lang( Z = 'Gnuplot' )    -> Z;
validate_lang( Z = 'Java' )       -> Z;
validate_lang( Z = 'Javascript' ) -> Z;
validate_lang( Z = 'Matlab' )     -> Z;
validate_lang( Z = 'Octave' )     -> Z;
validate_lang( Z = 'Perl' )       -> Z;
validate_lang( Z = 'Python' )     -> Z;
validate_lang( Z = 'R' )          -> Z;
validate_lang( Z = 'Racket' )     -> Z;
validate_lang( Z )                -> error( {bad_lang, Z} ).


-spec validate_pattern( Z :: _ ) -> r().

validate_pattern( Z = {r_var, X, T} ) ->
  validate_x( X ),
  validate_type( T ),
  Z;

validate_pattern( Z = {r_rcd, RLst} ) ->
  validate_xr_lst( RLst ),
  Z;

validate_pattern( Z ) ->
  error( {bad_pattern, Z} ).


-spec validate_xr( Z :: _ ) -> {x(), r()}.

validate_xr( Z = {X, R} ) ->
  validate_x( X ),
  validate_pattern( R ),
  Z;

validate_xr( Z ) ->
  error( {bad_xr, Z} ).


-spec validate_xr_lst( X :: _ ) -> [{x(), r()}].

validate_xr_lst( [] )    -> [];
validate_xr_lst( [H|T] ) -> [validate_xr( H )|validate_xr_lst( T )];
validate_xr_lst( Z )     -> error( {bad_xr_lst, Z} ).


-spec validate_info( Z :: _ ) -> info().

validate_info( Z = na ) ->
  Z;

validate_info( N ) when is_integer( N ), N > 0 ->
  N;

validate_info( Z = {B, N} ) when is_binary( B ), is_integer( N ), N > 0 ->
  Z;

validate_info( Z ) ->
  error( {bad_info, Z} ).


-spec validate_xt( Z :: _ ) -> {x(), t()}.

validate_xt( Xt = {X, T} ) ->
  validate_x( X ),
  validate_type( T ),
  Xt;

validate_xt( Z ) ->
  error( {bad_xt, Z} ).

-spec validate_xt_lst( Z :: _ ) -> [{x(), t()}].

validate_xt_lst( [] )    -> [];
validate_xt_lst( [H|T] ) -> [validate_xt( H )|validate_xt_lst( T )];
validate_xt_lst( Z )     -> error( {bad_xt_lst, Z} ).


-spec validate_xe( X :: _ ) -> {x(), e()}.

validate_xe( Z = {X, E} ) ->
  validate_x( X ),
  validate_expr( E ),
  Z;

validate_xe( Z ) ->
  error( {bad_xe, Z} ).

-spec validate_xe_lst( Lst :: _ ) -> [{x(), e()}].

validate_xe_lst( [] )    -> [];
validate_xe_lst( [H|T] ) -> [validate_xe( H )|validate_xe_lst( T )];
validate_xe_lst( Z )     -> error( {bad_xe_lst, Z} ).


-spec validate_xte( X :: _ ) -> {x(), t(), e()}.

validate_xte( Z = {X, T, E} ) ->
  validate_x( X ),
  validate_type( T ),
  validate_expr( E ),
  Z;

validate_xte( Z ) ->
  error( {bad_xte, Z} ).

-spec validate_xte_lst( Lst :: _ ) -> [{x(), t(), e()}].

validate_xte_lst( [] )    -> [];
validate_xte_lst( [H|T] ) -> [validate_xte( H )|validate_xte_lst( T )];
validate_xte_lst( Z )     -> error( {bad_xte_lst, Z} ).


-spec validate_expr( Z :: _ ) -> e().

validate_expr( E = {var, Info, X} ) ->
  validate_info( Info ),
  validate_x( X ),
  E;

validate_expr( E = {lam, Info, XtLst, {ntv, EBody}} ) ->
  validate_info( Info ),
  validate_xt_lst( XtLst ),
  validate_expr( EBody ),
  E;

validate_expr( E = {lam, Info, XtLst, {frn, X, T, L, S}} ) ->
  validate_info( Info ),
  validate_xt_lst( XtLst ),
  validate_x( X ),
  validate_type( T ),
  validate_lang( L ),
  validate_s( S ),
  E;

validate_expr( {lam, _, _, Z} ) ->
  error( {bad_body, Z} );

validate_expr( E = {app, Info, EFn, XeLst} )->
  validate_info( Info ),
  validate_expr( EFn ),
  validate_xe_lst( XeLst ),
  E;

validate_expr( E = {fix, Info, EOp} ) ->
  validate_info( Info ),
  validate_expr( EOp ),
  E;

validate_expr( E = {fut, Info, T, Hash} ) when is_binary( Hash ) ->
  validate_info( Info ),
  validate_type( T ),
  E;

validate_expr( {fut, _, _, Hash} ) ->
  error( {bad_binary, Hash} );

validate_expr( E = {str, Info, S} ) ->
  validate_info( Info ),
  validate_s( S ),
  E;

validate_expr( E = {file, Info, S} ) ->
  validate_info( Info ),
  validate_s( S ),
  E;

validate_expr( E = {true, Info} ) ->
  validate_info( Info ),
  E;

validate_expr( E = {false, Info} ) ->
  validate_info( Info ),
  E;

validate_expr( E = {cmp, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {conj, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {disj, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {neg, Info, EOp} ) ->
  validate_info( Info ),
  validate_expr( EOp ),
  E;

validate_expr( E = {isnil, Info, EOp} ) ->
  validate_info( Info ),
  validate_expr( EOp ),
  E;

validate_expr( E = {cnd, Info, E1, E2, E3} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  validate_expr( E3 ),
  E;

validate_expr( E = {null, Info, T} ) ->
  validate_info( Info ),
  validate_type( T ),
  E;

validate_expr( E = {cons, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {hd, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {tl, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( E = {append, Info, E1, E2} ) ->
  validate_info( Info ),
  validate_expr( E1 ),
  validate_expr( E2 ),
  E;

validate_expr( {for, _, _, [], _} ) ->
  error( for_empty_bind_list );

validate_expr( E = {for, Info, TRet, XteLst, EBody} ) ->
  validate_info( Info ),
  validate_type( TRet ),
  validate_xte_lst( XteLst ),
  validate_expr( EBody ),
  E;

validate_expr( E = {fold, Info, AccBind, LstBind, EBody} ) ->
  validate_info( Info ),
  validate_xte( AccBind ),
  validate_xte( LstBind ),
  validate_expr( EBody ),
  E;

validate_expr( {rcd, _, []} ) ->
  error( rcd_empty_field_list );

validate_expr( E = {rcd, Info, XeLst} ) ->
  validate_info( Info ),
  validate_xe_lst( XeLst ),
  E;

validate_expr( E = {proj, Info, X, EOp} ) ->
  validate_info( Info ),
  validate_x( X ),
  validate_expr( EOp ),
  E;

validate_expr( E = {err, Info, T, R} ) ->
  validate_info( Info ),
  validate_type( T ),
  validate_reason( R ),
  E;

validate_expr( Z ) ->
  error( {bad_expr, Z} ).


-spec validate_type( Z :: _ ) -> t().

validate_type( T = 'Str' )  ->
  T;

validate_type( T = 'File' ) ->
  T;

validate_type( T = 'Bool' ) ->
  T;

validate_type( T = {'Fn', XtLst, TRet} ) ->
  validate_xt_lst( XtLst ),
  validate_type( TRet ),
  T;

validate_type( T = {'Lst', TOp} ) ->
  validate_type( TOp ),
  T;

validate_type( T = {'Rcd', XtLst} ) ->
  validate_xt_lst( XtLst ),
  T;

validate_type( Z ) ->
  error( {bad_type, Z} ).


%%====================================================================
%% Info Binding and Unbinding
%%====================================================================

% @doc Drops source location of an expression.
% @param E expression
% @return expression with the same meaning but altered source location
-spec drop_info( E :: e() ) -> e().

drop_info( {lam, _, XtLst, Body = {frn, _, _, _, _}} ) -> {lam, na, XtLst, Body};
drop_info( {app, _, EFn, XeLst} )                      -> {app, na, EFn, XeLst};
drop_info( {str, _, S} )                               -> {str, na, S};
drop_info( {file, _, S} )                              -> {file, na, S};
drop_info( {true, _} )                                 -> {true, na};
drop_info( {false, _} )                                -> {false, na};
drop_info( {null, _, T} )                              -> {null, na, T};
drop_info( {cons, _, E1, E2} )                         -> {cons, na, drop_info( E1 ), drop_info( E2 )};
drop_info( Z )                                         -> error( {drop_info_undef, Z} ).


% @doc Imposes a source location on an expression.
% @param Info source location
% @param E expression
% @return expression with the same meaning but altered source location
-spec impose_info( Info :: info(), E :: e() ) -> e().

impose_info( Info, {str, _, S} )       -> {str, Info, S};
impose_info( Info, {file, _, S} )      -> {file, Info, S};
impose_info( Info, {true, _} )         -> {true, Info};
impose_info( Info, {false, _} )        -> {false, Info};
impose_info( Info, {null, _, T} )      -> {null, Info, T};
impose_info( Info, {cons, _, E1, E2} ) -> {cons, Info, impose_info( Info, E1 ), impose_info( Info, E2 )};
impose_info( Info, {rcd, _, XeLst} )   -> {rcd, Info, [{X, impose_info( Info, E )}|| {X, E} <- XeLst]};
impose_info( Info, {err, _, T, R} )    -> {err, Info, T, R};
impose_info( _, Z )                    -> error( {impose_info_undef, Z} ).