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
-include_lib( "cuneiform.hrl" ).

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
          alet/2, alet/3,
          asc/2, asc/3] ).

%% Patterns, Assignments, and Expansion
-export( [r_var/2, r_rcd/1, assign/2, assign/3, expand_closure/2] ).

%% Name Helpers
-export( [ambiguous_names/1,
          pattern_names/1,
          xt_names/1,
          xe_names/1,
          xte_names/1] ).

%% Validators
-export( [validate_x/1,   validate_x_lst/1,
          validate_xr/1,  validate_xr_lst/1,
          validate_xt/1,  validate_xt_lst/1,
          validate_xe/1,  validate_xe_lst/1,
          validate_xte/1, validate_xte_lst/1,
          validate_pattern/1,
          validate_info/1,
          validate_body/1,
          validate_expr/1,
          validate_type/1] ).

%% Contract Predicates
-export( [] ).

%%====================================================================
%% Language constructors
%%====================================================================

-spec l_awk()        -> l(). l_awk()        -> 'Awk'.
-spec l_bash()       -> l(). l_bash()       -> 'Bash'.
-spec l_elixir()     -> l(). l_elixir()     -> 'Elixir'.
-spec l_erlang()     -> l(). l_erlang()     -> 'Erlang'.
-spec l_gnuplot()    -> l(). l_gnuplot()    -> 'Gnuplot'.
-spec l_java()       -> l(). l_java()       -> 'Java'.
-spec l_javascript() -> l(). l_javascript() -> 'Javascript'.
-spec l_matlab()     -> l(). l_matlab()     -> 'Matlab'.
-spec l_octave()     -> l(). l_octave()     -> 'Octave'.
-spec l_perl()       -> l(). l_perl()       -> 'Perl'.
-spec l_python()     -> l(). l_python()     -> 'Python'.
-spec l_r()          -> l(). l_r()          -> 'R'.
-spec l_racket()     -> l(). l_racket()     -> 'Racket'.

%%====================================================================
%% Type constructors
%%====================================================================

-spec t_str() -> t().
      t_str() -> 'Str'.

-spec t_file() -> t().
      t_file() -> 'File'.

-spec t_bool() -> t().
      t_bool() -> 'Bool'.

-spec t_rcd( ArgLst :: [{x(), t()}] )        -> t().

t_rcd( ArgLst ) ->
  {'Rcd', validate_xt_lst( ArgLst )}.


-spec t_lst( T :: t() ) -> t().
      t_lst( T )        -> {'Lst', validate_type( T )}.

-spec t_fn( ArgLst :: [{x(), t()}], TRet :: t() ) -> t().

t_fn( ArgLst, TRet ) ->
  {'Fn', validate_xt_lst( ArgLst ),
         validate_type( TRet )}.


%%====================================================================
%% Expression constructors
%%====================================================================

-spec var( X :: x() ) -> e().
      var( X )        -> var( na, X ).

-spec var( Info :: info(), X :: x() ) -> e().

var( Info, X ) ->
  {var, validate_info( Info ),
        validate_x( X )}.

-spec lam( ArgLst, Body ) -> e()
when ArgLst :: [{x(), t()}],
     Body   :: {ntv, e()}
             | {frn, x(), t(), l(), s()}.

lam( ArgLst, Body ) ->
  lam( na, ArgLst, Body ).

-spec lam( Info, ArgLst, Body ) -> e()
when Info   :: info(),
     ArgLst :: [{x(), t()}],
     Body   :: {ntv, e()}
             | {frn, x(), t(), l(), s()}.

lam( Info, ArgLst, Body ) ->
  {lam, validate_info( Info ),
        validate_xt_lst( ArgLst ),
        validate_body( Body )}.

-spec app( F :: e(), ArgLst :: [{x(), e()}] ) -> e().
      app( F, ArgLst )                        -> app( na, F, ArgLst ).

-spec app( Info :: info(), F :: e(), ArgLst :: [{x(), e()}] ) -> e().

app( Info, F, ArgLst ) ->
  {app, validate_info( Info ),
        validate_expr( F ),
        validate_xe_lst( ArgLst )}.

-spec fix( E :: e() ) -> e().
      fix( E )        -> fix( na, E ).

-spec fix( Info :: info(), E :: e() ) -> e().
      fix( Info, E )                  -> {fix, Info, validate_expr( E )}.

-spec str( S :: s() ) -> e().
      str( S )        -> str( na, S ).

-spec str( Info :: info(), S :: s() )    -> e().
      str( Info, S ) when is_binary( S ) -> {str, validate_info( Info ), S}.

-spec file( S :: s() ) -> e().
      file( S ) -> file( na, S ).

-spec file( Info :: info(), S :: s() ) -> e().
file( Info, S ) when is_binary( S ) ->
  {file, validate_info( Info ), S}.

-spec true() -> e().
      true() -> true( na ).

-spec true( Info :: info() ) -> e().
      true( Info )           -> {true, validate_info( Info )}.

-spec false() -> e().
      false() -> false( na ).

-spec false( Info :: info() ) -> e().
      false( Info )           -> {false, validate_info( Info )}.

-spec cmp( E1 :: e(), E2 :: e() ) -> e().
      cmp( E1, E2 )               -> cmp( na, E1, E2 ).

-spec cmp( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

cmp( Info, E1, E2 ) ->
  {cmp, validate_info( Info ),
        validate_expr( E1 ),
        validate_expr( E2 )}.

-spec conj( E1 :: e(), E2 :: e() ) -> e().
      conj( E1, E2 )               -> conj( na, E1, E2 ).

-spec conj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

conj( Info, E1, E2 ) ->
  {conj, validate_info( Info ),
         validate_expr( E1 ),
         validate_expr( E2 )}.

-spec disj( E1 :: e(), E2 :: e() ) -> e().
      disj( E1, E2 )               -> disj( na, E1, E2 ).

-spec disj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

disj( Info, E1, E2 ) ->
  {disj, validate_info( Info ),
         validate_expr( E1 ),
         validate_expr( E2 )}.

-spec neg( E :: e() ) -> e().
      neg( E )        -> neg( na, E ).

-spec neg( Info :: info(), E :: e() ) -> e().

neg( Info, E ) ->
  {neg, validate_info( Info ),
        validate_expr( E )}.

-spec isnil( E :: e() ) -> e().
      isnil( E )        -> isnil( na, E ).

-spec isnil( Info :: info(), E :: e() )   -> e().

isnil( Info, E ) ->
  {isnil, validate_info( Info ), validate_expr( E )}.

-spec cnd( E1 :: e(), E2 :: e(), E3 :: e() ) -> e().
      cnd( E1, E2, E3 )                      -> cnd( na, E1, E2, E3 ).

-spec cnd( Info :: info(), E1 :: e(), E2 :: e(), E3 :: e() ) -> e().

cnd( Info, E1, E2, E3 ) ->
  {cnd, validate_info( Info ),
        validate_expr( E1 ),
        validate_expr( E2 ),
        validate_expr( E3 )}.
  
-spec null( T :: t() ) -> e().
      null( T )        -> null( na, T ).

-spec null( Info :: info(), T :: t() ) -> e().

null( Info, T ) ->
  {null, validate_info( Info ),
         validate_type( T )}.

-spec cons( E1 :: e(), E2 :: e() ) -> e().
      cons( E1, E2 )               -> cons( na, E1, E2 ).

-spec cons( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

cons( Info, E1, E2 ) ->
  {cons, validate_info( Info ),
         validate_expr( E1 ),
         validate_expr( E2 )}.

-spec hd( E1 :: e(), E2 :: e() ) -> e().
      hd( E1, E2 )               -> hd( na, E1, E2 ).

-spec hd( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

hd( Info, E1, E2 ) ->
  {hd, validate_info( Info ),
       validate_expr( E1 ),
       validate_expr( E2 )}.

-spec tl( E1 :: e(), E2 :: e() ) -> e().
      tl( E1, E2 )               -> tl( na, E1, E2 ).

-spec tl( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

tl( Info, E1, E2 ) ->
  {tl, validate_info( Info ),
       validate_expr( E1 ),
       validate_expr( E2 )}.

-spec append( E1 :: e(), E2 :: e() ) -> e().
      append( E1, E2 )               -> append( na, E1, E2 ).

-spec append( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

append( Info, E1, E2 ) ->
  {append, validate_info( Info ),
           validate_expr( E1 ),
           validate_expr( E2 )}.

-spec for( TRet :: t(), ArgLst :: [{x(), t(), e()}], E :: e() ) -> e().

for( TRet, ArgLst, E ) ->
  for( na, TRet, ArgLst, E ).

-spec for( Info, TRet, ArgLst, E ) -> e()
when Info   :: info(),
     TRet   :: t(),
     ArgLst :: [{x(), t(), e()}],
     E      :: e().

for( Info, TRet, ArgLst, E ) ->
  {for, validate_info( Info ),
        validate_type( TRet ),
        validate_xte_lst( ArgLst ),
        validate_expr( E )}.


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
  {fold, validate_info( Info ),
         validate_xte( AccBind ),
         validate_xte( LstBind ),
         validate_expr( E )}.


-spec rcd( ArgLst :: [{x(), e()}] ) -> e().
      rcd( ArgLst ) -> rcd( na, ArgLst ).

-spec rcd( Info :: info(), BindLst :: [{x(), e()}] ) -> e().

rcd( Info, BindLst ) ->
  {rcd, validate_info( Info ),
        validate_xe_lst( BindLst )}.

-spec proj( X :: x(), E :: e() ) -> e().
      proj( X, E )               -> proj( na, X, E ).

-spec proj( Info :: info(), X :: x(), E :: e() ) -> e().

proj( Info, X, E ) ->
  {proj, validate_info( Info ),
         validate_x( X ),
         validate_expr( E )}.

-spec err( T :: t(), Msg :: binary() ) -> e().
      err( T, Msg )                    -> err( na, T, Msg ).

-spec err( Info :: info(), T :: t(), Msg :: binary() ) -> e().

err( Info, T, Msg ) when is_binary( Msg ) ->
  {err, validate_info( Info ),
        validate_type( T ),
        {user, Msg}}.


%%====================================================================
%% Syntactic Sugar
%%====================================================================

-spec lst( T :: t(), ELst :: [e()] ) -> e().
      lst( T, ELst )                 -> lst( na, T, ELst ).


-spec lst( Info :: info(), T :: t(), ELst :: [e()] ) -> e().

lst( Info, T, [] )      -> null( Info, T );
lst( Info, T, [Hd|Tl] ) -> cons( Info, Hd, lst( Info, T, Tl ) ).


-spec alet( XteLst :: [{x(), t(), e()}], EBody :: e() ) -> e().

alet( XteLst, EBody ) ->
  alet( na, XteLst, EBody ).


-spec alet( Info :: info(), XteLst :: [{x(), t(), e()}], EBody :: e() ) -> e().

alet( Info, XteLst, EBody ) ->
  app( validate_info( Info ),
       lam( Info, [{X, T} || {X, T, _} <- XteLst], {ntv, EBody} ),
       [{X, E} || {X, _, E} <- XteLst] ).

-spec asc( E :: e(), T :: t() ) -> e().

asc( E, T ) ->
  asc( na, E, T ).

-spec asc( Info :: info(), E :: e(), T :: t() ) -> e().

asc( Info, E, T ) ->
  X = '$asc',
  alet( Info, [{X, T, E}], var( Info, X ) ).


%%====================================================================
%% Pattern constructors and Assignments
%%====================================================================

-spec r_var( X :: x(), T :: t() ) -> r().

r_var( X, T ) ->
  {r_var, validate_x( X ),
          validate_type( T )}.


-spec r_rcd( RLst :: [{x(), r()}] ) -> r().
      r_rcd( RLst )                 -> {r_rcd, validate_xr_lst( RLst )}.

-spec assign( R :: r(), E :: e() ) -> assign().
      assign( R, E )               -> assign( na, R, E ).


-spec assign( Info :: info(), R :: r(), E :: e() ) -> assign().

assign( Info, R, E ) ->
  {assign, validate_info( Info ),
           validate_pattern( R ),
           validate_expr( E )}.


-spec expand_closure( AssignLst, EBody ) -> e()
when AssignLst :: [assign()],
     EBody     :: e().

expand_closure( [], EBody ) ->
  validate_expr( EBody );

expand_closure( [Hd|Tl], EBody ) ->

  ExpandAssign =
    fun
      ExpandAssign( {assign, _Info, {r_var, X, T}, E} ) ->
        [{X, T, E}];

      ExpandAssign( {assign, _Info, {r_rcd, []}, _E} ) ->
        [];

      ExpandAssign( {assign, Info, {r_rcd, [{X, R}|T]}, E} ) ->
          ExpandAssign( assign( Info, R, proj( Info, X, E ) ) )
        ++ExpandAssign( assign( Info, {r_rcd, T}, E ) )
    end,

  XteLst = ExpandAssign( Hd ),

  alet( XteLst, expand_closure( Tl, EBody ) ).


%%====================================================================
%% Name Helpers
%%====================================================================

-spec ambiguous_names( NameLst :: [x()] ) -> [x()].

ambiguous_names( NameLst ) ->
  L = validate_x_lst( NameLst ),
  lists:usort( L--lists:usort( L ) ).


-spec pattern_names( Pattern :: r() ) -> [x()].

pattern_names( {r_var, X, _T} ) when is_atom( X ) ->
  [X];

pattern_names( {r_rcd, BindLst} ) ->
  lists:flatmap( fun( {_X, R} ) -> pattern_names( R ) end,
                 validate_xr_lst( BindLst ) ).

-spec xt_names( BindLst :: [{x(), t()}] ) -> [x()].

xt_names( BindLst ) ->
  [X || {X, _T} <- validate_xt_lst( BindLst )].

-spec xe_names( BindLst :: [{x(), e()}] ) -> [x()].

xe_names( BindLst ) ->
  [X || {X, _E} <- validate_xe_lst( BindLst )].

-spec xte_names( BindLst :: [{x(), t(), e()}] ) -> [x()].

xte_names( BindLst ) ->
  [X || {X, _T, _E} <- validate_xte_lst( BindLst )].


%%====================================================================
%% Validators
%%====================================================================

-spec validate_x_lst( X :: _ ) -> [x()].

validate_x_lst( [] )    -> [];
validate_x_lst( [H|T] ) -> [validate_x( H )|validate_x_lst( T )].

-spec validate_x( X :: _ ) -> x().

validate_x( X ) when is_atom( X ) -> X;
validate_x( X )                   -> error( {bad_symbol, X} ).


-spec validate_pattern( X :: _ ) -> r().

validate_pattern( X ) ->
  case is_pattern( X ) of
    true  -> X;
    false -> error( {bad_pattern, X} )
  end.

-spec validate_xr( X :: _ ) -> {x(), r()}.

validate_xr( X ) ->
  case is_xr( X ) of
    true  -> X;
    false -> {bad_xr, X}
  end.

-spec validate_xr_lst( X :: _ ) -> [{x(), r()}].

validate_xr_lst( [] )    -> [];
validate_xr_lst( [H|T] ) -> [validate_xr( H )|validate_xr_lst( T )].


-spec validate_info( X :: _ ) -> info().

validate_info( X ) ->
  case is_info( X ) of
    true  -> X;
    false -> error( {bad_info, X} )
  end.


-spec validate_xt( X :: _ ) -> {x(), t()}.

validate_xt( X ) ->
  case is_xt( X ) of
    true  -> X;
    false -> {bad_xt, X}
  end.

-spec validate_xt_lst( X :: _ ) -> [{x(), t()}].

validate_xt_lst( [] )    -> [];
validate_xt_lst( [H|T] ) -> [validate_xt( H )|validate_xt_lst( T )].


-spec validate_xe( X :: _ ) -> {x(), e()}.

validate_xe( X ) ->
  case is_xe( X ) of
    true  -> X;
    false -> {bad_xe, X}
  end.

-spec validate_xe_lst( Lst :: _ ) -> [{x(), e()}].

validate_xe_lst( [] )    -> [];
validate_xe_lst( [H|T] ) -> [validate_xe( H )|validate_xe_lst( T )].


-spec validate_xte( X :: _ ) -> {x(), t(), e()}.

validate_xte( X ) ->
  case is_xte( X ) of
    true  -> X;
    false -> {bad_xte, X}
  end.

-spec validate_xte_lst( Lst :: _ ) -> [{x(), t(), e()}].

validate_xte_lst( [] )    -> [];
validate_xte_lst( [H|T] ) -> [validate_xte( H )|validate_xte_lst( T )].


-spec validate_body( X :: _ ) -> {ntv, e()} | {frn, x(), t(), l(), s()}.

validate_body( {ntv, E} ) ->
  case is_expr( E ) of
    true  -> {ntv, E};
    false -> error( {bad_expr, E} )
  end;

validate_body( {frn, X, T, L, S} )
when is_atom( X ),
     is_binary( S ) ->
  case is_lang( L ) of
    true  ->
      case is_type( T ) of
        true  -> {frn, X, L, S};
        false -> error( {bad_type, T} )
      end;
    false -> error( {bad_lang, L} )
  end.


-spec validate_expr( X :: _ ) -> e().

validate_expr( E ) ->
  case is_expr( E ) of
    true  -> E;
    false -> error( {bad_expr, E} )
  end.


-spec validate_type( X :: _ ) -> t().

validate_type( X ) ->
  case is_type( X ) of
    true  -> X;
    false -> error( {bad_type, X} )
  end.


%%====================================================================
%% Contract Predicates
%%====================================================================

-spec is_xr( X :: _ ) -> boolean().

is_xr( {X, R} ) when is_atom( X ) -> is_pattern( R );
is_xr( _ )                        -> false.

-spec is_pattern( X :: _ ) -> boolean().

is_pattern( {r_var, X, T} ) when is_atom( X ) ->
  is_type( T );

is_pattern( {r_rcd, RLst} ) when is_list( RLst ) ->
  lists:all( fun is_xr/1, RLst );

is_pattern( _ ) ->
  false.


-spec is_xt( X :: _ ) -> boolean().

is_xt( {X, T} ) when is_atom( X ) -> is_type( T );
is_xt( _ )                        -> false.


-spec is_xe( X :: _ ) -> boolean().

is_xe( {X, E} ) when is_atom( X ) -> is_expr( E );
is_xe( _ )                        -> false.


-spec is_xte( X :: _ ) -> boolean().

is_xte( {X, T, E} ) when is_atom( X ) -> is_type( T ) andalso is_expr( E );
is_xte( _ )                           -> false.


-spec is_info( X :: _ ) -> boolean().

is_info( na ) ->
  true;

is_info( N ) when is_integer( N ), N > 0 ->
  true;

is_info( {B, N} ) when is_binary( B ), is_integer( N ), N > 0 ->
  true;

is_info( _Term ) ->
  false.


-spec is_type( X :: _ ) -> boolean().

is_type( 'Str' )  ->
  true;

is_type( 'File' ) ->
  true;

is_type( 'Bool' ) ->
  true;

is_type( {'Fn', ArgLst, TRet} )
when is_list( ArgLst ) ->
  is_type( TRet ) andalso lists:all( fun is_xt/1, ArgLst );

is_type( {'Lst', T} ) -> is_type( T );

is_type( {'Rcd', FieldLst} )
when is_list( FieldLst ) ->
  lists:all( fun is_xt/1, FieldLst );

is_type( _ ) ->
  false.


-spec is_lang( X :: _ ) -> boolean().

is_lang( 'Awk' )        -> true;
is_lang( 'Bash' )       -> true;
is_lang( 'Elixir' )     -> true;
is_lang( 'Erlang' )     -> true;
is_lang( 'Gnuplot' )    -> true;
is_lang( 'Java' )       -> true;
is_lang( 'Javascript' ) -> true;
is_lang( 'Matlab' )     -> true;
is_lang( 'Oxtave' )     -> true;
is_lang( 'Perl' )       -> true;
is_lang( 'Python' )     -> true;
is_lang( 'R' )          -> true;
is_lang( 'Racket' )     -> true;
is_lang( _ )            -> false.


-spec is_expr( X :: _ ) -> boolean().

is_expr( {var, Info, X} )
when is_atom( X ) ->
  is_info( Info );

is_expr( {lam, Info, ArgLst, {ntv, E}} )
when is_list( ArgLst ) ->
          is_info( Info )
  andalso lists:all( fun is_xt/1, ArgLst )
  andalso is_expr( E );

is_expr( {lam, Info, ArgLst, {frn, X, T, L, S}} )
when is_list( ArgLst ),
     is_atom( X ),
     is_binary( S ) ->
          is_info( Info )
  andalso lists:all( fun is_xt/1, ArgLst )
  andalso is_type( T )
  andalso is_lang( L );

is_expr( {app, Info, F, EBindLst} )
when is_list( EBindLst ) ->
          is_info( Info )
  andalso is_expr( F )
  andalso lists:all( fun is_xe/1, EBindLst );

is_expr( {fix, Info, E} ) ->
  is_info( Info ) andalso is_expr( E );

is_expr( {fut, Info, E} ) ->
  is_info( Info ) andalso is_expr( E );

is_expr( {str, Info, S} )
when is_binary( S ) ->
  is_info( Info );

is_expr( {file, Info, S} )
when is_binary( S ) ->
  is_info( Info );

is_expr( {true, Info} ) ->
  is_info( Info );

is_expr( {false, Info} ) ->
  is_info( Info );

is_expr( {cmp, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {conj, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {disj, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {neg, Info, E} ) ->
  is_info( Info ) andalso is_expr( E );

is_expr( {isnil, Info, E} ) ->
  is_info( Info ) andalso is_expr( E );

is_expr( {cnd, Info, E1, E2, E3} ) ->
          is_info( Info )
  andalso is_expr( E1 )
  andalso is_expr( E2 )
  andalso is_expr( E3 );

is_expr( {null, Info, T} ) ->
  is_info( Info ) andalso is_type( T );

is_expr( {cons, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {hd, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {tl, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {append, Info, E1, E2} ) ->
  is_info( Info ) andalso is_expr( E1 ) andalso is_expr( E2 );

is_expr( {for, Info, TRet, TypedBindLst, EBody} )
when is_list( TypedBindLst ) ->
          is_info( Info )
  andalso is_type( TRet )
  andalso lists:all( fun is_xte/1, TypedBindLst )
  andalso is_expr( EBody );

is_expr( {fold, Info, AccBind, LstBind, EBody} ) ->
          is_info( Info )
  andalso is_xte( AccBind )
  andalso is_xte( LstBind )
  andalso is_expr( EBody );

is_expr( {rcd, Info, BindLst} )
when is_list( BindLst ) ->
  is_info( Info ) andalso lists:all( fun is_xe/1, BindLst );

is_expr( {proj, Info, X, E} )
when is_atom( X ) ->
  is_info( Info ) andalso is_expr( E );

is_expr( {err, Info, T, R} ) ->
          is_info( Info )
  andalso is_type( T )
  andalso is_reason( R );

is_expr( _ ) ->
  false.


-spec is_reason( X :: _ ) -> boolean().

is_reason( {run, Node, AppId, LamName, ExtendedScript, Output} )
when is_binary( Node ),
     is_binary( AppId ),
     is_binary( LamName ),
     is_binary( ExtendedScript ),
     is_binary( Output ) ->
  true;

is_reason( {stagein, Node, AppId, LamName, FileLst} )
when is_binary( Node ),
     is_binary( AppId ),
     is_binary( LamName ),
     is_list( FileLst ) ->
  lists:all( fun is_binary/1, FileLst );

is_reason( {stageout, Node, AppId, LamName, FileLst} )
when is_binary( Node ),
     is_binary( AppId ),
     is_binary( LamName ),
     is_list( FileLst ) ->
  lists:all( fun is_binary/1, FileLst );

is_reason( {user, Msg} ) when is_binary( Msg ) ->
  true;

is_reason( _ ) ->
  false.