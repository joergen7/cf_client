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
          fut/1,    fut/2,
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
          validate_body/1,
          validate_expr/1,
          validate_type/1] ).

%% Contract Predicates
-export( [is_pattern/1,
          is_info/1,
          is_type/1,
          is_lang/1,
          is_expr/1,
          is_reason/1] ).

%% Renaming and Substitution
-export( [rename/3,
          protect_name/1,
          protect_expr/1,
          subst/3] ).

%% Properties
-export( [expr_vars/1,
          expr_free_vars/1,
          expr_size/1,
          is_alpha_equivalent/2,
          is_value/1] ).



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

-spec t_rcd( ArgLst :: [{x(), t()}] ) -> t().

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
             | {frn, s(), t(), l(), s()}.

lam( ArgLst, Body ) ->
  lam( na, ArgLst, Body ).

-spec lam( Info, ArgLst, Body ) -> e()
when Info   :: info(),
     ArgLst :: [{x(), t()}],
     Body   :: {ntv, e()}
             | {frn, s(), t(), l(), s()}.

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

fix( Info, E ) ->
  {fix, validate_info( Info ), validate_expr( E )}.

-spec fut( E :: e() ) -> e().
      fut( E )        -> fut( na, E ).

-spec fut( Info :: info(), E :: e() ) -> e().

fut( Info, E ) ->
  {fut, validate_info( Info ), validate_expr( E )}.

-spec str( S :: s() ) -> e().
      str( S )        -> str( na, S ).

-spec str( Info :: info(), S :: s() )    -> e().

str( Info, S ) when is_binary( S ) ->
  {str, validate_info( Info ), S};

str( _, Z ) ->
  error( {bad_binary, Z} ).

-spec file( S :: s() ) -> e().
      file( S ) -> file( na, S ).

-spec file( Info :: info(), S :: s() ) -> e().

file( Info, S ) when is_binary( S ) ->
  {file, validate_info( Info ), S};

file( _, Z ) ->
  error( {bad_binary, Z} ).

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

alet( _Info, [], EBody ) ->
  validate_expr( EBody );

alet( Info, XteLst, EBody ) ->
  app( validate_info( Info ),
       lam( Info, [{X, T} || {X, T, _} <- XteLst], {ntv, EBody} ),
       [{X, E} || {X, _, E} <- XteLst] ).

-spec asc( E :: e(), T :: t() ) -> e().

asc( E, T ) ->
  asc( na, E, T ).

-spec asc( Info :: info(), E :: e(), T :: t() ) -> e().

asc( Info, E, T ) ->
  X = '#asc',
  alet( Info, [{X, T, E}], var( Info, X ) ).


%%====================================================================
%% Pattern Constructors, Assignments, and Expansion
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
  EBody;

expand_closure( [Hd|Tl], EBody ) ->
  XteLst = expand_assign( Hd ),
  alet( XteLst, expand_closure( Tl, EBody ) ).


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

-spec validate_lang( L :: _ ) -> boolean().

validate_lang( Z ) ->
  case is_lang( Z ) of
    true  -> Z;
    false -> error( {bad_lang, Z} )
  end.

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

validate_xt( Xt = {X, T} ) when is_atom( X ) ->
  case is_type( T ) of
    true -> Xt;
    false -> error( {bad_type, T} )
  end;

validate_xt( {Z, _} ) ->
  error( {bad_name, Z} );

validate_xt( Z ) ->
  error( {bad_xt, Z} ).

-spec validate_xt_lst( X :: _ ) -> [{x(), t()}].

validate_xt_lst( [] )    -> [];
validate_xt_lst( [H|T] ) -> [validate_xt( H )|validate_xt_lst( T )];
validate_xt_lst( Z )     -> error( {bad_xt_lst, Z} ).


-spec validate_xe( X :: _ ) -> {x(), e()}.

validate_xe( {X, E} )
when is_atom( X ) ->
  {X, validate_expr( E )};

validate_xe( {Z, _} ) ->
  error( {bad_name, Z} );

validate_xe( Z ) ->
  error( {bad_xe, Z} ).

-spec validate_xe_lst( Lst :: _ ) -> [{x(), e()}].

validate_xe_lst( [] )    -> [];
validate_xe_lst( [H|T] ) -> [validate_xe( H )|validate_xe_lst( T )];
validate_xe_lst( Z )     -> error( {bad_xe_lst, Z} ).


-spec validate_xte( X :: _ ) -> {x(), t(), e()}.

validate_xte( {X, T, E} )
when is_atom( X ) ->
  {X, validate_type( T ), validate_expr( E )};

validate_xte( {Z, _, _} ) ->
  error( {bad_name, Z} );

validate_xte( Z ) ->
  error( {bad_xte, Z} ).

-spec validate_xte_lst( Lst :: _ ) -> [{x(), t(), e()}].

validate_xte_lst( [] )    -> [];
validate_xte_lst( [H|T] ) -> [validate_xte( H )|validate_xte_lst( T )];
validate_xte_lst( Z )     -> error( {bad_xte_lst, Z} ).


-spec validate_body( X :: _ ) -> {ntv, e()} | {frn, s(), t(), l(), s()}.

validate_body( {ntv, E} ) ->
  {ntv, validate_expr( E )};

validate_body( {frn, X, T, L, S} )
when is_binary( X ),
     is_binary( S ) ->
{frn, X, validate_type( T ), validate_lang( L ), S};

validate_body( {frn, X, _, _, _} )
when not is_binary( X ) ->
  error( {bad_fn_name, X} );

validate_body( {frn, _, _, _, S} )
when not is_binary( S ) ->
  error( {bad_script, S} );

validate_body( Z ) ->
  error( {bad_body, Z} ).


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
is_lang( 'Octave' )     -> true;
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
     is_binary( X ),
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


%%====================================================================
%% Renaming and Substitution
%%====================================================================


-spec rename_xt( {x(), t()}, x(), x() ) -> {x(), t()}.

rename_xt( {X1, T}, X1, X2 ) -> {X2, T};
rename_xt( Xt, _, _ )        -> Xt.

-spec rename_xt_lst( [{x(), t()}], x(), x() ) -> [{x(), t()}].

rename_xt_lst( [], _, _ ) ->
  [];

rename_xt_lst( [H|T], X1, X2 ) ->
  [rename_xt( H, X1, X2 )|rename_xt_lst( T, X1, X2 )].

-spec rename_xte( {x(), t(), e()}, x(), x() ) -> {x(), t(), e()}.

rename_xte( {X1, T, E}, X1, X2 ) -> {X2, T, rename( E, X1, X2 )};
rename_xte( {X, T, E}, X1, X2 )  -> {X, T, rename( E, X1, X2 )}.

-spec rename_xte_lst( [{x(), t(), e()}], x(), x() ) -> [{x(), t(), e()}].

rename_xte_lst( [], _, _ )      ->
  [];

rename_xte_lst( [H|T], X1, X2 ) ->
  [rename_xte( H, X1, X2 )|rename_xte_lst( T, X1, X2 )].

-spec rename( E :: e(), X1 :: x(), X2 :: x() ) -> e().

rename( {var, Info, X1}, X1, X2 ) -> var( Info, X2 );
rename( E = {var, _, _}, _, _ )   -> E;

rename( {lam, Info, ArgLst, {ntv, E}}, X1, X2 ) ->
  lam( Info, rename_xt_lst( ArgLst, X1, X2 ), {ntv, rename( E, X1, X2 )} );

rename( E = {lam, _, _, {frn, _, _, _, _}}, _, _ ) -> E;

rename( {app, Info, F, XeLst}, X1, X2 ) ->
  app( Info, rename( F, X1, X2 ),
             [{X, rename( E, X1, X2 )} || {X, E} <- XeLst] );

rename( {fix, Info, E}, X1, X2 ) ->
  fix( Info, rename( E, X1, X2 ) );

rename( E = {fut, _, _}, _, _ )  -> E;
rename( E = {str, _, _}, _, _ )  -> E;
rename( E = {file, _, _}, _, _ ) -> E;
rename( E = {true, _}, _, _ )    -> E;
rename( E = {false, _}, _, _ )   -> E;

rename( {cmp, Info, E1, E2}, X1, X2 ) ->
  cmp( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {conj, Info, E1, E2}, X1, X2 ) ->
  conj( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {disj, Info, E1, E2}, X1, X2 ) ->
  disj( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {neg, Info, E}, X1, X2 ) ->
  neg( Info, rename( E, X1, X2 ) );

rename( {isnil, Info, E}, X1, X2 ) ->
  isnil( Info, rename( E, X1, X2 ) );

rename( {cnd, Info, E1, E2, E3}, X1, X2 ) ->
  cnd( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ), rename( E3, X1, X2 ) );

rename( E = {null, _, _}, _, _ ) -> E;

rename( {cons, Info, E1, E2}, X1, X2 ) ->
  cons( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {hd, Info, E1, E2}, X1, X2 ) ->
  hd( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {tl, Info, E1, E2}, X1, X2 ) ->
  tl( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {append, Info, E1, E2}, X1, X2 ) ->
  append( Info, rename( E1, X1, X2 ), rename( E2, X1, X2 ) );

rename( {for, Info, TRet, XteLst, EBody}, X1, X2 ) ->
  for( Info, TRet, rename_xte_lst( XteLst, X1, X2 ), rename( EBody, X1, X2 ) );

rename( {fold, Info, Xte1, Xte2, EBody}, X1, X2 ) ->
  fold( Info, rename_xte( Xte1, X1, X2 ),
              rename_xte( Xte2, X1, X2 ),
              rename( EBody, X1, X2 ) );

rename( {rcd, Info, XeLst}, X1, X2 ) ->
  rcd( Info, [{X, rename( E, X1, X2 )} || {X, E} <- XeLst] );

rename( {proj, Info, X, E}, X1, X2 ) ->
  proj( Info, X, rename( E, X1, X2 ) );

rename( E={err, _, _, _}, _, _ ) -> E.


-spec protect_name( x() ) -> x().

protect_name( X ) ->
  Y = atom_to_list( X ),
  [Z|_] = string:split( Y, "$" ),
  N = integer_to_list( erlang:unique_integer( [positive] ) ),
  list_to_atom( Z++[36|N] ).


-spec protect_expr( E :: e() ) -> e().

protect_expr( E = {var, _, _} )                    -> E;

protect_expr( {lam, Info, [], {ntv, EBody}} ) ->
  lam( Info, [], {ntv, protect_expr( EBody )} );

protect_expr( {lam, Info, [{X1, T}|XtLst], {ntv, EBody}} ) ->
  {lam, _, XtLst1, {ntv, EBody1}} = protect_expr( lam( Info, XtLst, {ntv, EBody} ) ),
  X2 = protect_name( X1 ),
  EBody2 = rename( EBody1, X1, X2 ),
  lam( Info, [{X2, T}|XtLst1], {ntv, EBody2} );

protect_expr( E = {lam, _, _, {frn, _, _, _, _}} ) -> E;

protect_expr( {app, Info, EFn, XeLst} ) ->
  app( Info, protect_expr( EFn ),
             [{X, protect_expr( E )} || {X, E} <- XeLst] );

protect_expr( {fix, Info, E} ) ->
  fix( Info, protect_expr( E ) );

protect_expr( E = {fut, _, _} )                    -> E;
protect_expr( E = {str, _, _} )                    -> E;
protect_expr( E = {file, _, _} )                   -> E;
protect_expr( E = {true, _} )                      -> E;
protect_expr( E = {false, _} )                     -> E;

protect_expr( {cmp, Info, E1, E2} ) ->
  cmp( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {conj, Info, E1, E2} ) ->
  conj( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {disj, Info, E1, E2} ) ->
  disj( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {neg, Info, E} ) ->
  neg( Info, protect_expr( E ) );

protect_expr( {isnil, Info, E} ) ->
  isnil( Info, protect_expr( E ) );

protect_expr( {cnd, Info, E1, E2, E3} ) ->
  cnd( Info, protect_expr( E1 ), protect_expr( E2 ), protect_expr( E3 ) );

protect_expr( E = {null, _, _} )                   -> E;

protect_expr( {cons, Info, E1, E2} ) ->
  cons( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {hd, Info, E1, E2} ) ->
  hd( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {tl, Info, E1, E2} ) ->
  tl( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {append, Info, E1, E2} ) ->
  append( Info, protect_expr( E1 ), protect_expr( E2 ) );

protect_expr( {for, Info, TRet, [], EBody} ) ->
  for( Info, TRet, [], protect_expr( EBody ) );

protect_expr( {for, Info, TRet, [{X1, T, E1}|XteLst], EBody} ) ->
  {for, _, _, XteLst1, EBody1} = protect_expr( for( Info, TRet, XteLst, EBody ) ),
  X2 = protect_name( X1 ),
  E2 = protect_expr( E1 ),
  EBody2 = rename( EBody1, X1, X2 ),
  for( Info, TRet, [{X2, T, E2}|XteLst1], EBody2 );

protect_expr( {fold, Info, {X1, T1, E1}, {X2, T2, E2}, EBody} ) ->
  X3 = protect_name( X1 ),
  X4 = protect_name( X2 ),
  fold( Info, {X3, T1, protect_expr( E1 )},
              {X4, T2, protect_expr( E2 )},
              rename( rename( protect_expr( EBody ), X1, X3 ), X2, X4 ) );

protect_expr( {rcd, Info, XeLst} ) ->
  rcd( Info, [{X, protect_expr( E )} || {X, E} <- XeLst] );

protect_expr( {proj, Info, X, E} ) ->
  proj( Info, X, protect_expr( E ) );

protect_expr( E = {err, _, _, _} )                 -> E.


-spec subst_protected( e(), x(), e() ) -> e().

subst_protected( {var, _, X1}, X1, E1 ) ->
  E1;

subst_protected( E = {var, _, _}, _, _ )    -> E;

subst_protected( {lam, Info, XtLst, {ntv, EBody}}, X1, E1 ) ->
  lam( Info, XtLst, {ntv, subst_protected( EBody, X1, E1)} );

subst_protected( E = {lam, _, _, {frn, _, _, _, _}}, _, _ ) ->
  E;

subst_protected( {app, Info, EFn, XeLst}, X1, E1 ) ->
  app( Info, subst_protected( EFn, X1, E1 ),
             [{X, subst_protected( E, X1, E1 )} || {X, E} <- XeLst] );

subst_protected( {fix, Info, E}, X1, E1 ) ->
  fix( Info, subst_protected( E, X1, E1 ) );

subst_protected( E = {fut, _, _}, _, _ )    -> E;
subst_protected( E = {str, _, _}, _, _ )    -> E;
subst_protected( E = {file, _, _}, _, _ )   -> E;
subst_protected( E = {true, _}, _, _ )      -> E;
subst_protected( E = {false, _}, _, _ )     -> E;

subst_protected( {cmp, Info, ELeft, ERight}, X1, E1 ) ->
  cmp( Info, subst_protected( ELeft, X1, E1 ),
             subst_protected( ERight, X1, E1 ) );

subst_protected( {conj, Info, ELeft, ERight}, X1, E1 ) ->
  conj( Info, subst_protected( ELeft, X1, E1 ),
              subst_protected( ERight, X1, E1 ) );

subst_protected( {disj, Info, ELeft, ERight}, X1, E1 ) ->
  disj( Info, subst_protected( ELeft, X1, E1 ),
              subst_protected( ERight, X1, E1 ) );

subst_protected( {neg, Info, E}, X1, E1 ) ->
  neg( Info, subst_protected( E, X1, E1 ) );

subst_protected( {isnil, Info, E}, X1, E1 ) ->
  isnil( Info, subst_protected( E, X1, E1 ) );

subst_protected( {cnd, Info, EA, EB, EC}, X1, E1 ) ->
  cnd( Info, subst_protected( EA, X1, E1 ),
             subst_protected( EB, X1, E1 ),
             subst_protected( EC, X1, E1 ) );

subst_protected( E = {null, _, _}, _, _ )   -> E;

subst_protected( {cons, Info, ELeft, ERight}, X1, E1 ) ->
  cons( Info, subst_protected( ELeft, X1, E1 ),
              subst_protected( ERight, X1, E1 ) );

subst_protected( {hd, Info, ELeft, ERight}, X1, E1 ) ->
  hd( Info, subst_protected( ELeft, X1, E1 ),
            subst_protected( ERight, X1, E1 ) );

subst_protected( {tl, Info, ELeft, ERight}, X1, E1 ) ->
  tl( Info, subst_protected( ELeft, X1, E1 ),
            subst_protected( ERight, X1, E1 ) );

subst_protected( {append, Info, ELeft, ERight}, X1, E1 ) ->
  append( Info, subst_protected( ELeft, X1, E1 ),
                subst_protected( ERight, X1, E1 ) );

subst_protected( {for, Info, TRet, XteLst, EBody}, X1, E1 ) ->
  for( Info, TRet,
             [{X, T, subst_protected( E, X1, E1 )} || {X, T, E} <- XteLst],
             subst_protected( EBody, X1, E1 ) );

subst_protected( {fold, Info, {XA, TA, EA}, {XB, TB, EB}, EBody}, X1, E1 ) ->
  fold( Info, {XA, TA, subst_protected( EA, X1, E1 )},
              {XB, TB, subst_protected( EB, X1, E1 )},
              subst_protected( EBody, X1, E1 ) );

subst_protected( {rcd, Info, XeLst}, X1, E1 ) ->
  rcd( Info, [{X, subst_protected( E, X1, E1 )} || {X, E} <- XeLst] );

subst_protected( {proj, Info, X, E}, X1, E1 ) ->
  proj( Info, X, subst_protected( E, X1, E1 ) );

subst_protected( E = {err, _, _, _}, _, _ ) -> E.


-spec subst( e(), x(), e() ) -> e().

subst( E, X1, E1 ) ->
  E2 = protect_expr( E ),
  subst_protected( E2, X1, E1 ).


%%====================================================================
%% Expression Analysis
%%====================================================================

-spec expr_vars( e() ) -> [x()].

expr_vars( {var, _, X} )                    -> [X];
expr_vars( {lam, _, _, {ntv, E}} )          -> expr_vars( E );
expr_vars( {lam, _, _, {frn, _, _, _, _}} ) -> [];

expr_vars( {app, _, EFn, XeLst} ) ->
  lists:usort(   expr_vars( EFn )
               ++lists:flatmap( fun( {_, E} ) -> expr_vars( E ) end, XeLst ) );

expr_vars( {fix, _, E} )                    -> expr_vars( E );
expr_vars( {fut, _, _} )                    -> [];
expr_vars( {str, _, _} )                    -> [];
expr_vars( {file, _, _} )                   -> [];
expr_vars( {true, _} )                      -> [];
expr_vars( {false, _} )                     -> [];

expr_vars( {cmp, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {conj, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {disj, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {neg, _, E} )                    -> expr_vars( E );
expr_vars( {isnil, _, E} )                  -> expr_vars( E );

expr_vars( {cnd, _, E1, E2, E3} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 )++expr_vars( E3 ) );

expr_vars( {null, _, _} )                   -> [];

expr_vars( {cons, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {hd, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {tl, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {append, _, E1, E2} ) ->
  lists:usort( expr_vars( E1 )++expr_vars( E2 ) );

expr_vars( {for, _, _, XteLst, EBody} ) ->
  lists:usort(   expr_vars( EBody )
               ++lists:flatmap( fun( {_, _, E} ) -> expr_vars( E ) end,
                                XteLst ) );

expr_vars( {fold, _, {_, _, EAcc}, {_, _, ELst}, EBody} ) ->
  lists:usort( expr_vars( EAcc )++expr_vars( ELst)++expr_vars( EBody ) );

expr_vars( {rcd, _, XeLst} ) ->
  lists:usort( lists:flatmap( fun( {_, E} ) -> expr_vars( E ) end, XeLst ) );

expr_vars( {proj, _, _, E} )                -> expr_vars( E );
expr_vars( {err, _, _, _} )                 -> [].


-spec expr_free_vars( e() ) -> [x()].

expr_free_vars( {var, _, X} ) ->
  [X];

expr_free_vars( {lam, _, XtLst, {ntv, EBody}} ) ->
  expr_free_vars( EBody )--[X || {X, _} <- XtLst];

expr_free_vars( {lam, _, _, {frn, _, _, _, _}} ) ->
  [];

expr_free_vars( {app, _, EFn, XeLst} ) ->
  lists:usort(   expr_free_vars( EFn )
               ++lists:flatmap( fun( {_, E} ) -> expr_free_vars( E ) end, XeLst ) );

expr_free_vars( {fix, _, E} ) ->
  expr_free_vars( E );

expr_free_vars( {fut, _, _} )  -> [];
expr_free_vars( {str, _, _} )  -> [];
expr_free_vars( {file, _, _} ) -> [];
expr_free_vars( {true, _} )    -> [];
expr_free_vars( {false, _} )   -> [];

expr_free_vars( {cmp, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {conj, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {disj, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {neg, _, E} ) ->
  expr_free_vars( E );

expr_free_vars( {isnil, _, E} ) ->
  expr_free_vars( E );

expr_free_vars( {cnd, _, E1, E2, E3} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 )++expr_free_vars( E3 ) );

expr_free_vars( {null, _, _} ) -> [];

expr_free_vars( {cons, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {hd, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {tl, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {append, _, E1, E2} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 ) );

expr_free_vars( {for, _, _, XteLst, EBody} ) ->
  lists:usort(   lists:flatmap( fun( {_, _, E} ) -> expr_free_vars( E ) end, XteLst )
               ++( expr_free_vars( EBody )--[X || {X, _, _} <- XteLst] ) );

expr_free_vars( {fold, _, {X1, _, E1}, {X2, _, E2}, EBody} ) ->
  lists:usort( expr_free_vars( E1 )++expr_free_vars( E2 )++( expr_free_vars( EBody )--[X1, X2] ) );

expr_free_vars( {rcd, _, XeLst} ) ->
  lists:usort( lists:flatmap( fun( {_, E} ) -> expr_free_vars( E ) end, XeLst ) );

expr_free_vars( {proj, _, _, E} ) ->
  expr_free_vars( E );

expr_free_vars( {err, _, _, _} ) -> [].


-spec expr_size( E :: e() ) -> non_neg_integer().

expr_size( {var, _, _} )                    -> 0;
expr_size( {lam, _, _, {ntv, EBody}} )      -> 1+expr_size( EBody );
expr_size( {lam, _, _, {frn, _, _, _, _}} ) -> 1;

expr_size( {app, _, EFn, XeLst} ) ->
  1+expr_size( EFn )+lists:sum( [expr_size( E ) || {_, E} <- XeLst] );

expr_size( {fix, _, E} )                    -> 1+expr_size( E );
expr_size( {fut, _, _} )                    -> 1;
expr_size( {str, _, _} )                    -> 1;
expr_size( {file, _, _} )                   -> 1;
expr_size( {true, _} )                      -> 1;
expr_size( {false, _} )                     -> 1;

expr_size( {cmp, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {conj, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {disj, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {neg, _, E} )                    -> 1+expr_size( E );
expr_size( {isnil, _, E} )                  -> 1+expr_size( E );

expr_size( {cnd, _, E1, E2, E3} ) ->
  1+expr_size( E1 )+expr_size( E2 )+expr_size( E3 );

expr_size( {null, _, _} )                   -> 1;

expr_size( {cons, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {hd, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {tl, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {append, _, E1, E2} ) ->
  1+expr_size( E1 )+expr_size( E2 );

expr_size( {for, _, _, XteLst, EBody} ) ->
  1+expr_size( EBody )+lists:sum( [expr_size( E ) || {_, _, E} <- XteLst] );

expr_size( {fold, _, {_, _, EAcc}, {_, _, ELst}, EBody} ) ->
  1+expr_size( EAcc )+expr_size( ELst )+expr_size( EBody );

expr_size( {rcd, _, XeLst} ) ->
  1+lists:sum( [expr_size( E ) || {_, E} <- XeLst] );

expr_size( {proj, _, _, E} )                -> 1+expr_size( E );
expr_size( {err, _, _, _} )                 -> 1.


-spec is_alpha_equivalent( e(), e() ) -> boolean().

is_alpha_equivalent( E1, E2 ) ->
  is_alpha_equivalent_protected( protect_expr( E1 ), protect_expr( E2 ), [] ).


-spec is_alpha_equivalent_protected( e(), e(), [x()] ) -> boolean().

is_alpha_equivalent_protected( {var, _, X1}, {var, _, X2}, BoundLst ) ->
  Bound1 = lists:member( X1, BoundLst ),
  Bound2 = lists:member( X2, BoundLst ),
  if
    Bound1 andalso Bound2      -> X1 =:= X2;
    not (Bound1 orelse Bound2) -> true;
    true                       -> false
  end;

is_alpha_equivalent_protected( {lam, _, [], {ntv, EBody1}},
                               {lam, _, [], {ntv, EBody2}},
                               BoundLst ) ->
  is_alpha_equivalent_protected( EBody1, EBody2, BoundLst );

is_alpha_equivalent_protected( {lam, I1, [{X1, _}|XtLst1], {ntv, EBody1}},
                               {lam, I2, [{X2, _}|XtLst2], {ntv, EBody2}},
                               BoundLst ) ->
  is_alpha_equivalent_protected( lam( I1, XtLst1, {ntv, EBody1} ),
                                 lam( I2, XtLst2, {ntv, rename( EBody2, X2, X1 )} ),
                                 [X1|BoundLst] );

is_alpha_equivalent_protected( {lam, _, XtLst1, {frn, _, _, L, S}},
                               {lam, _, XtLst2, {frn, _, _, L, S}},
                               _BoundLst ) ->
  xt_names( XtLst1 ) =:= xt_names( XtLst2 );

is_alpha_equivalent_protected( {app, _, EFn1, []},
                     {app, _, EFn2, []},
                     BoundLst ) ->
  is_alpha_equivalent_protected( EFn1, EFn2, BoundLst );

is_alpha_equivalent_protected( {app, Info1, EFn1, [{_, E1}|XeLst1]},
                     {app, Info2, EFn2, [{_, E2}|XeLst2]},
                     BoundLst ) ->
  case is_alpha_equivalent_protected( E1, E2, BoundLst ) of
    true  -> is_alpha_equivalent_protected( app( Info1, EFn1, XeLst1 ),
                                  app( Info2, EFn2, XeLst2 ),
                                  BoundLst );
    false -> false
  end;

is_alpha_equivalent_protected( {fix, _, E1}, {fix, _, E2}, BoundLst ) ->
  is_alpha_equivalent_protected( E1, E2, BoundLst );

is_alpha_equivalent_protected( {fut, _, E1}, {fut, _, E2}, _BoundLst ) ->
  is_alpha_equivalent_protected( E1, E2, [] );

is_alpha_equivalent_protected( {str, _, S}, {str, _, S}, _ )   -> true;
is_alpha_equivalent_protected( {file, _, S}, {file, _, S}, _ ) -> true;
is_alpha_equivalent_protected( {true, _}, {true, _}, _ )       -> true;
is_alpha_equivalent_protected( {false, _}, {false, _}, _ )     -> true;

is_alpha_equivalent_protected( {cmp, _, E11, E12}, {cmp, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {conj, _, E11, E12}, {conj, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {disj, _, E11, E12}, {disj, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {neg, _, E1}, {neg, _, E2}, BoundLst ) ->
  is_alpha_equivalent_protected( E1, E2, BoundLst );

is_alpha_equivalent_protected( {isnil, _, E1}, {isnil, _, E2}, BoundLst ) ->
  is_alpha_equivalent_protected( E1, E2, BoundLst );

is_alpha_equivalent_protected( {cnd, _, E11, E12, E13},
                     {cnd, _, E21, E22, E23},
                     BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst )
  andalso is_alpha_equivalent_protected( E13, E23, BoundLst );

is_alpha_equivalent_protected( {null, _, _}, {null, _, _}, _ ) -> true;

is_alpha_equivalent_protected( {cons, _, E11, E12}, {cons, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {hd, _, E11, E12}, {hd, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {tl, _, E11, E12}, {tl, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {append, _, E11, E12}, {append, _, E21, E22}, BoundLst ) ->
          is_alpha_equivalent_protected( E11, E21, BoundLst )
  andalso is_alpha_equivalent_protected( E12, E22, BoundLst );

is_alpha_equivalent_protected( {for, _, _, [], EBody1},
                               {for, _, _, [], EBody2},
                               BoundLst ) ->
  is_alpha_equivalent_protected( EBody1, EBody2, BoundLst );

is_alpha_equivalent_protected( {for, I1, T1, [{X1, _, E1}|XteLst1], EBody1},
                               {for, I2, T2, [{X2, _, E2}|XteLst2], EBody2},
                               BoundLst ) ->
  case is_alpha_equivalent_protected( E1, E2, BoundLst ) of
    true ->
      is_alpha_equivalent_protected(
        for( I1, T1, XteLst1, EBody1 ),
        for( I2, T2, XteLst2, rename( EBody2, X2, X1 ) ),
        [X1|BoundLst] );
    false ->
      false
  end;

is_alpha_equivalent_protected( {fold, _, {X11, _, E11}, {X12, _, E12}, EBody1},
                     {fold, _, {X21, _, E21}, {X22, _, E22}, EBody2},
                     BoundLst ) ->
  C1 = is_alpha_equivalent_protected( E11, E21, BoundLst ),
  C2 = is_alpha_equivalent_protected( E12, E22, BoundLst ),
  EBody3 = rename( rename( EBody2, X21, X11 ), X22, X12 ),
  if
    C1 and C2 -> is_alpha_equivalent_protected( EBody1, EBody3, [X11, X12|BoundLst] );
    true      -> false
  end;

is_alpha_equivalent_protected( {rcd, _, []}, {rcd, _, []}, _ ) -> true;

is_alpha_equivalent_protected( {rcd, I1, [{X, E1}|Tl1]},
                     {rcd, I2, [{X, E2}|Tl2]},
                     BoundLst ) ->
  case is_alpha_equivalent_protected( E1, E2, BoundLst ) of
    true  -> is_alpha_equivalent_protected( rcd( I1, Tl1 ),
                                  rcd( I2, Tl2 ),
                                  BoundLst );
    false -> false
  end;

is_alpha_equivalent_protected( {proj, _, X, E1}, {proj, _, X, E2}, BoundLst ) ->
  is_alpha_equivalent_protected( E1, E2, BoundLst );

is_alpha_equivalent_protected( {err, _, _, _}, {err, _, _, _}, _BoundLst ) ->
  true;

is_alpha_equivalent_protected( _, _, _ ) ->
  false.


-spec is_value( e() ) -> boolean().

is_value( {lam, _, _, _} )    -> true;
is_value( {str, _, _} )       -> true;
is_value( {file, _, _} )      -> true;
is_value( {true, _} )         -> true;
is_value( {false, _} )        -> true;
is_value( {null, _, _} )      -> true;
is_value( {cons, _, E1, E2} ) -> is_value( E1 ) andalso is_value( E2 );
is_value( {rcd, _, XeLst} )   -> lists:all( fun( {_, E} ) -> is_value( E ) end, XeLst );
is_value( _ )                 -> false.