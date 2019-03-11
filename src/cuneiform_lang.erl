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

%% Type constructors
-export( [t_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3, t_rcd/1, t_lst/1] ).

%% Expression constructors
-export( [typed_bind/3, e_bind/2, r_bind/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2, lam_frn/5,
          app/2, cmp/2, conj/2, disj/2, neg/1, lst/2, append/2, isnil/1,
          for/3, fold/3, rcd/1, proj/2, fix/1, cons/2, null/1, err/2] ).
-export( [str/2, file/2, file/3, true/1, false/1, cnd/4, var/2, lam_ntv/3, lam_frn/6,
          app/3, cmp/3, conj/3, disj/3, neg/2, lst/3, append/3, isnil/2,
          for/4, fold/4, rcd/2, proj/3, fix/2, cons/3, null/2, err/3] ).

%% Assignment
-export( [assign/2, assign/3, create_closure/2] ).

%% Pattern constructors
-export( [r_var/2, r_rcd/1] ).

%% Language constructors
-export( [l_awk/0, l_bash/0, l_elixir/0, l_erlang/0, l_gnuplot/0, l_java/0,
          l_javascript/0, l_matlab/0, l_octave/0, l_perl/0, l_python/0, l_r/0,
          l_racket/0] ).

-export( [find_ambiguous/1, pattern_names/1] ).

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
%% Pattern constructors
%%====================================================================

-spec r_var( X :: x(), T :: t() )     -> r().
      r_var( X, T ) when is_atom( X ) -> {r_var, X, T}.

-spec r_rcd( RLst :: [r_bind()] )        -> r().
      r_rcd( RLst ) when is_list( RLst ) -> {r_rcd, RLst}.


%%====================================================================
%% Type constructors
%%====================================================================

-spec t_arg( X :: x(), T :: t() )     -> t_arg().
      t_arg( X, T ) when is_atom( X ) -> {X, T}.

-spec t_str() -> t().
      t_str() -> 'Str'.

-spec t_file() -> t().
      t_file() -> 'File'.

-spec t_bool() -> t().
      t_bool() -> 'Bool'.

-spec t_rcd( ArgLst :: [t_arg()] )           -> t().
      t_rcd( ArgLst ) when is_list( ArgLst ) -> {'Rcd', ArgLst}.

-spec t_lst( T :: t() ) -> t().
      t_lst( T )        -> {'Lst', T}.


-spec t_fn( Tau :: tau(), ArgLst :: [t_arg()], TRet :: t() ) -> t().

t_fn( Tau, ArgLst, TRet )
when Tau =:= ntv orelse Tau =:= frn,
     is_list( ArgLst ) ->
  {'Fn', Tau, ArgLst, TRet}.


%%====================================================================
%% Expression constructors
%%====================================================================

-spec typed_bind( X :: x(), T :: t(), E :: e() ) -> typed_bind().
      typed_bind( X, T, E ) when is_atom( X )    -> {X, T, E}.

-spec e_bind( X :: x(), E :: e() )     -> e_bind().
      e_bind( X, E ) when is_atom( X ) -> {X, E}.

-spec r_bind( X :: x(), R :: r() )     -> r_bind().
      r_bind( X, R ) when is_atom( X ) -> {X, R}.

-spec str( S :: s() ) -> e().
      str( S )        -> str( na, S ).

-spec str( Info :: info(), S :: s() )    -> e().
      str( Info, S ) when is_binary( S ) -> {str, Info, S}.


-spec file( S :: s() ) -> e().
      file( S ) -> file( na, S ).

-spec file( Info :: info(), S :: s() ) -> e().
      file( Info, S )                  -> file( Info, S, na ).

-spec file( Info :: info(), S :: s(), H :: _ )  -> e().
      file( Info, S, H ) when is_binary( S )    -> {file, Info, S, H}.

-spec true() -> e().
      true() -> true( na ).

-spec true( Info :: info() ) -> e().
      true( Info )           -> {true, Info}.

-spec false() -> e().
      false() -> false( na ).

-spec false( Info :: info() ) -> e().
      false( Info )           -> {false, Info}.

-spec cnd( E1 :: e(), E2 :: e(), E3 :: e() ) -> e().
      cnd( E1, E2, E3 )                      -> cnd( na, E1, E2, E3 ).

-spec cnd( Info :: info(), E1 :: e(), E2 :: e(), E3 :: e() ) -> e().
      cnd( Info, E1, E2, E3 ) -> {cnd, Info, E1, E2, E3}.
  

-spec var( X :: x() ) -> e().
      var( X ) -> var( na, X ).

-spec var( Info :: info(), X :: x() )  -> e().
      var( Info, X ) when is_atom( X ) -> {var, Info, X}.


-spec lam_ntv( ArgLst :: [t_arg()], EBody :: e() ) -> e().
      lam_ntv( ArgLst, EBody ) -> lam_ntv( na, ArgLst, EBody ).

-spec lam_ntv( Info :: info(), ArgLst :: [t_arg()], EBody :: e() ) -> e().
lam_ntv( Info, ArgLst, EBody ) when is_list( ArgLst ) ->
  {lam_ntv, Info, ArgLst, EBody}.


-spec lam_frn( FName, ArgLst, RetType, L, Body ) -> e()
when FName   :: x(),
     ArgLst  :: [t_arg()],
     RetType :: t(),
     L       :: l(),
     Body    :: s().

lam_frn( FName, ArgLst, RetType, L, SBody ) ->
  lam_frn( na, FName, ArgLst, RetType, L, SBody ).


-spec lam_frn( Info, FName, ArgLst, RetType, L, Body ) -> e()
when Info    :: info(),
     FName   :: x(),
     ArgLst  :: [t_arg()],
     RetType :: t(),
     L       :: l(),
     Body    :: s().

lam_frn( Info, FName, ArgLst, RetType, L, Body )
when is_atom( FName ),
     is_list( ArgLst ),
     is_atom( L ),
     is_binary( Body ) ->
  {lam_frn, Info, FName, ArgLst, RetType, L, Body}.


-spec app( F :: e(), ArgLst :: [e_bind()] ) -> e().
      app( F, ArgLst )                      -> app( na, F, ArgLst ).

-spec app( Info :: info(), F :: e(), ArgLst :: [e_bind()] ) -> e().
      app( Info, F, ArgLst ) when is_list( ArgLst ) -> {app, Info, F, ArgLst}.

-spec cmp( E1 :: e(), E2 :: e() ) -> e().
      cmp( E1, E2 )               -> cmp( na, E1, E2 ).

-spec cmp( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      cmp( Info, E1, E2 )                         -> {cmp, Info, E1, E2}.

-spec neg( E :: e() ) -> e().
      neg( E )        -> neg( na, E ).

-spec neg( Info :: info(), E :: e() ) -> e().
      neg( Info, E )                  -> {neg, Info, E}.

-spec conj( E1 :: e(), E2 :: e() ) -> e().
      conj( E1, E2 )               -> conj( na, E1, E2 ).

-spec conj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      conj( Info, E1, E2 )                         -> {conj, Info, E1, E2}.

-spec disj( E1 :: e(), E2 :: e() ) -> e().
      disj( E1, E2 )               -> disj( na, E1, E2 ).

-spec disj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      disj( Info, E1, E2 )                         -> {disj, Info, E1, E2}.

-spec lst( T :: t(), ELst :: [e()] ) -> e().
      lst( T, ELst )                 -> lst( na, T, ELst ).

-spec lst( Info :: info(), T :: t(), ELst :: [e()] ) -> e().
      lst( Info, T, [] )      -> null( Info, T );
      lst( Info, T, [Hd|Tl] ) -> cons( Info, Hd, lst( Info, T, Tl ) ).

-spec null( T :: t() ) -> e().
      null( T )        -> null( na, T ).

-spec null( Info :: info(), T :: t() ) -> e().
      null( Info, T )                  -> {null, Info, T}.

-spec cons( E1 :: e(), E2 :: e() ) -> e().
      cons( E1, E2 )               -> cons( na, E1, E2 ).

-spec cons( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      cons( Info, E1, E2 ) -> {cons, Info, E1, E2}.

-spec append( E1 :: e(), E2 :: e() ) -> e().
      append( E1, E2 )               -> append( na, E1, E2 ).

-spec append( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      append( Info, E1, E2 )                         -> {append, Info, E1, E2}.

-spec isnil( E :: e() ) -> e().
      isnil( E )        -> isnil( na, E ).

-spec isnil( Info :: info(), E :: e() ) -> e().
      isnil( Info, E )                  -> {isnil, Info, E}.

-spec for( TRet :: t(), ArgLst :: [typed_bind()], E :: e() ) -> e().

for( TRet, ArgLst, E ) ->
  for( na, TRet, ArgLst, E ).

-spec for( Info, TRet, ArgLst, E ) -> e()
when Info   :: info(),
     TRet   :: t(),
     ArgLst :: [typed_bind()],
     E      :: e().

for( Info, TRet, ArgLst, E ) when is_list( ArgLst ) ->
  {for, Info, TRet, ArgLst, E}.


-spec fold( AccBind :: typed_bind(), ArgBind :: typed_bind(), E :: e() ) -> e().

fold( AccBind, ArgBind, E ) ->
  fold( na, AccBind, ArgBind, E ).


-spec fold( Info, AccBind, ArgBind, E ) -> e()
when Info    :: info(),
     AccBind :: typed_bind(),
     ArgBind :: typed_bind(),
     E       :: e().

fold( Info, {XAcc, TAcc, EAcc}, {XArg, TArg, EArg}, E )
when is_atom( XAcc ), is_atom( XArg ) ->
  {fold, Info, {XAcc, TAcc, EAcc}, {XArg, TArg, EArg}, E}.


-spec rcd( ArgLst :: [e_bind()] ) -> e().
      rcd( ArgLst ) -> rcd( na, ArgLst ).

-spec rcd( Info :: info(), ArgLst :: [e_bind()] ) -> e().
      rcd( Info, ArgLst ) when is_list( ArgLst )  -> {rcd, Info, ArgLst}.

-spec proj( X :: x(), E :: e() ) -> e().
      proj( X, E )               -> proj( na, X, E ).

-spec proj( Info :: info(), X :: x(), E :: e() ) -> e().
      proj( Info, X, E ) when is_atom( X )       -> {proj, Info, X, E}.

-spec fix( E :: e() ) -> e().
      fix( E )        -> fix( na, E ).

-spec fix( Info :: info(), E :: e() ) -> e().
      fix( Info, E )                  -> {fix, Info, E}.

-spec err( T :: t(), Msg :: binary() ) -> e().
      err( T, Msg )                    -> err( na, T, Msg ).

-spec err( Info :: info(), T :: t(), Msg :: binary() ) -> e().

err( Info, T, Msg ) when is_binary( Msg ) ->
  {err, Info, T, {user, Msg}}.

-spec assign( R :: r(), E :: e() ) -> assign().
      assign( R, E )               -> assign( na, R, E ).

-spec assign( Info :: info(), R :: r(), E :: e() ) -> assign().
      assign( Info, R, E ) when is_tuple( R )      -> {assign, Info, R, E}.


-spec create_closure( AssignLst, EBody ) ->
        {ok, e()} | {error, {ambiguous_name, info(), x()}}
when AssignLst :: [assign()],
     EBody     :: e().

create_closure( AssignLst, EBody ) ->

  F =
    fun

      F( {assign, Info, {r_var, X, T}, E}, EAcc ) ->
        app( Info,
             lam_ntv( Info, [t_arg( X, T )], EAcc ),
             [e_bind( X, E )] );

      F( {assign, _Info, {r_rcd, []}, _E}, EAcc ) ->
        EAcc;

      F( {assign, Info, Z = {r_rcd, [{X, R}|Tl]}, E}, EAcc ) ->

        case find_ambiguous( pattern_names( Z ) ) of

          unambiguous ->
            EAcc1 = F( assign( Info, R, proj( Info, X, E ) ), EAcc ),
            F( assign( Info, r_rcd( Tl ), E ), EAcc1 );

          {ambiguous, Name} ->
            throw( {ambiguous_name, Info, Name} )

        end

    end,

  try lists:foldr( F, EBody, AssignLst ) of
    E1 -> {ok, E1}
  catch
    throw:E2 -> {error, E2}
  end.


-spec find_ambiguous( NameLst :: [x()] ) -> unambiguous | {ambiguous, x()}.

find_ambiguous( NameLst ) ->

  F =
    fun

      F( [], _SeenLst ) ->
        unambiguous;
      
      F( [H|T], SeenLst ) when is_atom( H ) ->
        case lists:member( H, SeenLst ) of
          true  -> {ambiguous, H};
          false -> F( T, [H|SeenLst] )
        end

    end,

  F( NameLst, [] ).



-spec pattern_names( Pattern :: r() ) -> [x()].

pattern_names( {r_var, X, _T} )    ->
  [X];

pattern_names( {r_rcd, RBindLst} ) ->

  F =
    fun( {_X, R} ) ->
      pattern_names( R )
    end,

  lists:flatmap( F, RBindLst ).

