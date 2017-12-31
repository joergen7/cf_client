-module( cuneiform_lang ).
-include_lib( "cuneiform.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

%% Type constructors
-export( [t_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3, t_rcd/1, t_lst/1] ).

%% Expression constructors
-export( [lam_ntv_arg/2, e_bind/2, r_bind/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2, lam_frn/5,
          app/2, cmp/2, conj/2, disj/2, neg/1, lst/2, append/2, isnil/1,
          for/3, fold/3, rcd/1, proj/2, fix/1, cons/3, null/1] ).
-export( [str/2, file/2, file/3, true/1, false/1, cnd/4, var/2, lam_ntv/3, lam_frn/6,
          app/3, cmp/3, conj/3, disj/3, neg/2, lst/3, append/3, isnil/2,
          for/4, fold/4, rcd/2, proj/3, fix/2, cons/4, null/2] ).

%% Assignment
-export( [assign/2, assign/3, create_closure/2] ).

%% Pattern constructors
-export( [r_var/2, r_rcd/1] ).

%% Language constructors
-export( [l_bash/0, l_octave/0, l_perl/0, l_python/0, l_r/0, l_racket/0] ).

-export( [find_ambiguous/1] ).
-export( [lst_literal_to_list/1, is_lst_literal/1, lst_literal_type/1] ).

%%====================================================================
%% Language constructors
%%====================================================================

-spec l_bash()   -> l(). l_bash()   -> 'Bash'.
-spec l_octave() -> l(). l_octave() -> 'Octave'.
-spec l_perl()   -> l(). l_perl()   -> 'Perl'.
-spec l_python() -> l(). l_python() -> 'Python'.
-spec l_r()      -> l(). l_r()      -> 'R'.
-spec l_racket() -> l(). l_racket() -> 'Racket'.

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

-spec lam_ntv_arg( X :: x(), T :: t() )     -> lam_ntv_arg().
      lam_ntv_arg( X, T ) when is_atom( X ) -> {X, X, T}.

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


-spec lam_ntv( ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().
      lam_ntv( ArgLst, EBody ) -> lam_ntv( na, ArgLst, EBody ).

-spec lam_ntv( Info :: info(), ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().
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
      lst( Info, T, [Hd|Tl] ) -> cons( Info, T, Hd, lst( Info, T, Tl ) ).

-spec null( T :: t() ) -> e().
      null( T )        -> null( na, T ).

-spec null( Info :: info(), T :: t() ) -> e().
      null( Info, T )                  -> {null, Info, T}.

-spec cons( T :: t(), E1 :: e(), E2 :: e() ) -> e().
      cons( T, E1, E2 )                      -> cons( na, T, E1, E2 ).

-spec cons( Info :: info(), T :: t(), E1 :: e(), E2 :: e() ) -> e().
      cons( Info, T, E1, E2 ) -> {cons, Info, T, E1, E2}.

-spec append( E1 :: e(), E2 :: e() ) -> e().
      append( E1, E2 )               -> append( na, E1, E2 ).

-spec append( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      append( Info, E1, E2 )                         -> {append, Info, E1, E2}.

-spec isnil( E :: e() ) -> e().
      isnil( E )        -> isnil( na, E ).

-spec isnil( Info :: info(), E :: e() ) -> e().
      isnil( Info, E )                  -> {isnil, Info, E}.

-spec for( TRet :: t(), ArgLst :: [e_bind()], E :: e() ) -> e().
      for( TRet, ArgLst, E )                             -> for( na, TRet, ArgLst, E ).

-spec for( Info :: info(), TRet :: t(), ArgLst :: [e_bind()], E :: e() ) -> e().
      for( Info, TRet, ArgLst, E ) when is_list( ArgLst ) -> {for, Info, TRet, ArgLst, E}.

-spec fold( InitBind :: e_bind(), LstBind :: e_bind(), E :: e() ) -> e().
      fold( InitBind, LstBind, E ) -> fold( na, InitBind, LstBind, E ).


-spec fold( Info, InitBind, LstBind, E ) -> e()
when Info    :: info(),
     InitBind :: e_bind(),
     LstBind :: e_bind(),
     E       :: e().

fold( Info, InitBind, LstBind, E )
when is_tuple( InitBind ),
     is_tuple( LstBind ) ->
  {fold, Info, InitBind, LstBind, E}.


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

-spec assign( R :: r(), E :: e() ) -> assign().
      assign( R, E )               -> assign( na, R, E ).

-spec assign( Info :: info(), R :: r(), E :: e() ) -> assign().
      assign( Info, R, E ) when is_tuple( R )      -> {assign, Info, R, E}.


-spec create_closure( AssignLst, EBody ) -> e()
when AssignLst :: [assign()],
     EBody     :: e().

create_closure( AssignLst, EBody ) ->

  F =
    fun

      F( {assign, Info, {r_var, X, T}, E}, EAcc ) ->
        app( Info,
             lam_ntv( Info, [lam_ntv_arg( X, T )], EAcc ),
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


-spec is_lst_literal( E :: e() ) -> boolean().

is_lst_literal( {cons, _, _, _, _} ) -> true;
is_lst_literal( {null, _, _} )       -> true;
is_lst_literal( _ )                  -> false.


-spec lst_literal_to_list( E ) -> [e()]
when E :: {cons, info(), t(), e(), e()}
        | {null, info(), t()}.

lst_literal_to_list( {cons, _, _, E1, E2} ) -> [E1|lst_literal_to_list( E2 )];
lst_literal_to_list( {null, _, _} )         -> [].


-spec lst_literal_type( E ) -> t()
when E :: {cons, info(), t(), e(), e()}
        | {null, info(), t()}.

lst_literal_type( {cons, _, T, _, _} ) -> T;
lst_literal_type( {null, _, T} )       -> T.