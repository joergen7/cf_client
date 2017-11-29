-module( cuneiform_lang ).
-include_lib( "cuneiform.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

%% Type constructors
-export( [t_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3, t_rcd/1] ).

%% Expression constructors
-export( [lam_ntv_arg/3, lam_frn_arg/2, app_arg/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2,
          lam_frn/5, app/2, cmp/2, conj/2, disj/2, neg/1] ).
-export( [str/2, file/2, true/1, false/1, cnd/4, var/2, lam_ntv/3,
          lam_frn/6, app/3, cmp/3, conj/3, disj/3, neg/2] ).

%% Pattern constructors
-export( [r_rcd_pair/2] ).
-export( [r_var/3, r_rcd/2] ).

%% Language constructors
-export( [l_bash/0, l_octave/0, l_perl/0, l_python/0, l_r/0, l_racket/0] ).

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

-spec r_arg( X :: x(), R :: r() )     -> {x(), r()}.
      r_arg( X, R ) when is_atom( X ) -> {X, R}.

-spec r_var( Info :: info(), X :: x(), T :: t() ) -> r().
      r_var( Info, X, T ) when is_atom( X )       -> {r_var, Info, X, T}.

-spec r_rcd( Info :: info(), RLst :: [r_arg()] ) -> r().
      r_rcd( Info, RLst ) when is_list( RLst )   -> {r_rcd, Info, RLst}.


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

-spec lam_ntv_arg( XIn :: x(), XEx :: x(), T :: t() ) -> lam_ntv_arg().

lam_ntv_arg( XIn, XEx, T )
when is_atom( XIn ), is_atom( XEx ) ->
  {X, S, T}.

-spec x_bind( X :: x(), E :: e() )     -> x_bind().
      x_bind( X, E ) when is_atom( X ) -> {X, E}.

-spec r_bind( R :: r(), E :: e() )     -> r_bind().
      r_bind( R, E )                   -> {R, E}.

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


-spec lam_frn( FName, ArgLst, L, Body ) -> e()
when FName  :: x(),
     ArgLst :: [lam_frn_arg()],
     L      :: l(),
     Body   :: s().

lam_frn( FName, ArgLst, RetType, L, SBody ) ->
  lam_frn( na, FName, ArgLst, RetType, L, SBody ).


-spec lam_frn( Info, FName, ArgLst, RetType, L, Body ) -> e()
when Info    :: info(),
     FName   :: x(),
     ArgLst  :: [lam_frn_arg()],
     RetType :: t(),
     L       :: l(),
     Body    :: s().

lam_frn( Info, FName, ArgLst, RetType, L, Body )
when is_atom( FName ),
     is_list( ArgLst ),
     is_atom( L ),
     is_binary( Body ) ->
  {lam_frn, Info, SName, ArgLst, RetType, L, Body}.


-spec app( F :: e(), ArgLst :: [x_bind()] ) -> e().
      app( F, ArgLst )                      -> app( na, F, ArgLst ).

-spec app( Info :: info(), F :: e(), ArgLst :: [x_bind()] ) -> e().
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

-spec fut( H :: _ ) -> e().
      fut( H )      -> fut( na, H ).

-spec fut( Info :: info(), H :: _ ) -> e().
      fut( Info, H )                -> {fut, Info, H}.

-spec lst( T :: t(), ELst :: [e()] ) -> e().
      lst( T, ELst )                 -> lst( na, T, ELst )

-spec lst( Info :: info(), T :: t(), ELst :: [e()] ) -> e().
      lst( Info, T, ELst ) when is_list( ELst )      -> {lst, Info, T, ELst}.

-spec append( E1 :: e(), E2 :: e() ) -> e().
      append( E1, E2 )               -> append( na, E1, E2 ).

-spec append( Info :: info(), E1 :: e(), E2 :: e() ) -> e().
      append( Info, E1, E2 )                         -> {append, Info, E1, E2}.

-spec isnil( E :: e() ) -> e().
      isnil( E )        -> isnil( na, E ).

-spec isnil( Info :: info(), E :: e() ) -> e().
      isnil( Info, E )                  -> {isnil, Info, E}.

-spec for( ArgLst :: [x_bind()], E :: e() ) -> e().
      for( ArgLst, E )                      -> for( na, ArgLst, E ).

-spec for( Info :: info(), ArgLst :: [x_bind()], E :: e() ) -> e().
      for( Info, ArgLst, E ) when is_list( ArgLst )         -> {for ArgLst, E}.

-spec fold( InitArg :: x_bind(), ArgLst :: [x_bind()], E :: e() ) -> e().
      fold( InitArg, ArgLst, E ) -> fold( na, InitArg, ArgLst, E ).


-spec fold( Info, InitArg, ArgLst, E ) -> e()
when Info    :: info(),
     InitArg :: x_bind(),
     ArgLst  :: [x_bind()],
     E       :: e().

fold( Info, InitArg, ArgLst, E )
when is_tuple( InitArg ),
     is_list( ArgLst ) ->
  {fold, Info, InitArg, ArgLst, E}.


-spec rcd( ArgLst :: [x_bind()] ) -> e().
      rcd( ArgLst ) -> rcd( na, ArgLst ).

-spec rcd( Info :: info(), ArgLst :: [x_bind()] ) -> e().
      rcd( Info, ArgLst ) when is_list( ArgLst )  -> {rcd, Info, ArgLst}.


