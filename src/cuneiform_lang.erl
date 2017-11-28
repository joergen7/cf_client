-module( cuneiform_lang ).
-include_lib( "cuneiform.hrl" ).

%%====================================================================
%% Exports
%%====================================================================

%% Type constructors
-export( [t_fn_arg/2] ).
-export( [t_str/0, t_file/0, t_bool/0, t_fn/3] ).

%% Expression constructors
-export( [lam_ntv_arg/3, app_arg/2] ).
-export( [str/1, file/1, true/0, false/0, cnd/3, var/1, lam_ntv/2, app/2,
          cmp/2, conj/2, disj/2, neg/1] ).
-export( [str/2, file/2, true/1, false/1, cnd/4, var/2, lam_ntv/3, app/3,
          cmp/3, conj/3, disj/3, neg/2] ).

%% Pattern constructors
-export( [r_rcd_pair/2] ).
-export( [r_var/3, r_rcd/2] ).

%%====================================================================
%% Pattern constructors
%%====================================================================

-spec r_var( Info :: info(), X :: x(), T :: t() ) -> r().

r_var( Info, X, T ) when is_atom( X ) ->
  {r_var, Info, X, T}.


-spec r_rcd_pair( X :: x(), R :: r() ) -> {x(), r()}.

r_rcd_pair( X, R ) when is_atom( X ) ->
  {X, R}.


-spec r_rcd( Info :: info(), RLst :: [r()] ) -> r().

r_rcd( Info, RLst ) when is_list( RLst ) ->
  {r_var, Info, RLst}.


%%====================================================================
%% Type constructors
%%====================================================================

-spec t_fn_arg( S :: s(), T :: t() ) -> t_fn_arg().

t_fn_arg( S, T ) when is_list( S ) ->
  {S, T}.


-spec t_str() -> t().

t_str() ->
  'Str'.


-spec t_file() -> t().

t_file() ->
  'File'.


-spec t_bool() -> t().

t_bool() ->
  'Bool'.


-spec t_fn( Tau :: tau(), ArgLst :: [t_fn_arg()], TRet :: t() ) -> t().

t_fn( Tau, ArgLst, TRet )
when Tau =:= ntv orelse Tau =:= frn,
     is_list( ArgLst ) ->
  {'Fn', Tau, ArgLst, TRet}.


%%====================================================================
%% Expression constructors
%%====================================================================

-spec lam_ntv_arg( X :: x(), S :: s(), T :: t() ) -> lam_ntv_arg().

lam_ntv_arg( X, S, T )
when is_atom( X ),
     is_list( S ) ->
  {X, S, T}.


-spec app_arg( S :: s(), E :: e() ) -> app_arg().

app_arg( S, E ) when is_list( S ) ->
  {S, E}.


-spec str( S :: s() ) -> e().

str( S ) ->
  str( na, S ).


-spec str( Info :: info(), S :: s() ) -> e().

str( Info, S )
when is_list( S ) ->
  {str, Info, S}.


-spec file( S :: s() ) -> e().

file( S ) ->
  file( na, S ).


-spec file( Info :: info(), S :: s() ) -> e().

file( Info, S )
when is_list( S ) ->
  {file, Info, S}.


-spec true() -> e().

true() ->
  true( na ).


-spec true( Info :: info() ) -> e().

true( Info ) ->
  {true, Info}.


-spec false() -> e().

false() ->
  false( na ).


-spec false( Info :: info() ) -> e().

false( Info ) ->
  {false, Info}.


-spec cnd( EIf :: e(), EThen :: e(), EElse :: e() ) -> e().

cnd( EIf, EThen, EElse ) ->
  cnd( na, EIf, EThen, EElse ).


-spec cnd( Info :: info(), EIf :: e(), EThen :: e(), EElse :: e() ) -> e().

cnd( Info, EIf, EThen, EElse ) ->
  {cnd, Info, EIf, EThen, EElse}.
  

-spec var( X :: x() ) -> e().

var( X ) ->
  var( na, X ).


-spec var( Info :: info(), X :: x() ) -> e().

var( Info, X )
when is_atom( X ) ->
  {var, Info, X}.


-spec lam_ntv( ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().

lam_ntv( ArgLst, EBody ) ->
  lam_ntv( na, ArgLst, EBody ).


-spec lam_ntv( Info :: info(), ArgLst :: [lam_ntv_arg()], EBody :: e() ) -> e().

lam_ntv( Info, ArgLst, EBody )
when is_list( ArgLst ) ->
  {lam_ntv, Info, ArgLst, EBody}.


-spec app( F :: e(), ArgLst :: [app_arg()] ) -> e().

app( F, ArgLst ) ->
  app( na, F, ArgLst ).


-spec app( Info :: info(), F :: e(), ArgLst :: [app_arg()] ) -> e().

app( Info, F, ArgLst )
when is_list( ArgLst ) ->
  {app, Info, F, ArgLst}.


-spec cmp( E1 :: e(), E2 :: e() ) -> e().

cmp( E1, E2 ) ->
  cmp( na, E1, E2 ).


-spec cmp( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

cmp( Info, E1, E2 ) ->
  {cmp, Info, E1, E2}.


-spec neg( E :: e() ) -> e().

neg( E ) ->
  neg( na, E ).


-spec neg( Info :: info(), E :: e() ) -> e().

neg( Info, E ) ->
  {neg, Info, E}.


-spec conj( E1 :: e(), E2 :: e() ) -> e().

conj( E1, E2 ) ->
  conj( na, E1, E2 ).


-spec conj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

conj( Info, E1, E2 ) ->
  {conj, Info, E1, E2}.


-spec disj( E1 :: e(), E2 :: e() ) -> e().

disj( E1, E2 ) ->
  disj( na, E1, E2 ).


-spec disj( Info :: info(), E1 :: e(), E2 :: e() ) -> e().

disj( Info, E1, E2 ) ->
  {disj, Info, E1, E2}.


