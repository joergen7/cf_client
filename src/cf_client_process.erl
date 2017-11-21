-module( cf_client_process ).
-behaviour( cre_client ).

-include_lib( "cuneiform.hrl" ).


%%====================================================================
%% Exports
%%====================================================================

%% CRE client callbacks
-export( [init/1, is_value/2, recv/4, step/2] ).

%% API functions
-export( [start_link/1, start_link/2] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( F ) when is_function( F, 0 ) ->
  case F() of
    {ok, CreName} -> start_link( CreName );
    {error, E}    -> {error, E}
  end;

start_link( CreName ) ->
  cre_client:start_link( CreName, ?MODULE, [] ).


start_link( ClientName, F ) when is_function( F, 0 ) ->
  case F() of
    {ok, CreName} -> start_link( ClientName, CreName );
    {error, E}    -> {error, E}
  end;

start_link( Clientname, CreName ) ->
  cre_client:start_link( Clientname, CreName, ?MODULE, [] ).


%%====================================================================
%% CRE client callback implementations
%%====================================================================

-spec init( ClientArg :: _ ) -> [].

init( _ClientArg ) -> [].


-spec is_value( E :: e(), UsrInfo :: _ ) -> boolean().

is_value( E, _ ) ->
  cuneiform_sem:is_value( E ).


-spec recv( E, A, Delta, UsrInfo ) -> e()
when E       :: e(),
     A       :: e(),
     Delta   :: e(),
     UsrInfo :: _.

recv( E, A, Delta, _UsrInfo ) ->
  cuneiform_sem:subst_fut( E, A, Delta ).


-spec step( E, UsrInfo ) -> Result
when E       :: e(),
     UsrInfo :: _,
     Result  :: {ok, e()}
              | {ok_send, e(), e()}
              | norule.

step( E, _UsrInfo ) ->
  case cuneiform_sem:find_context( E, hole ) of

    {ok, E, Ctx} ->
      E1 = cuneiform_sem:reduce( E ),
      E2 = cuneiform_sem:in_hole( E1, Ctx ),
      {ok, E2};

    no_ctx ->
      norule

  end.
