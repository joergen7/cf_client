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

recv( E, Request, Reply, _UsrInfo ) ->

  #{ app_id := AppId } = Reply,
  E1 = cf_client_effi:effi_reply_to_expr( Request, Reply ),

  cuneiform_sem:subst_fut( E, AppId, E1 ).


-spec step( E, UsrInfo ) -> Result
when E       :: e(),
     UsrInfo :: _,
     Result  :: {ok, e()}
              | {ok_send, e(), e()}
              | norule.

step( E, _UsrInfo ) ->
  case cuneiform_sem:find_context( E ) of

    {ok, E1, Ctx} ->
      case E1 of

        {app, Info, {lam_frn, _, _, _, _, _, _}, _} ->

          EffiRequest = cf_client_effi:app_to_effi_request( E1 ),
          #{ app_id := AppId } = EffiRequest,

          E2 = {fut, Info, AppId},
          E3 = cuneiform_sem:in_hole( E2, Ctx ),

          {ok_send, E3, EffiRequest};

        _ ->
          E2 = cuneiform_sem:reduce( E1 ),
          E3 = cuneiform_sem:in_hole( E2, Ctx ),
          {ok, E3}

      end;

    no_ctx ->
      norule

  end.


