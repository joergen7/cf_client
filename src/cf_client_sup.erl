%%%-------------------------------------------------------------------
%% @doc cf_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module( cf_client_sup ).
-behaviour( supervisor ).

%% API
-export( [start_link/1] ).

%% Supervisor callbacks
-export( [init/1] ).

%%====================================================================
%% API functions
%%====================================================================

start_link( CreNode ) ->
  supervisor:start_link( ?MODULE, CreNode ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init( CreNode ) when is_atom( CreNode ) ->

  F =
    fun() ->
      cre:pid( CreNode )
    end,

  SupFlags = #{
               strategy  => one_for_one,
               intensity => 1,
               period    => 5
              },

  ClientNodeSpec = #{
                     id       => cf_client,
                     start    => {cf_client_process,
                                  start_link,
                                  [{local, cf_client}, F]},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [cf_client_process]
                    },

  SpecLst = [ClientNodeSpec],

  {ok, {SupFlags, SpecLst}}.

%%====================================================================
%% Internal functions
%%====================================================================
