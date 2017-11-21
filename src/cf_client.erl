%%%-------------------------------------------------------------------
%% @doc cf_client public API
%% @end
%%%-------------------------------------------------------------------

-module( cf_client ).
-behaviour( application ).

%% Escript export
-export( [main/1] ).

%% API exports
-export( [start/0, setup_env/1] ).

%% Application callbacks
-export( [start/2, stop/1] ).


%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( ?MODULE ).


setup_env( CreNode ) when is_atom( CreNode ) ->
  ok = application:set_env( ?MODULE, cre_node, CreNode ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  CreNode =
    case application:get_env( ?MODULE, cre_node ) of

      undefined ->
        node();
              
      {ok, C} ->
        C

    end,

  cf_client_sup:start_link( CreNode ).

stop(_State) ->
  ok.



%%====================================================================
%% Escript main function
%%====================================================================

main( [CreNodeStr] ) ->

  CreNode = list_to_atom( CreNodeStr ),

  io:format( "application:     cf_client~nnode name:       ~p~n", [node()] ),

  % start client application
  ok = setup_env( CreNode ),
  ok = start(),

  io:format( "connected nodes: ~p~n", [nodes()] ),

  % a workflow expression falls from the sky
  E = cuneiform_lang:str( "blub" ),

  io:format( "state:           starting workflow~n" ),

  Result = cre_client:eval( cf_client, E ),

  io:format( "state:           workflow finished~n" ),

  io:format( "result:          ~p~n", [Result] ).
