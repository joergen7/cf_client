%%%-------------------------------------------------------------------
%% @doc cf_client public API
%% @end
%%%-------------------------------------------------------------------

-module( cf_client ).
-behaviour( application ).

%% API exports
-export( [start/0] ).

%% Application callbacks
-export( [start/2, stop/1] ).


%%====================================================================
%% API functions
%%====================================================================

start() ->
  application:start( ?MODULE ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  {ok, DefaultMap} = application:get_env( ?MODULE, default_map ),
  {ok, GlobalFile} = application:get_env( ?MODULE, global_file ),
  {ok, UserFile}   = application:get_env( ?MODULE, user_file ),

  SupplFile =
    case application:get_env( ?MODULE, suppl_file ) of
      {ok, S}   -> S;
      undefined -> undefined
    end,

  FlagMap =
    case application:get_env( ?MODULE, flag_map ) of
      {ok, M}   -> M;
      undefined -> #{}
    end,

  ConfMap = lib_conf:create_conf( DefaultMap, GlobalFile, UserFile, SupplFile,
                                  FlagMap ),


  CreNode =
    case maps:get( cre_node, ConfMap ) of

      <<"node">> ->
        ok = cre:start(),
        ok = cf_worker:start(),
        node();
              
      B when is_binary( B ) ->
        binary_to_atom( B, utf8 )

    end,

  error_logger:info_report( [{application, cf_worker},
                             {node,        node()},
                             {cre_node,    CreNode}] ),


  cf_client_sup:start_link( CreNode ).

stop(_State) ->
  ok.





