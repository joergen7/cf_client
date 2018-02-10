%% -*- erlang -*-
%%
%% A Cuneiform client implementation.
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------


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

  error_logger:info_report( [{info,        "ready to start supervision tree"},
                             {application, cf_client},
                             {node,        node()},
                             {cre_node,    CreNode}] ),


  cf_client_sup:start_link( CreNode ).

stop(_State) ->
  ok.





