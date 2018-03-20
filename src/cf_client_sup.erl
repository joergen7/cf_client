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
%% @version 0.1.2
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------


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
  supervisor:start_link( {local, cf_client_sup}, ?MODULE, CreNode ).

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
               intensity => 0,
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
