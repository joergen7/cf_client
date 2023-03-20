%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2013 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2013
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_client_process ).
-behaviour( cre_client ).


%%====================================================================
%% Exports
%%====================================================================

%% CRE client callbacks
-export( [init/1, is_value/2, recv/3, step/2, load/2, unload/2] ).

%% API functions
-export( [start_link/1, start_link/2] ).

-include( "cuneiform_lang.hrl" ).
-include( "cuneiform_cek.hrl" ).

-import( cuneiform_cek,  [ev/1, extract_outbox/1, is_finished/1, load/1,
                          recv_result/3, unload/1, set_unknown/1] ).
-import( cf_client_effi, [app_to_effi_request/2, effi_reply_to_expr/2] ).

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

-spec is_value( P :: prog(), UsrInfo :: _ ) -> boolean().

is_value( P, _ ) ->
  is_finished( P ).


-spec recv( P, ReplyLst, UsrInfo ) -> prog()
when P        :: prog(),
     ReplyLst :: [{#{ atom() => _ }, #{ atom() => _ }}],
     UsrInfo  :: _.

recv( P, ReplyLst, _UsrInfo ) ->

  F =
    fun( {A, R}, Prog ) ->
      #{ app_id := H } = A,
      E = effi_reply_to_expr( A, R ),
      recv_result( Prog, H, E )
    end,


  lists:foldl( F, P, ReplyLst ).


-spec step( P, UsrInfo ) -> Result
when P       :: prog(),
     UsrInfo :: _,
     Result  :: {ok, prog(), [e()]}.

step( P, _UsrInfo ) ->
  {Outbox, P1} = extract_outbox( ev( set_unknown( P ) ) ),
  Outbox1 = [app_to_effi_request( H, E ) || {H, E} <- Outbox],
  {ok, P1, Outbox1}.

-spec load( E, UserInfo ) -> prog()
when E        :: e(),
     UserInfo :: _.

load( E, _ ) ->
  load( E ).

-spec unload( P, UserInfo ) -> e()
when P        :: prog(),
     UserInfo :: _.

unload( P, _ ) ->
  unload( P ).
