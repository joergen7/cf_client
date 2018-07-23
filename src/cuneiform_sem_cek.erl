%% -*- erlang -*-
%%
%% cf_client
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
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_sem_cek ).
-behavior( cuneiform_sem ).


%%====================================================================
%% Exports
%%====================================================================

-export( [step/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_sem, [is_value/1] ).

%%====================================================================
%% Type definitions
%%====================================================================


%% Environment

-type env() :: #{ x() => {e(), env()} }.


%% Continuation Element

-type k() :: {lhs, info(), e(), env()}
           | {rhs, info(), e()}
           | {pred, e(), e(), env()}
           | neg.


%% Program

-type prog() :: {{e(), env()}, % closure (control string + environment)
                 [k()],        % continuation
                 [e()]}.       % foreign function applications




%%====================================================================
%% cuneiform_sem callback function implementations
%%====================================================================

-spec step( E ) -> Result
when E      :: e(),
     Result :: {ok, e(), [e()]}
             | norule.

step( E ) ->
  P = {{E, #{}}, [], []},
  {{E1, _}, [], Outbox} = eval_cek( P ),
  {ok, E1, Outbox}.


%%====================================================================
%% API functions
%%====================================================================

-spec eval_cek( P :: prog() ) -> prog().

eval_cek( P ) ->
  case step_cek( P ) of
    norule -> P;
    P1     -> eval_cek( P1 )
  end.


-spec step_cek( P :: prog() ) -> prog() | norule.


% descending rules

step_cek( {{{cmp, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{lhs, Info, E2, Env}|K], Outbox};

step_cek( {{{cnd, _, E1, E2, E3}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{pred, E2, E3, Env}|K], Outbox};

step_cek( {{{neg, _, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [neg|K], Outbox};


% ascending rules

step_cek( {{E1, _}, [{lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{rhs, Info, E1}|K], Outbox};

step_cek( {{E2, _}, [{rhs, Info, E1}|K], Outbox} ) ->
  case {E1, E2} of
    {{str, _, S1}, {str, _, S2}} ->
      {{{S1 =:= S2, Info}, #{}}, K, Outbox};
    {{B1, _}, {B2, _}} ->
      {{{B1 =:= B2, Info}, #{}}, K, Outbox};
    _ ->
      norule
  end;

step_cek( {{E1, _}, [{pred, E2, E3, Env}|K], Outbox} ) ->
  case E1 of
    {true, _}  -> {{E2, Env}, K, Outbox};
    {false, _} -> {{E3, Env}, K, Outbox}
  end;

step_cek( {{{B, Info}, _}, [neg|K], Outbox} ) ->
  {{{not B, Info}, #{}}, K, Outbox};


step_cek( _ ) ->
  norule. 