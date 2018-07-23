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

-type k() :: {cmp_lhs, info(), e(), env()}
           | {cmp_rhs, info(), e()}
           | {pred, e(), e(), env()}
           | {neg, info()}
           | {conj_lhs, info(), e(), env()}
           | {conj_rhs, info(), e()}.


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
  {{E1, Env}, [{cmp_lhs, Info, E2, Env}|K], Outbox};

step_cek( {{{cnd, _, E1, E2, E3}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{pred, E2, E3, Env}|K], Outbox};

step_cek( {{{neg, Info, E1}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{neg, Info}|K], Outbox};

step_cek( {{{conj, Info, E1, E2}, Env}, K, Outbox} ) ->
  {{E1, Env}, [{conj_lhs, Info, E2, Env}|K], Outbox};


% ascending rules

step_cek( {{E1, _}, [{cmp_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{cmp_rhs, Info, E1}|K], Outbox};

step_cek( {{{str, _, S2}, _}, [{cmp_rhs, Info, {str, _, S1}}|K], Outbox} ) ->
  {{{S1 =:= S2, Info}, #{}}, K, Outbox};

step_cek( {{{B2, _}, _}, [{cmp_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 =:= B2, Info}, #{}}, K, Outbox};

step_cek( {{{true, _}, _}, [{pred, E2, _, Env}|K], Outbox} ) ->
  {{E2, Env}, K, Outbox};

step_cek( {{{false, _}, _}, [{pred, _, E3, Env}|K], Outbox} ) ->
  {{E3, Env}, K, Outbox};

step_cek( {{{B1, _}, _}, [{neg, Info}|K], Outbox} ) ->
  {{{not B1, Info}, #{}}, K, Outbox};

step_cek( {{{B1, Info1}, _}, [{conj_lhs, Info, E2, Env}|K], Outbox} ) ->
  {{E2, Env}, [{conj_rhs, Info, {B1, Info1}}|K], Outbox};

step_cek( {{{B2, _}, _}, [{conj_rhs, Info, {B1, _}}|K], Outbox} ) ->
  {{{B1 and B2, Info}, #{}}, K, Outbox};


step_cek( _ ) ->
  norule. 