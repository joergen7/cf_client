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

%%====================================================================
%% Type definitions
%%====================================================================

-type k() :: mt |
             {app_fn, info(), [{x(), e()}], env(), k()} |
             {app_arg, info(),
                       e(),
                       [{x(), e()}],
                       [cprop()],
                       x(),
                       [{x(), e()}],
                       env(),
                       k()} |
             {fix_op, info(), k()} |
             {cmp_lhs, info(), e(), env(), k()} |
             {cmp_rhs, info(), e(), cprop(), env(), k()} |
             {conj_lhs, info(), e(), env(), k()} |
             {conj_rhs, info(), e(), k()} |
             {disj_lhs, info(), e(), env(), k()} |
             {disj_rhs, info(), e(), k()} |
             {neg_op, info(), k()} |
             {isnil_op, info(), k()} |
             {cnd_if, info(), e(), e(), env(), k()} |
             {cons_hd, info(), e(), env(), k()} |
             {cons_tl, info(), e(), cprop(), k()} |
             {hd_op, info(), e(), env(), k()} |
             {tl_op, info(), e(), env(), k()} |
             {append_lhs, info(), e(), env(), k()} |
             {append_rhs, info(), e(), k()} |
             {for_arg, info(),
                       t(),
                       [{x(), t(), e()}],
                       x(),
                       t(),
                       [{x(), t(), e()}],
                       e(),
                       env(),
                       k()} |
             {fold_arg, info(), {x(), t(), e()}, x(), t(), e(), env(), k()} |
             {rcd_field, info(),
                         [{x(), e()}],
                         [cprop()],
                         x(),
                         [{x(), e()}],
                         env(),
                         k()} |
             {proj_op, info(), x(), k()}.

-type cprop() :: unknown |
                 stalled |
                 value.

-type comm() :: {[{binary(), e()}],  % outbox
                 sets:set(binary()),  % awaybox
                 #{binary() => e()}}.  % inbox

-type prog() :: {comm(),  % state of communication
                 e(),  % control string
                 env(),  % scope environment
                 k(),  % continuation
                 cprop()}.  % control string property
