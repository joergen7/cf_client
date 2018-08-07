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


%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform.hrl" ).


%%====================================================================
%% Type definitions
%%====================================================================


%% Environment

-type env() :: #{ x() => {e(), env()} }.


%% Continuation Element

-type k() :: {cmp_lhs, info(), e(), env()}
           | {cmp_rhs, info(), e()}
           | {cnd_pred, info(), e(), e(), env()}
           | {neg_op, info()}
           | {conj_lhs, info(), e(), env()}
           | {conj_rhs, info(), e()}
           | {disj_lhs, info(), e(), env()}
           | {disj_rhs, info(), e()}
           | {app_fn, info(), [e_bind()], env()}
           | {app_body, info(), e(), [e_bind()]}
           | {app_arg, info(), e(), [e_bind()], x(), [e_bind()], env()}
           | {cons_hd, info(), e(), env()}
           | {cons_tl, info(), e()}
           | {append_lhs, info(), e(), env()}
           | {append_rhs, info(), e()}
           | {isnil_op, info()}
           | {rcd_field, info(), [e_bind()], x(), [e_bind()], env()}
           | {proj_op, info(), x()}
           | {fix_op, info()}
           | {for_arg, info(), t(), [e_bind()], x(), [e_bind()], e(), env()}
           | {for_acc, info(), x(), e_bind(), e(), env()}
           | {fold_arg, info(), e_bind(), x(), e(), env()}.


%% Program

-type prog() :: {{e() | {stalled, e()}, env()}, % closure (control string + environment)
                 [k()],                         % continuation
                 [e()]}.                        % foreign function applications




