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

-type e()           :: {var, info(), x()}
                     | {lam, info(), [{x(), t()}], {ntv, e()}}                   % binding form
                     | {lam, info(), [{x(), t()}], {frn, x(), t(), l(), s()}}
                     | {app, info(), e(), [{x(), e()}]}                          % binding form
                     | {fix, info(), e()}
                     | {fut, info(), t(), binary()}
                     | {str, info(), s()}
                     | {file, info(), s()}
                     | {true, info()}
                     | {false, info()}
                     | {cmp, info(), e(), e()}
                     | {conj, info(), e(), e()}
                     | {disj, info(), e(), e()}
                     | {neg, info(), e()}
                     | {isnil, info(), e()}
                     | {cnd, info(), e(), e(), e()}
                     | {null, info(), t()}
                     | {cons, info(), e(), e()}
                     | {hd, info(), e(), e()}
                     | {tl, info(), e(), e()}
                     | {append, info(), e(), e()}
                     | {for, info(), t(), nonempty_list( {x(), t(), e()} ), e()} % binding form
                     | {fold, info(), {x(), t(), e()}, {x(), t(), e()}, e()}     % binding form
                     | {rcd, info(), nonempty_list( {x(), e()} )}
                     | {proj, info(), x(), e()}
                     | {err, info(), t(), reason()}
                     | {close, info(), e(), env()}.

-type env()         :: #{ x() => {e(), env()} }.

-type info()        :: na
                     | pos_integer()
                     | {binary(), pos_integer()}.

-type reason()      :: {run, Node :: binary(), AppId :: binary(),
                             LamName :: x(), ExtendedScript :: binary(),
                             Output :: binary()}
                     | {stagein, Node :: binary(), AppId :: binary(),
                                 LamName :: x(), FileLst :: nonempty_list( s() )}
                     | {stageout, Node :: binary(), AppId :: binary(),
                                  LamName :: x(), FileLst :: nonempty_list( s() )}
                     | {user, Msg :: s()}.

-type x()           :: atom().

-type s()           :: binary().

-type t()           :: 'Str'
                     | 'File'
                     | 'Bool'
                     | {'Fn', [{x(), t()}], t()}
                     | {'Lst', t()}
                     | {'Rcd', [{x(), t()}]}.

-type l()           :: 'Awk'
                     | 'Bash'
                     | 'Elixir'
                     | 'Erlang'
                     | 'Gnuplot'
                     | 'Java'
                     | 'Javascript'
                     | 'Matlab'
                     | 'Octave'
                     | 'Perl'
                     | 'Python'
                     | 'R'
                     | 'Racket'
					 | 'Wal'.

-type r()           :: {r_var, x(), t()}
                     | {r_rcd, [{x(), r()}]}.

-type assign()      :: {assign, info(), r(), e()}.

