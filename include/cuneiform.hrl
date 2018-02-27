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
%% @version 0.1.1
%% @copyright 2015-2018 Jörgen Brandt
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

-type info()        :: na
                     | pos_integer()
                     | {string(), pos_integer()}.

-type hash()        :: na
                     | binary().

-type reason()      :: {run, AppId :: binary(), LamName :: binary(),
                             ExtendedScript :: binary(), Output :: binary()}
                     | {stagein, AppId :: binary(), LamName :: binary(),
                                 FileLst :: [binary()]}
                     | {stageout, AppId :: binary(), LamName :: binary(),
                                  FileLst :: [binary()]}
                     | {user, Msg :: binary()}.

-type x()           :: atom().

-type s()           :: binary().

-type tau()         :: ntv
                     | frn.

-type t_arg()       :: {x(), t()}.

-type t()           :: 'Str'
                     | 'File'
                     | 'Bool'
                     | {'Fn', tau(), [t_arg()], t()}
                     | {'Rcd', [t_arg()]}
                     | {'Lst', t()}.

-type l()           :: 'Bash'
                     | 'Matlab'
                     | 'Octave'
                     | 'Perl'
                     | 'Python'
                     | 'R'
                     | 'Racket'.

-type lam_ntv_arg() :: {InternalName :: x(), ExternalName :: x(), t()}.

-type e_bind()      :: {x(), e()}.
-type r_bind()      :: {x(), r()}.

-type e()           :: {str, info(), s()}
                     | {cmp, info(), e(), e()}
                     | {file, info(), s(), hash()}
                     | {true, info()}
                     | {false, info()}
                     | {cnd, info(), e(), e(), e()}
                     | {neg, info(), e()}
                     | {conj, info(), e(), e()}
                     | {disj, info(), e(), e()}
                     | {var, info(), x()}
                     | {lam_ntv, info(), [lam_ntv_arg()], e()} % binding form
                     | {lam_frn, info(), x(), [t_arg()], t(), l(), s()}
                     | {app, info(), e(), [e_bind()]}
                     | {fut, info(), t(), hash()}
                     | {null, info(), t()}
                     | {cons, info(), t(), e(), e()}
                     | {append, info(), e, e}
                     | {isnil, info(), e}
                     | {for, info(), t(), [e_bind()], e()}     % binding form
                     | {fold, info(), e_bind(), e_bind(), e()} % binding form
                     | {rcd, info(), [e_bind()]}
                     | {proj, info(), x(), e()}
                     | {fix, info(), e()}
                     | {err, info(), t(), reason()}.

-type assign()      :: {assign, info(), r(), e()}.

-type r()           :: {r_var, x(), t()}
                     | {r_rcd, [r_bind()]}.

-type ctx()         :: hole
                     | {cmp, info(), ctx(), e()}
                     | {cmp, info(), e(), ctx()}
                     | {cnd, info(), ctx(), e(), e()}
                     | {neg, info(), ctx()}
                     | {conj, info(), ctx(), e()}
                     | {conj, info(), e(), ctx()}
                     | {disj, info(), ctx(), e()}
                     | {disj, info(), e(), ctx()}
                     | {app, info(), ctx(), [e_bind()]}
                     | {app, info(), e(), [{x(), e() | ctx()}]}
                     | {lst, info(), t(), [e() | ctx()]}
                     | {append, info(), ctx(), e()}
                     | {append, info(), e(), ctx()}
                     | {isnil, info(), ctx()}
                     | {for, info(), [{x(), e() | ctx()}], e()}
                     | {for, info(), [e_bind()], ctx()}
                     | {fold, info(), {x(), ctx()}, e_bind(), e()}
                     | {fold, info(), e_bind(), {x(), ctx()}, e()}
                     | {fold, info(), e_bind(), e_bind(), ctx()}
                     | {rcd, info(), [{x(), e() | ctx()}]}
                     | {proj, info(), x(), ctx()}
                     | {fix, info(), ctx()}.

-type type_error()  :: {unbound_var, info(), x()}
                     | {type_mismatch, info(), {t(), t()}}
                     | {ambiguous_name, info(), x()}
                     | {key_missing, info(), x()}
                     | {superfluous_key, info(), x()}
                     | {key_mismatch, info(), {x(), x()}}
                     | {no_record_type, info(), t()}
                     | {no_native_function_type, info(), t()}
                     | {no_argument, info(), t()}
                     | {no_list_type, info(), t()}
                     | {no_comparable_type, info(), t()}.

-type stage() :: load
               | scan
               | parse
               | type
               | runtime.

