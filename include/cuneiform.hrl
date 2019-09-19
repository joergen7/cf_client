%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @version 0.1.7
%% @copyright 2015-2019
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
                     | {lam, info(), [{x(), t()}], {ntv, e()}}                % binding form
                     | {lam, info(), [{x(), t()}], {frn, x(), t(), l(), s()}}
                     | {app, info(), e(), [{x(), e()}]}                       % binding form
                     | {fix, info(), e()}
                     | {fut, info(), e()}
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
                     | {for, info(), t(), [{x(), t(), e()}], e()}             % binding form
                     | {fold, info(), {x(), t(), e()}, {x(), t(), e()}, e()}  % binding form
                     | {rcd, info(), [{x(), e()}]}
                     | {proj, info(), x(), e()}
                     | {err, info(), t(), reason()}.

-type info()        :: na
                     | pos_integer()
                     | {binary(), pos_integer()}.

-type reason()      :: {run, Node :: binary(), AppId :: binary(),
                             LamName :: x(), ExtendedScript :: binary(),
                             Output :: binary()}
                     | {stagein, Node :: binary(), AppId :: binary(),
                                 LamName :: x(), FileLst :: [binary()]}
                     | {stageout, Node :: binary(), AppId :: binary(),
                                  LamName :: x(), FileLst :: [binary()]}
                     | {user, Msg :: binary()}.

-type x()           :: binary().

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
                     | 'Racket'.

-type r()           :: {r_var, x(), t()}
                     | {r_rcd, [{x(), r()}]}.

-type assign()      :: {assign, info(), r(), e()}.

-type stage() :: load
               | scan
               | parse
               | type
               | runtime.

-type type_error()  :: {unbound_var,                               info(), x()}
                     | {ambiguous_name,                            info(), [x()]}
                     | {frn_fn_ambiguous_arg_or_return_field_name, info(), [x()]}
                     | {awk_frn_fn_first_arg_no_file,              info(), {x(), t()}}
                     | {awk_frn_fn_no_arg,                         info()}
                     | {awk_frn_fn_result_field_no_file,           info(), t()}
                     | {awk_frn_fn_no_result_field,                info()}
                     | {app_missing_bind,                          info(), {x(), t()}}
                     | {app_dangling_bind,                         info(), {x(), e()}}
                     | {app_bind_type_mismatch,                    info(), {x(), t(), e(), t()}}
                     | {app_arg_name_mismatch,                     info(), {x(), x()}}
                     | {fix_fn_no_arg,                             info(), {e(), t()}}
                     | {fix_fn_arg_type_mismatch,                  info(), {x(), t(), t()}}
                     | {fix_no_fn,                                 info(), {e(), t()}}
                     | {cmp_incomparable,                          info(), {e(), t(), e(), t()}}
                     | {conj_lhs_no_bool,                          info(), {e(), t()}}
                     | {conj_rhs_no_bool,                          info(), {e(), t()}}
                     | {disj_lhs_no_bool,                          info(), {e(), t()}}
                     | {disj_rhs_no_bool,                          info(), {e(), t()}}
                     | {neg_no_bool,                               info(), {e(), t()}}
                     | {isnil_no_list,                             info(), {e(), t()}}
                     | {cnd_result_type_mismatch,                  info(), {t(), t()}}
                     | {cnd_case_no_bool,                          info(), {e(), t()}}
                     | {cons_element_type_mismatch,                info(), {t(), e(), t()}}
                     | {cons_no_list,                              info(), {e(), t()}}
                     | {hd_type_mismatch,                          info(), {t(), e(), t()}}
                     | {hd_no_list,                                info(), {e(), t()}}
                     | {tl_type_mismatch,                          info(), {t(), e(), t()}}
                     | {tl_no_list,                                info(), {e(), t()}}
                     | {append_lhs_no_list,                        info(), {e(), t()}}
                     | {append_rhs_no_list,                        info(), {e(), t()}}
                     | {append_element_type_mismatch,              info(), {t(), t()}}
                     | {for_ambiguous_bind_name,                   info(), [x()]}
                     | {for_bind_type_mismatch,                    info(), {x(), t(), e(), t()}}
                     | {for_bind_no_list,                          info(), {x(), t(), e(), t()}}
                     | {for_body_type_mismatch,                    info(), {t(), e(), t()}}
                     | {fold_ambiguous_bind_name,                  info(), x()}
                     | {fold_acc_bind_type_mismatch,               info(), {x(), t(), e(), t()}}
                     | {fold_list_bind_type_mismatch,              info(), {x(), t(), e(), t()}}
                     | {fold_no_list_type,                         info(), {x(), t(), e(), t()}}
                     | {fold_body_type_mismatch,                   info(), {t(), e(), t()}}
                     | {rcd_ambiguous_field_name,                  info(), [x()]}
                     | {proj_field_missing,                        info(), x()}
                     | {proj_no_record,                            info(), {x(), t(), e()}}
                     .

