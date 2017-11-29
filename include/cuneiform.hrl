%%====================================================================
%% Type definitions
%%====================================================================

-type info()        :: na
                     | pos_integer()
                     | {string(), pos_integer()}.

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
                     | 'Octave'
                     | 'Perl'
                     | 'Python'
                     | 'R'
                     | 'Racket'.

-type lam_ntv_arg() :: {x(), x(), t()}.

-type x_bind()      :: {x(), e()}.
-type r_bind()      :: {r(), e()}.

-type e()           :: {str, info(), s()}
                     | {cmp, info(), e(), e()}
                     | {file, info(), s(), _}
                     | {true, info()}
                     | {false, info()}
                     | {cnd, info(), e(), e(), e()}
                     | {neg, info(), e()}
                     | {conj, info(), e(), e()}
                     | {disj, info(), e(), e()}
                     | {var, info(), x()}
                     | {lam_ntv, info(), [lam_ntv_arg()], e()}
                     | {lam_frn, info(), x(), [t_arg()], t(), l(), s()}
                     | {app, info(), e(), [x_bind()]}
                     | {fut, info(), _}
                     | {lst, info(), t(), [e()]}
                     | {append, info(), e, e}
                     | {isnil, info(), e}
                     | {for, info(), [x_bind()], e()}
                     | {fold, info(), x_bind(), [x_bind()], e()}
                     | {rcd, info(), [x_bind()]}
                     | {proj, info(), x(), e()}
                     | {fix, info(), e()}
                     | {assign, r_bind(), e()}.

-type r_arg()       :: {x(), r()}.

-type r()           :: {r_var, info(), x(), t()}
                     | {r_rcd, info(), [r_arg()]}.

-type ctx()         :: hole
                     | {cnd, info(), ctx(), e(), e()}
                     | {app, info(), ctx(), [s_bind()]}.

