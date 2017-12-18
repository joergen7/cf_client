%%====================================================================
%% Type definitions
%%====================================================================

-type info()        :: na
                     | pos_integer()
                     | {string(), pos_integer()}.

-type hash()        :: na
                     | binary().

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
                     | {fut, info(), hash()}
                     | {lst, info(), t(), [e()]}
                     | {append, info(), e, e}
                     | {isnil, info(), e}
                     | {for, info(), [e_bind()], e()}          % binding form
                     | {fold, info(), e_bind(), e_bind(), e()} % binding form
                     | {rcd, info(), [e_bind()]}
                     | {proj, info(), x(), e()}
                     | {fix, info(), e()}.
                     %| {assign, info(), r(), e(), e()}.        % binding form

-type assign()      :: {assign, info(), r(), e()}.

-type r()           :: {r_var, x(), t()}
                     | {r_rcd, [r_bind()]}.

-type ctx()         :: hole
                     | {cnd, info(), ctx(), e(), e()}
                     | {app, info(), ctx(), [e_bind()]}.

-type type_error()  :: {unbound_var, info(), x()}
                     | {type_mismatch, info(), {t(), t()}}
                     | {ambigious_name, info(), x()}
                     | {key_missing, info(), x()}
                     | {superfluous_key, info(), x()}
                     | {key_mismatch, info(), {x(), x()}}
                     | {no_record_type, info(), t()}
                     | {no_native_function_type, info(), t()}
                     | {no_argument, info(), t()}
                     | {no_list_type, info(), t()}.