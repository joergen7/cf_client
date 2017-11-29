%%====================================================================
%% Type definitions
%%====================================================================

-type info()        :: na
                     | pos_integer()
                     | {string(), pos_integer()}.

-type s()           :: string().

-type x()           :: atom().


-type tau()         :: ntv
                     | frn.

-type t_fn_arg()    :: {s(), t()}.

-type t_rcd_arg()   :: {x(), t()}.

-type t()           :: 'Str'
                     | 'File'
                     | 'Bool'
                     | {'Fn', tau(), [t_fn_arg()], t()}
                     | {'Rcd', [t_rcd_arg()]}.


-type lam_ntv_arg() :: {x(), s(), t()}.

-type lam_frn_arg() :: {s(), t()}.

-type app_arg()     :: {s(), e()}.

-type rcd_arg()     :: {x(), e()}.

-type e()           :: {str, info(), s()}
                     | {file, info(), s()}
                     | {true, info()}
                     | {false, info()}
                     | {cnd, info(), e(), e(), e()}
                     | {var, info(), x()}
                     | {lam_ntv, info(), [lam_ntv_arg()], e()}
                     | {lam_frn, info(), s(), [lam_frn_arg()], l(), s()}
                     | {app, info(), e(), [app_arg()]}
                     | {cmp, info(), e(), e()}
                     | {neg, info(), e()}
                     | {conj, info(), e(), e()}
                     | {disj, info(), e(), e()}
                     | {rcd, info(), [rcd_arg()]}
                     | {proj, info(), x(), e()}.

-type ctx()         :: hole
                     | {cnd, info(), ctx(), e(), e()}
                     | {app, info(), ctx(), [app_arg()]}.

-type r_rcd_arg()   :: {x(), r()}.

-type r()           :: {r_var, info(), x(), t()}
                     | {r_rcd, info(), [r_rcd_arg()]}.

-type l()           :: 'Bash'.