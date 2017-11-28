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

-type t()           :: 'Str'
                     | 'File'
                     | 'Bool'
                     | {'Fn', tau(), [t_fn_arg()], t()}.


-type lam_ntv_arg() :: {x(), s(), t()}.

-type app_arg()     :: {s(), e()}.

-type e()           :: {str, info(), s()}
                     | {file, info(), s()}
                     | {true, info()}
                     | {false, info()}
                     | {cnd, info(), e(), e(), e()}
                     | {var, info(), x()}
                     | {lam_ntv, info(), [lam_ntv_arg()], e()}
                     | {app, info(), e(), [app_arg()]}.

-type ctx()         :: hole
                     | {cnd, info(), ctx(), e(), e()}
                     | {app, info(), ctx(), [app_arg()]}.


