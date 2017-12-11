%%====================================================================
%% Symbol Declaration
%%====================================================================

Nonterminals
  define e imp l t_arg_lst t_arg r script t stat e_bind_lst e_bind r_bind_lst
  r_bind e_lst define_lst from_lst from.

Terminals
  l_bash l_octave l_perl l_python l_r l_racket
  t_str t_file t_bool t_fn_frn t_fn_ntv
  assign bar wedge cmp cnd colon
  comma def do else eq false fold for halt import in
  isnil larrow lbrace lparen lsquarebr ltag neg vee plus
  rarrow rbrace rparen rsquarebr rtag semicolon then true id
  intlit strlit filelit body.

%%====================================================================
%% Syntax Definition
%%====================================================================

Rootsymbol script.

script          -> stat                       : '$1'.
script          -> stat script                : join_stat( '$1', '$2' ).

stat            -> imp                        : {['$1'], [], [], []}.
stat            -> define                     : {[], [], ['$1'], []}.
stat            -> e semicolon                : {[], [], [], ['$1']}.

imp             -> import filelit semicolon   : visit_import( '$2' ).

define          -> assign r eq e semicolon                                             : {'$2', '$4'}.
define          -> def id lparen rparen rarrow ltag t_arg_lst rtag in l body           : visit_def_frn( '$2', [], '$7', '$10', '$11' ).
define          -> def id lparen t_arg_lst rparen rarrow ltag t_arg_lst rtag in l body : visit_def_frn( '$2', '$4', '$8', '$11', '$12' ).
define          -> def id lparen rparen rarrow t lbrace e rbrace                       : visit_def_ntv( '$2', [], '$6', [], '$8' ).
define          -> def id lparen rparen rarrow t lbrace define_lst e rbrace            : visit_def_ntv( '$2', [], '$6', '$8', '$9' ).
define          -> def id lparen t_arg_lst rparen rarrow t lbrace e rbrace             : visit_def_ntv( '$2', '$4', '$7', [], '$9' ).
define          -> def id lparen t_arg_lst rparen rarrow t lbrace define_lst e rbrace  : visit_def_ntv( '$2', '$4', '$7', '$9', '$10' ).

define_lst      -> define                     : ['$1'].
define_lst      -> define define_lst          : ['$1'|'$2'].

r               -> id colon t                 : visit_r_var( '$1', '$3' ).
r               -> ltag r_bind_lst rtag       : visit_r_rcd( '$1', '$2' ).

l               -> l_bash                     : cuneiform_lang:l_bash().
l               -> l_octave                   : cuneiform_lang:l_octave().
l               -> l_perl                     : cuneiform_lang:l_perl().
l               -> l_python                   : cuneiform_lang:l_python().
l               -> l_r                        : cuneiform_lang:l_r().
l               -> l_racket                   : cuneiform_lang:l_racket().

t               -> t_str                                     : cuneiform_lang:t_str().
t               -> t_file                                    : cuneiform_lang:t_file().
t               -> t_bool                                    : cuneiform_lang:t_bool().
t               -> t_fn_ntv lparen rparen rarrow t           : cuneiform_lang:t_fn( ntv, [], '$5' ).
t               -> t_fn_ntv lparen t_arg_lst rparen rarrow t : cuneiform_lang:t_fn( ntv, '$3', '$6' ).
t               -> t_fn_frn lparen rparen rarrow t           : cuneiform_lang:t_fn( frn, [], '$5' ).
t               -> t_fn_frn lparen t_arg_lst rparen rarrow t : cuneiform_lang:t_fn( frn, '$3', '$6' ).
t               -> lsquarebr t rsquarebr                     : cuneiform_lang:t_lst( '$2' ).
t               -> ltag t_arg_lst rtag                       : cuneiform_lang:t_rcd( '$2' ).

t_arg           -> id colon t                 : visit_t_arg( '$1', '$3' ).

t_arg_lst       -> t_arg                      : ['$1'].
t_arg_lst       -> t_arg comma t_arg_lst      : ['$1'|'$3'].


e               -> id                                             : visit_var( '$1' ).
e               -> strlit                                         : visit_str( '$1' ).
e               -> intlit                                         : visit_str( '$1' ).
e               -> filelit                                        : visit_file( '$1' ).
e               -> true                                           : visit_true( '$1' ).
e               -> false                                          : visit_false( '$1' ).
e               -> lparen e cmp e rparen                          : visit_cmp( '$2', '$3', '$4' ).
e               -> cnd e then e else e halt                       : visit_cnd( '$1', '$2', [], '$4', [], '$6' ).
e               -> cnd e then define_lst e else e halt            : visit_cnd( '$1', '$2', '$4', '$5', [], '$7' ).
e               -> cnd e then e else define_lst e halt            : visit_cnd( '$1', '$2', [], '$4', '$6', '$7' ).
e               -> cnd e then define_lst e else define_lst e halt : visit_cnd( '$1', '$2', '$4', '$5', '$7', '$8' ).
e               -> neg e                                          : visit_neg( '$1', '$2' ).
e               -> lparen e wedge e rparen                        : visit_conj( '$2', '$3', '$4' ).
e               -> lparen e vee e rparen                          : visit_disj( '$2', '$3', '$4' ).
e               -> id lparen rparen                               : visit_app( '$1', [] ).
e               -> id lparen e_bind_lst rparen                    : visit_app( '$1', '$3' ).
e               -> ltag e_bind_lst rtag                           : visit_rcd( '$1', '$2' ).
e               -> lparen e bar id rparen                         : visit_proj( '$2', '$4' ).
e               -> lparen e plus e rparen                         : visit_append( '$2', '$3', '$4' ).
e               -> lsquarebr colon t rsquarebr                    : visit_lst( [], '$2', '$3' ).
e               -> lsquarebr e_lst colon t rsquarebr              : visit_lst( '$2', '$3', '$4' ).
e               -> isnil e                                        : visit_isnil( '$1', '$2' ).
e               -> for from_lst do e halt                         : visit_for( '$1', '$2', [], '$4' ).
e               -> for from_lst do define_lst e halt              : visit_for( '$1', '$2', '$4', '$5' ).
e               -> fold e_bind comma from do e halt               : visit_fold( '$1', '$2', '$4', [], '$6' ).
e               -> fold e_bind comma from do define_lst e halt    : visit_fold( '$1', '$2', '$4', '$6', '$7' ).

from_lst        -> from                       : ['$1'].
from_lst        -> from comma from_lst        : ['$1'|'$3'].

from            -> id larrow e                : visit_from( '$1', '$3' ).

e_lst           -> e                          : ['$1'].
e_lst           -> e comma e_lst              : ['$1'|'$3'].

e_bind_lst      -> e_bind                     : ['$1'].
e_bind_lst      -> e_bind comma e_bind_lst    : ['$1'|'$3'].

e_bind          -> id eq e                    : visit_e_bind( '$1', '$3' ).

r_bind_lst      -> r_bind                     : ['$1'].
r_bind_lst      -> r_bind comma r_bind_lst    : ['$1'|'$3'].

r_bind          -> id eq r                    : visit_r_bind( '$1', '$3' ).


%%====================================================================
%% Erlang Code
%%====================================================================

Erlang code.

-include_lib( "cuneiform.hrl" ).

-export( [string/1, file/1] ).


-spec string( S :: string() ) -> {ok, e()} | {error, _}.

string( S ) ->
  case cf_scan:string( S ) of
    {error, ScanErrorInfo, _} -> {error, ScanErrorInfo};
    {ok, TokenLst, _}         ->
    try parse( TokenLst ) of
      Ret -> {ok, Ret}
    catch
      throw:E -> {error, E}
    end
  end.


-spec file( Filename :: string() ) -> {ok, e()} | {error, _}.

file( Filename ) ->
  case file:read_file( Filename ) of
    {error, Reason} -> {error, Reason};
    {ok, B}         ->
      S = binary_to_list( B ),
      string( S )
  end.

-spec join_stat( T1, T2 ) -> {[_], [_], [_], [_]}
when T1 :: {[_], [_], [_], [_]},
     T2 :: {[_], [_], [_], [_]}.

join_stat( {A1, B1, C1, D1}, {A2, B2, C2, D2} ) ->
  {A1++A2, B1++B2, C1++C2, D1++D2}.

-spec create_closure( DefLst :: [{r(), e()}], EBody :: e() ) -> e().

create_closure( DefLst, EBody ) ->
  F =
    fun( {R1, E1}, EAcc ) ->
      cuneiform_lang:assign( element( 2, R1 ), R1, E1, EAcc )
    end,
  lists:foldr( F, EBody, DefLst ).


-spec visit_import( {filelit, L :: _, S :: string()} ) -> {import, _, string()}.

visit_import( {filelit, L, S} ) ->
  {import, L, S}.


-spec visit_r_var( {id, L :: _, S :: string()}, T :: t() ) -> r().

visit_r_var( {id, L, S}, T ) ->
  cuneiform_lang:r_var( L, list_to_atom( S ), T ).


-spec visit_var( {id, L :: _, Varname :: string()} ) -> e().

visit_var( {id, L, Varname} ) ->
  cuneiform_lang:var( L, list_to_atom( Varname ) ).


-spec visit_file( {filelit, L :: _, S :: string()} ) -> e().

visit_file( {filelit, L, S} ) ->
  cuneiform_lang:file( L, list_to_binary( S ) ).


-spec visit_str( {_, L :: _, S :: string()} ) -> e().

visit_str( {_, L, S} ) ->
  cuneiform_lang:str( L, list_to_binary( S ) ).


-spec visit_true( {true, L :: _, _} ) -> e().

visit_true( {true, L, _} ) ->
  cuneiform_lang:true( L ).


-spec visit_false( {false, L :: _, _} ) -> e().

visit_false( {false, L, _} ) ->
  cuneiform_lang:false( L ).


-spec visit_cmp( E1 :: e(), {cmp, L :: _, _}, E2 :: e() ) -> e().

visit_cmp( E1, {cmp, L, _}, E2 ) ->
  cuneiform_lang:cmp( L, E1, E2 ).


-spec visit_conj( E1 :: e(), {wedge, L :: _, _}, E2 :: e() ) -> e().

visit_conj( E1, {wedge, L, _}, E2 ) ->
  cuneiform_lang:conj( L, E1, E2 ).


-spec visit_disj( E1 :: e(), {vee, L :: _, _}, E2 :: e() ) -> e().

visit_disj( E1, {vee, L, _}, E2 ) ->
  cuneiform_lang:disj( L, E1, E2 ).


-spec visit_neg( {neg, L :: _, _}, E :: e() ) -> e().

visit_neg( {neg, L, _}, E ) ->
  cuneiform_lang:neg( L, E ).


-spec visit_cnd( {cnd, L, _}, EIf, DefLstThen, EThen, DefLstElse, EElse ) -> e()
when L          :: _,
     EIf        :: e(),
     DefLstThen :: [{r(), e()}],
     EThen      :: e(),
     DefLstElse :: [{r(), e()}],
     EElse      :: e().

visit_cnd( {cnd, L, _}, EIf, DefLstThen, EThen, DefLstElse, EElse ) ->
  E2 = create_closure( DefLstThen, EThen ),
  E3 = create_closure( DefLstElse, EElse ),
  cuneiform_lang:cnd( L, EIf, E2, E3 ).


-spec visit_def_frn( Id, ArgLst, UArgLst, Lang, Body ) -> {r(), e()}
when Id      :: {id, _, string()},
     ArgLst  :: [t_arg()],
     UArgLst :: [t_arg()],
     Lang    :: l(),
     Body    :: {body, _, string()}.

visit_def_frn( {id, L, SName}, ArgLst, UArgLst, Lang, {body, _, SBody} ) ->
  BBody = list_to_binary( SBody ),
  FName = list_to_atom( SName ),
  RetType = cuneiform_lang:t_rcd( UArgLst ),
  T = cuneiform_lang:t_fn( frn, ArgLst, RetType ), 
  R = cuneiform_lang:r_var( L, FName, T ),
  Lam = cuneiform_lang:lam_frn( L, FName, ArgLst, RetType, Lang, BBody ),
  {R, Lam}.


-spec visit_def_ntv( {id, L, SName}, ArgLst, RetType, DefLst, EBody ) -> {r(), e()}
when L       :: _,
     SName   :: string(),
     ArgLst  :: [t_arg()],
     RetType :: t(),
     DefLst  :: [{r(), e()}],
     EBody   :: e().

visit_def_ntv( {id, L, SName}, ArgLst, RetType, DefLst, EBody ) ->
  FName = list_to_atom( SName ),
  TFn = cuneiform_lang:t_fn( ntv, ArgLst, RetType ),
  Lam = cuneiform_lang:fix(
          L,
          cuneiform_lang:lam_ntv(
            L,
            [cuneiform_lang:lam_ntv_arg( FName, TFn )|[{X, X, T} || {X, T} <- ArgLst]],
            create_closure( DefLst, EBody ) ) ),
  R = cuneiform_lang:r_var( L, FName, TFn ),
  {R, Lam}.


-spec visit_t_arg( {id, _, S :: string()}, T :: t() ) -> t_arg().

visit_t_arg( {id, _, S}, T ) ->
  cuneiform_lang:t_arg( list_to_atom( S ), T ).


-spec visit_app( {id, L :: _, S :: string()}, EBindLst :: [e_bind()] ) -> e().

visit_app( {id, L, S}, EBindLst ) ->
  cuneiform_lang:app( L, cuneiform_lang:var( L, list_to_atom( S ) ), EBindLst ).


-spec visit_e_bind( {id, _, S :: string()}, E :: e() ) -> e_bind().

visit_e_bind( {id, _, S}, E ) ->
  cuneiform_lang:e_bind( list_to_atom( S ), E ).


-spec visit_r_bind( {id, _, S :: string()}, R :: r() ) -> r_bind().

visit_r_bind( {id, _, S}, R ) ->
  cuneiform_lang:r_bind( list_to_atom( S ), R ).


-spec visit_r_rcd( {ltag, L :: _, _}, RBindLst :: [r_bind()] ) -> r().

visit_r_rcd( {ltag, L, _}, RBindLst ) ->
  cuneiform_lang:r_rcd( L, RBindLst ).


-spec visit_rcd( {ltag, L :: _, _}, EBindLst :: [e_bind()] ) -> e().

visit_rcd( {ltag, L, _}, EBindLst ) ->
  cuneiform_lang:rcd( L, EBindLst ).


-spec visit_proj( E :: e(), {id, L :: _, S :: string()} ) -> e().

visit_proj( E, {id, L, S} ) ->
  cuneiform_lang:proj( L, list_to_atom( S ), E ).


-spec visit_append( E :: e(), {plus, L :: _, _}, E :: e() ) -> e().

visit_append( E1, {plus, L, _}, E2 ) ->
  cuneiform_lang:append( L, E1, E2 ).


-spec visit_lst( ELst :: [e()], {colon, L :: _, _}, T :: t() ) -> e().

visit_lst( ELst, {colon, L, _}, T ) ->
  cuneiform_lang:lst( L, T, ELst ).


-spec visit_isnil( {isnil, L :: _, _}, E :: e() ) -> e().

visit_isnil( {isnil, L, _}, E ) ->
  cuneiform_lang:isnil( L, E ).


-spec visit_for( {for, L :: _, _}, FromLst :: [e_bind()], DefLst :: [{r(), e()}], E :: e() ) -> e().

visit_for( {for, L, _}, FromLst, DefLst, E ) ->
  cuneiform_lang:for( L, FromLst, create_closure( DefLst, E ) ).


-spec visit_from( {id, _, S :: string()}, E :: e() ) -> e_bind().

visit_from( {id, _, S}, E ) ->
  cuneiform_lang:e_bind( list_to_atom( S ), E ).

-spec visit_fold( {fold, L :: _, _}, AccBind :: e_bind(), LstBind :: e_bind(), DefLst :: [{r(), e()}], EBody :: e() ) -> e().

visit_fold( {fold, L, _}, AccBind, LstBind, DefLst, EBody ) ->
  cuneiform_lang:fold( L, AccBind, LstBind, create_closure( DefLst, EBody ) ).
