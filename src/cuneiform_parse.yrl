%%====================================================================
%% Symbol Declaration
%%====================================================================

Nonterminals
  define e imp l t_arg_lst t_arg r script t stat e_bind_lst e_bind r_bind_lst
  r_bind.

Terminals
  l_bash l_octave l_perl l_python l_r l_racket
  t_str t_file t_bool t_fn_frn t_fn_ntv
  assign bar wedge cmp cnd colon
  comma def do dot else eq false fold for import in
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
stat            -> e dot                      : {[], [], [], ['$1']}.

imp             -> import filelit semicolon   : visit_import( '$2' ).

define          -> assign r eq e semicolon    : {'$2', '$4'}.
define          -> def id lparen rparen rarrow ltag t_arg_lst rtag in l body
                                              : visit_def_frn( '$2', [], '$7', '$10', '$11' ).
define          -> def id lparen t_arg_lst rparen rarrow ltag t_arg_lst rtag in l body
                                              : visit_def_frn( '$2', '$4', '$8', '$11', '$12' ).
define          -> def id lparen rparen rarrow t lbrace e rbrace
                                              : visit_def_ntv( '$2', [], '$6', '$8' ).
define          -> def id lparen t_arg_lst rparen rarrow t lbrace e rbrace
                                              : visit_def_ntv( '$2', '$4', '$7', '$9' ).

r               -> id colon t                 : visit_r_var( '$1', '$3' ).
r               -> ltag r_bind_lst rtag       : visit_r_rcd( '$1', '$2' ).

l               -> l_bash                     : cuneiform_lang:l_bash().
l               -> l_octave                   : cuneiform_lang:l_octave().
l               -> l_perl                     : cuneiform_lang:l_perl().
l               -> l_python                   : cuneiform_lang:l_python().
l               -> l_r                        : cuneiform_lang:l_r().
l               -> l_racket                   : cuneiform_lang:l_racket().

t               -> t_str                      : cuneiform_lang:t_str().
t               -> t_file                     : cuneiform_lang:t_file().
t               -> t_bool                     : cuneiform_lang:t_bool().
t               -> t_fn_ntv lparen rparen rarrow t
                                              : cuneiform_lang:t_fn( ntv, [], '$5' ).
t               -> t_fn_frn lparen rparen rarrow t
                                              : cuneiform_lang:t_fn( frn, [], '$5' ).
t               -> lsquarebr t rsquarebr      : cuneiform_lang:t_lst( '$2' ).
t               -> ltag t_arg_lst rtag        : cuneiform_lang:t_rcd( '$2' ).

% TODO: Native function type with non-empty argument list
% TODO: Foreign function type with non-empty argument list
% TODO: List type

t_arg           -> id colon t                 : visit_t_arg( '$1', '$3' ).

t_arg_lst       -> t_arg                      : ['$1'].
t_arg_lst       -> t_arg comma t_arg_lst      : ['$1'|'$3'].


e               -> id                         : visit_var( '$1' ).
e               -> strlit                     : visit_str( '$1' ).
e               -> intlit                     : visit_str( '$1' ).
e               -> filelit                    : visit_file( '$1' ).
e               -> true                       : visit_true( '$1' ).
e               -> false                      : visit_false( '$1' ).
e               -> lparen e cmp e rparen      : visit_cmp( '$2', '$3', '$4' ).
e               -> cnd e then e else e        : visit_cnd( '$1', '$2', '$4', '$6' ).
e               -> neg e                      : visit_neg( '$1', '$2' ).
e               -> lparen e wedge e rparen    : visit_conj( '$2', '$3', '$4' ).
e               -> lparen e vee e rparen      : visit_disj( '$2', '$3', '$4' ).
e               -> id lparen rparen           : visit_app( '$1', [] ).
e               -> id lparen e_bind_lst rparen
                                              : visit_app( '$1', '$3' ).
e               -> ltag e_bind_lst rtag       : visit_rcd( '$1', '$2' ).

% TODO: list literal
% TODO: list append
% TODO: isnil test
% TODO: for
% TODO: fold
% TODO: projection


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


-spec visit_cnd( {cnd, L :: _, _}, E1 :: e(), E2 :: e(), E3 :: e() ) -> e().

visit_cnd( {cnd, L, _}, EIf, EThen, EElse ) ->
  cuneiform_lang:cnd( L, EIf, EThen, EElse ).


-spec visit_def_frn( Id, ArgLst, RetType, Lang, Body ) -> {r(), e()}
when Id      :: {id, _, string()},
     ArgLst  :: [t_arg()],
     RetType :: t(),
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


-spec visit_def_ntv( {id, L, SName}, ArgLst, RetType, EBody ) -> {r(), e()}
when L       :: _,
     SName   :: string(),
     ArgLst  :: [t_arg()],
     RetType :: t(),
     EBody   :: e().

visit_def_ntv( {id, L, SName}, ArgLst, RetType, EBody ) ->
  FName = list_to_atom( SName ),
  TFn = cuneiform_lang:t_fn( ntv, ArgLst, RetType ),
  Lam = cuneiform_lang:fix(
          L,
          cuneiform_lang:lam_ntv(
            L,
            [cuneiform_lang:lam_ntv_arg( FName, TFn )|[{X, X, T} || {X, T} <- ArgLst]],
            EBody ) ),
  R = cuneiform_lang:r_var( L, FName, TFn ),
  {R, Lam}.


-spec visit_t_arg( {id, _, S :: string()}, T :: t() ) -> t_arg().

visit_t_arg( {id, _, S}, T ) ->
  cuneiform_lang:t_arg( list_to_atom( S ), T ).


-spec visit_app( {id, L :: _, S :: string()}, EBindLst :: [e_bind()] ) -> e().

visit_app( {id, L, S}, EBindLst ) ->
  cuneiform_lang:app( L, cuneiform_lang:var( L, list_to_atom( S ) ), EBindLst ).


-spec visit_e_bind( {id, _, S :: string()}, E :: e() ) -> e().

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