%%====================================================================
%% Symbol Declaration
%%====================================================================

Nonterminals
  define e u_arg_lst u_arg imp l
  lam_ntv_arg lam_ntv_arg_lst r script t u stat x_bind_lst x_bind.

Terminals
  l_bash l_octave l_perl l_python l_r l_racket
  t_str t_file t_bool t_lam_frn t_lam_ntv
  assign bar wedge cmp cnd colon
  comma def do dot else eq false fix fold for import in
  isnil lambda larrow lbrace lparen lsquarebr ltag neg vee plus
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
define          -> def id lparen rparen rarrow ltag u_arg_lst rtag in l body
                                              : visit_def_frn( '$2', [], '$7', '$10', '$11' ).
define          -> def id lparen u_arg_lst rparen rarrow ltag u_arg_lst rtag in l body
                                              : visit_def_frn( '$2', '$4', '$8', '$11', '$12' ).
% TODO: native function definition

r               -> id colon t                 : visit_r_var( '$1', '$3' ).
% TODO: r_rcd

l               -> l_bash                     : cuneiform_lang:l_bash().
l               -> l_octave                   : cuneiform_lang:l_octave().
l               -> l_perl                     : cuneiform_lang:l_perl().
l               -> l_python                   : cuneiform_lang:l_python().
l               -> l_r                        : cuneiform_lang:l_r().
l               -> l_racket                   : cuneiform_lang:l_racket().

u_arg_lst       -> u_arg                      : ['$1'].
u_arg_lst       -> u_arg comma u_arg_lst      : ['$1'|'$3'].

u_arg           -> id colon u                 : visit_u_arg( '$1', '$3' ).

u               -> t_str                      : cuneiform_lang:t_str().
u               -> t_file                     : cuneiform_lang:t_file().
u               -> t_bool                     : cuneiform_lang:t_bool().
u               -> lsquarebr t_str rsquarebr  : cuneiform_lang:t_lst( cuneiform_lang:t_str() ).
u               -> lsquarebr t_file rsquarebr : cuneiform_lang:t_lst( cuneiform_lang:t_file() ).
u               -> lsquarebr t_bool rsquarebr : cuneiform_lang:t_lst( cuneiform_lang:t_bool() ).

t               -> t_str                      : cuneiform_lang:t_str().
t               -> t_file                     : cuneiform_lang:t_file().
t               -> t_bool                     : cuneiform_lang:t_bool().
t               -> t_lam_ntv lparen rparen rarrow t
                                              : cuneiform_lang:t_fn( ntv, [], '$5' ).
% TODO: Native function type with non-empty argument list
% TODO: Foreign function type
% TODO: Record type
% TODO: List type

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
e               -> lambda lparen rparen e     : visit_lambda( '$1', [], '$4' ).
% TODO: lambda with no arguments but a number of let bindings before e
e               -> lambda lparen lam_ntv_arg_lst rparen e
                                              : visit_lambda( '$1', '$3', '$5' ).
% TODO: lambda with non-empty argument list and a number of let bindings before e
e               -> id lparen rparen           : visit_app( '$1', [] ).
e               -> id lparen x_bind_lst rparen
                                              : visit_app( '$1', '$3' ).
% TODO: native function definition with no arguments and no let bindings before e
% TODO: native function definition with no arguments but a number of let bindings before e
% TODO: native function definition with non-empty argument list but no let bindings before e
% TODO: native function definition with non-empty argument list and a number of let bindings before e
% TODO: list literal
% TODO: list append
% TODO: isnil test
% TODO: for
% TODO: fold
% TODO: record literal
% TODO: projection
e               -> fix e                      : visit_fix( '$1', '$2' ).

lam_ntv_arg_lst -> lam_ntv_arg                : ['$1'].
lam_ntv_arg_lst -> lam_ntv_arg comma lam_ntv_arg_lst
                                              : ['$1'|'$3'].

lam_ntv_arg     -> id colon t                 : visit_lam_ntv_arg( '$1', '$3' ).

x_bind_lst      -> x_bind                     : ['$1'].
x_bind_lst      -> x_bind comma x_bind_lst    : ['$1'|'$3'].

x_bind          -> id eq e                    : visit_x_bind( '$1', '$3' ).


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


-spec visit_lambda( {lambda, L :: _, _}, ArgLst :: [lam_ntv_arg()], E :: e() ) -> e().

visit_lambda( {lambda, L, _}, ArgLst, EBody ) ->
  cuneiform_lang:lam_ntv( L, ArgLst, EBody ).


-spec visit_lam_ntv_arg( {id, _, X :: string()}, T :: t() ) -> lam_ntv_arg().

visit_lam_ntv_arg( {id, _, S}, T ) ->
  X = list_to_atom( S ),
  cuneiform_lang:lam_ntv_arg( X, T ).

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
  Ptn = cuneiform_lang:r_var( L, FName, T ),
  Lam = cuneiform_lang:lam_frn( L, FName, ArgLst, RetType, Lang, BBody ),
  {Ptn, Lam}.


-spec visit_u_arg( {id, _, S :: string()}, T :: t() ) -> t_arg().

visit_u_arg( {id, _, S}, T ) ->
  cuneiform_lang:t_arg( list_to_atom( S ), T ).


-spec visit_app( {id, L :: _, S :: string()}, ArgLst :: [x_bind()] ) -> e().

visit_app( {id, L, S}, ArgLst ) ->
  cuneiform_lang:app( L, cuneiform_lang:var( L, list_to_atom( S ) ), ArgLst ).


-spec visit_x_bind( {id, _, S :: string()}, E :: e() ) -> e().

visit_x_bind( {id, _, S}, E ) ->
  cuneiform_lang:x_bind( list_to_atom( S ), E ).


-spec visit_fix( {fix, L :: _, _}, E :: e() ) -> e().

visit_fix( {fix, L, _}, E ) ->
  cuneiform_lang:fix( L, E ).