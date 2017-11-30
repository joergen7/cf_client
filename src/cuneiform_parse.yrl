%% ==================================================================
%% Symbol Declaration
%% ==================================================================

Nonterminals
  define define_lst e u_arg_lst u_arg ground imp imp_lst imp_f_lst l
  lam_ntv_arg lam_ntv_arg_lst r script t u.

Terminals
  l_bash l_octave l_perl l_python l_r l_racket
  t_str t_file t_bool t_frn t_ntv
  assign bar wedge cmp cnd colon
  comma def do dot else eq false fix fold for import in
  isnil lambda larrow lbrace lparen lsquarebr ltag neg vee plus
  rarrow rbrace rparen rsquarebr rtag semicolon then true id
  intlit strlit filelit body.

%% ==================================================================
%% Syntax Definition
%% ==================================================================

Rootsymbol script.

script          -> e                          : {[], [], [], '$1'}.
script          -> define_lst e               : {[], [], '$1', '$2'}.
script          -> imp_lst e                  : {'$1', [], [], '$2'}.
script          -> imp_lst define_lst e       : {'$1', [], '$2', '$3'}.

imp_lst         -> imp                        : '$1'.
imp_lst         -> imp imp_lst                : '$1'++'$2'.

imp             -> import filelit semicolon   : [visit_import( '$2' )].

define_lst      -> define                     : ['$1'].
define_lst      -> define define_lst          : ['$1'|'$2'].

define          -> assign r eq e semicolon    : {'$2', '$4'}.
define          -> def id lparen rparen rarrow ltag u_arg_lst rtag in l body
                                              : visit_def_frn( '$2', [], '$7', '$10', '$11' ).

r               -> id colon t                 : visit_r_var( '$1', '$3' ).

l               -> l_bash                     : cuneiform_lang:l_bash().

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
e               -> lambda lparen lam_ntv_arg_lst rparen e
                                              : visit_lambda( '$1', '$3', '$5' ).
e               -> id lparen rparen           : visit_app( '$1', [] ).
% e               -> e lparen x_bind_lst rparen : visit_app( '$1', '$3' ).

lam_ntv_arg_lst -> lam_ntv_arg                : ['$1'].
lam_ntv_arg_lst -> lam_ntv_arg comma lam_ntv_arg_lst
                                              : ['$1'|'$3'].

lam_ntv_arg     -> id colon t                 : visit_lam_ntv_arg( '$1', '$3' ).

% x_bind_lst      -> x_bind                     : ['$1'].
% x_bind_


%% ==================================================================
%% Erlang Code
%% ==================================================================

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


-spec visit_import( {filelit, L, S} ) -> {import, pos_integer(), string()}
when L :: pos_integer(),
     S :: string().

visit_import( {filelit, L, S} ) -> {import, L, S}.


-spec visit_r_var( {id, L, S}, T ) -> r()
when L :: pos_integer(),
     S :: string(),
     T :: t().

visit_r_var( {id, L, S}, T ) ->
  cuneiform_lang:r_var( L, list_to_atom( S ), T ).


-spec visit_var( {id, L :: pos_integer(), Varname :: string()} ) -> e().

visit_var( {id, L, Varname} ) ->
  cuneiform_lang:var( L, list_to_atom( Varname ) ).


-spec visit_file( {filelit, L :: pos_integer(), S :: string()} ) -> e().

visit_file( {filelit, L, S} ) ->
  cuneiform_lang:file( L, S ).


-spec visit_str( {_, L :: pos_integer(), S :: string()} ) -> e().

visit_str( {_, L, S} ) ->
  cuneiform_lang:str( L, S ).


-spec visit_true( {true, L :: pos_integer(), _} ) -> e().

visit_true( {true, L, _} ) ->
  cuneiform_lang:true( L ).


-spec visit_false( {false, L :: pos_integer(), _} ) -> e().

visit_false( {false, L, _} ) ->
  cuneiform_lang:false( L ).


-spec visit_cmp( E1 :: e(), {cmp, L :: pos_integer(), _}, E2 :: e() ) -> e().

visit_cmp( E1, {cmp, L, _}, E2 ) ->
  cuneiform_lang:cmp( L, E1, E2 ).


-spec visit_conj( E1 :: e(), {wedge, L :: pos_integer(), _}, E2 :: e() ) -> e().

visit_conj( E1, {wedge, L, _}, E2 ) ->
  cuneiform_lang:conj( L, E1, E2 ).


-spec visit_disj( E1 :: e(), {vee, L :: pos_integer(), _}, E2 :: e() ) -> e().

visit_disj( E1, {vee, L, _}, E2 ) ->
  cuneiform_lang:disj( L, E1, E2 ).


-spec visit_neg( {neg, L :: pos_integer(), _}, E :: e() ) -> e().

visit_neg( {neg, L, _}, E ) ->
  cuneiform_lang:neg( L, E ).


-spec visit_cnd( {cnd, L, _}, EIf, EThen, EElse ) -> e()
when L     :: pos_integer(),
     EIf   :: e(),
     EThen :: e(),
     EElse :: e().

visit_cnd( {cnd, L, _}, EIf, EThen, EElse ) ->
  cuneiform_lang:cnd( L, EIf, EThen, EElse ).


-spec visit_lambda( {lambda, L, _}, ArgLst, EBody ) -> e()
when L      :: pos_integer(),
     ArgLst :: [lam_ntv_arg()],
     EBody  :: e().

visit_lambda( {lambda, L, _}, ArgLst, EBody ) ->
  cuneiform_lang:lam_ntv( L, ArgLst, EBody ).


-spec visit_lam_ntv_arg( {id, _, X :: string()}, T :: t() ) -> lam_ntv_arg().

visit_lam_ntv_arg( {id, _, X}, T ) ->
  cuneiform_lang:lam_ntv_arg( list_to_atom( X ), X, T ).

-spec visit_def_frn( Id, ArgLst, RetType, Lang, Body ) -> {r(), e()}
when Id      :: {id, pos_integer(), string()},
     ArgLst  :: [t_arg()],
     RetType :: t(),
     Lang    :: l(),
     Body    :: {body, _, string()}.

visit_def_frn( {id, Line, SName}, ArgLst, UArgLst, Lang, {body, _, SBody} ) ->
  BBody = list_to_binary( SBody ),
  FName = list_to_atom( SName ),
  RetType = cuneiform_lang:t_rcd( UArgLst ),
  T = cuneiform_lang:t_fn( frn, ArgLst, RetType ), 
  Ptn = cuneiform_lang:r_var( Line, FName, T ),
  Lam = cuneiform_lang:lam_frn( Line, FName, ArgLst, RetType, Lang, BBody ),
  {Ptn, Lam}.


visit_u_arg( {id, _, X}, T ) ->
  cuneiform_lang:t_arg( list_to_atom( X ), T ).


-spec visit_app( {id, L, X}, ArgLst ) -> e()
when L      :: pos_integer(),
     X      :: string(),
     ArgLst :: [x_bind()].

visit_app( {id, L, X}, ArgLst ) ->
  cuneiform_lang:app( L, cuneiform_lang:var( L, list_to_atom( X ) ), ArgLst ).
