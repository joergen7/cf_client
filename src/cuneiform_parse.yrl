%% ==================================================================
%% Symbol Declaration
%% ==================================================================

Nonterminals
  define define_lst expr g_lst g_pair ground imp imp_lst imp_f_lst lang
  ntv_arg ntv_arg_lst ptn script type.

Terminals
  assign bash octave perl python r racket wedge bool cmp cnd colon
  comma def do dot else eq false file fix fold for frn import in
  isnil lambda larrow lbrace lparen lsquarebr ltag neg ntv vee plus
  rarrow rbrace rparen rsquarebr rtag semicolon str then true id
  intlit strlit filelit body.

%% ==================================================================
%% Syntax Definition
%% ==================================================================

Rootsymbol script.

script      -> expr           : {[], [], [], '$1'}.
script      -> define_lst expr
                              : {[], [], '$1', '$2'}.
script      -> imp_lst expr   : {'$1', [], [], '$2'}.
script      -> imp_lst define_lst expr
                              : {'$1', [], '$2', '$3'}.

define_lst  -> define         : ['$1'].
define_lst  -> define define_lst
                              : ['$1'|'$2'].

define      -> assign ptn eq expr semicolon
                              : {'$2', '$4'}.
define      -> def id lparen rparen rarrow ltag g_lst rtag in lang body
                              : visit_def_frn( '$2', [], '$7', '$10', '$11' ).

ptn         -> id colon type  : visit_r_var( '$1', '$3' ).

imp_lst     -> imp            : '$1'.
imp_lst     -> imp imp_lst    : '$1'++'$2'.

imp         -> import imp_f_lst semicolon
                              : '$2'.

imp_f_lst   -> filelit        : [visit_import( '$1' )].
imp_f_lst   -> filelit comma imp_f_lst
                              : [visit_import( '$1' )|'$3'].

lang        -> bash           : cuneiform_lang:l_bash().

type        -> str            : cuneiform_lang:t_str().
type        -> file           : cuneiform_lang:t_file().

g_lst       -> g_pair         : ['$1'].
g_lst       -> g_pair comma g_lst
                              : ['$1'|'$3'].

g_pair      -> id colon ground
                              : visit_g_pair( '$1', '$3' ).

ground      -> str            : cuneiform_lang:t_str().
ground      -> file           : cuneiform_lang:t_file().
ground      -> lsquarebr str rsquarebr
                              : cuneiform_lang:t_lst( cuneiform_lang:t_str() ).
ground      -> lsquarebr file rsquarebr
                              : cuneiform_lang:t_lst( cuneiform_lang:t_file() ).

expr        -> id             : visit_var( '$1' ).
expr        -> strlit         : visit_str( '$1' ).
expr        -> intlit         : visit_str( '$1' ).
expr        -> filelit        : visit_file( '$1' ).
expr        -> true           : visit_true( '$1' ).
expr        -> false          : visit_false( '$1' ).
expr        -> lparen expr cmp expr rparen
                              : visit_cmp( '$2', '$3', '$4' ).
expr        -> cnd expr then expr else expr
                              : visit_cnd( '$1', '$2', '$4', '$6' ).
expr        -> neg expr       : visit_neg( '$1', '$2' ).
expr        -> lparen expr wedge expr rparen
                              : visit_conj( '$2', '$3', '$4' ).
expr        -> lparen expr vee expr rparen
                              : visit_disj( '$2', '$3', '$4' ).
expr        -> lambda lparen rparen expr
                              : visit_lambda( '$1', [], '$4' ).
expr        -> lambda lparen ntv_arg_lst rparen expr
                              : visit_lambda( '$1', '$3', '$5' ).

ntv_arg_lst -> ntv_arg        : ['$1'].
ntv_arg_lst -> ntv_arg comma ntv_arg_lst
                              : ['$1'|'$3'].

ntv_arg     -> id colon type  : visit_arg( '$1', '$3' ).


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
when Id   :: {id, pos_integer(), string()},
     Arg
     Lang :: l(),
     Body :: {body, _, string()}.

visit_def_frn( {id, Line, SName}, ArgLst, RetType, Lang, {body, _, SBody} ) ->
  T = cuneiform_lang:t_fn( frn, ArgLst, RetType ), 
  Ptn = cuneiform_lang:r_var( Line, list_to_atom( SName ), T ),
  Lam = cuneiform_lang:lam_frn( Line, SName, ArgLst, RetType, Lang, SBody ),
  {Ptn, Lam}.


visit_g_pair( {id, _, X}, T ) ->
  cuneiform_lang:t_rcd_pair( list_to_atom( X ), T ).