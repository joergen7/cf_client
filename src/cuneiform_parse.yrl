%% ==================================================================
%% Symbol Declaration
%% ==================================================================

Nonterminals
  define define_lst expr imp imp_lst imp_str_lst ptn script type.

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

script      -> expr                          : {[], [], [], '$1'}.
script      -> define_lst expr               : {[], [], '$1', '$2'}.
script      -> imp_lst expr                  : {'$1', [], [], '$2'}.
script      -> imp_lst define_lst expr       : {'$1', [], '$2', '$3'}.

define_lst  -> define                        : ['$1'].
define_lst  -> define define_lst             : ['$1'|'$2'].

define      -> assign ptn eq expr semicolon  : {'$2', '$4'}.

ptn         -> id colon type                 : visit_r_var( '$1', '$3' ).

imp_lst     -> imp                           : '$1'.
imp_lst     -> imp imp_lst                   : '$1'++'$2'.

imp         -> import imp_str_lst semicolon  : '$2'.

imp_str_lst -> strlit                        : [visit_import( '$1' )].
imp_str_lst -> strlit comma imp_str_lst      : [visit_import( '$1' )|'$3'].

type        -> str                           : cuneiform_lang:t_str().

expr        -> id                            : visit_var( '$1' ).
expr        -> strlit                        : visit_str( '$1' ).
expr        -> filelit                       : visit_file( '$1' ).
expr        -> true                          : visit_true( '$1' ).
expr        -> false                         : visit_false( '$1' ).
expr        -> lparen expr cmp expr rparen   : visit_cmp( '$2', '$3', '$4' ).
expr        -> lparen expr wedge expr rparen : visit_conj( '$2', '$3', '$4' ).
expr        -> lparen expr vee expr rparen   : visit_disj( '$2', '$3', '$4' ).


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


-spec visit_import( {strlit, L :: pos_integer(), S :: string()} ) -> string().

visit_import( {strlit, L, S} ) -> {import, L, S}.


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


-spec visit_str( {strlit, L :: pos_integer(), S :: string()} ) -> e().

visit_str( {strlit, L, S} ) ->
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