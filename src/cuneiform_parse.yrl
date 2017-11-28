%% ==================================================================
%% Symbol Declaration
%% ==================================================================

Nonterminals
  define define_lst expr imp imp_lst pattern script strlit_lst type.

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

script     -> expr                             : {[], [], [], '$1'}.
script     -> define_lst expr                  : {[], [], '$1', '$2'}.
script     -> imp_lst expr                     : {'$1', [], [], '$2'}.
script     -> imp_lst define_lst expr          : {'$1', [], '$2', '$3'}.

define_lst -> define                           : ['$1'].
define_lst -> define define_lst                : ['$1'|'$2'].

define     -> assign pattern eq expr semicolon : {'$2', '$4'}.

pattern    -> id colon type                    : visit_r_var( '$1', '$3' ).

imp_lst    -> imp                              : '$1'.
imp_lst    -> imp imp_lst                      : '$1'++'$2'.

imp        -> import strlit_lst semicolon      : '$2'.

strlit_lst -> strlit                           : [get_string( '$1' )].
strlit_lst -> strlit strlit_lst                : [get_string( '$1' )|'$2'].

type       -> str                              : cuneiform_lang:t_str().

expr       -> id                               : visit_var( '$1' ).
expr       -> strlit                           : visit_str( '$1' ).


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


-spec visit_str( {strlit, L :: pos_integer(), S :: string()} ) -> e().

visit_str( {strlit, L, S} ) ->
  cuneiform_lang:str( L, S ).


-spec visit_var( {id, L :: pos_integer(), Varname :: string()} ) -> e().

visit_var( {id, L, Varname} ) ->
  cuneiform_lang:var( L, list_to_atom( Varname ) ).


-spec visit_r_var( {id, L, S}, T ) -> r()
when L :: pos_integer(),
     S :: string(),
     T :: t().

visit_r_var( {id, L, S}, T ) ->
  cuneiform_lang:r_var( L, list_to_atom( S ), T ).




-spec get_string( {_, _, S :: string()} ) -> string().

get_string( {_, _, S} ) -> S.