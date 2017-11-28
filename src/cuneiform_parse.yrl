%% ==================================================================
%% Symbol Declaration
%% ==================================================================

Nonterminals
  expr.

Terminals
  bash octave perl python r racket wedge bool cmp cnd colon comma
  def do dot else eq false file fix fold for frn import in isnil
  lambda larrow lbrace lparen lsquarebr ltag neg ntv vee plus rarrow
  rbrace rparen rsquarebr rtag semicolon str then true id intlit
  strlit filelit body.

expr   -> strlit : visit_strlit( '$1' ).


%% ==================================================================
%% Erlang Code
%% ==================================================================

Erlang code.

-include_lib( "cuneiform.hrl" ).


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

visit_strlit( {strlit, L, S} ) ->
  cuneiform_lang:str( L, S ).