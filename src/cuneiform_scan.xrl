%% ==================================================================
%% Definitions
%% ==================================================================


Definitions.

BASH      = [Bb]ash
OCTAVE    = [Oo]ctave
PERL      = [Pp]erl
PYTHON    = [Pp]ython
R         = [Rr]
RACKET    = [Rr]acket

AND       = and
BOOL      = Bool
CMP       = ==
CND       = if
COLON     = :
COMMA     = ,
DEF       = def
DO        = do
DOT       = \.
ELSE      = else
EQ        = =
FALSE     = false
FILE      = File
FIX       = fix
FOLD      = fold
FOR       = for
FRN       = Frn
IMPORT    = import
IN        = in
ISNIL     = isnil
LAMBDA    = \\
LARROW    = <-
LBRACE    = \{
LET       = let
LPAREN    = \(
LSQUAREBR = \[
LTAG      = <
NOT       = not
NTV       = Ntv
OR        = or
PLUS      = \+
RARROW    = ->
RBRACE    = \}
RPAREN    = \)
RSQUAREBR = \]
RTAG      = >
SEMICOLON = ;
STR       = Str
THEN      = then
TRUE      = true

ID        = [A-Za-z][A-Za-z0-9\-_]*
INTLIT    = -?([0-9]|[1-9][0-9]*)
STRLIT    = "[^"]*"
FILELIT   = '[^']*'
BODY      = \*\{([^}]|\}+[^}*])*\}+\*

COMMENT1  = (#|//|%).*
COMMENT2  = /\*([^*]|\*+[^/*])*\*+/
WS        = [\000-\s]


%% ==================================================================
%% Rules
%% ==================================================================

Rules.

{BASH}      : {token, {bash, TokenLine, TokenChars}}.
{OCTAVE}    : {token, {octave, TokenLine, TokenChars}}.
{PERL}      : {token, {perl, TokenLine, TokenChars}}.
{PYTHON}    : {token, {python, TokenLine, TokenChars}}.
{R}         : {token, {r, TokenLine, TokenChars}}.
{RACKET}    : {token, {racket, TokenLine, TokenChars}}.

{AND}       : {token, {wedge, TokenLine, TokenChars}}.
{BOOL}      : {token, {bool, TokenLine, TokenChars}}.
{CMP}       : {token, {cmp, TokenLine, TokenChars}}.
{CND}       : {token, {cnd, TokenLine, TokenChars}}.
{COLON}     : {token, {colon, TokenLine, TokenChars}}.
{COMMA}     : {token, {comma, TokenLine, TokenChars}}.
{DEF}       : {token, {def, TokenLine, TokenChars}}.
{DO}        : {token, {do, TokenLine, TokenChars}}.
{DOT}       : {token, {dot, TokenLine, TokenChars}}.
{ELSE}      : {token, {else, TokenLine, TokenChars}}.
{EQ}        : {token, {eq, TokenLine, TokenChars}}.
{FALSE}     : {token, {false, TokenLine, TokenChars}}.
{FILE}      : {token, {file, TokenLine, TokenChars}}.
{FIX}       : {token, {fix, TokenLine, TokenChars}}.
{FOLD}      : {token, {fold, TokenLine, TokenChars}}.
{FOR}       : {token, {for, TokenLine, TokenChars}}.
{FRN}       : {token, {frn, TokenLine, TokenChars}}.
{IMPORT}    : {token, {import, TokenLine, TokenChars}}.
{IN}        : {token, {in, TokenLine, TokenChars}}.
{ISNIL}     : {token, {isnil, TokenLine, TokenChars}}.
{LAMBDA}    : {token, {lambda, TokenLine, TokenChars}}.
{LARROW}    : {token, {larrow, TokenLine, TokenChars}}.
{LBRACE}    : {token, {lbrace, TokenLine, TokenChars}}.
{LET}       : {token, {assign, TokenLine, TokenChars}}.
{LPAREN}    : {token, {lparen, TokenLine, TokenChars}}.
{LSQUAREBR} : {token, {lsquarebr, TokenLine, TokenChars}}.
{LTAG}      : {token, {ltag, TokenLine, TokenChars}}.
{NOT}       : {token, {neg, TokenLine, TokenChars}}.
{NTV}       : {token, {ntv, TokenLine, TokenChars}}.
{OR}        : {token, {vee, TokenLine, TokenChars}}.
{PLUS}      : {token, {plus, TokenLine, TokenChars}}.
{RARROW}    : {token, {rarrow, TokenLine, TokenChars}}.
{RBRACE}    : {token, {rbrace, TokenLine, TokenChars}}.
{RPAREN}    : {token, {rparen, TokenLine, TokenChars}}.
{RSQUAREBR} : {token, {rsquarebr, TokenLine, TokenChars}}.
{RTAG}      : {token, {rtag, TokenLine, TokenChars}}.
{SEMICOLON} : {token, {semicolon, TokenLine, TokenChars}}.
{STR}       : {token, {str, TokenLine, TokenChars}}.
{THEN}      : {token, {then, TokenLine, TokenChars}}.
{TRUE}      : {token, {true, TokenLine, TokenChars}}.

{ID}        : {token, {id, TokenLine, TokenChars}}.
{INTLIT}    : {token, {intlit, TokenLine, TokenChars}}.
{STRLIT}    : {token, {strlit, TokenLine, trim_lit( TokenChars )}}.
{FILELIT}   : {token, {filelit, TokenLine, trim_lit( TokenChars )}}.
{BODY}      : {token, {body, TokenLine, trim_body( TokenChars )}}.

{COMMENT1}  : skip_token.
{COMMENT2}  : skip_token.
{WS}        : skip_token.


%% ==================================================================
%% Erlang Code
%% ==================================================================


Erlang code.

% -export( [yyrev/2] ).

-spec trim_body( S :: string() ) -> string().

trim_body( S )
when is_list( S ), length( S ) >= 4 ->
  string:substr( S, 3, length( S )-4 ).


-spec trim_lit( S :: string() ) -> string().

trim_lit( S )
when is_list( S ), length( S ) >= 2 ->
  string:substr( S, 2, length( S )-2 ).