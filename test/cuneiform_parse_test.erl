-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [str/2, var/2, file/2, true/1, false/1, cmp/3,
                          conj/3, disj/3, neg/2, cnd/4, lam_ntv/3,
                          lam_ntv_arg/2, t_str/0, t_file/0, app/3,
                          r_var/3, t_fn/3, t_rcd/1, t_arg/2,
                          l_bash/0, lam_frn/6, t_lst/1, l_octave/0, t_bool/0,
                          l_perl/0, l_python/0, l_r/0, e_bind/2, l_racket/0,
                          fix/2, t_fn/4, rcd/2, r_rcd/2, r_bind/2, proj/3,
                          append/3, lst/3, isnil/2] ).

parse_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"one variable definition",    fun one_variable_definition/0},
    {"two_variable_definition",    fun two_variable_definition/0},
    {"one single import",          fun one_single_import/0},
    {"two single imports",         fun two_single_imports/0},
    {"import definition",          fun import_definition/0},
    {"variable",                   fun variable/0},
    {"string",                     fun string/0},
    {"integer",                    fun integer/0},
    {"file",                       fun file/0},
    {"true",                       fun true/0},
    {"false",                      fun false/0},
    {"compare string",             fun compare_string/0},
    {"conditional",                fun conditional/0},
    {"negation",                   fun negation/0},
    {"conjunction",                fun conjunction/0},
    {"disjunction",                fun disjunction/0},
    {"no arg function",            fun no_arg_function/0},
    {"two arg function",           fun two_arg_function/0},
    {"foreign function bash",      fun foreign_function_bash/0},
    {"foreign function octave",    fun foreign_function_octave/0},
    {"foreign function perl",      fun foreign_function_perl/0},
    {"foreign function python",    fun foreign_function_python/0},
    {"foreign function r",         fun foreign_function_r/0},
    {"foreign function racket",    fun foreign_function_racket/0},
    {"foreign function alias",     fun foreign_function_alias/0},
    {"record pattern match",       fun record_pattern_match/0},
    {"record pattern multi match", fun record_pattern_multi_match/0},
    {"record field access",        fun record_field_access/0},
    {"empty list",                 fun empty_list/0},
    {"isnil list",                 fun isnil_list/0},
    {"list append",                fun list_append/0}
   ]
  }.

one_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {id, 2, "x"}, {dot, 2, "."}],
  ?assertEqual( {ok, {[],
                      [],
                      [{r_var( 1, x, 'Str' ), str( 1, <<"bla">> )}],
                      [var( 2, x )]}}, parse( TokenLst ) ).

two_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "y"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "blub"},
              {semicolon, 2, ";"}, {id, 3, "x"}, {dot, 3, "."}],
  ?assertEqual( {ok, {[],
                      [],
                      [{r_var( 1, x, t_str() ), str( 1, <<"bla">> )},
                       {r_var( 2, y, t_str() ), str( 2, <<"blub">> )}],
                      [var( 3, x )]}}, parse( TokenLst ) ).

one_single_import() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {strlit, 2, "bla"}, {dot, 2, "."}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [],
                      [str( 2, <<"bla">> )]}}, parse( TokenLst ) ).

two_single_imports() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {import, 2, "import"}, {filelit, 2, "b.cuf"}, {semicolon, 2, ";"},
              {strlit, 3, "bla"}, {dot, 3, "."}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}, {import, 2, "b.cuf"}],
                      [],
                      [],
                      [str( 3, <<"bla">> )]}}, parse( TokenLst ) ).

import_definition() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "x"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "bla"},
              {semicolon, 2, ";"},
              {id, 3, "x"}, {dot, 3, "."}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [{r_var( 2, x, t_str() ), str( 2, <<"bla">> )}],
                      [var( 3, x )]}}, parse( TokenLst ) ).

variable() ->
  TokenLst = [{id, 1, "x"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [var( 1, x )]}}, parse( TokenLst ) ).

string() ->
  TokenLst = [{strlit, 1, "bla"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [str( 1, <<"bla">> )]}}, parse( TokenLst ) ).

integer() ->
  TokenLst = [{intlit, 1, "-5"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [str( 1, <<"-5">> )]}}, parse( TokenLst ) ).

file() ->
  TokenLst = [{filelit, 1, "blub.txt"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [file( 1, <<"blub.txt">> )]}}, parse( TokenLst ) ).

true() ->
  TokenLst = [{true, 1, "true"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [true( 1 )]}}, parse( TokenLst ) ).

false() ->
  TokenLst = [{false, 1, "false"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [false( 1 )]}}, parse( TokenLst ) ).

compare_string() ->
  TokenLst = [{lparen, 1, "("}, {strlit, 1, "bla"}, {cmp, 1, "=="},
              {strlit, 1, "blub"}, {rparen, 1, ")"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [cmp( 1, str( 1, <<"bla">> ), str( 1, <<"blub">> ) )]}},
                parse( TokenLst ) ).

conditional() ->
  TokenLst = [{cnd, 1, "if"}, {true, 1, "true"},
              {then, 2, "then"},
              {strlit, 3, "bla"},
              {else, 4, "else"},
              {strlit, 5, "blub"}, {dot, 5, "."}],
  E = cnd( 1, true( 1 ), str( 3, <<"bla">> ), str( 5, <<"blub">> ) ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).

negation() ->
  TokenLst = [{neg, 1, "not"}, {false, 1, "false"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [neg( 1, false( 1 ) )]}}, parse( TokenLst ) ).

conjunction() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {wedge, 1, "and"},
              {false, 1, "false"}, {rparen, 1, ")"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [conj( 1, true( 1 ), false( 1 ) )]}},
                parse( TokenLst ) ).

disjunction() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {vee, 1, "or"},
              {false, 1, "false"}, {rparen, 1, ")"}, {dot, 1, "."}],
  ?assertEqual( {ok, {[], [], [], [disj( 1, true( 1 ), false( 1 ) )]}},
                parse( TokenLst ) ).

no_arg_function() ->
  TokenLst = [{def, 1, "def"}, {id, 1, "f"}, {lparen, 1, "("},
              {rparen, 1, ")"}, {rarrow, 1, "->"}, {t_str, "1", "Str"},
              {lbrace, 1, "{"}, {strlit, 2, "blub"}, {rbrace, 3, "}"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [] ),
  T = t_fn( ntv, [], t_str() ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T )], str( 2, <<"blub">> ) ) ),
  R = r_var( 1, f, T ),
  Def = {R, F},
  ?assertEqual( {ok, {[], [], [Def], [E]}}, parse( TokenLst ) ).

two_arg_function() ->
  TokenLst = [{def, 1, "def"}, {id, 1, "f"}, {lparen, 1, "("}, {id, 1, "a"},
              {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "b"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"}, {rarrow, 1, "->"}, {t_str, 1, "Str"},
              {lbrace, 1, "{"}, {id, 2, "a"}, {rbrace, 3, "}"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "a"}, {eq, 5, "="},
              {strlit, 5, "bla"}, {comma, 5, ","}, {id, 5, "b"}, {eq, 5, "="},
              {filelit, 5, "blub.txt"}, {rparen, 5, ")"}, {dot, 1, "."}],
  E = app( 5, var( 5, f ), [e_bind( a, str( 5, <<"bla">> ) ),
                  e_bind( b, file( 5, <<"blub.txt">> ) )] ),
  T = t_fn( ntv, [t_arg( a, t_str() ), t_arg( b, t_file() )], t_str() ),
  F = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, T ),
                        lam_ntv_arg( a, t_str() ),
                        lam_ntv_arg( b, t_file() )], var( 2, a ) ) ),
  R = r_var( 1, f, T ),
  Def = {R, F},
  ?assertEqual( {ok, {[], [], [Def], [E]}}, parse( TokenLst ) ).

foreign_function_bash() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_bash, 1, "Bash"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out, t_str() )] ),
                      l_bash(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_octave() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out1"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_str, 1, "Str"}, {rsquarebr, 1, "]"}, {comma, 1, ","},
              {id, 1, "out2"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_octave, 1, "Octave"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [], t_rcd( [t_arg( out1, t_lst( t_str() ) ),
                                                 t_arg( out2, t_file() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out1, t_lst( t_str() ) ),
                                        t_arg( out2, t_file() )] ),
                      l_octave(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_perl() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out1"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_file, 1, "File"}, {rsquarebr, 1, "]"}, {comma, 1, ","},
              {id, 1, "out2"}, {colon, 1, ":"}, {t_bool, 1, "Bool"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_perl, 1, "Perl"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [], t_rcd( [t_arg( out1, t_lst( t_file() ) ),
                                                 t_arg( out2, t_bool() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out1, t_lst( t_file() ) ),
                                        t_arg( out2, t_bool() )] ),
                      l_perl(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_python() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {lsquarebr, 1, "["},
              {t_bool, 1, "Bool"}, {rsquarebr, 1, "]"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_python, 1, "Python"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [], t_rcd( [t_arg( out, t_lst( t_bool() ) )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out, t_lst( t_bool() ) )] ),
                      l_python(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_r() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "x"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_r, 1, "R"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "x"}, {eq, 5, "="},
              {id, 5, "x"}, {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [e_bind( x, var( 5, x ) )] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [t_arg( x, t_str() )], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [t_arg( x, t_str() )], t_rcd( [t_arg( out, t_str() )] ),
                      l_r(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_racket() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {id, 1, "x"}, {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "y"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"}, {colon, 1, ":"}, {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"}, {l_racket, 1, "Racket"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {id, 5, "x"}, {eq, 5, "="},
              {id, 5, "x"}, {comma, 5, ","}, 
              {id, 5, "y"}, {eq, 5, "="}, {id, 5, "y"},
              {rparen, 5, ")"}, {dot, 5, "."}],
  E = app( 5, var( 5, f ), [e_bind( x, var( 5, x ) ), e_bind( y, var( 5, y ) )] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ),
                      l_racket(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

foreign_function_alias() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {in, 1, "in"},
              {l_bash, 1, "Bash"},
              {body, 1, "blub"},
              {assign, 2, "let"},
              {id, 2, "g"},
              {colon, 2, ":"},
              {t_fn_frn, 2, "Frn"},
              {lparen, 2, "("},
              {rparen, 2, ")"},
              {rarrow, 2, "->"},
              {ltag, 2, "<"},
              {id, 2, "x"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rtag, 2, ">"},
              {eq, 2, "="},
              {id, 2, "f"},
              {semicolon, 2, ";"},
              {id, 3, "g"},
              {lparen, 3, "("},
              {rparen, 3, ")"},
              {dot, 3, "."}],
  RetType = t_rcd( [t_arg( x, t_str() )] ),
  T = t_fn( frn, [], RetType ),
  F = lam_frn( 1, f, [], RetType, l_bash(), <<"blub">> ),
  G = var( 2, f ),
  H = app( 3, var( 3, g ), [] ),
  RF = {r_var, 1, f, T},
  RG = {r_var, 2, g, T},
  DefF = {RF, F},
  DefG = {RG, G},
  ?assertEqual( {ok, {[], [], [DefF, DefG], [H]}}, parse( TokenLst ) ).

record_pattern_match() ->
  TokenLst = [{assign, 1, "let"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {strlit, 1, "blub"},
              {rtag, 1, ">"},
              {semicolon, 1, ";"},
              {id, 2, "x"},
              {dot, 2, "."}],
  R = r_rcd( 1, [r_bind( a, r_var( 1, x, t_str() ) )] ),
  E = rcd( 1, [e_bind( a, str( 1, <<"blub">> ) )] ),
  ?assertEqual( {ok, {[], [], [{R, E}], [var( 2, x )]}}, parse( TokenLst ) ).

record_pattern_multi_match() ->
  TokenLst = [{assign, 1, "let"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {id, 1, "x"},
              {colon, 1, ":"},
              {t_bool, 1, "Bool"},
              {comma, 1, ","},
              {id, 1, "b"},
              {eq, 1, "="},
              {id, 1, "y"},
              {colon, 1, ":"},
              {lsquarebr, 1, "["},
              {t_file, 1, "File"},
              {rsquarebr, 1, "]"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {id, 1, "z"},
              {semicolon, 1, ";"},
              {id, 2, "y"},
              {dot, 2, "."}],
  R = r_rcd( 1, [r_bind( a, r_var( 1, x, t_bool() ) ),
                 r_bind( b, r_var( 1, y, t_lst( t_file() ) ) )] ),
  E = var( 1, z ),
  ?assertEqual( {ok, {[], [], [{R, E}], [var( 2, y )]}}, parse( TokenLst ) ).

record_field_access() ->
  TokenLst = [{assign, 1, "let"},
              {id, 1, "x"},
              {colon, 1, ":"},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rtag, 1, ">"},
              {eq, 1, "="},
              {ltag, 1, "<"},
              {id, 1, "a"},
              {eq, 1, "="},
              {strlit, 1, "blub"},
              {rtag, 1, ">"},
              {semicolon, 1, ";"},
              {lparen, 1, "("},
              {id, 2, "x"},
              {dot, 2, "."},
              {id, 2, "a"},
              {rparen, 2, ")"},
              {dot, 2, "."}],
  R = r_var( 1, x, t_rcd( [t_arg( a, t_str() )] ) ),
  E1 = rcd( 1, [e_bind( a, str( 1, <<"blub">> ) )] ),
  E2 = proj( 2, a, var( 2, x ) ),
  ?assertEqual( {ok, {[], [], [{R, E1}], [E2]}}, parse( TokenLst ) ).

empty_list() ->
  TokenLst = [{lsquarebr, 1, "["},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
               {dot, 1, "."}],
  E = lst( 1, t_str(), [] ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).

isnil_list() ->
  TokenLst = [{assign, 1, "let"},
              {id, 1, "l"},
              {colon, 1, ":"},
              {lsquarebr, 1, "["},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {eq, 1, "="},
              {lsquarebr, 2, "["},
              {strlit, 2, "bla"},
              {comma, 2, ","},
              {strlit, 2, "blub"},
              {colon, 2, ":"},
              {t_str, 2, "Str"},
              {rsquarebr, 2, "]"},
              {semicolon, 2, ";"},
              {isnil, 3, "isnil"},
              {id, 3, "l"},
              {dot, 3, "."}],
  R = r_var( 1, l, t_lst( t_str() ) ),
  E1 = lst( 2, t_str(), [str( 2, <<"bla">> ), str( 2, <<"blub">> )] ),
  E2 = isnil( 3, var( 3, l ) ),
  ?assertEqual( {ok, {[], [], [{R, E1}], [E2]}}, parse( TokenLst ) ).

list_append() ->
  TokenLst = [{lparen, 1, "("},
              {lsquarebr, 1, "["},
              {strlit, 1, "bla"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {plus, 1, "+"},
              {lsquarebr, 1, "["},
              {strlit, 1, "blub"},
              {colon, 1, ":"},
              {t_str, 1, "Str"},
              {rsquarebr, 1, "]"},
              {rparen, 1, ")"},
              {dot, 1, "."}],
  E = append( 1, lst( 1, t_str(), [str( 1, <<"bla">> )] ),
                 lst( 1, t_str(), [str( 1, <<"blub">> )] ) ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).

