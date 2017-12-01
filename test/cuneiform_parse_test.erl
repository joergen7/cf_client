-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [str/2, var/2, file/2, true/1, false/1, cmp/3,
                          conj/3, disj/3, neg/2, cnd/4, lam_ntv/3,
                          lam_ntv_arg/2, t_str/0, t_file/0, app/3,
                          r_var/3, t_fn/3, t_rcd/1, t_arg/2,
                          l_bash/0, lam_frn/6, t_lst/1, l_octave/0, t_bool/0,
                          l_perl/0, l_python/0, l_r/0, x_bind/2, l_racket/0,
                          fix/2] ).

parse_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"one variable definition", fun one_variable_definition/0},
    {"two_variable_definition", fun two_variable_definition/0},
    {"one single import",       fun one_single_import/0},
    {"two single imports",      fun two_single_imports/0},
    {"import definition",       fun import_definition/0},
    {"variable",                fun variable/0},
    {"string",                  fun string/0},
    {"integer",                 fun integer/0},
    {"file",                    fun file/0},
    {"true",                    fun true/0},
    {"false",                   fun false/0},
    {"compare string",          fun compare_string/0},
    {"conditional",             fun conditional/0},
    {"negation",                fun negation/0},
    {"conjunction",             fun conjunction/0},
    {"disjunction",             fun disjunction/0},
    {"no arg lambda",           fun no_arg_lambda/0},
    {"two arg lambda",          fun two_arg_lambda/0},
    {"foreign function bash",   fun foreign_function_bash/0},
    {"foreign function octave", fun foreign_function_octave/0},
    {"foreign function perl",   fun foreign_function_perl/0},
    {"foreign function python", fun foreign_function_python/0},
    {"foreign function r",      fun foreign_function_r/0},
    {"foreign function racket", fun foreign_function_racket/0},
    {"fixpoint operator",       fun fixpoint_operator/0}
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

no_arg_lambda() ->
  TokenLst = [{lambda, 1, "\\"}, {lparen, 1, "("},
              {rparen, 1, ")"}, {id, 1, "a"}, {dot, 1, "."}],
  E = lam_ntv( 1, [], var( 1, a ) ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).

two_arg_lambda() ->
  TokenLst = [{lambda, 1, "\\"}, {lparen, 1, "("}, {id, 1, "a"},
              {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "b"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"}, {id, 1, "a"}, {dot, 1, "."}],
  E = lam_ntv( 1, [lam_ntv_arg( a, t_str() ),
                   lam_ntv_arg( b, t_file() )], var( 1, a ) ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).

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
  E = app( 5, var( 5, f ), [x_bind( x, var( 5, x ) )] ),
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
  E = app( 5, var( 5, f ), [x_bind( x, var( 5, x ) ), x_bind( y, var( 5, y ) )] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [t_arg( x, t_str() ), t_arg( y, t_file() )], t_rcd( [t_arg( out, t_str() )] ),
                      l_racket(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, [E]}}, parse( TokenLst ) ).

fixpoint_operator() ->
  TokenLst = [{fix, 1, "fix"},
              {lambda, 1, "\\"},
              {lparen, 1, "("},
              {id, 1, "f"},
              {colon, 1, ":"},
              {t_lam_ntv, 1, "Ntv"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {t_bool, 1, "Bool"},
              {rparen, 1, ")"},
              {id, 2, "f"},
              {lparen, 2, "("},
              {rparen, 2, ")"},
              {dot, 2, "."}],
  E = fix( 1, lam_ntv( 1, [lam_ntv_arg( f, t_fn( ntv, [], t_bool() ) )],
      app( 2, var( 2, f ), [] ) ) ),
  ?assertEqual( {ok, {[], [], [], [E]}}, parse( TokenLst ) ).