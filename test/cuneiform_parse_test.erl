-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [str/2, var/2, file/2, true/1, false/1, cmp/3,
                          conj/3, disj/3, neg/2, cnd/4, lam_ntv/3,
                          lam_ntv_arg/3, t_str/0, t_file/0, app/3,
                          r_var/3, t_fn/3, t_rcd/1, t_arg/2,
                          l_bash/0, lam_frn/6] ).

parse_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"one variable definition", fun one_variable_definition/0},
    {"two_variable_definition", fun two_variable_definition/0},
    {"one single import",       fun one_single_import/0},
    {"two single imports",      fun two_single_imports/0},
    {"import definition query", fun import_definition_query/0},
    {"variable query",          fun variable_query/0},
    {"string query",            fun string_query/0},
    {"integer query",           fun integer_query/0},
    {"file query",              fun file_query/0},
    {"true query",              fun true_query/0},
    {"false query",             fun false_query/0},
    {"compare query",           fun compare_query/0},
    {"conditional query",       fun conditional_query/0},
    {"negation query",          fun negation_query/0},
    {"conjunction query",       fun conjunction_query/0},
    {"disjunction query",       fun disjunction_query/0},
    {"no arg lambda query",     fun no_arg_lambda_query/0},
    {"two arg lambda query",    fun two_arg_lambda_query/0},
    {"foreign function bash",   fun foreign_function_bash/0}
   ]
  }.

one_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {id, 2, "x"}],
  ?assertEqual( {ok, {[],
                      [],
                      [{r_var( 1, x, 'Str' ), str( 1, <<"bla">> )}],
                      var( 2, x )}}, parse( TokenLst ) ).

two_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {t_str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "y"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "blub"},
              {semicolon, 2, ";"}, {id, 3, "x"}],
  ?assertEqual( {ok, {[],
                      [],
                      [{r_var( 1, x, t_str() ), str( 1, <<"bla">> )},
                       {r_var( 2, y, t_str() ), str( 2, <<"blub">> )}],
                      var( 3, x )}}, parse( TokenLst ) ).

one_single_import() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {strlit, 2, "bla"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [],
                      str( 2, <<"bla">> )}}, parse( TokenLst ) ).

two_single_imports() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {import, 2, "import"}, {filelit, 2, "b.cuf"}, {semicolon, 2, ";"},
              {strlit, 3, "bla"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}, {import, 2, "b.cuf"}],
                      [],
                      [],
                      str( 3, <<"bla">> )}}, parse( TokenLst ) ).

import_definition_query() ->
  TokenLst = [{import, 1, "import"}, {filelit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "x"}, {colon, 2, ":"},
              {t_str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "bla"},
              {semicolon, 2, ";"},
              {id, 3, "x"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [{r_var( 2, x, t_str() ), str( 2, <<"bla">> )}],
                      var( 3, x )}}, parse( TokenLst ) ).

variable_query() ->
  TokenLst = [{id, 1, "x"}],
  ?assertEqual( {ok, {[], [], [], var( 1, x )}}, parse( TokenLst ) ).

string_query() ->
  TokenLst = [{strlit, 1, "bla"}],
  ?assertEqual( {ok, {[], [], [], str( 1, <<"bla">> )}}, parse( TokenLst ) ).

integer_query() ->
  TokenLst = [{intlit, 1, "-5"}],
  ?assertEqual( {ok, {[], [], [], str( 1, <<"-5">> )}}, parse( TokenLst ) ).

file_query() ->
  TokenLst = [{filelit, 1, "blub.txt"}],
  ?assertEqual( {ok, {[], [], [], file( 1, <<"blub.txt">> )}}, parse( TokenLst ) ).

true_query() ->
  TokenLst = [{true, 1, "true"}],
  ?assertEqual( {ok, {[], [], [], true( 1 )}}, parse( TokenLst ) ).

false_query() ->
  TokenLst = [{false, 1, "false"}],
  ?assertEqual( {ok, {[], [], [], false( 1 )}}, parse( TokenLst ) ).

compare_query() ->
  TokenLst = [{lparen, 1, "("}, {strlit, 1, "bla"}, {cmp, 1, "=="},
              {strlit, 1, "blub"}, {rparen, 1, ")"}],
  ?assertEqual( {ok, {[], [], [], cmp( 1, str( 1, <<"bla">> ), str( 1, <<"blub">> ) )}},
                parse( TokenLst ) ).

conditional_query() ->
  TokenLst = [{cnd, 1, "if"}, {true, 1, "true"},
              {then, 2, "then"},
              {strlit, 3, "bla"},
              {else, 4, "else"},
              {strlit, 5, "blub"}],
  E = cnd( 1, true( 1 ), str( 3, <<"bla">> ), str( 5, <<"blub">> ) ),
  ?assertEqual( {ok, {[], [], [], E}}, parse( TokenLst ) ).

negation_query() ->
  TokenLst = [{neg, 1, "not"}, {false, 1, "false"}],
  ?assertEqual( {ok, {[], [], [], neg( 1, false( 1 ) )}}, parse( TokenLst ) ).

conjunction_query() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {wedge, 1, "and"},
              {false, 1, "false"}, {rparen, 1, ")"}],
  ?assertEqual( {ok, {[], [], [], conj( 1, true( 1 ), false( 1 ) )}},
                parse( TokenLst ) ).

disjunction_query() ->
  TokenLst = [{lparen, 1, "("}, {true, 1, "true"}, {vee, 1, "or"},
              {false, 1, "false"}, {rparen, 1, ")"}],
  ?assertEqual( {ok, {[], [], [], disj( 1, true( 1 ), false( 1 ) )}},
                parse( TokenLst ) ).

no_arg_lambda_query() ->
  TokenLst = [{lambda, 1, "\\"}, {lparen, 1, "("},
              {rparen, 1, ")"}, {id, 1, "a"}],
  E = lam_ntv( 1, [], var( 1, a ) ),
  ?assertEqual( {ok, {[], [], [], E}}, parse( TokenLst ) ).

two_arg_lambda_query() ->
  TokenLst = [{lambda, 1, "\\"}, {lparen, 1, "("}, {id, 1, "a"},
              {colon, 1, ":"}, {t_str, 1, "Str"}, {comma, 1, ","},
              {id, 1, "b"}, {colon, 1, ":"}, {t_file, 1, "File"},
              {rparen, 1, ")"}, {id, 1, "a"}],
  E = lam_ntv( 1, [lam_ntv_arg( a, a, t_str() ),
                   lam_ntv_arg( b, b, t_file() )], var( 1, a ) ),
  ?assertEqual( {ok, {[], [], [], E}}, parse( TokenLst ) ).

foreign_function_bash() ->
  TokenLst = [{def, 1, "def"},
              {id, 1, "f"},
              {lparen, 1, "("},
              {rparen, 1, ")"},
              {rarrow, 1, "->"},
              {ltag, 1, "<"},
              {id, 1, "out"},
              {colon, 1, ":"}, {t_str, 1, "Str"}, {rtag, 1, ">"},
              {in, 1, "in"}, {l_bash, 1, "Bash"}, {body, 1, "blablub"},
              {id, 5, "f"}, {lparen, 5, "("}, {rparen, 5, ")"}],
  E = app( 5, var( 5, f ), [] ),
  DefLst = [{r_var( 1, f, t_fn( frn, [], t_rcd( [t_arg( out, t_str() )] ) ) ),
             lam_frn( 1, f, [], t_rcd( [t_arg( out, t_str() )] ),
                      l_bash(), <<"blablub">> )}],
  ?assertEqual( {ok, {[], [], DefLst, E}}, parse( TokenLst ) ).