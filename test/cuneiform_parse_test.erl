-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [str/2, var/2, file/2, true/1, false/1, cmp/3, conj/3,
                          disj/3, neg/2] ).

parse_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"one variable definition", fun one_variable_definition/0},
    {"two_variable_definition", fun two_variable_definition/0},
    {"one single import",       fun one_single_import/0},
    {"one double import",       fun one_double_import/0},
    {"two single imports",      fun two_single_imports/0},
    {"import definition query", fun import_definition_query/0},
    {"variable query",          fun variable_query/0},
    {"string query",            fun string_query/0},
    {"file query",              fun file_query/0},
    {"true query",              fun true_query/0},
    {"false query",             fun false_query/0},
    {"compare query",           fun compare_query/0},
    {"negation query",          fun negation_query/0},
    {"conjunction query",       fun conjunction_query/0},
    {"disjunction query",       fun disjunction_query/0}
   ]
  }.

one_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {id, 2, "x"}],
  ?assertEqual( {ok, {[],
                      [],
                      [{{r_var, 1, x, 'Str'}, {str, 1, "bla"}}],
                      var( 2, x )}}, parse( TokenLst ) ).

two_variable_definition() ->
  TokenLst = [{assign, 1, "let"}, {id, 1, "x"}, {colon, 1, ":"},
              {str, 1, "Str"}, {eq, 1, "="}, {strlit, 1, "bla"},
              {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "y"}, {colon, 2, ":"},
              {str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "blub"},
              {semicolon, 2, ";"}, {id, 3, "x"}],
  ?assertEqual( {ok, {[],
                      [],
                      [{{r_var, 1, x, 'Str'}, {str, 1, "bla"}},
                       {{r_var, 2, y, 'Str'}, {str, 2, "blub"}}],
                      var( 3, x )}}, parse( TokenLst ) ).

one_single_import() ->
  TokenLst = [{import, 1, "import"}, {strlit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {strlit, 2, "bla"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [],
                      {str, 2, "bla"}}}, parse( TokenLst ) ).

one_double_import() ->
  TokenLst = [{import, 1, "import"}, {strlit, 1, "a.cuf"}, {comma, 1, ","},
              {strlit, 1, "b.cuf"}, {semicolon, 1, ";"},
              {strlit, 2, "bla"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}, {import, 1, "b.cuf"}],
                      [],
                      [],
                      {str, 2, "bla"}}}, parse( TokenLst ) ).

two_single_imports() ->
  TokenLst = [{import, 1, "import"}, {strlit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {import, 2, "import"}, {strlit, 2, "b.cuf"}, {semicolon, 2, ";"},
              {strlit, 3, "bla"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}, {import, 2, "b.cuf"}],
                      [],
                      [],
                      {str, 3, "bla"}}}, parse( TokenLst ) ).

import_definition_query() ->
  TokenLst = [{import, 1, "import"}, {strlit, 1, "a.cuf"}, {semicolon, 1, ";"},
              {assign, 2, "let"}, {id, 2, "x"}, {colon, 2, ":"},
              {str, 2, "Str"}, {eq, 2, "="}, {strlit, 2, "bla"},
              {semicolon, 2, ";"},
              {id, 3, "x"}],
  ?assertEqual( {ok, {[{import, 1, "a.cuf"}],
                      [],
                      [{{r_var, 2, x, 'Str'}, {str, 2, "bla"}}],
                      {var, 3, x}}}, parse( TokenLst ) ).

variable_query() ->
  TokenLst = [{id, 1, "x"}],
  ?assertEqual( {ok, {[], [], [], var( 1, x )}}, parse( TokenLst ) ).

string_query() ->
  TokenLst = [{strlit, 1, "bla"}],
  ?assertEqual( {ok, {[], [], [], str( 1, "bla" )}}, parse( TokenLst ) ).

file_query() ->
  TokenLst = [{filelit, 1, "blub.txt"}],
  ?assertEqual( {ok, {[], [], [], file( 1, "blub.txt" )}}, parse( TokenLst ) ).

true_query() ->
  TokenLst = [{true, 1, "true"}],
  ?assertEqual( {ok, {[], [], [], true( 1 )}}, parse( TokenLst ) ).

false_query() ->
  TokenLst = [{false, 1, "false"}],
  ?assertEqual( {ok, {[], [], [], false( 1 )}}, parse( TokenLst ) ).

compare_query() ->
  TokenLst = [{lparen, 1, "("}, {strlit, 1, "bla"}, {cmp, 1, "=="},
              {strlit, 1, "blub"}, {rparen, 1, ")"}],
  ?assertEqual( {ok, {[], [], [], cmp( 1, str( 1, "bla" ), str( 1, "blub" ) )}},
                parse( TokenLst ) ).

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
