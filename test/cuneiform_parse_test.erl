-module( cuneiform_parse_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_parse, [parse/1] ).
-import( cuneiform_lang, [str/2, var/2] ).

scan_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string query",            fun string_query/0},
    {"variable query",          fun variable_query/0},
    {"one variable definition", fun one_variable_definition/0},
    {"two_variable_definition", fun two_variable_definition/0}
   ]
  }.

string_query() ->
  TokenLst = [{strlit, 1, "bla"}],
  ?assertEqual( {ok, {[], [], [], str( 1, "bla" )}}, parse( TokenLst ) ).

variable_query() ->
  TokenLst = [{id, 1, "x"}],
  ?assertEqual( {ok, {[], [], [], var( 1, x )}}, parse( TokenLst ) ).

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
