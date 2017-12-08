-module( cuneiform_type_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_type, [type/1] ).

-import( cuneiform_lang, [str/1, t_str/0, file/1, t_file/0, true/0, false/0,
                          t_bool/0, cmp/2, var/1, lam_ntv/2, lam_ntv_arg/2,
                          t_arg/2, t_fn/3] ).

type_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string literal typable",     fun string_literal_typable/0},
    {"file literal typable",       fun file_literal_typable/0},
    {"true typable",               fun true_typable/0},
    {"false typable",              fun false_typable/0},
    {"string comparison typable",  fun string_comparison_typable/0},
    {"native lambda typable",      fun native_lambda_typable/0},
    {"bound variable typable",     fun bound_variable_typable/0},
    {"unbound variable untypable", fun unbound_variable_untypable/0},
    {"comparison with invalid lhs untypable",
     fun comparison_with_invalid_lhs_untypable/0},
    {"comparison with invalid rhs untypable",
     fun comparison_with_invalid_rhs_untypable/0}
   ]
  }.

string_literal_typable() ->
  ?assertEqual( {ok, t_str()}, type( str( <<"blub">> ) ) ).

file_literal_typable() ->
  ?assertEqual( {ok, t_file()}, type( file( <<"blub.txt">> ) ) ).

true_typable() ->
  ?assertEqual( {ok, t_bool()}, type( true() ) ).

false_typable() ->
  ?assertEqual( {ok, t_bool()}, type( false() ) ).

string_comparison_typable() ->
  E = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).

native_lambda_typable() ->
  E = lam_ntv( [], str( <<"blub">> ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_str() )}, type( E ) ).

bound_variable_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_str() )], t_str() )}, type( E ) ).

unbound_variable_untypable() ->
  ?assertEqual( {error, {unbound, na, x}}, type( var( x ) ) ).

comparison_with_invalid_lhs_untypable() ->
  E = cmp( var( x ), str( <<"blub">> ) ),
  ?assertEqual( {error, {unbound, na, x}}, type( E ) ).

comparison_with_invalid_rhs_untypable() ->
  E = cmp( str( <<"bla">> ), var( y ) ),
  ?assertEqual( {error, {unbound, na, y}}, type( E ) ).