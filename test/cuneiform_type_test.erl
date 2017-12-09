-module( cuneiform_type_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_type, [type/1] ).

-import( cuneiform_lang, [str/1, t_str/0, file/1, t_file/0, true/0, false/0,
                          t_bool/0, cmp/2, var/1, lam_ntv/2, lam_ntv_arg/2,
                          t_arg/2, t_fn/3, neg/1] ).

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
     fun comparison_with_invalid_rhs_untypable/0},
    {"comparison with file lhs untypable",
     fun comparison_with_file_lhs_untypable/0},
    {"comparison with file rhs untypable",
     fun comparison_with_file_rhs_untypable/0},
    {"comparison with lhs variable typable",
     fun comparison_with_lhs_variable_typable/0},
    {"comparison with rhs variable typable",
     fun comparison_with_rhs_variable_typable/0},
    {"negation typable",           fun negation_typable/0},
    {"negation of non-Boolean untypable",
     fun negation_of_nonbool_untypable/0}
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
  E1 = lam_ntv( [], str( <<"blub">> ) ),
  E2 = lam_ntv( [], file( <<"bla.txt">> ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_file() )}, type( E2 ) ).

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

comparison_with_file_lhs_untypable() ->
  E = cmp( file( <<"bla.txt">> ), str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}}, type( E ) ).

comparison_with_file_rhs_untypable() ->
  E = cmp( str( <<"bla">> ), file( <<"blub.txt">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}}, type( E ) ).

comparison_with_lhs_variable_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )],
               cmp( var( x ), str( <<"blub">> ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_str() )], t_bool() )}, type( E ) ).

comparison_with_rhs_variable_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )],
               cmp( str( <<"bla">> ), var( x ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_str() )], t_bool() )}, type( E ) ).

negation_typable() ->
  ?assertEqual( {ok, t_bool()}, type( neg( true() ) ) ).

negation_of_nonbool_untypable() ->
  E = neg( str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).