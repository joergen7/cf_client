-module( cuneiform_type_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_type, [type/1] ).

-import( cuneiform_lang, [str/1, t_str/0, file/1, t_file/0, true/0, false/0,
                          t_bool/0, cmp/2, var/1, lam_ntv/2, lam_ntv_arg/2,
                          t_arg/2, t_fn/3, neg/1, cnd/3, conj/2, disj/2,
                          t_rcd/1, l_bash/0, lam_frn/5, e_bind/2, rcd/1, app/2
                         ] ).

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
    {"native lambda with ambigious argument name untypable",
     fun native_lambda_with_ambigious_argument_name_untypable/0},
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
     fun negation_of_nonbool_untypable/0},
    {"negation with invalid expression untypable",
     fun negation_with_invalid_expression_untypable/0},
    {"negation with variable typable",
     fun negation_with_variable_typable/0},
    {"condition typable",          fun condition_typable/0},
    {"condition with invalid predicate untypable",
     fun condition_with_invalid_predicate_untypable/0},
    {"condition with nonbool predicate untypable",
     fun condition_with_nonbool_predicate_untypable/0},
    {"condition with invalid then expression untypable",
     fun condition_with_invalid_then_expression_untypable/0},
    {"condition with invalid else expression untypable",
     fun condition_with_invalid_else_expression_untypable/0},
    {"condition with non-matching then and else expression untypable",
     fun condition_with_nonmatching_then_and_else_expression_untypable/0},
    {"condition with variable predicate typable",
     fun condition_with_variable_predicate_typable/0},
    {"condition with variable then expression typable",
     fun condition_with_variable_then_expression_typable/0},
    {"condition with variable else expression typable",
     fun condition_with_variable_else_expression_typable/0},
    {"conjunction typable",        fun conjunction_typable/0},
    {"conjunction with invalid lhs untypable",
     fun conjunction_with_invalid_lhs_untypable/0},
    {"conjunction with invalid rhs untypable",
     fun conjunction_with_invalid_rhs_untypable/0},
    {"conjunction with non-Boolean lhs untypable",
     fun conjunction_with_nonboolean_lhs_untypable/0},
    {"conjunction with non-Boolean rhs untypable",
     fun conjunction_with_nonboolean_rhs_untypable/0},
    {"conjunction with variable lhs typable",
     fun conjunction_with_variable_lhs_typable/0},
    {"conjunction with variable rhs typable",
     fun conjunction_with_variable_rhs_typable/0},
    {"disjunction typable",        fun disjunction_typable/0},
    {"disjunction with invalid lhs untypable",
     fun disjunction_with_invalid_lhs_untypable/0},
    {"disjunction with invalid rhs untypable",
     fun disjunction_with_invalid_rhs_untypable/0},
    {"disjunction with non-Boolean lhs untypable",
     fun disjunction_with_nonboolean_lhs_untypable/0},
    {"disjunction with non-Boolean rhs untypable",
     fun disjunction_with_nonboolean_rhs_untypable/0},
    {"disjunction with variable lhs typable",
     fun disjunction_with_variable_lhs_typable/0},
    {"disjunction with variable rhs typable",
     fun disjunction_with_variable_rhs_typable/0},
    {"record_typable",             fun record_typable/0},
    {"record with invalid field untypable",
     fun record_with_invalid_field_untypable/0},
    {"record with variable field typable",
     fun record_with_variable_field_typable/0},
    {"record with ambigious field name untypable",
     fun record_with_ambigious_field_name_untypable/0},
    {"foreign lambda typable",
     fun foreign_lambda_typable/0},
    {"foreign lambda with ambigious argument name untypable",
     fun foreign_lambda_with_ambigious_argument_name_untypable/0},
    {"foreign lambda with ambigious return field name untypable",
     fun foreign_lambda_with_ambigious_return_field_name_untypable/0},
    {"foreign lambda with ambigious argument and return field untypable",
     fun foreign_lambda_with_ambigious_argument_and_return_field_untypable/0},
    {"application typable",
     fun application_typable/0},
    {"application with invalid function expression untypable",
     fun application_with_invalid_function_expression_untypable/0},
    {"application with variable function expression typable",
     fun application_with_variable_function_expression_typable/0},
    {"application with too few arguments untypable",
     fun application_with_too_few_arguments_untypable/0},
    {"application with too many arguments untypable",
     fun application_with_too_many_arguments_untypable/0},
    {"application with argument name mismatch untypable",
     fun application_with_argument_name_mismatch_untypable/0},
    {"application with invalid argument untypable",
     fun application_with_invalid_argument_untypable/0},
    {"application with argument type mismatch untypable",
     fun application_with_argument_type_mismatch_untypable/0},
    {"application with variable argument typable",
     fun application_with_variable_argument_typable/0}
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

native_lambda_with_ambigious_argument_name_untypable() ->
  E = lam_ntv(  [lam_ntv_arg( x, t_str() ),
                 lam_ntv_arg( x, t_file() )], var( x ) ),
  ?assertEqual( {error, {ambigious_name, na, x}}, type( E ) ).

bound_variable_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_str() )], t_str() )}, type( E ) ).

unbound_variable_untypable() ->
  ?assertEqual( {error, {unbound_var, na, x}}, type( var( x ) ) ).

comparison_with_invalid_lhs_untypable() ->
  E = cmp( var( x ), str( <<"blub">> ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

comparison_with_invalid_rhs_untypable() ->
  E = cmp( str( <<"bla">> ), var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

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

negation_with_invalid_expression_untypable() ->
  E = neg( var( x ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

negation_with_variable_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_bool() )], neg( var( x ) ) ),
  T = t_fn( ntv, [t_arg( x, t_bool() )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

condition_typable() ->
  E1 = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  E2 = cnd( true(), file( <<"bla">> ), file( <<"blub">> ) ),
  ?assertEqual( {ok, t_str()}, type( E1 ) ),
  ?assertEqual( {ok, t_file()}, type( E2 ) ).

condition_with_invalid_predicate_untypable() ->
  E = cnd( var( x ), str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

condition_with_nonbool_predicate_untypable() ->
  E = cnd( str( <<"true">> ), str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

condition_with_invalid_then_expression_untypable() ->
  E = cnd( true(), var( x ), str( <<"blub">> ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

condition_with_invalid_else_expression_untypable() ->
  E = cnd( true(), str( <<"bla">> ), var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

condition_with_nonmatching_then_and_else_expression_untypable() ->
  E = cnd( true(), str( <<"bla">> ), file( <<"blub.txt">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}},
                type( E ) ).

condition_with_variable_predicate_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_bool() )],
               cnd( var( x ), str( <<"bla">> ), str( <<"blub">> ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_bool() )], t_str() )}, type( E ) ).

condition_with_variable_then_expression_typable() ->
  E = lam_ntv( [lam_ntv_arg( y, t_str() )],
               cnd( true(),var( y ), str( <<"blub">> ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( y, t_str() )], t_str() )}, type( E ) ).

condition_with_variable_else_expression_typable() ->
  E = lam_ntv( [lam_ntv_arg( z, t_str() )],
               cnd( true(), str( <<"bla">> ), var( z ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( z, t_str() )], t_str() )}, type( E ) ).

conjunction_typable() ->
  ?assertEqual( {ok, t_bool()}, type( conj( true(), false() ) ) ).

conjunction_with_invalid_lhs_untypable() ->
  E = conj( var( x ), true() ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

conjunction_with_invalid_rhs_untypable() ->
  E = conj( true(), var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

conjunction_with_nonboolean_lhs_untypable() ->
  E = conj( str( <<"bla">> ), true() ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

conjunction_with_nonboolean_rhs_untypable() ->
  E = conj( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

conjunction_with_variable_lhs_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_bool() )], conj( var( x ), true() ) ),
  T = t_fn( ntv, [t_arg( x, t_bool() )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

conjunction_with_variable_rhs_typable() ->
  E = lam_ntv( [lam_ntv_arg( y, t_bool() )], conj( true(), var( y ) ) ),
  T = t_fn( ntv, [t_arg( y, t_bool() )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

disjunction_typable() ->
  ?assertEqual( {ok, t_bool()}, type( disj( true(), false() ) ) ).

disjunction_with_invalid_lhs_untypable() ->
  E = disj( var( x ), true() ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

disjunction_with_invalid_rhs_untypable() ->
  E = disj( true(), var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

disjunction_with_nonboolean_lhs_untypable() ->
  E = disj( str( <<"bla">> ), true() ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

disjunction_with_nonboolean_rhs_untypable() ->
  E = disj( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

disjunction_with_variable_lhs_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_bool() )], disj( var( x ), true() ) ),
  T = t_fn( ntv, [t_arg( x, t_bool() )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

disjunction_with_variable_rhs_typable() ->
  E = lam_ntv( [lam_ntv_arg( y, t_bool() )], disj( true(), var( y ) ) ),
  T = t_fn( ntv, [t_arg( y, t_bool() )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

record_typable() ->
  E = rcd( [e_bind( x, str( <<"blub">> ) )] ),
  ?assertEqual( {ok, t_rcd( [t_arg( x, t_str() )] )}, type( E ) ).

record_with_invalid_field_untypable() ->
  E1 = rcd( [e_bind( x, var( y ) )] ),
  E2 = rcd( [e_bind( x, str( <<"blub">> ) ), e_bind( y, var( z ) )] ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E1 ) ),
  ?assertEqual( {error, {unbound_var, na, z}}, type( E2 ) ).

record_with_variable_field_typable() ->
  E1 = lam_ntv( [lam_ntv_arg( x, t_str() )], rcd( [e_bind( y, var( x ) )] ) ),
  T1 = t_fn( ntv, [t_arg( x, t_str() )], t_rcd( [t_arg( y, t_str() )] ) ),
  E2 = lam_ntv( [lam_ntv_arg( x, t_str() )],
                rcd( [e_bind( y, file( <<"bla.txt">> ) ),
                      e_bind( z, var( x ) )] ) ),
  T2 = t_fn( ntv, [t_arg( x, t_str() )], t_rcd( [t_arg( y, t_file() ),
                                                 t_arg( z, t_str() )] ) ),
  ?assertEqual( {ok, T1}, type( E1 ) ),
  ?assertEqual( {ok, T2}, type( E2 ) ).

record_with_ambigious_field_name_untypable() ->
  E = rcd( [e_bind( x, str( <<"bla">> ) ),
            e_bind( x, file( <<"blub.txt">> ) )] ),
  ?assertEqual( {error, {ambigious_name, na, x}}, type( E ) ).

foreign_lambda_typable() ->
  TRet = t_rcd( [t_arg( out, t_str() )] ),
  E = lam_frn( f, [], TRet, l_bash(), <<"blub">> ),
  T = t_fn( frn, [], TRet ),
  ?assertEqual( {ok, T}, type( E ) ).

foreign_lambda_with_ambigious_argument_name_untypable() ->
  TRet = t_rcd( [t_arg( out, t_str() )] ),
  E = lam_frn( f, [t_arg( x, t_str() ),
                   t_arg( x, t_file() )], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambigious_name, na, x}}, type( E ) ).

foreign_lambda_with_ambigious_return_field_name_untypable() ->
  TRet = t_rcd( [t_arg( out, t_str() ), t_arg( out, t_file() )] ),
  E = lam_frn( f, [], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambigious_name, na, out}}, type( E ) ).

foreign_lambda_with_ambigious_argument_and_return_field_untypable() ->
  TRet = t_rcd( [t_arg( x, t_str() )] ),
  E = lam_frn( f, [t_arg( x, t_str() )], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambigious_name, na, x}}, type( E ) ).

application_typable() ->
  EF = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( EF, [e_bind( x, str( <<"bla">> ) )] ),
  ?assertEqual( {ok, t_str()}, type( EA ) ).

application_with_invalid_function_expression_untypable() ->
  E = app( var( f ), [e_bind( x, str( <<"bla">> ) )] ),
  ?assertEqual( {error, {unbound_var, na, f}}, type( E ) ).

application_with_variable_function_expression_typable() ->
  EA = app( var( f ), [] ),
  ELam = lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )], EA ),
  TLam = t_fn( ntv, [t_arg( f, t_fn( ntv, [], t_str() ) )], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

application_with_too_few_arguments_untypable() ->
  ELam = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( ELam, [] ),
  ?assertEqual( {error, {argument_missing, na, x}}, type( EA ) ).

application_with_too_many_arguments_untypable() ->
  ELam = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( ELam, [e_bind( x, str( <<"bla">> ) ),
                   e_bind( y, str( <<"blub">> ) )] ),
  ?assertEqual( {error, {superfluous_argument, na, y}}, type( EA ) ).

application_with_argument_name_mismatch_untypable() ->
  EF = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( EF, [e_bind( y, str( <<"bla">> ) )] ),
  ?assertEqual( {error, {argument_mismatch, na, {x, y}}}, type( EA ) ).

application_with_invalid_argument_untypable() ->
  EF = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( EF, [e_bind( x, var( y ) )] ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( EA ) ).

application_with_argument_type_mismatch_untypable() ->
  EF = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( EF, [e_bind( x, file( <<"bla">> ) )] ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}}, type( EA ) ).

application_with_variable_argument_typable() ->
  EF = lam_ntv( [lam_ntv_arg( y, t_str() )], var( y ) ),
  EA = app( EF, [e_bind( y, var( x ) )] ),
  ELam = lam_ntv( [lam_ntv_arg( x, t_str() )], EA ),
  TLam = t_fn( ntv, [t_arg( x, t_str() )], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).