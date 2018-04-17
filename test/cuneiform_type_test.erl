%% -*- erlang -*-
%%
%% A Cuneiform client implementation.
%%
%% Copyright 2015-2018 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.4
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_type_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_type, [type/1] ).

-import( cuneiform_lang, [str/1, t_str/0, file/1, t_file/0, true/0, false/0,
                          t_bool/0, cmp/2, var/1, lam_ntv/2, lam_ntv_arg/2,
                          t_arg/2, t_fn/3, neg/1, cnd/3, conj/2, disj/2,
                          t_rcd/1, l_bash/0, lam_frn/5, e_bind/2, rcd/1, app/2,
                          proj/2, fix/1, lst/2, t_lst/1, append/2, isnil/1,
                          for/3, fold/3, err/2
                         ] ).

type_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"not well formed expression produces error",
     fun not_well_formed_expression_produces_error/0},

    {"string literal typable",     fun string_literal_typable/0},
    {"file literal typable",       fun file_literal_typable/0},
    {"true typable",               fun true_typable/0},
    {"false typable",              fun false_typable/0},
    {"string comparison typable",  fun string_comparison_typable/0},
    {"boolean comparison typable", fun boolean_comparison_typable/0},
    {"native lambda typable",      fun native_lambda_typable/0},

    {"native lambda with invalid body expression untypable",
     fun native_lambda_with_invalid_body_expression_untypable/0},

    {"native lambda with ambiguous argument name untypable",
     fun native_lambda_with_ambiguous_argument_name_untypable/0},

    {"native lambda body expression can access closure",
     fun native_lambda_body_expression_can_access_closure/0},

    {"bound variable typable",     fun bound_variable_typable/0},
    {"unbound variable untypable", fun unbound_variable_untypable/0},

    {"comparison with invalid lhs untypable",
     fun comparison_with_invalid_lhs_untypable/0},

    {"comparison with invalid rhs untypable",
     fun comparison_with_invalid_rhs_untypable/0},

    {"comparison with file operands untypable",
     fun comparison_with_file_operands_untypable/0},

    {"comparison with non-matching operands untypable",
     fun comparison_with_nonmatching_operands_untypable/0},

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
    {"record with ambiguous field name untypable",
     fun record_with_ambiguous_field_name_untypable/0},
    {"foreign lambda typable",
     fun foreign_lambda_typable/0},
    {"foreign lambda with ambiguous argument name untypable",
     fun foreign_lambda_with_ambiguous_argument_name_untypable/0},
    {"foreign lambda with ambiguous return field name untypable",
     fun foreign_lambda_with_ambiguous_return_field_name_untypable/0},
    {"foreign lambda with ambiguous argument and return field untypable",
     fun foreign_lambda_with_ambiguous_argument_and_return_field_untypable/0},
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
     fun application_with_variable_argument_typable/0},
    {"record field access typable",
     fun record_field_access_typable/0},
    {"record field access with invalid record expression untypable",
     fun record_field_access_with_invalid_record_expression_untypable/0},
    {"record field access with non-record record expression untypable",
     fun record_field_access_with_nonrecord_record_expression_untypable/0},
    {"record field access with variable record expression typable",
     fun record_field_access_with_variable_record_expression_typable/0},
    {"record field access with non-existing field name untypable",
     fun record_field_access_with_nonexisting_field_name_untypable/0},
    {"fixpoint typable",           fun fixpoint_typable/0},
    {"fixpoint with argument typable",
     fun fixpoint_with_argument_typable/0},
    {"fixpoint with invalid function expression untypable",
     fun fixpoint_with_invalid_function_expression_untypable/0},
    {"fixpoint with non-function function expression untypable",
     fun fixpoint_with_nonfunction_function_expression_untypable/0},
    {"fixpoint with foreign function expression untypable",
     fun fixpoint_with_foreign_function_expression_untypable/0},
    {"fixpoint with variable function expression typable",
     fun fixpoint_with_variable_function_expression_typable/0},
    {"fixpoint with constant function expression untypable",
     fun fixpoint_with_constant_function_expression_untypable/0},
    {"recursive fixpoint typable", fun recursive_fixpoint_typable/0},
    {"list typable",               fun list_typable/0},
    {"list with invalid element untypable",
     fun list_with_invalid_element_untypable/0},
    {"list with non-matching element untypable",
     fun list_with_nonmatching_element_untypable/0},
    {"list with variable element typable",
     fun list_with_variable_element_typable/0},
    {"list append typable",        fun list_append_typable/0},
    {"list append with invalid lhs untypable",
     fun list_append_with_invalid_lhs_untypable/0},
    {"list append with invalid rhs untypable",
     fun list_append_with_invalid_rhs_untypable/0},
    {"list append with non-list lhs untypable",
     fun list_append_with_nonlist_lhs_untypable/0},
    {"list append with non-list rhs untypable",
     fun list_append_with_nonlist_rhs_untypable/0},
    {"list append with variable lhs typable",
     fun list_append_with_variable_lhs_typable/0},
    {"list append with variable rhs typable",
     fun list_append_with_variable_rhs_typable/0},
    {"list append with non-matching operands untypable",
     fun list_append_with_nonmatching_operands_untypable/0},
    {"isnil typable",              fun isnil_typable/0},
    {"isnil with invalid list expression untypable",
     fun isnil_with_invalid_list_expression_untypable/0},
    {"isnil with non-list list expression untypable",
     fun isnil_with_nonlist_list_expression_untypable/0},
    {"isnil with variable list expression typable",
     fun isnil_with_variable_list_expression_typable/0},
    {"for typable",                fun for_typable/0},
    {"for with invalid list expression untypable",
     fun for_with_invalid_list_expression_untypable/0},
    {"for with non-list list expression untypable",
     fun for_with_nonlist_list_expression_untypable/0},
    {"for with variable list expression typable",
     fun for_with_variable_list_expression_typable/0},
    {"for with invalid body expression untypable",
     fun for_with_invalid_body_expression_untypable/0},
    {"for variable body expression typable",
     fun for_variable_body_expression_typable/0},

    {"for with non-matching body expression untypable",
     fun for_with_nonmatching_body_expression_untypable/0},
      
    {"fold typable",               fun fold_typable/0},
    {"fold with invalid accumulator expression untypable",
     fun fold_with_invalid_accumulator_expression_untypable/0},
    {"fold with variable accumulator expression typable",
     fun fold_with_variable_accumulator_expression_typable/0},
    {"fold with invalid list expression untypable",
     fun fold_with_invalid_list_expression_untypable/0},
    {"fold with non-list list expression untypable",
     fun fold_with_nonlist_list_expression_untypable/0},
    {"fold with variable list expression typable",
     fun fold_with_variable_list_expression_typable/0},
    {"fold with invalid body expression untypable",
     fun fold_with_invalid_body_expression_untypable/0},
    {"fold with variable body expression typable",
     fun fold_with_variable_body_expression_typable/0},

    {"fold with non-matching accumulator and body expression untypable",
     fun fold_with_nonmatching_accumulator_and_body_expression_untypable/0},

    {"fold with non-matching accumulator and list expression untypable",
     fun fold_with_nonmatching_accumulator_and_list_expression_untypable/0},
      
    {"fold with ambiguous accumulator and list expression name untypable",
     fun fold_with_ambiguous_accumulator_and_list_expression_name_untypable/0},

    {"future is typable",
     fun future_is_typable/0},

    {"error is typable",
     fun error_is_typable/0}
   ]
  }.

not_well_formed_expression_produces_error() ->
  ?assertError( {bad_expr, x}, type( x ) ).

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

boolean_comparison_typable() ->
  E = cmp( true(), false() ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).


native_lambda_typable() ->
  E1 = lam_ntv( [], str( <<"blub">> ) ),
  E2 = lam_ntv( [], file( <<"bla.txt">> ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_file() )}, type( E2 ) ).

native_lambda_with_invalid_body_expression_untypable() ->
  E1 = lam_ntv( [], var( x ) ),
  E2 = lam_ntv( [lam_ntv_arg( x, t_str() )], var( y ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E1 ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E2 ) ).

native_lambda_with_ambiguous_argument_name_untypable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() ),
                lam_ntv_arg( x, t_file() )], var( x ) ),
  ?assertEqual( {error, {ambiguous_name, na, x}}, type( E ) ).

native_lambda_body_expression_can_access_closure() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], lam_ntv( [], var( x ) ) ),
  T = t_fn( ntv, [t_arg( x, t_str() )], t_fn( ntv, [], t_str() ) ),
  ?assertEqual( {ok, T}, type( E ) ).

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

comparison_with_file_operands_untypable() ->
  E = cmp( file( <<"bla.txt">> ), file( <<"blub">> ) ),
  ?assertEqual( {error, {no_comparable_type, na, t_file()}}, type( E ) ).

comparison_with_nonmatching_operands_untypable() ->
  E = cmp( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

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

record_with_ambiguous_field_name_untypable() ->
  E = rcd( [e_bind( x, str( <<"bla">> ) ),
            e_bind( x, file( <<"blub.txt">> ) )] ),
  ?assertEqual( {error, {ambiguous_name, na, x}}, type( E ) ).

foreign_lambda_typable() ->
  TRet = t_rcd( [t_arg( out, t_str() )] ),
  E = lam_frn( f, [], TRet, l_bash(), <<"blub">> ),
  T = t_fn( frn, [], TRet ),
  ?assertEqual( {ok, T}, type( E ) ).

foreign_lambda_with_ambiguous_argument_name_untypable() ->
  TRet = t_rcd( [t_arg( out, t_str() )] ),
  E = lam_frn( f, [t_arg( x, t_str() ),
                   t_arg( x, t_file() )], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambiguous_name, na, x}}, type( E ) ).

foreign_lambda_with_ambiguous_return_field_name_untypable() ->
  TRet = t_rcd( [t_arg( out, t_str() ), t_arg( out, t_file() )] ),
  E = lam_frn( f, [], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambiguous_name, na, out}}, type( E ) ).

foreign_lambda_with_ambiguous_argument_and_return_field_untypable() ->
  TRet = t_rcd( [t_arg( x, t_str() )] ),
  E = lam_frn( f, [t_arg( x, t_str() )], TRet, l_bash(), <<"blub">> ),
  ?assertEqual( {error, {ambiguous_name, na, x}}, type( E ) ).

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
  ?assertEqual( {error, {key_missing, na, x}}, type( EA ) ).

application_with_too_many_arguments_untypable() ->
  ELam = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( ELam, [e_bind( x, str( <<"bla">> ) ),
                   e_bind( y, str( <<"blub">> ) )] ),
  ?assertEqual( {error, {superfluous_key, na, y}}, type( EA ) ).

application_with_argument_name_mismatch_untypable() ->
  EF = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  EA = app( EF, [e_bind( y, str( <<"bla">> ) )] ),
  ?assertEqual( {error, {key_mismatch, na, {x, y}}}, type( EA ) ).

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

record_field_access_typable() ->
  E1 = proj( x, rcd( [e_bind( x, str( <<"blub">> ) )] ) ),
  E2 = proj( x, rcd( [e_bind( x, file( <<"blub">> ) )] ) ),
  ?assertEqual( {ok, t_str()}, type( E1 ) ),
  ?assertEqual( {ok, t_file()}, type( E2 ) ).

record_field_access_with_invalid_record_expression_untypable() ->
  E = proj( x, var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

record_field_access_with_nonrecord_record_expression_untypable() ->
  E = proj( x, str( <<"blub">> ) ),
  ?assertEqual( {error, {no_record_type, na, t_str()}}, type( E ) ).

record_field_access_with_variable_record_expression_typable() ->
  EProj = proj( x, rcd( [e_bind( x, var( y ) )] ) ),
  ELam = lam_ntv( [lam_ntv_arg( y, t_str() )], EProj ),
  T = t_fn( ntv, [t_arg( y, t_str() )], t_str() ),
  ?assertEqual( {ok, T}, type( ELam ) ).

record_field_access_with_nonexisting_field_name_untypable() ->
  E = proj( y, rcd( [e_bind( x, str( <<"blub">> ) )] ) ),
  ?assertEqual( {error, {key_missing, na, y}}, type( E ) ).

fixpoint_typable() ->
  E1 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                     str( <<"blub">> ) ) ),
  E2 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_file() ) )],
                     file( <<"blub">> ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_file() )}, type( E2 ) ).
  

fixpoint_with_argument_typable() ->
  E = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_file() ) ),
                      lam_ntv_arg( x, t_str() )],
                    var( x ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [t_arg( x, t_str() )], t_str() )}, type( E ) ).

fixpoint_with_invalid_function_expression_untypable() ->
  ?assertEqual( {error, {unbound_var, na, f}}, type( fix( var( f ) ) ) ).

fixpoint_with_nonfunction_function_expression_untypable() ->
  E = fix( str( <<"blub">> ) ),
  ?assertEqual( {error, {no_native_function_type, na, t_str()}}, type( E ) ).

fixpoint_with_foreign_function_expression_untypable() ->
  TRet = t_rcd( [t_arg( out, t_str() )] ),
  ELam = lam_frn( f, [t_arg( f, t_str() )], TRet, l_bash(), <<"blub">> ),
  TLam = t_fn( frn, [t_arg( f, t_str() )], TRet ),
  E = fix( ELam ),
  ?assertEqual( {error, {no_native_function_type, na, TLam}}, type( E ) ).

fixpoint_with_variable_function_expression_typable() ->
  EFix = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                       var( x ) ) ),
  ELam = lam_ntv( [lam_ntv_arg( x, t_str() )], EFix ),
  TLam = t_fn( ntv, [t_arg( x, t_str() )], t_fn( ntv, [], t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fixpoint_with_constant_function_expression_untypable() ->
  E = fix( lam_ntv( [], str( <<"blub">> ) ) ),
  T = t_fn( ntv, [], t_str() ),
  ?assertEqual( {error, {no_argument, na, T}}, type( E ) ).

recursive_fixpoint_typable() ->
  E = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                    app( var( f ), [] ) ) ),
  ?assertEqual( {ok, t_fn( ntv, [], t_str() )}, type( E ) ).
  
list_typable() ->
  E1 = lst( t_str(), [] ),
  E2 = lst( t_file(), [file( <<"bla">> )] ),
  E3 = lst( t_bool(), [true(), false()] ),
  ?assertEqual( {ok, t_lst( t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_lst( t_file() )}, type( E2 ) ),
  ?assertEqual( {ok, t_lst( t_bool() )}, type( E3 ) ).

list_with_invalid_element_untypable() ->
  E1 = lst( t_str(), [var( x )] ),
  E2 = lst( t_str(), [str( <<"bla">> ), var( x )] ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E1 ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E2 ) ).

list_with_nonmatching_element_untypable() ->
  E = lst( t_str(), [file( <<"blub.txt">> )] ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}}, type( E ) ).

list_with_variable_element_typable() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], lst( t_str(), [var( x )] ) ),
  T = t_fn( ntv, [t_arg( x, t_str() )], t_lst( t_str() ) ),
  ?assertEqual( {ok, T}, type( E ) ).

list_append_typable() ->
  L1 = lst( t_str(), [str( <<"bla">> )] ),
  L2 = lst( t_str(), [str( <<"blub">> )] ),
  L3 = lst( t_file(), [file( <<"bla.txt">> )] ),
  L4 = lst( t_file(), [file( <<"blub.txt">> )] ),
  E1 = append( L1, L2 ),
  E2 = append( L3, L4 ),
  ?assertEqual( {ok, t_lst( t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_lst( t_file() )}, type( E2 ) ).

list_append_with_invalid_lhs_untypable() ->
  E = append( var( l ), lst( t_str(), [] ) ),
  ?assertEqual( {error, {unbound_var, na, l}}, type( E ) ).

list_append_with_invalid_rhs_untypable() ->
  E = append( lst( t_str(), [] ), var( l ) ),
  ?assertEqual( {error, {unbound_var, na, l}}, type( E ) ).

list_append_with_nonlist_lhs_untypable() ->
  E = append( str( <<"blub">> ), lst( t_str(), [] ) ),
  ?assertEqual( {error, {no_list_type, na, t_str()}}, type( E ) ).

list_append_with_nonlist_rhs_untypable() ->
  E = append( lst( t_str(), [] ), str( <<"blub">> ) ),
  ?assertEqual( {error, {no_list_type, na, t_str()}}, type( E ) ).

list_append_with_variable_lhs_typable() ->
  EA = append( var( l ),
               lst( t_str(), [str( <<"blub">> )] ) ),
  ELam = lam_ntv( [lam_ntv_arg( l, t_lst( t_str() ) )], EA ),
  TLam = t_fn( ntv, [t_arg( l, t_lst( t_str() ) )], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

list_append_with_variable_rhs_typable() ->
  EA = append( lst( t_str(), [str( <<"blub">> )] ),
               var( l ) ),
  ELam = lam_ntv( [lam_ntv_arg( l, t_lst( t_str() ) )], EA ),
  TLam = t_fn( ntv, [t_arg( l, t_lst( t_str() ) )], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

list_append_with_nonmatching_operands_untypable() ->
  L1 = lst( t_str(), [] ),
  L2 = lst( t_file(), [] ),
  E = append( L1, L2 ),
  ?assertEqual( {error, {type_mismatch,
                         na,
                         {t_lst( t_str() ), t_lst( t_file() )}}},
                type( E ) ).

isnil_typable() ->
  E = isnil( lst( t_str(), [] ) ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).

isnil_with_invalid_list_expression_untypable() ->
  E = isnil( var( l ) ),
  ?assertEqual( {error, {unbound_var, na, l}}, type( E ) ).

isnil_with_nonlist_list_expression_untypable() ->
  E = isnil( str( <<"blub">> ) ),
  ?assertEqual( {error, {no_list_type, na, t_str()}}, type( E ) ).

isnil_with_variable_list_expression_typable() ->
  E = lam_ntv( [lam_ntv_arg( l, t_lst( t_str() ) )], isnil( var( l ) ) ),
  T = t_fn( ntv, [t_arg( l, t_lst( t_str() ) )], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

for_typable() ->
  E1 = for( t_str(),
            [e_bind( x, lst( t_str(), [str( <<"bla">> ), str( <<"blub">> )] ) )],
           var( x ) ),
  T1 = t_lst( t_str() ),
  E2 = for( t_rcd( [t_arg( a, t_str() ), t_arg( b, t_file() )]),
            [e_bind( x, lst( t_str(), [str( <<"bla">> ), str( <<"blub">> )] ) ),
             e_bind( y, lst( t_file(), [file( <<"bla.txt">> ),
                                        file( <<"blub.txt">> )] ) )],
           rcd( [e_bind( a, var( x ) ), e_bind( b, var( y ) )] ) ),
  T2 = t_lst( t_rcd( [t_arg( a, t_str() ), t_arg( b, t_file() )] ) ),
  ?assertEqual( {ok, T1}, type( E1 ) ),
  ?assertEqual( {ok, T2}, type( E2 ) ).

for_with_invalid_list_expression_untypable() ->
  E = for( t_str(), [e_bind( x, var( y ) )], var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

for_with_nonlist_list_expression_untypable() ->
  E = for( t_str(), [e_bind( x, str( <<"blub">> ) )], var( x ) ),
  ?assertEqual( {error, {no_list_type, na, t_str()}}, type( E ) ).

for_with_variable_list_expression_typable() ->
  EFor = for( t_str(), [e_bind( x, var( l ) )], var( x ) ),
  ELam = lam_ntv( [lam_ntv_arg( l, t_lst( t_str() ) )], EFor ),
  TLam = t_fn( ntv, [t_arg( l, t_lst( t_str() ) )], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

for_with_invalid_body_expression_untypable() ->
  E = for( t_str(), [e_bind( x, lst( t_str(), [str( <<"bla">> )] ) )], var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

for_with_nonmatching_body_expression_untypable() ->
  E = for( t_str(), [e_bind( x, lst( t_str(), [str( <<"bla">> )] ) )], file( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_str(), t_file()}}}, type( E ) ).

for_variable_body_expression_typable() ->
  EFor = for( t_rcd( [t_arg( a, t_str() ), t_arg( b, t_file() )] ),
              [e_bind( x, lst( t_str(), [str( <<"bla">> )] ) )],
              rcd( [e_bind( a, var( x ) ), e_bind( b, var( y ) )] ) ),
  ELam = lam_ntv( [lam_ntv_arg( y, t_file() )], EFor ),
  TLam = t_fn( ntv,
              [t_arg( y, t_file() )],
               t_lst( t_rcd( [t_arg( a, t_str() ), t_arg( b, t_file() )] ) ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_typable() ->
  E1 = fold( e_bind( x_acc, str( <<"0">> ) ),
             e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
             var( x ) ),
  E2 = fold( e_bind( x_acc, str( <<"0">> ) ),
             e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
             var( x_acc ) ),
  ?assertEqual( {ok, t_str()}, type( E1 ) ),
  ?assertEqual( {ok, t_str()}, type( E2 ) ).

fold_with_invalid_accumulator_expression_untypable() ->
  E = fold( e_bind( x_acc, var( y ) ),
            e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
            var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_variable_accumulator_expression_typable() ->
  EFold = fold( e_bind( x_acc, var( y ) ),
                e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
                var( x ) ),
  ELam = lam_ntv( [lam_ntv_arg( y, t_str() )], EFold ),
  TLam = t_fn( ntv, [t_arg( y, t_str() )], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_invalid_list_expression_untypable() ->
  E = fold( e_bind( x_acc, str( <<"bla">> ) ),
            e_bind( x, var( y ) ),
            var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_nonlist_list_expression_untypable() ->
  E = fold( e_bind( x_acc, str( <<"bla">> ) ),
            e_bind( x, str( <<"blub">> ) ),
            var( x ) ),
  ?assertEqual( {error, {no_list_type, na, t_str()}}, type( E ) ).

fold_with_variable_list_expression_typable() ->
  EFold = fold( e_bind( x_acc, str( <<"bla">> ) ),
                e_bind( x, var( l ) ),
                var( x ) ),
  ELam = lam_ntv( [lam_ntv_arg( l, t_lst( t_str() ) )], EFold ),
  TLam = t_fn( ntv, [t_arg( l, t_lst( t_str() ) )], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_invalid_body_expression_untypable() ->
  E = fold( e_bind( x_acc, str( <<"0">> ) ),
            e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
            var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_variable_body_expression_typable() ->
  EFold = fold( e_bind( x_acc, str( <<"bla">> ) ),
                e_bind( x, lst( t_str(), [str( <<"blub">> )] ) ),
                var( y ) ),
  ELam = lam_ntv( [lam_ntv_arg( y, t_str() )], EFold ),
  TLam = t_fn( ntv, [t_arg( y, t_str() )], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_nonmatching_accumulator_and_body_expression_untypable() ->
  E = fold( e_bind( x_acc, file( <<"0">> ) ),
            e_bind( x, lst( t_file(), [file( <<"1">> ), file( <<"2">> )] ) ),
            str( <<"blub">> ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_file(),
                                             t_str()}}}, type( E ) ).

fold_with_nonmatching_accumulator_and_list_expression_untypable() ->
  E = fold( e_bind( x_acc, file( <<"0">> ) ),
            e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
            var( x ) ),
  ?assertEqual( {error, {type_mismatch, na, {t_lst( t_file() ),
                                             t_lst( t_str() )}}}, type( E ) ).

fold_with_ambiguous_accumulator_and_list_expression_name_untypable() ->
  E = fold( e_bind( x, str( <<"0">> ) ),
            e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
            var( x ) ),
  ?assertEqual( {error, {ambiguous_name, na, x}}, type( E ) ).


future_is_typable() ->
  E = {fut, na, t_str(), na},
  ?assertEqual( {ok, t_str()}, type( E ) ).


error_is_typable() ->
  ?assertEqual( {ok, t_str()}, type( err( t_str(), <<"blub">> ) ) ).