%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2013-2021 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.7
%% @copyright 2013-2020
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_type_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_type, [type/1, is_type_equivalent/2] ).

-import( cuneiform_lang, [str/1, t_str/0, file/1, t_file/0, true/0, false/0,
                          cons/2, hd/2, tl/2,
                          t_bool/0, cmp/2, var/1, lam/2,
                          t_fn/2, neg/1, cnd/3, conj/2, disj/2,
                          t_rcd/1, l_bash/0, l_awk/0, rcd/1, app/2,
                          proj/2, fix/1, lst/2, t_lst/1, append/2, isnil/1,
                          for/3, fold/3, err/2, null/1, fut/2] ).

type_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string literal typable",
     fun string_literal_typable/0},
    {"file literal typable",
     fun file_literal_typable/0},
    {"true typable",
     fun true_typable/0},
    {"false typable",
     fun false_typable/0},
    {"string comparison typable",
     fun string_comparison_typable/0},
    {"boolean comparison typable",
     fun boolean_comparison_typable/0},
    {"native lambda typable",
     fun native_lambda_typable/0},
    {"native lambda with invalid body expression untypable",
     fun native_lambda_with_invalid_body_expression_untypable/0},
    {"native lambda with ambiguous argument name untypable",
     fun native_lambda_with_ambiguous_argument_name_untypable/0},
    {"native lambda body expression can access closure",
     fun native_lambda_body_expression_can_access_closure/0},
    {"bound variable typable",
     fun bound_variable_typable/0},
    {"unbound variable untypable",
     fun unbound_variable_untypable/0},
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
    {"negation typable",
     fun negation_typable/0},
    {"negation of non-Boolean untypable",
     fun negation_of_nonbool_untypable/0},
    {"negation with invalid expression untypable",
     fun negation_with_invalid_expression_untypable/0},
    {"negation with variable typable",
     fun negation_with_variable_typable/0},
    {"condition typable",
     fun condition_typable/0},
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
    {"conjunction typable",
     fun conjunction_typable/0},
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
    {"disjunction typable",
     fun disjunction_typable/0},
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
    {"record_typable",
     fun record_typable/0},
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
    {"foreign lambda with nonrecord return type untypable",
     fun foreign_lambda_with_nonrecord_return_type_untypable/0},
    {"application typable",
     fun application_typable/0},
    {"app no fn untypable",
     fun app_no_fn_untypable/0},
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
    {"app with multiple dangling arg binds untypable",
     fun app_with_multiple_dangling_arg_binds_untypable/0},
    {"app with multiple missing arg binds untypable",
     fun app_with_multiple_missing_arg_binds_untypable/0},
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
    {"fixpoint typable",
     fun fixpoint_typable/0},
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
    {"recursive fixpoint typable",
     fun recursive_fixpoint_typable/0},
    {"future is typable",
     fun future_is_typable/0},
    {"error is typable",
     fun error_is_typable/0},
    {"fixpoint with wrong first argument type untypable",
     fun fixpoint_with_wrong_first_argument_type_untypable/0},
    {"native function with return type mismatch reports properly",
     fun native_function_with_return_type_mismatch_reports_properly/0},
    {"app rcd field order no effect",
     fun app_rcd_field_order_no_effect/0},
    {"cmp rcd field order no effect",
     fun cmp_rcd_field_order_no_effect/0},
    {"cnd rcd field order no effect",
     fun cnd_rcd_field_order_no_effect/0},
    {"awk foreign lambda typable",
     fun awk_foreign_lambda_typable/0},




%% lists -------------------------------------------------------------

    {"list typable",
     fun list_typable/0},
    {"list with invalid element untypable",
     fun list_with_invalid_element_untypable/0},
    {"list with non-matching element untypable",
     fun list_with_nonmatching_element_untypable/0},
    {"list with variable element typable",
     fun list_with_variable_element_typable/0},
    {"list append typable",
     fun list_append_typable/0},
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
    {"isnil typable",
     fun isnil_typable/0},
    {"isnil with invalid list expression untypable",
     fun isnil_with_invalid_list_expression_untypable/0},
    {"isnil with non-list list expression untypable",
     fun isnil_with_nonlist_list_expression_untypable/0},
    {"isnil with variable list expression typable",
     fun isnil_with_variable_list_expression_typable/0},
    {"cons rcd field order no effect",
     fun cons_rcd_field_order_no_effect/0},
    {"hd rcd field order no effect",
     fun hd_rcd_field_order_no_effect/0},
    {"tl rcd field order no effect",
     fun tl_rcd_field_order_no_effect/0},
    {"append rcd field order no effect",
     fun append_rcd_field_order_no_effect/0},

%% for ---------------------------------------------------------------

    {"for typable",
     fun for_typable/0},
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
    {"for argument mismatch untypable",
     fun for_argument_mismatch_untypable/0},
    {"for with ambiguous name untypable",
     fun for_with_ambiguous_name_untypable/0},
    {"for body rcd field order no effect",
     fun for_body_rcd_field_order_no_effect/0},
    {"for list bind rcd field order no effect",
     fun for_list_bind_rcd_field_order_no_effect/0},
    {"for invalid second list binding untypable",
     fun for_invalid_second_list_binding_untypable/0},


%% fold --------------------------------------------------------------

    {"fold typable",
     fun fold_typable/0},
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
    {"fold with independent accumulator and list types typable",
     fun fold_with_independent_accumulator_and_list_types_typable/0},
    {"fold with nonmatching accumulator type untypable",
     fun fold_with_nonmatching_accumulator_type_untypable/0},
    {"fold with nonmatching list argument type untypable",
     fun fold_with_nonmatching_list_argument_type_untypable/0},
    {"fold acc bind rcd field order no effect",
     fun fold_acc_bind_rcd_field_order_no_effect/0},
    {"fold list bind rcd field order no effect",
     fun fold_list_bind_rcd_field_order_no_effect/0},
    {"fold body rcd field order no effect",
     fun fold_body_rcd_field_order_no_effect/0}

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

boolean_comparison_typable() ->
  E = cmp( true(), false() ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).

native_lambda_typable() ->
  E1 = lam( [], {ntv, str( <<"blub">> )} ),
  E2 = lam( [], {ntv, file( <<"bla.txt">> )} ),
  ?assertEqual( {ok, t_fn( [], t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_fn( [], t_file() )}, type( E2 ) ).

native_lambda_with_invalid_body_expression_untypable() ->
  E1 = lam( [], {ntv, var( x )} ),
  E2 = lam( [{x, t_str()}], {ntv, var( y )} ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E1 ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E2 ) ).

native_lambda_with_ambiguous_argument_name_untypable() ->
  E = lam( [{x, t_str()},
            {x, t_file()}],
           {ntv, var( x )} ),
  ?assertEqual( {error, {ntv_fn_ambiguous_arg_name, na, [x]}}, type( E ) ).

native_lambda_body_expression_can_access_closure() ->
  E = lam( [{x, t_str()}], {ntv, lam( [], {ntv, var( x )} )} ),
  T = t_fn( [{x, t_str()}], t_fn( [], t_str() ) ),
  ?assertEqual( {ok, T}, type( E ) ).

bound_variable_typable() ->
  E = lam( [{x, t_str()}], {ntv, var( x )} ),
  ?assertEqual( {ok, t_fn( [{x, t_str()}], t_str() )}, type( E ) ).

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
  ?assertEqual( {error, {cmp_no_comparable_type, na, {file( <<"bla.txt">> ), t_file()}}},
                type( E ) ).

comparison_with_nonmatching_operands_untypable() ->
  E = cmp( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {cmp_incomparable, na, {true(), t_bool(), str( <<"blub">> ), t_str()}}},
                        type( E ) ).

comparison_with_lhs_variable_typable() ->
  E = lam( [{x, t_str()}],
           {ntv, cmp( var( x ), str( <<"blub">> ) )} ),
  ?assertEqual( {ok, t_fn( [{x, t_str()}], t_bool() )}, type( E ) ).

comparison_with_rhs_variable_typable() ->
  E = lam( [{x, t_str()}],
           {ntv, cmp( str( <<"bla">> ), var( x ) )} ),
  ?assertEqual( {ok, t_fn( [{x, t_str()}], t_bool() )}, type( E ) ).

negation_typable() ->
  ?assertEqual( {ok, t_bool()}, type( neg( true() ) ) ).

negation_of_nonbool_untypable() ->
  E = neg( str( <<"blub">> ) ),
  ?assertEqual( {error, {neg_no_bool, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

negation_with_invalid_expression_untypable() ->
  E = neg( var( x ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

negation_with_variable_typable() ->
  E = lam( [{x, t_bool()}], {ntv, neg( var( x ) )} ),
  T = t_fn( [{x, t_bool()}], t_bool() ),
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
  ?assertEqual( {error, {cnd_test_no_bool, na, {str( <<"true">> ), t_str()}}},
                type( E ) ).

condition_with_invalid_then_expression_untypable() ->
  E = cnd( true(), var( x ), str( <<"blub">> ) ),
  ?assertEqual( {error, {unbound_var, na, x}}, type( E ) ).

condition_with_invalid_else_expression_untypable() ->
  E = cnd( true(), str( <<"bla">> ), var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

condition_with_nonmatching_then_and_else_expression_untypable() ->
  E = cnd( true(), str( <<"bla">> ), file( <<"blub.txt">> ) ),
  ?assertEqual( {error, {cnd_result_type_mismatch, na, {str( <<"bla">> ), t_str(), file( <<"blub.txt">> ), t_file()}}},
                type( E ) ).

condition_with_variable_predicate_typable() ->
  E = lam( [{x, t_bool()}],
           {ntv, cnd( var( x ), str( <<"bla">> ), str( <<"blub">> ) )} ),
  ?assertEqual( {ok, t_fn( [{x, t_bool()}], t_str() )}, type( E ) ).

condition_with_variable_then_expression_typable() ->
  E = lam( [{y, t_str()}],
           {ntv, cnd( true(),var( y ), str( <<"blub">> ) )} ),
  ?assertEqual( {ok, t_fn( [{y, t_str()}], t_str() )}, type( E ) ).

condition_with_variable_else_expression_typable() ->
  E = lam( [{z, t_str()}],
           {ntv, cnd( true(), str( <<"bla">> ), var( z ) )} ),
  ?assertEqual( {ok, t_fn( [{z, t_str()}], t_str() )}, type( E ) ).

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
  ?assertEqual( {error, {conj_lhs_no_bool, na, {str( <<"bla">> ), t_str()}}},
                type( E ) ).

conjunction_with_nonboolean_rhs_untypable() ->
  E = conj( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {conj_rhs_no_bool, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

conjunction_with_variable_lhs_typable() ->
  E = lam( [{x, t_bool()}], {ntv, conj( var( x ), true() )} ),
  T = t_fn( [{x, t_bool()}], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

conjunction_with_variable_rhs_typable() ->
  E = lam( [{y, t_bool()}], {ntv, conj( true(), var( y ) )} ),
  T = t_fn( [{y, t_bool()}], t_bool() ),
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
  ?assertEqual( {error, {disj_lhs_no_bool, na, {str( <<"bla">> ), t_str()}}},
                type( E ) ).

disjunction_with_nonboolean_rhs_untypable() ->
  E = disj( true(), str( <<"blub">> ) ),
  ?assertEqual( {error, {disj_rhs_no_bool, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

disjunction_with_variable_lhs_typable() ->
  E = lam( [{x, t_bool()}], {ntv, disj( var( x ), true() )} ),
  T = t_fn( [{x, t_bool()}], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

disjunction_with_variable_rhs_typable() ->
  E = lam( [{y, t_bool()}], {ntv, disj( true(), var( y ) )} ),
  T = t_fn( [{y, t_bool()}], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

record_typable() ->
  E = rcd( [{x, str( <<"blub">> )}] ),
  ?assertEqual( {ok, t_rcd( [{x, t_str()}] )}, type( E ) ).

record_with_invalid_field_untypable() ->
  E1 = rcd( [{x, var( y )}] ),
  E2 = rcd( [{x, str( <<"blub">> )}, {y, var( z )}] ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E1 ) ),
  ?assertEqual( {error, {unbound_var, na, z}}, type( E2 ) ).

record_with_variable_field_typable() ->
  E1 = lam( [{x, t_str()}], {ntv, rcd( [{y, var( x )}] )} ),
  T1 = t_fn( [{x, t_str()}], t_rcd( [{y, t_str()}] ) ),
  E2 = lam( [{x, t_str()}],
                {ntv, rcd( [{y, file( <<"bla.txt">> )},
                            {z, var( x )}] )} ),
  T2 = t_fn( [{x, t_str()}], t_rcd( [{y, t_file()}, {z, t_str()}] ) ),
  ?assertEqual( {ok, T1}, type( E1 ) ),
  ?assertEqual( {ok, T2}, type( E2 ) ).

record_with_ambiguous_field_name_untypable() ->
  E = rcd( [{x, str( <<"bla">> )},
            {x, file( <<"blub.txt">> )}] ),
  ?assertEqual( {error, {rcd_ambiguous_field_name, na, [x]}}, type( E ) ).

foreign_lambda_typable() ->
  TRet = t_rcd( [{out, t_str()}] ),
  E = lam( [], {frn, f, TRet, l_bash(), <<"blub">>} ),
  T = t_fn( [], TRet ),
  ?assertEqual( {ok, T}, type( E ) ).

foreign_lambda_with_ambiguous_argument_name_untypable() ->
  TRet = t_rcd( [{out, t_str()}] ),
  E = lam( [{x, t_str()}, {x, t_file()}],
           {frn, f, TRet, l_bash(), <<"blub">>} ),
  ?assertEqual( {error, {frn_fn_ambiguous_arg_or_return_field_name, na, [x]}},
                type( E ) ).

foreign_lambda_with_ambiguous_return_field_name_untypable() ->
  TRet = t_rcd( [{out, t_str()}, {out, t_file()}] ),
  E = lam( [], {frn, f, TRet, l_bash(), <<"blub">>} ),
  ?assertEqual( {error, {frn_fn_ambiguous_arg_or_return_field_name, na, [out]}},
                type( E ) ).

foreign_lambda_with_ambiguous_argument_and_return_field_untypable() ->
  TRet = t_rcd( [{x, t_str()}] ),
  E = lam( [{x, t_str()}], {frn, f, TRet, l_bash(), <<"blub">>} ),
  ?assertEqual( {error, {frn_fn_ambiguous_arg_or_return_field_name, na, [x]}},
                type( E ) ).

application_typable() ->
  EF = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( EF, [{x, str( <<"bla">> )}] ),
  ?assertEqual( {ok, t_str()}, type( EA ) ).

application_with_invalid_function_expression_untypable() ->
  E = app( var( f ), [{x, str( <<"bla">> )}] ),
  ?assertEqual( {error, {unbound_var, na, f}}, type( E ) ).

application_with_variable_function_expression_typable() ->
  EA = app( var( f ), [] ),
  ELam = lam( [{f, t_fn( [], t_str() )}], {ntv, EA} ),
  TLam = t_fn( [{f, t_fn( [], t_str() )}], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

application_with_too_few_arguments_untypable() ->
  ELam = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( ELam, [] ),
  ?assertEqual( {error, {app_missing_bind, na, [{x, t_str()}]}},
                type( EA ) ).

application_with_too_many_arguments_untypable() ->
  ELam = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( ELam, [{x, str( <<"bla">> )},
                   {y, str( <<"blub">> )}] ),
  ?assertEqual( {error, {app_dangling_bind, na, [{y, str( <<"blub">> )}]}},
                type( EA ) ).

application_with_argument_name_mismatch_untypable() ->
  EF = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( EF, [{y, str( <<"bla">> )}] ),
  ?assertEqual( {error, {app_arg_name_mismatch, na, {x, y}}},
                type( EA ) ).

application_with_invalid_argument_untypable() ->
  EF = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( EF, [{x, var( y )}] ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( EA ) ).

application_with_argument_type_mismatch_untypable() ->
  EF = lam( [{x, t_str()}], {ntv, var( x )} ),
  EA = app( EF, [{x, file( <<"bla">> )}] ),
  ?assertEqual( {error, {app_bind_type_mismatch, na, {x, t_str(), file( <<"bla">> ), t_file()}}},
                type( EA ) ).

application_with_variable_argument_typable() ->
  EF = lam( [{y, t_str()}], {ntv, var( y )} ),
  EA = app( EF, [{y, var( x )}] ),
  ELam = lam( [{x, t_str()}], {ntv, EA} ),
  TLam = t_fn( [{x, t_str()}], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

record_field_access_typable() ->
  E1 = proj( x, rcd( [{x, str( <<"blub">> )}] ) ),
  E2 = proj( x, rcd( [{x, file( <<"blub">> )}] ) ),
  ?assertEqual( {ok, t_str()}, type( E1 ) ),
  ?assertEqual( {ok, t_file()}, type( E2 ) ).

record_field_access_with_invalid_record_expression_untypable() ->
  E = proj( x, var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

record_field_access_with_nonrecord_record_expression_untypable() ->
  E = proj( x, str( <<"blub">> ) ),
  ?assertEqual( {error, {proj_no_record, na, {str( <<"blub">> ), t_str()}}}, type( E ) ).

record_field_access_with_variable_record_expression_typable() ->
  EProj = proj( x, rcd( [{x, var( y )}] ) ),
  ELam = lam( [{y, t_str()}], {ntv, EProj} ),
  T = t_fn( [{y, t_str()}], t_str() ),
  ?assertEqual( {ok, T}, type( ELam ) ).

record_field_access_with_nonexisting_field_name_untypable() ->
  E = proj( y, rcd( [{x, str( <<"blub">> )}] ) ),
  ?assertEqual( {error, {proj_field_missing, na, y}}, type( E ) ).

fixpoint_typable() ->
  E1 = fix( lam( [{f, t_fn( [], t_str() )}],
                 {ntv, str( <<"blub">> )} ) ),
  E2 = fix( lam( [{f, t_fn( [], t_file() )}],
                 {ntv, file( <<"blub">> )} ) ),
  ?assertEqual( {ok, t_fn( [], t_str() )}, type( E1 ) ),
  ?assertEqual( {ok, t_fn( [], t_file() )}, type( E2 ) ).
  

fixpoint_with_argument_typable() ->
  E = fix( lam( [{f, t_fn( [{x, t_str()}], t_str() )},
                 {x, t_str()}],
                {ntv, var( x )} ) ),
  ?assertEqual( {ok, t_fn( [{x, t_str()}], t_str() )}, type( E ) ).

fixpoint_with_invalid_function_expression_untypable() ->
  ?assertEqual( {error, {unbound_var, na, f}}, type( fix( var( f ) ) ) ).

fixpoint_with_nonfunction_function_expression_untypable() ->
  E = fix( str( <<"blub">> ) ),
  ?assertEqual( {error, {fix_no_fn, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

fixpoint_with_foreign_function_expression_untypable() ->
  TRet = t_rcd( [{out, t_str()}] ),
  ELam = lam( [{f, t_str()}], {frn, f, TRet, l_bash(), <<"blub">>} ),
  E = fix( ELam ),
  ?assertEqual( {error, {fix_fn_arg_no_fn, na, {f, t_str()}}},
                type( E ) ).

fixpoint_with_variable_function_expression_typable() ->
  EFix = fix( lam( [{f, t_fn( [], t_str() )}],
                   {ntv, var( x )} ) ),
  ELam = lam( [{x, t_str()}], {ntv, EFix} ),
  TLam = t_fn( [{x, t_str()}], t_fn( [], t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fixpoint_with_constant_function_expression_untypable() ->
  ELam = lam( [], {ntv, str( <<"blub">> )} ),
  TLam = t_fn( [], t_str() ),
  E = fix( ELam ),
  ?assertEqual( {error, {fix_fn_no_arg, na, {ELam, TLam}}}, type( E ) ).

recursive_fixpoint_typable() ->
  E = fix( lam( [{f, t_fn( [], t_str() )}],
                {ntv, app( var( f ), [] )} ) ),
  ?assertEqual( {ok, t_fn( [], t_str() )}, type( E ) ).
  
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
  ?assertEqual( {error, {cons_element_type_mismatch, na, {t_str(), file( <<"blub.txt">> ), t_file()}}},
                type( E ) ).

list_with_variable_element_typable() ->
  E = lam( [{x, t_str()}], {ntv, lst( t_str(), [var( x )] )} ),
  T = t_fn( [{x, t_str()}], t_lst( t_str() ) ),
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
  ?assertEqual( {error, {append_lhs_no_list, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

list_append_with_nonlist_rhs_untypable() ->
  E = append( lst( t_str(), [] ), str( <<"blub">> ) ),
  ?assertEqual( {error, {append_rhs_no_list, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

list_append_with_variable_lhs_typable() ->
  EA = append( var( l ),
               lst( t_str(), [str( <<"blub">> )] ) ),
  ELam = lam( [{l, t_lst( t_str() )}], {ntv, EA} ),
  TLam = t_fn( [{l, t_lst( t_str() )}], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

list_append_with_variable_rhs_typable() ->
  EA = append( lst( t_str(), [str( <<"blub">> )] ),
               var( l ) ),
  ELam = lam( [{l, t_lst( t_str() )}], {ntv, EA} ),
  TLam = t_fn( [{l, t_lst( t_str() )}], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

list_append_with_nonmatching_operands_untypable() ->
  L1 = lst( t_str(), [] ),
  L2 = lst( t_file(), [] ),
  E = append( L1, L2 ),
  ?assertEqual( {error, {append_element_type_mismatch,
                         na,
                         {t_str(), t_file()}}},
                type( E ) ).

isnil_typable() ->
  E = isnil( lst( t_str(), [] ) ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).

isnil_with_invalid_list_expression_untypable() ->
  E = isnil( var( l ) ),
  ?assertEqual( {error, {unbound_var, na, l}}, type( E ) ).

isnil_with_nonlist_list_expression_untypable() ->
  E = isnil( str( <<"blub">> ) ),
  ?assertEqual( {error, {isnil_no_list, na, {str( <<"blub">> ), t_str()}}},
                type( E ) ).

isnil_with_variable_list_expression_typable() ->
  E = lam( [{l, t_lst( t_str() )}], {ntv, isnil( var( l ) )} ),
  T = t_fn( [{l, t_lst( t_str() )}], t_bool() ),
  ?assertEqual( {ok, T}, type( E ) ).

for_typable() ->
  E1 = for( t_str(),
            [{x, t_str(), lst( t_str(), [str( <<"bla">> ), str( <<"blub">> )] )}],
            var( x ) ),
  T1 = t_lst( t_str() ),
  E2 = for( t_rcd( [{a, t_str()}, {b, t_file()}]),
            [{x, t_str(), lst( t_str(), [str( <<"bla">> ), str( <<"blub">> )] )},
             {y, t_file(), lst( t_file(), [file( <<"bla.txt">> ),
                                           file( <<"blub.txt">> )] )}],
           rcd( [{a, var( x )}, {b, var( y )}] ) ),
  T2 = t_lst( t_rcd( [{a, t_str()}, {b, t_file()}] ) ),
  ?assertEqual( {ok, T1}, type( E1 ) ),
  ?assertEqual( {ok, T2}, type( E2 ) ).

for_with_invalid_list_expression_untypable() ->
  E = for( t_str(), [{x, t_str(), var( y )}], var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

for_with_nonlist_list_expression_untypable() ->
  E = for( t_str(), [{x, t_str(), str( <<"blub">> )}], var( x ) ),
  ?assertEqual( {error, {for_bind_no_list, na, {t_lst( t_str() ), str( <<"blub">> ), t_str()}}},
                type( E ) ).

for_with_variable_list_expression_typable() ->
  EFor = for( t_str(), [{x, t_str(), var( l )}], var( x ) ),
  ELam = lam( [{l, t_lst( t_str() )}], {ntv, EFor} ),
  TLam = t_fn( [{l, t_lst( t_str() )}], t_lst( t_str() ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

for_with_invalid_body_expression_untypable() ->
  E = for( t_str(), [{x, t_str(), lst( t_str(), [str( <<"bla">> )] )}], var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

for_with_nonmatching_body_expression_untypable() ->
  E = for( t_str(), [{x, t_str(), lst( t_str(), [str( <<"bla">> )] )}], file( <<"blub">> ) ),
  ?assertEqual( {error, {for_body_type_mismatch, na, {t_str(), file( <<"blub">> ), t_file()}}},
                type( E ) ).

for_variable_body_expression_typable() ->
  EFor = for( t_rcd( [{a, t_str()}, {b, t_file()}] ),
              [{x, t_str(), lst( t_str(), [str( <<"bla">> )] )}],
              rcd( [{a, var( x )}, {b, var( y )}] ) ),
  ELam = lam( [{y, t_file()}], {ntv, EFor} ),
  TLam = t_fn( [{y, t_file()}],
               t_lst( t_rcd( [{a, t_str()}, {b, t_file()}] ) ) ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

for_argument_mismatch_untypable() ->
  X = x,
  T = t_str(),
  E = lst( t_file(), [file( <<"bla">> )] ),
  EFor = for( t_str(),
              [{X, T, E}],
              str( <<"bla">> ) ),
  ?assertEqual( {error, {for_bind_type_mismatch, na, {X, T, E, t_file()}}},
                        type( EFor ) ).

fold_typable() ->
  E1 = fold( {x_acc, t_str(), str( <<"0">> )},
             {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
             var( x ) ),
  E2 = fold( {x_acc, t_str(), str( <<"0">> )},
             {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
             var( x_acc ) ),
  ?assertEqual( {ok, t_str()}, type( E1 ) ),
  ?assertEqual( {ok, t_str()}, type( E2 ) ).

fold_with_invalid_accumulator_expression_untypable() ->
  E = fold( {x_acc, t_str(), var( y )},
            {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
            var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_variable_accumulator_expression_typable() ->
  EFold = fold( {x_acc, t_str(), var( y )},
                {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
                var( x ) ),
  ELam = lam( [{y, t_str()}], {ntv, EFold} ),
  TLam = t_fn( [{y, t_str()}], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_invalid_list_expression_untypable() ->
  E = fold( {x_acc, t_str(), str( <<"bla">> )},
            {x, t_str(), var( y )},
            var( x ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_nonlist_list_expression_untypable() ->
  X = x,
  T = t_str(),
  E = str( <<"blub">> ),
  E1 = fold( {x_acc, t_str(), str( <<"bla">> )},
             {X, T, E},
             var( x ) ),
  ?assertEqual( {error, {fold_list_bind_no_list, na, {t_lst( t_str() ), E, t_str()}}},
                type( E1 ) ).

fold_with_variable_list_expression_typable() ->
  EFold = fold( {x_acc, t_str(), str( <<"bla">> )},
                {x, t_str(), var( l )},
                var( x ) ),
  ELam = lam( [{l, t_lst( t_str() )}], {ntv, EFold} ),
  TLam = t_fn( [{l, t_lst( t_str() )}], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_invalid_body_expression_untypable() ->
  E = fold( {x_acc, t_str(), str( <<"0">> )},
            {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
            var( y ) ),
  ?assertEqual( {error, {unbound_var, na, y}}, type( E ) ).

fold_with_variable_body_expression_typable() ->
  EFold = fold( {x_acc, t_str(), str( <<"bla">> )},
                {x, t_str(), lst( t_str(), [str( <<"blub">> )] )},
                var( y ) ),
  ELam = lam( [{y, t_str()}], {ntv, EFold} ),
  TLam = t_fn( [{y, t_str()}], t_str() ),
  ?assertEqual( {ok, TLam}, type( ELam ) ).

fold_with_nonmatching_accumulator_and_body_expression_untypable() ->
  E = fold( {x_acc, t_file(), file( <<"0">> )},
            {x, t_file(), lst( t_file(), [file( <<"1">> ), file( <<"2">> )] )},
            str( <<"blub">> ) ),
  ?assertEqual( {error, {fold_body_type_mismatch, na, {t_file(), str( <<"blub">> ), t_str()}}},
                type( E ) ).

fold_with_nonmatching_accumulator_and_list_expression_untypable() ->
  EBody = var( x ),
  TAcc = t_file(),
  E = fold( {x_acc, TAcc, file( <<"0">> )},
            {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
            EBody ),
  ?assertEqual( {error, {fold_body_type_mismatch, na, {TAcc, EBody, t_str()}}},
                type( E ) ).

fold_with_ambiguous_accumulator_and_list_expression_name_untypable() ->
  E = fold( {x, t_str(), str( <<"0">> )},
            {x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] )},
            var( x ) ),
  ?assertEqual( {error, {fold_ambiguous_bind_name, na, x}}, type( E ) ).

fold_with_independent_accumulator_and_list_types_typable() ->
  E = fold( {acc, t_str(), str( <<"bla">> )},
            {x, t_bool(), null( t_bool() )},
            var( acc ) ),
  ?assertEqual( {ok, t_str()}, type( E ) ).

fold_with_nonmatching_accumulator_type_untypable() ->
  X = acc,
  T = t_str(),
  E = file( <<"bla">> ),
  E1 = fold( {X, T, E},
             {x, t_str(), null( t_str() )},
             str( <<"bla">> ) ),
  ?assertEqual( {error, {fold_acc_bind_type_mismatch, na, {X, T, E, t_file()}}},
                type( E1 ) ).

fold_with_nonmatching_list_argument_type_untypable() ->
  X = x,
  T = t_str(),
  E = lst( t_bool(), [true(), false()] ),
  E1 = fold( {acc, t_str(), str( <<"blub">> )},
             {X, T, E},
             str( <<"bla">> ) ),
  ?assertEqual( {error, {fold_list_bind_type_mismatch, na, {X, T, E, t_bool()}}},
                type( E1 ) ).

future_is_typable() ->
  E = fut( t_str(), <<"1234">> ),
  ?assertEqual( {ok, t_str()}, type( E ) ).

error_is_typable() ->
  ?assertEqual( {ok, t_str()}, type( err( t_str(), {user, <<"blub">>} ) ) ).

for_with_ambiguous_name_untypable() ->
  E = for( t_str(), [{x, t_str(), null( t_str() )},
                     {x, t_str(), null( t_str() )}],
           var( x ) ),
  ?assertEqual( {error, {for_ambiguous_bind_name, na, [x]}}, type( E ) ).

fixpoint_with_wrong_first_argument_type_untypable() ->
  E = fix( lam( [{f, t_str()}], {ntv, str( <<"5">> )} ) ),
  ?assertEqual( {error, {fix_fn_arg_no_fn, na, {f, t_str()}}},
                type( E ) ).

native_function_with_return_type_mismatch_reports_properly() ->
  E = fix( lam( [{f, t_fn( [], t_bool() )}], {ntv, str( <<"5">> )} ) ),
  ?assertEqual( {error, {fix_return_type_mismatch, na, {t_bool(), t_str()}}}, type( E ) ).

app_rcd_field_order_no_effect() ->
  T = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E = app( lam( [{x, T}],
                {ntv, var( x )} ),
           [{x, rcd( [{b, str( <<"bla">> )},
                            {a, true()}] )}] ),
  ?assertEqual( {ok, T}, type( E ) ).

cmp_rcd_field_order_no_effect() ->
  E1 = rcd( [{a, true()}, {b, str( <<"bla">> )}] ),
  E2 = rcd( [{b, str( <<"bla">> )}, {a, true()}] ),
  E3 = cmp( E1, E2 ),
  ?assertEqual( {ok, t_bool()}, type( E3 ) ).

cnd_rcd_field_order_no_effect() ->
  E1 = rcd( [{a, true()}, {b, str( <<"bla">> )}] ),
  T1 = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E2 = rcd( [{b, str( <<"bla">> )}, {a, true()}] ),
  E3 = cnd( false(), E1, E2 ),
  ?assertEqual( {ok, T1}, type( E3 ) ).

cons_rcd_field_order_no_effect() ->
  E1 = rcd( [{b, str( <<"bla">> )}, {a, true()}] ),
  T = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E2 = cons( E1, null( T ) ),
  ?assertEqual( {ok, t_lst( T )}, type( E2 ) ).

hd_rcd_field_order_no_effect() ->
  T = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E1 = null( T ),
  E2 = rcd( [{b, str( <<"bla">> )}, {a, true()}] ),
  ?assertEqual( {ok, T}, type( hd( E1, E2 ) ) ).

tl_rcd_field_order_no_effect() ->
  T = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E1 = null( T ),
  E2 = rcd( [{b, str( <<"bla">> )}, {a, true()}] ),
  ?assertEqual( {ok, T}, type( tl( E1, E2 ) ) ).

append_rcd_field_order_no_effect() ->
  T1 = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  T2 = t_rcd( [{b, t_str()}, {a, t_bool()}] ),
  E1 = null( T1 ),
  E2 = null( T2 ),
  ?assertEqual( {ok, t_lst( T1 )}, type( append( E1, E2 ) ) ).

for_list_bind_rcd_field_order_no_effect() ->
  T1 = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  T2 = t_rcd( [{b, t_str()}, {a, t_bool()}] ),
  E = for( t_bool(),
           [{x, T1, null( T2 )}],
           proj( a, var( x ) ) ),
  ?assertEqual( {ok, t_lst( t_bool() )}, type( E ) ).

for_body_rcd_field_order_no_effect() ->
  T = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  E = for( T, [{x, t_bool(), null( t_bool() )}],
           rcd( [{b, str( <<"blub">> )}, {a, var( x )}]) ),
  ?assertEqual( {ok, t_lst( T )}, type( E ) ).

for_invalid_second_list_binding_untypable() ->
  E = for( t_bool(), [{x, t_bool(), null( t_bool() )},
                      {y, t_bool(), null( t_str() )}], true() ),
  ?assertEqual( {error, {for_bind_type_mismatch, na, {y, t_bool(), null( t_str() ), t_str()}}},
                type( E ) ).

fold_acc_bind_rcd_field_order_no_effect() ->
  TAcc = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  EAcc = rcd( [{b, str( <<"blub">> )}, {a, true()}] ),
  E = fold( {acc, TAcc, EAcc},
             {x, t_bool(), null( t_bool() )},
             rcd( [{a, conj( proj( a, var( acc ) ), var( x ) )},
                   {b, proj( b, var( acc ) )}] ) ),
  ?assertEqual( {ok, TAcc}, type( E ) ).

fold_list_bind_rcd_field_order_no_effect() ->
  TLst = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  ELst = null( t_rcd( [{b, t_str()}, {a, t_bool()}] ) ),
  E = fold( {acc, t_bool(), false()},
            {x, TLst, ELst},
            conj( var( acc ), proj( a, var( x ) ) ) ),
  ?assertEqual( {ok, t_bool()}, type( E ) ).

fold_body_rcd_field_order_no_effect() ->
  TAcc = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  EAcc = rcd( [{a, true()}, {b, str( <<"blub">> )}] ),
  E = fold( {acc, TAcc, EAcc},
             {x, t_bool(), null( t_bool() )},
             rcd( [{b, proj( b, var( acc ) )},
                   {a, conj( proj( a, var( acc ) ), var( x ) )}] ) ),
  ?assertEqual( {ok, TAcc}, type( E ) ).

app_no_fn_untypable() ->
  E = {app,na,{str,na,<<>>},[]},
  ?assertEqual( {error, {app_lhs_no_function, na, {{str,na,<<>>}, t_str()}}},
                type( E ) ).

foreign_lambda_with_nonrecord_return_type_untypable() ->
  E = lam( [], {frn, f, t_str(), l_bash(), <<"blub">>} ),
  ?assertEqual( {error, {frn_fn_returns_no_rcd, na, t_str()}}, type( E ) ).

app_with_multiple_dangling_arg_binds_untypable() ->
  E = {app,na,{lam,na, [],
                       {ntv,{str,na,<<5,0>>}}},
              [{<<>>,{var,na,<<>>}},{<<>>,{var,na,<<>>}}]},
  ?assertEqual( {error, {app_dangling_bind, na, [{<<>>,{var,na,<<>>}},{<<>>,{var,na,<<>>}}]}},
                type( E ) ).

app_with_multiple_missing_arg_binds_untypable() ->
  E = {app,na,{lam,na, [{a,{var,na,<<>>}},{b,{var,na,<<>>}}],
                       {ntv,{str,na,<<5,0>>}}},
              []},
  ?assertEqual( {error, {app_missing_bind, na, [{a,{var,na,<<>>}},{b,{var,na,<<>>}}]}},
                type( E ) ).


awk_foreign_lambda_typable() ->
  ArgLst = [{file, t_file()}],
  TRet = t_rcd( [{result, t_file()}] ),
  Body = {frn, f, TRet, l_awk(), <<"blub">>},
  E = lam( ArgLst, Body ),
  T = t_fn( ArgLst, TRet ),
  ?assertEqual( {ok, T}, type( E ) ).

is_type_equivalent_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"rcd type with changed field order is equivalent",
     fun rcd_type_with_changed_field_order_is_equivalent/0}
   ]
  }.

rcd_type_with_changed_field_order_is_equivalent() ->
  T1 = t_rcd( [{a, t_bool()}, {b, t_str()}] ),
  T2 = t_rcd( [{b, t_str()}, {a, t_bool()}] ),
  ?assert( is_type_equivalent( T1, T2 ) ).


