%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @copyright 2015-2019
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_sem_x_test ).

%%====================================================================
%% Definitions
%%====================================================================

-define( CUNEIFORM_SEM, cuneiform_sem_cek ).


%%====================================================================
%% Includes
%%====================================================================

-include_lib( "eunit/include/eunit.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( ?CUNEIFORM_SEM, [step/1, split_zip/1, bind_all/3] ).

-import( cuneiform_lang, [t_str/0, t_bool/0, t_fn/3, t_rcd/1, t_lst/1] ).
-import( cuneiform_lang, [e_bind/2, t_arg/2] ).
-import( cuneiform_lang, [l_bash/0] ).
-import( cuneiform_lang, [
                          str/1, true/0, false/0, cnd/3, var/1, lam_frn/5,
                          lam_ntv/2, app/2, cmp/2, neg/1, conj/2, disj/2,
                          lst/2, append/2, isnil/1, for/3, fold/3, err/2,
                          rcd/1, proj/2, fix/1, null/1, cons/2, file/1,
                          typed_bind/3
                         ] ).

-import( cf_client_effi, [app_to_effi_request/1] ).

 
%%====================================================================
%% Expression definitions
%%====================================================================

e_lam_const() ->
  lam_ntv( [], str( <<"blub">> ) ).

e_lam_id() ->
  lam_ntv( [t_arg( x, t_str() )], var( x ) ).

e_app_id() ->
  app( e_lam_id(), [e_bind( x, str( <<"blub">> ) )] ).


e_app_frn_str() ->
  TRet = t_rcd( [t_arg( a, t_str() )] ),
  Lam = lam_frn( f, [], TRet, l_bash(), <<"shalala">> ),
  app( Lam, [] ).

e_app_frn_bool() ->
  TRet = t_rcd( [t_arg( a, t_bool() )] ),
  Lam = lam_frn( f, [], TRet, l_bash(), <<"shalala">> ),
  app( Lam, [] ).


%%====================================================================
%% Notion of reduction
%%====================================================================

step_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"comparison of equal strings reduces to true",
     fun comparison_of_equal_strings_reduces_to_true/0},

    {"comparison of unequal strings reduces to false",
     fun comparison_of_unequal_strings_reduces_to_false/0},

    {"comparison of equal booleans reduces to true",
     fun comparison_of_equal_booleans_reduces_to_true/0},

    {"comparison of unequal booleans reduces to false",
     fun comparison_of_unequal_booleans_reduces_to_false/0},

    {"comparison with expression lhs reduces",
     fun comparison_with_expression_lhs_reduces/0},

    {"comparison with expression rhs reduces",
     fun comparison_with_expression_rhs_reduces/0},

    {"condition with true if expression reduces to then expression",
     fun cnd_with_true_if_expr_reduces_to_then_expr/0},

    {"condition with false if expression reduces to then expression",
     fun cnd_with_false_if_expr_reduces_to_then_expr/0},

    {"condition with expression predicate reduces",
     fun cnd_with_expression_predicate_reduces/0},

    {"negation with true operand reduces to false",
     fun negation_with_true_operand_reduces_to_false/0},

    {"negation with false operand reduces to true",
     fun negation_with_false_operand_reduces_to_true/0},

    {"negation with expression operand reduces",
     fun negation_with_expression_operand_reduces/0},

    {"true and true reduces to true",
     fun true_and_true_reduces_to_true/0},

    {"true and false reduces to false",
     fun true_and_false_reduces_to_false/0},

    {"false and true reduces to false",
     fun false_and_true_reduces_to_false/0},

    {"false and false reduces to false",
     fun false_and_false_reduces_to_false/0},

    {"conjunction with expression lhs reduces",
     fun conjunction_with_expression_lhs_reduces/0},

    {"conjunction with expression rhs reduces",
     fun conjunction_with_expression_rhs_reduces/0},

    {"true or true reduces to true",
     fun true_or_true_reduces_to_true/0},

    {"true or false reduces to true",
     fun true_or_false_reduces_to_true/0},

    {"false or true reduces to true",
     fun false_or_true_reduces_to_true/0},

    {"false or false reduces to false",
     fun false_or_false_reduces_to_false/0},

    {"disjunction with expression lhs reduces",
     fun disjunction_with_expression_lhs_reduces/0},

    {"disjunction with expression rhs reduces",
     fun disjunction_with_expression_rhs_reduces/0},

    {"native application without arguments reduces to lambda body",
     fun nat_app_without_arg_reduces_to_lam_body/0},

    {"native application identity reduces to argument",
     fun nat_app_identity_reduces_to_arg/0},

    {"nat lambda shadows bound occurrences",
     fun nat_lambda_shadows_bound_occurrences/0},

    {"nat lambda with function expression reduces",
     fun nat_lambda_with_function_expression_reduces/0},

    {"nat lambda with argument expression reduces",
     fun nat_lambda_with_argument_expression_reduces/0},

    {"foreign function application no arg reduces to future",
     fun foreign_function_application_no_arg_reduces_to_future/0},

    {"foreign function application one arg reduces to future",
     fun foreign_function_application_one_arg_reduces_to_future/0},

    {"foreign function application reduces function expression",
     fun foreign_function_application_reduces_function_expression/0},

    {"foreign function application reduces argument expression",
     fun foreign_function_application_reduces_argument_expression/0},

    {"cons car reduces",
     fun cons_car_reduces/0},

    {"cons cdr reduces",
     fun cons_cdr_reduces/0},

    {"append nil and list reduces to list",
     fun append_nil_and_list_reduces_to_list/0},

    {"append cons list1 and list2 reduces to cons append list1 and list2",
     fun append_cons_list1_and_list2_reduces_to_cons_append_list1_and_list2/0},

    {"append with expression lhs reduces",
     fun append_with_expression_lhs_reduces/0},

    {"append with expression rhs reduces",
     fun append_with_expression_rhs_reduces/0},

    {"isnil empty list reduces to true",
     fun isnil_empty_list_reduces_to_true/0},

    {"isnil non-empty list reduces to false",
     fun isnil_nonempty_list_reduces_to_false/0},

    {"isnil with expression operand reduces",
     fun isnil_with_expression_operand_reduces/0},

    {"record with expression field reduces",
     fun record_with_expression_field_reduces/0},

    {"projection reduces to record field",
     fun projection_reduces_to_record_field/0},

    {"projection with expression operand reduces",
     fun projection_with_expression_operand_reduces/0},

    {"fixpoint operator reduces",
     fun fixpoint_operator_reduces/0},

    {"map reduces to list",
     fun map_reduces_to_list/0},

    {"zip reduces to list",
     fun zip_reduces_to_list/0},

    {"for over empty list reduces to empty list",
     fun for_over_empty_list_reduces_to_empty_list/0},


    {"fold reduces",
     fun fold_reduces/0},

    {"folding over empty list reduces to initial accumulator",
     fun folding_over_empty_list_reduces_to_initial_accumulator/0},

    {"nested error reduces to error",
     fun nested_error_reduces_to_error/0},

    {"project foreign app stalls",
     fun project_foreign_app_stalls/0},

    {"comparison with foreign app lhs stalls",
     fun comparison_with_foreign_app_lhs_stalls/0},

    {"comparison with foreign app lhs stalls",
     fun comparison_with_foreign_app_rhs_stalls/0},

    {"disjunction with foreign app or false leaves foreign app",
     fun disjunction_with_foreign_app_or_false_leaves_foreign_app/0},

    {"disjunction with two foreign apps stalls",
     fun disjunction_with_two_foreign_apps_stalls/0},

    {"condition with foreign app stalls",
     fun condition_with_foreign_app_stalls/0},

    {"chaining two foreign apps stalls",
     fun chaining_two_foreign_apps_stalls/0},

    {"foreign app with two args",
     fun foreign_app_with_two_args/0},

    {"foreign app as for argument",
     fun foreign_app_as_for_argument/0},

    {"conjunction with foreign app and true leaves foreign app",
     fun conjunction_with_foreign_app_and_true_leaves_foreign_app/0},

    {"conjunction with two foreign apps stalls",
     fun conjunction_with_two_foreign_apps_stalls/0},

    {"list with foreign app car stalls",
     fun list_with_foreign_app_car_stalls/0},

    {"list with both car and cdr foreign app stalls",
     fun list_with_both_car_and_cdr_foreign_app_stalls/0},

    {"list with foreign app cdr stalls",
     fun list_with_foreign_app_cdr_stalls/0},

    {"append with foreign app lhs stalls",
     fun append_with_foreign_app_lhs_stalls/0},

    {"append with foreign apps as both operands stalls",
     fun append_with_foreign_apps_as_both_operands_stalls/0},

    {"foreign app in only record field stalls",
     fun foreign_app_in_only_record_field_stalls/0},

    {"foreign app in first record field stalls",
     fun foreign_app_in_first_record_field_stalls/0},

    {"evaluation descends records with two fields",
     fun evaluation_descends_records_with_two_fields/0},

    {"foreign app as fold argument stalls",
     fun foreign_app_as_fold_argument_stalls/0},

    {"unused fold accumulator does not appear in evaluation context",
     fun unused_fold_accumulator_does_not_appear_in_evaluation_context/0}
   ]
  }.

comparison_of_equal_strings_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( cmp( str( <<"bla">> ), str( <<"bla">> ) ) ) ).

comparison_of_unequal_strings_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( cmp( str( <<"bla">> ), str( <<"blub">> ) ) ) ).

comparison_of_equal_booleans_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( cmp( true(), true() ) ) ),
  ?assertEqual(
    {ok, true(), []},
    step( cmp( false(), false() ) ) ).

comparison_of_unequal_booleans_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( cmp( true(), false() ) ) ),
  ?assertEqual(
    {ok, false(), []},
    step( cmp( false(), true() ) ) ).

comparison_with_expression_lhs_reduces() ->
  E = cmp( cmp( str( <<"bla">> ), str( <<"blub">> ) ), false() ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

comparison_with_expression_rhs_reduces() ->
  E = cmp( false(), cmp( str( <<"bla">> ), str( <<"blub">> ) ) ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

cnd_with_true_if_expr_reduces_to_then_expr() ->
  ETrue = str( <<"blub">> ),
  E1 = cnd( true(), ETrue, str( <<"bla">> ) ),
  ?assertEqual(
    {ok, ETrue, []},
    step( E1 ) ).

cnd_with_false_if_expr_reduces_to_then_expr() ->
  EFalse = str( <<"bla">> ),
  E1 = cnd( false(), str( <<"blub">> ), EFalse ),
  ?assertEqual(
    {ok, EFalse, []},
    step( E1 ) ).

cnd_with_expression_predicate_reduces() ->
  E1 = cmp( str( <<"bla">>), str( <<"bla">>) ),
  E = cnd( E1, str( <<"ok">> ), str( <<"not ok">> ) ),
  ?assertEqual(
    {ok, str( <<"ok">> ), []},
    step( E ) ).

negation_with_true_operand_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( neg( true() ) ) ).

negation_with_false_operand_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( neg( false() ) ) ).

negation_with_expression_operand_reduces() ->
  E = neg( cmp( str( <<"bla">> ), str( <<"blub">> ) ) ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

true_and_true_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( conj( true(), true() ) ) ).

true_and_false_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( conj( true(), false() ) ) ).

false_and_true_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( conj( false(), true() ) ) ).

false_and_false_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( conj( false(), false() ) ) ).

conjunction_with_expression_lhs_reduces() ->
  E = conj( cmp( str( <<"bla">> ), str( <<"bla">> ) ), true() ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

conjunction_with_expression_rhs_reduces() ->
  E = conj( true(), cmp( str( <<"bla">> ), str( <<"bla">> ) ) ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

true_or_true_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( disj( true(), true() ) ) ).

true_or_false_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( disj( true(), false() ) ) ).

false_or_true_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( disj( false(), true() ) ) ).

false_or_false_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( disj( false(), false() ) ) ).

disjunction_with_expression_lhs_reduces() ->
  E = disj( cmp( str( <<"bla">> ), str( <<"bla">> ) ), true() ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

disjunction_with_expression_rhs_reduces() ->
  E = disj( true(), cmp( str( <<"bla">> ), str( <<"bla">> ) ) ),
  ?assertEqual(
    {ok, true(), []},
    step( E ) ).

nat_app_without_arg_reduces_to_lam_body() ->
  E1 = app( e_lam_const(), [] ),
  E2 = str( <<"blub">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

nat_app_identity_reduces_to_arg() ->
  E1 = e_app_id(),
  E2 = str( <<"blub">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

nat_lambda_shadows_bound_occurrences() ->
  E1 =
    app(
      lam_ntv(
        [t_arg( x, t_str() )],
        app(
          lam_ntv(
            [t_arg( x, t_str() )],
            var( x ) ),
          [e_bind( x, str( <<"5">> ) )]) ),
      [e_bind( x, str( <<"4">> ) )] ),
  E2 = str( <<"5">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

nat_lambda_with_function_expression_reduces() ->
  E1 =
    app(
      lam_ntv(
        [t_arg( f, t_fn( ntv, [t_arg( x, t_str() )], t_str() ) )],
        app( var( f ), [e_bind( x, str( <<"5">> ) )] ) ),
      [e_bind( f, lam_ntv( [t_arg( x, t_str() )], var( x ) ) )] ),
  ?assertEqual(
    {ok, str( <<"5">> ), []},
    step( E1 ) ).

nat_lambda_with_argument_expression_reduces() ->
  E1 =
    app(
      lam_ntv( [t_arg( x, t_bool() )], var( x ) ),
      [e_bind( x, cmp( str( <<"bla">> ), str( <<"blub">> ) ) )] ),
  ?assertEqual(
    {ok, false(), []},
    step( E1 ) ).

foreign_function_application_no_arg_reduces_to_future() ->
  E1 = app( lam_frn( f, [], t_rcd( [t_arg( out, t_str() )]), l_bash(), <<"blub">> ), [] ),
  ?assertMatch(
    {ok, {fut, na, {'Rcd', [{out, 'Str'}]}, _}, [_]},
    step( E1 ) ).

foreign_function_application_one_arg_reduces_to_future() ->
  E1 =
    app(
      lam_frn(
        f,
        [t_arg( x, t_str() )],
        t_rcd( [t_arg( out, t_str() )]),
        l_bash(),
        <<"blub">> ),
      [e_bind( x, str( <<"bla">> ) )] ),
  ?assertMatch(
    {ok, {fut, na, {'Rcd', [{out, 'Str'}]}, _}, [_]},
    step( E1 ) ).

foreign_function_application_reduces_function_expression() ->
  E1 =
    app(
      lam_ntv(
        [t_arg( f, t_fn( frn, [], t_rcd( [t_arg( out, t_str() )] ) ) )],
        app( var( f ), [] ) ),
      [e_bind(
        f,
        lam_frn(
          f,
          [],
          t_rcd( [t_arg( out, t_str() )] ),
          l_bash(),
          <<"blub">> ) )] ),
  ?assertMatch(
    {ok, {app, na,
               {lam_ntv, na, [_], {fut, na, {'Rcd', [{out, 'Str'}]}, _}},
               [{f, _}]}, [_]},
    step( E1 ) ).

foreign_function_application_reduces_argument_expression() ->
  E1 =
    app(
      lam_frn(
        f,
        [t_arg( x, t_bool() )],
        t_rcd( [t_arg( out, t_bool() )] ),
        l_bash(),
        <<"blub">> ),
      [e_bind( x, neg( true() ) )] ),
  ?assertMatch(
    {ok, {fut, na, {'Rcd', [{out, 'Bool'}]}, _}, [_]},
    step( E1 ) ).

cons_car_reduces() ->
  E1 =
    cons( cmp( str( <<"bla">> ), str( <<"blub">> ) ), null( t_bool() ) ),
  ?assertEqual(
    {ok, cons( false(), null( t_bool() ) ), []},
    step( E1 ) ).

cons_cdr_reduces() ->
  E1 =
    cons( false(), cnd( true(), null( t_bool() ), null( t_bool() ) ) ),
  ?assertEqual(
    {ok, cons( false(), null( t_bool() ) ), []},
    step( E1 ) ).


append_nil_and_list_reduces_to_list() ->
  E2 = lst( t_bool(), [false(), true()] ),
  E1 = append( null( t_bool() ), E2 ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

append_cons_list1_and_list2_reduces_to_cons_append_list1_and_list2() ->
  L1 = lst( t_bool(), [true(), false()] ),
  L2 = lst( t_bool(), [false(), true()] ),
  E1 = append( L1, L2 ),
  E2 = lst( t_bool(), [true(), false(), false(), true()] ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

append_with_expression_lhs_reduces() ->
  L1 = cnd( true(), null( t_bool() ), null( t_bool() ) ),
  L2 = null( t_bool() ),
  E1 = append( L1, L2 ),
  ?assertEqual(
    {ok, null( t_bool() ), []},
    step( E1 ) ).

append_with_expression_rhs_reduces() ->
  L1 = null( t_bool() ),
  L2 = cnd( true(), null( t_bool() ), null( t_bool() ) ),
  E1 = append( L1, L2 ),
  ?assertEqual(
    {ok, null( t_bool() ), []},
    step( E1 ) ).

isnil_empty_list_reduces_to_true() ->
  E1 = isnil( lst( t_bool(), [] ) ),
  E2 = true(),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

isnil_nonempty_list_reduces_to_false() ->
  E1 = isnil( lst( t_bool(), [false()] ) ),
  E2 = false(),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

isnil_with_expression_operand_reduces() ->
  L = cnd( true(), null( t_bool() ), null( t_bool() ) ),
  E1 = isnil( L ),
  ?assertEqual(
    {ok, true(), []},
    step( E1 ) ).

record_with_expression_field_reduces() ->
  E0 = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  E1 = rcd( [e_bind( a, E0 )] ),
  E2 = rcd( [e_bind( a, false() )] ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

projection_reduces_to_record_field() ->
  E2 = str( <<"bla">> ),
  E1 = proj( a, rcd( [e_bind( a, E2 )] ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

projection_with_expression_operand_reduces() ->
  E2 = cmp( str( <<"blub">> ), str( <<"bla">> ) ),
  E1 = proj( a, rcd( [e_bind( a, E2 )] ) ),
  ?assertEqual(
    {ok, false(), []},
    step( E1 ) ).

fixpoint_operator_reduces() ->
  E2 = fix( lam_ntv( [t_arg( f, t_fn( ntv, [], t_str() ) )],
                     str( <<"blub">> ) ) ),
  E1 = app( E2, [] ),
  ?assertEqual(
    {ok, str( <<"blub">> ), []},
    step( E1 ) ).

map_reduces_to_list() ->
  E1 = for( t_bool(),
            [typed_bind( x, t_bool(), lst( t_bool(), [true(), false()] ) )],
            neg( var( x ) ) ),
  E2 = cons( false(), cons( true(), null( t_bool() ) ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

zip_reduces_to_list() ->
  E1 = for( t_bool(),
            [typed_bind( x, t_bool(), lst( t_bool(), [true(), false()] ) ),
             typed_bind( y, t_bool(), lst( t_bool(), [false(), false()] ) )],
            disj( var( x ), var( y ) ) ),
  E2 = cons( true(), cons( false(), null( t_bool() ) ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

fold_reduces() ->
  E1 = fold( typed_bind( x_acc, t_str(), str( <<"0">> ) ),
             typed_bind( x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
             var( x ) ),
  ?assertEqual(
    {ok, str( <<"2">> ), []},
    step( E1 ) ).

for_over_empty_list_reduces_to_empty_list() ->
  E1 = for( t_str(), [typed_bind( x, t_str(), null( t_str() ) )], var( x ) ),
  E2 = null( t_str() ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

folding_over_empty_list_reduces_to_initial_accumulator() ->
  E1 = fold( typed_bind( acc, t_str(), str( <<"bla">> ) ),
             typed_bind( x, t_str(), null( t_str() ) ), var( x ) ),
  E2 = str( <<"bla">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

nested_error_reduces_to_error() ->
  Err = err( t_bool(), <<"blub">> ),
  E1 = neg( Err ),
  ?assertEqual(
    {ok, Err, []},
    step( E1 ) ).

project_foreign_app_stalls() ->
  App = e_app_frn_str(),
  E1 = proj( a, App ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 = proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), AppId} ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

comparison_with_foreign_app_lhs_stalls() ->
  App = e_app_frn_str(),
  E1 = cmp( proj( a, App ), str( <<"blub">> ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    cmp(
      proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), AppId} ),
      str( <<"blub">> ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

comparison_with_foreign_app_rhs_stalls() ->
  App = e_app_frn_str(),
  E1 = cmp( str( <<"blub">> ), proj( a, App ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    cmp(
      str( <<"blub">> ),
      proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), AppId} ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

disjunction_with_foreign_app_or_false_leaves_foreign_app() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = disj( P, false() ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

disjunction_with_two_foreign_apps_stalls() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = disj( P, P ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    disj(
      proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ),
      proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ) ),
  ?assertEqual( {ok, E2, [Request, Request]}, step( E1 ) ).

condition_with_foreign_app_stalls() ->
  App = e_app_frn_bool(),
  E1 = cnd( proj( a, App ), str( <<"bla">> ), str( <<"blub">> ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    cnd( proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ),
         str( <<"bla">> ),
         str( <<"blub">> ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

chaining_two_foreign_apps_stalls() ->

  App1 = e_app_frn_str(),
  Lam2 = lam_frn( g,
                  [t_arg( x, t_str() )],
                  t_rcd( [t_arg( x, t_str() )] ),
                  l_bash(),
                  <<"shala">> ),
  E1 = app( Lam2, [e_bind( x, proj( a, App1 ) )] ),
  Request = app_to_effi_request( App1 ),
  #{ app_id := AppId } = Request,
  E2 = app( Lam2, [e_bind( x, proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), AppId} ) )] ),

  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

foreign_app_with_two_args() ->

  Lam = lam_frn( f,
                 [t_arg( x, t_str() ), t_arg( y, t_bool() )],
                 t_rcd( [t_arg( a, t_bool() )] ),
                 l_bash(),
                 <<"shalala">> ),

  E1 = app( Lam, [e_bind( x, str( <<"bla">> ) ),
                  e_bind( y, true() )] ),

  Request = app_to_effi_request( E1 ),
  #{ app_id := AppId } = Request,

  E2 = {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId},

  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

foreign_app_as_for_argument() ->

  TRet = t_rcd( [t_arg( a, t_lst( t_str() ) )] ),

  Lam = lam_frn( f,
                 [],
                 TRet,
                 l_bash(),
                 <<"shalala">> ),

  App = app( Lam, [] ),

  ForArg = proj( a, App ),

  E1 = for( t_str(),
            [typed_bind( x, t_str(), ForArg )],
            var( x ) ),

  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,

  E2 = for( t_str(),
            [typed_bind( x,
                         t_str(),
                         proj( a, {fut, na, TRet, AppId} ) )],
            var( x ) ),

  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

conjunction_with_foreign_app_and_true_leaves_foreign_app() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = conj( P, true() ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

conjunction_with_two_foreign_apps_stalls() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = conj( P, P ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  E2 =
    conj(
      proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ),
      proj( a, {fut, na, t_rcd( [t_arg( a, t_bool() )] ), AppId} ) ),
  ?assertEqual( {ok, E2, [Request, Request]}, step( E1 ) ).

list_with_foreign_app_car_stalls() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = cons( P, null( t_bool() ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_bool() )] ),
  E2 = cons( proj( a, {fut, na, TRet, AppId} ), null( t_bool() ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

list_with_both_car_and_cdr_foreign_app_stalls() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = cons( P, cons( P, null( t_bool() ) ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_bool() )] ),
  E2 = cons( proj( a, {fut, na, TRet, AppId} ), 
             cons( proj( a, {fut, na, TRet, AppId} ),
                   null( t_bool() ) ) ),
  ?assertEqual( {ok, E2, [Request, Request]}, step( E1 ) ).

list_with_foreign_app_cdr_stalls() ->
  App = e_app_frn_bool(),
  P = proj( a, App ),
  E1 = cons( true(), cons( P, null( t_bool() ) ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_bool() )] ),
  E2 = cons( true(), 
             cons( proj( a, {fut, na, TRet, AppId} ),
                   null( t_bool() ) ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

append_with_foreign_app_lhs_stalls() ->
  App = e_app_frn_str(),
  P = proj( a, App ),
  E1 = append( P, str( <<"blub">> ) ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_str() )] ),
  E2 = append( proj( a, {fut, na, TRet, AppId} ), str( <<"blub">> ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

append_with_foreign_apps_as_both_operands_stalls() ->
  App = e_app_frn_str(),
  P = proj( a, App ),
  E1 = append( P, P ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_str() )] ),
  E2 = append( proj( a, {fut, na, TRet, AppId} ),
               proj( a, {fut, na, TRet, AppId} ) ),
  ?assertEqual( {ok, E2, [Request, Request]}, step( E1 ) ).

foreign_app_in_only_record_field_stalls() ->
  App = e_app_frn_str(),
  P = proj( a, App ),
  E1 = rcd( [e_bind( b, P )] ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_str() )] ),
  E2 = rcd( [e_bind( b, proj( a, {fut, na, TRet, AppId} ) )] ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

foreign_app_in_first_record_field_stalls() ->
  App = e_app_frn_str(),
  P = proj( a, App ),
  E1 = rcd( [e_bind( b, P ), e_bind( c, str( <<"blub">> ) )] ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  TRet = t_rcd( [t_arg( a, t_str() )] ),
  E2 = rcd( [e_bind( b, proj( a, {fut, na, TRet, AppId} ) ),
             e_bind( c, str( <<"blub">> ) )] ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

evaluation_descends_records_with_two_fields() ->
  Prop = conj( true(), false() ),
  E1 = rcd( [e_bind( a, Prop ), e_bind( b, Prop )] ),
  E2 = rcd( [e_bind( a, false() ), e_bind( b, false() )] ),
  ?assertEqual( {ok, E2, []}, step( E1 ) ).

foreign_app_as_fold_argument_stalls() ->
  TRet = t_rcd( [t_arg( a, t_lst( t_str() ) )] ),
  Lam = lam_frn( f, [], TRet, l_bash(), <<"shala">> ),
  App = app( Lam, [] ),
  Request = app_to_effi_request( App ),
  #{ app_id := AppId } = Request,
  FoldArg = proj( a, App ),
  E1 = fold( typed_bind( acc, t_str(), str( <<"blub">> ) ),
             typed_bind( x, t_str(), FoldArg ),
             var( x ) ),
  E2 = fold( typed_bind( acc, t_str(), str( <<"blub">> ) ),
             typed_bind( x, t_str(), proj( a, {fut, na, TRet, AppId} ) ),
             var( x ) ),
  ?assertEqual( {ok, E2, [Request]}, step( E1 ) ).

unused_fold_accumulator_does_not_appear_in_evaluation_context() ->
  E2 = str( <<"blub">> ),
  Acc0 = err( t_str(), <<"never evaluate">> ),
  L = lst( t_str(), [E2] ),
  E1 = fold( typed_bind( acc, t_str(), Acc0 ),
             typed_bind( x, t_str(), L ),
             var( x ) ),
  ?assertEqual( {ok, E2, []}, step( E1 ) ).








error_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"error and true returns error",
     fun error_and_true_returns_error/0},
    {"error and false returns error",
     fun error_and_false_returns_error/0},
    {"error or true returns error",
     fun error_or_true_returns_error/0},
    {"error or false returns error",
     fun error_or_false_returns_error/0},
    {"comparing errors returns error",
     fun comparing_errors_returns_error/0},
    {"consing error to list returns error",
     fun consing_error_to_list_returns_error/0},
    {"consing string to error returns error",
     fun consing_string_to_error_returns_error/0}
   ]
  }.

error_and_true_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E12 = true(),
  E1 = conj( E11, E12 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

error_and_false_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E12 = false(),
  E1 = conj( E11, E12 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

error_or_true_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E12 = true(),
  E1 = disj( E11, E12 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

error_or_false_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E12 = false(),
  E1 = disj( E11, E12 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

comparing_errors_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E12 = err( t_str(), <<"blub">> ),
  E1 = cmp( E11, E12 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

consing_error_to_list_returns_error() ->
  E11 = err( t_str(), <<"bla">> ),
  E1 = cons( E11, null( t_str() ) ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).

consing_string_to_error_returns_error() ->
  E11 = err( t_lst( t_str() ), <<"bla">> ),
  E1 = cons( str( <<"blub">> ), E11 ),
  ?assertEqual( {ok, E11, []}, step( E1 ) ).
