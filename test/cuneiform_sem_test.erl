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
%% @version 0.1.0
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_sem_test ).
-include_lib( "eunit/include/eunit.hrl" ).

%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_sem, [reduce/1] ).
-import( cuneiform_sem, [is_value/1] ).
-import( cuneiform_sem, [rename_pattern/3, rename/3, subst/3, gensym/1, subst_fut/3] ).
-import( cuneiform_sem, [in_hole/2, find_context/1] ).
-import( cuneiform_sem, [set_info/2] ).

-import( cuneiform_lang, [r_var/2, r_rcd/1, r_bind/2] ).
-import( cuneiform_lang, [l_bash/0] ).
-import( cuneiform_lang, [
                          t_str/0, t_file/0, t_bool/0, t_fn/3, t_arg/2, t_rcd/1,
                          t_lst/1
                         ] ).
-import( cuneiform_lang, [lam_ntv_arg/2, e_bind/2] ).
-import( cuneiform_lang, [
                          str/1, file/1, true/0, false/0, cnd/3, var/1, file/2,
                          lam_ntv/2, app/2, cmp/2, neg/1, conj/2, disj/2,
                          lam_frn/5, lst/2, append/2, isnil/1, for/3, fold/3,
                          rcd/1, proj/2, fix/1, assign/3, null/1, cons/3, err/2,
                          err/3
                         ] ).
 
%%====================================================================
%% Expression definitions
%%====================================================================

e_lam1() ->
  lam_ntv( [lam_ntv_arg( x, t_str() ),
            lam_ntv_arg( y, t_file() )], var( x ) ).

e_lam_first() ->
  lam_ntv( [lam_ntv_arg( x, t_str() ),
            lam_ntv_arg( y, t_str() )], var( x ) ).

e_lam_const() ->
  lam_ntv( [], str( <<"blub">> ) ).

e_lam_id() ->
  lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ).

e_lam_greet() ->
  lam_frn( greet, [t_arg( person, t_str() )], t_rcd( [t_arg( out, t_str() )] ), l_bash(), <<"out=\"hello $person\"">> ).

e_app_id() ->
  app( e_lam_id(), [e_bind( x, str( <<"blub">> ) )] ).


%%====================================================================
%% Notion of reduction
%%====================================================================

reduce_test_() ->
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

    {"condition with true if expression reduces to then expression",
     fun cnd_with_true_if_expr_reduces_to_then_expr/0},

    {"condition with false if expression reduces to then expression",
     fun cnd_with_false_if_expr_reduces_to_then_expr/0},

    {"negation with true operand reduces to false",
     fun negation_with_true_operand_reduces_to_false/0},

    {"negation with false operand reduces to true",
     fun negation_with_false_operand_reduces_to_true/0},

    {"true and true reduces to true",
     fun true_and_true_reduces_to_true/0},

    {"true and false reduces to false",
     fun true_and_false_reduces_to_false/0},

    {"false and true reduces to false",
     fun false_and_true_reduces_to_false/0},

    {"false and false reduces to false",
     fun false_and_false_reduces_to_false/0},

    {"true or true reduces to true",
     fun true_or_true_reduces_to_true/0},

    {"true or false reduces to true",
     fun true_or_false_reduces_to_true/0},

    {"false or true reduces to true",
     fun false_or_true_reduces_to_true/0},

    {"false or false reduces to false",
     fun false_or_false_reduces_to_false/0},

    {"native application without arguments reduces to lambda body",
     fun nat_app_without_arg_reduces_to_lam_body/0},

    {"native application with single argument reduces to empty application",
     fun nat_app_with_single_arg_reduces_to_empty_application/0},

    {"list append reduces to list",
     fun list_append_reduces_to_list/0},

    {"isnil empty list reduces to true",
     fun isnil_empty_list_reduces_to_true/0},

    {"isnil non-empty list reduces to false",
     fun isnil_nonempty_list_reduces_to_false/0},

    {"projection reduces to record field",
     fun projection_reduces_to_record_field/0},

    {"map reduces to list",
     fun map_reduces_to_list/0},

    {"zip reduces to list",
     fun zip_reduces_to_list/0},

    {"fold reduces",
     fun fold_reduces/0},

    {"fixpoint operator reduces",
     fun fixpoint_operator_reduces/0},

    {"fixpoint operator inserts function definition",
     fun fixpoint_operator_inserts_function_definition/0}
   ]
  }.

comparison_of_equal_strings_reduces_to_true() ->
  ?assertEqual( true(), reduce( cmp( str( <<"bla">> ), str( <<"bla">> ) ) ) ).

comparison_of_unequal_strings_reduces_to_false() ->
  ?assertEqual( false(), reduce( cmp( str( <<"bla">> ), str( <<"blub">> ) ) ) ).

comparison_of_equal_booleans_reduces_to_true() ->
  ?assertEqual( true(), reduce( cmp( true(), true() ) ) ),
  ?assertEqual( true(), reduce( cmp( false(), false() ) ) ).

comparison_of_unequal_booleans_reduces_to_false() ->
  ?assertEqual( false(), reduce( cmp( true(), false() ) ) ),
  ?assertEqual( false(), reduce( cmp( false(), true() ) ) ).


cnd_with_true_if_expr_reduces_to_then_expr() ->
  ETrue = str( <<"blub">> ),
  E1 = cnd( true(), ETrue, str( <<"bla">> ) ),
  ?assertEqual( ETrue, reduce( E1 ) ).

cnd_with_false_if_expr_reduces_to_then_expr() ->
  EFalse = str( <<"bla">> ),
  E1 = cnd( false(), str( <<"blub">> ), EFalse ),
  ?assertEqual( EFalse, reduce( E1 ) ).

negation_with_true_operand_reduces_to_false() ->
  ?assertEqual( false(), reduce( neg( true() ) ) ).

negation_with_false_operand_reduces_to_true() ->
  ?assertEqual( true(), reduce( neg( false() ) ) ).

true_and_true_reduces_to_true() ->
  ?assertEqual( true(), reduce( conj( true(), true() ) ) ).

true_and_false_reduces_to_false() ->
  ?assertEqual( false(), reduce( conj( true(), false() ) ) ).

false_and_true_reduces_to_false() ->
  ?assertEqual( false(), reduce( conj( false(), true() ) ) ).

false_and_false_reduces_to_false() ->
  ?assertEqual( false(), reduce( conj( false(), false() ) ) ).

true_or_true_reduces_to_true() ->
  ?assertEqual( true(), reduce( disj( true(), true() ) ) ).

true_or_false_reduces_to_true() ->
  ?assertEqual( true(), reduce( disj( true(), false() ) ) ).

false_or_true_reduces_to_true() ->
  ?assertEqual( true(), reduce( disj( false(), true() ) ) ).

false_or_false_reduces_to_false() ->
  ?assertEqual( false(), reduce( disj( false(), false() ) ) ).

nat_app_without_arg_reduces_to_lam_body() ->
  E1 = app( e_lam_const(), [] ),
  E2 = str( <<"blub">> ),
  ?assertEqual( E2, reduce( E1 ) ).

nat_app_with_single_arg_reduces_to_empty_application() ->
  E1 = e_app_id(),
  E2 = app( lam_ntv( [], str( <<"blub">> ) ), [] ),
  ?assertEqual( E2, reduce( E1 ) ).

list_append_reduces_to_list() ->
  E1 = append( lst( t_bool(), [true(), false()] ),
               lst( t_bool(), [false(), true()] ) ),
  E2 = lst( t_bool(), [true(), false(), false(), true()] ),
  ?assertEqual( E2, reduce( E1 ) ).

isnil_empty_list_reduces_to_true() ->
  E1 = isnil( lst( t_bool(), [] ) ),
  E2 = true(),
  ?assertEqual( E2, reduce( E1 ) ).

isnil_nonempty_list_reduces_to_false() ->
  E1 = isnil( lst( t_bool(), [false()] ) ),
  E2 = false(),
  ?assertEqual( E2, reduce( E1 ) ).

projection_reduces_to_record_field() ->
  E2 = str( <<"bla">> ),
  E1 = proj( a, rcd( [e_bind( a, E2 )] ) ),
  ?assertEqual( E2, reduce( E1 ) ).

map_reduces_to_list() ->
  E1 = for( t_bool(), [e_bind( x, lst( t_bool(), [true(), false()] ) )], neg( var( x ) ) ),
  E2 = cons( t_bool(),
             neg( true() ),
             for( t_bool(), [e_bind( x, lst( t_bool(), [false()] ) )], neg( var( x ) ) ) ),
  ?assertEqual( E2, reduce( E1 ) ).

zip_reduces_to_list() ->
  E1 = for( t_bool(),
            [e_bind( x, lst( t_bool(), [true(), false()] ) ),
             e_bind( y, lst( t_bool(), [false(), false()] ) )],
            disj( var( x ), var( y ) ) ),
  E2 = cons( t_bool(),
             disj( true(), false() ),
             for( t_bool(),
                  [e_bind( x, lst( t_bool(), [false()] ) ),
                   e_bind( y, lst( t_bool(), [false()] ) )],
                  disj( var( x ), var( y ) ) ) ),

  ?assertEqual( E2, reduce( E1 ) ).

fold_reduces() ->
  E1 = fold( e_bind( x_acc, str( <<"0">> ) ),
             e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
             var( x ) ),
  E2 = fold( e_bind( x_acc, str( <<"1">> ) ),
             e_bind( x, lst( t_str(), [str( <<"2">> )] ) ),
             var( x ) ),
  ?assertEqual( E2, reduce( E1 ) ).

fixpoint_operator_reduces() ->
  E1 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                     str( <<"blub">> ) ) ),
  E2 = lam_ntv( [], str( <<"blub">> ) ),
  ?assertEqual( E2, reduce( E1 ) ).

fixpoint_operator_inserts_function_definition() ->
  E1 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                    var( f ) ) ),
  E2 = lam_ntv( [], E1 ),
  ?assertEqual( E2, reduce( E1 ) ).


%%====================================================================
%% Determining values
%%====================================================================

is_value_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"str is value",            fun str_is_value/0},
    {"comparison is no value",  fun comparison_is_no_value/0},
    {"file is value",           fun file_is_value/0},
    {"true is value",           fun true_is_value/0},
    {"false is value",          fun false_is_value/0},
    {"condition is no value",   fun cnd_is_no_value/0},
    {"negation is no value",    fun negation_is_no_value/0},
    {"conjunction is no value", fun conjunction_is_no_value/0},
    {"disjunction is no value", fun disjunction_is_no_value/0},
    {"variable is no value",    fun variable_is_no_value/0},
    {"native lambda is value",  fun lam_ntv_is_value/0},
    {"foreign lambda is value", fun foreign_lambda_is_value/0},
    {"application is no value", fun app_is_no_value/0},
    {"future is no value",      fun future_is_no_value/0},
    {"empty list is value",     fun empty_list_is_value/0},
    {"list of value is value",  fun list_of_value_is_value/0},

    {"list of non-value is no value",
     fun list_of_nonvalue_is_no_value/0},

    {"append is no value",      fun append_is_no_value/0},
    {"isnil is no value",       fun isnil_is_no_value/0},
    {"for is no value",         fun for_is_no_value/0},
    {"fold is no value",        fun fold_is_no_value/0},

    {"record of value is value",
     fun record_of_value_is_value/0},

    {"record of non-value is no value",
     fun record_of_nonvalue_is_no_value/0},

    {"projection is no value",  fun projection_is_no_value/0},
    {"fixpoint is no value",    fun fixpoint_is_no_value/0},
    {"error is value",          fun error_is_value/0}
   ]
  }.

str_is_value() ->
  ?assert( is_value( str( <<"blub">> ) ) ).

comparison_is_no_value() ->
  ?assertNot( is_value( cmp( str( <<"bla">> ), str( <<"blub">> ) ) ) ).

file_is_value() ->
  ?assert( is_value( file( <<"blub">> ) ) ).

true_is_value() ->
  ?assert( is_value( true() ) ).

false_is_value() ->
  ?assert( is_value( false() ) ).

cnd_is_no_value() ->
  ?assertNot( is_value( cnd( true(), true(), false() ) ) ).

negation_is_no_value() ->
  ?assertNot( is_value( neg( true() ) ) ).

conjunction_is_no_value() ->
  ?assertNot( is_value( conj( true(), false() ) ) ).

disjunction_is_no_value() ->
  ?assertNot( is_value( disj( true(), false() ) ) ).

variable_is_no_value() ->
  ?assertNot( is_value( var( x ) ) ).

lam_ntv_is_value() ->
  ?assert( is_value( e_lam1() ) ),
  ?assert( is_value( e_lam_first() ) ),
  ?assert( is_value( e_lam_const() ) ),
  ?assert( is_value( e_lam_id() ) ).

foreign_lambda_is_value() ->
  LamFrn = lam_frn( f, [], t_rcd( [t_arg( a, t_str() )] ), l_bash(),
                    <<"blub">> ),
  ?assert( is_value( LamFrn ) ).

app_is_no_value() ->
  ?assertNot( is_value( e_app_id() ) ),
  ?assertNot( is_value( app( e_lam_const(), [] ) ) ).

future_is_no_value() ->
  ?assertNot( is_value( {fut, na, t_str(), na} ) ).

empty_list_is_value() ->
  ?assert( is_value( lst( t_str(), [] ) ) ).

list_of_value_is_value() ->
  ?assert( is_value( lst( t_str(), [str( <<"bla">> )] ) ) ).

list_of_nonvalue_is_no_value() ->
  ?assertNot( is_value( lst( t_bool(), [neg( true() )] ) ) ).

append_is_no_value() ->
  E = append( lst( t_bool(), [true()] ), lst( t_bool(), [false()] ) ),
  ?assertNot( is_value( E ) ).

isnil_is_no_value() ->
  ?assertNot( is_value( isnil( lst( t_str(), [] ) ) ) ).

for_is_no_value() ->
  E = for( t_str(), [e_bind( x, lst( t_str(), [str( <<"bla">> )] ) )], var( x ) ),
  ?assertNot( is_value( E ) ).

fold_is_no_value() ->
  E = fold( e_bind( x_acc, str( <<"0">> ) ),
            e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
            var( x ) ),
  ?assertNot( is_value( E ) ).

record_of_value_is_value() ->
  E = rcd( [e_bind( a, str( <<"bla">> ) )] ),
  ?assert( is_value( E ) ).

record_of_nonvalue_is_no_value() ->
  E = rcd( [e_bind( a, neg( true() ) )] ),
  ?assertNot( is_value( E ) ).

projection_is_no_value() ->
  E = proj( a, rcd( [e_bind( a, str( <<"bla">> ) )] ) ),
  ?assertNot( is_value( E ) ).

fixpoint_is_no_value() ->
  ELam = lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                  str( <<"bla">> ) ),
  E = fix( ELam ),
  ?assertNot( is_value( E ) ).

error_is_value() ->
  ?assertEqual( true, is_value( err( t_str(), <<"blub">> ) ) ).

%%====================================================================
%% Substitution and renaming
%%====================================================================

rename_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"rename leaves str alone",   fun rename_leaves_str_alone/0},

    {"rename propagates to comparison lhs",
     fun rename_propagates_to_comparison_lhs/0},

    {"rename propagates to comparison rhs",
     fun rename_propagates_to_comparison_rhs/0},

    {"rename leaves file alone",  fun rename_leaves_file_alone/0},
    {"rename leaves true alone",  fun rename_leaves_true_alone/0},
    {"rename leaves false alone", fun rename_leaves_false_alone/0},

    {"rename propagates to condition if expression",
     fun rename_propagates_to_cnd_if_expr/0},

    {"rename propagates to condition then expression",
     fun rename_propagates_to_cnd_then_expr/0},

    {"rename propagates to condition else expression",
     fun rename_propagates_to_cnd_else_expr/0},

    {"rename propagates to negation operand",
     fun rename_propagates_to_negation_operand/0},

    {"rename propagates to conjunction lhs",
     fun rename_propagates_to_conjunction_lhs/0},

    {"rename propagates to conjunction rhs",
     fun rename_propagates_to_conjunction_rhs/0},

    {"rename propagates to disjunction lhs",
     fun rename_propagates_to_disjunction_lhs/0},

    {"rename propagates to disjunction rhs",
     fun rename_propagates_to_disjunction_rhs/0},

    {"rename leaves non-matching variable alone",
     fun rename_leaves_nonmatching_var_alone/0},

    {"matching variable is renamed",
     fun matching_var_is_renamed/0},

    {"rename propagates to native lambda argument binding",
     fun rename_propagates_to_lam_ntv_arg_lst/0},

    {"rename leaves non-matching lambda argument alone",
     fun rename_leaves_nonmatching_lam_ntv_arg_alone/0},

    {"rename propagates to native lambda body",
     fun rename_propagates_to_lam_ntv_body/0},

    {"rename leaves foreign lambda alone",
     fun rename_leaves_foreign_lambda_alone/0},

    {"rename propagates to application function",
     fun rename_propagates_to_app_function/0},

    {"rename propagates to application arguments",
     fun rename_propagates_to_app_e_bind_lst/0},

    {"rename leaves future alone",
     fun rename_leaves_future_alone/0},

    {"rename propagates to list elements",
     fun rename_propagates_to_list_elements/0},

    {"rename propagates to append lhs",
     fun rename_propagates_to_append_lhs/0},

    {"rename propagates to append rhs",
     fun rename_propagates_to_append_rhs/0},

    {"rename propagates to isnil operand",
     fun rename_propagates_to_isnil_operand/0},

    {"rename propagates to for bound variable",
     fun rename_propagates_to_for_bound_variable/0},

    {"rename propagates to for list expression",
     fun rename_propagates_to_for_list_expression/0},

    {"rename propagates to for body expression",
     fun rename_propagates_to_for_body_expression/0},

    {"rename propagates to fold accumulator bound variable",
     fun rename_propagates_to_accumulator_bound_argument/0},

    {"rename propagates to fold accumulator init expression",
     fun rename_propagates_to_fold_accumulator_init_expression/0},

    {"rename propagates to fold iterator bound variable",
     fun rename_propagates_to_fold_iterator_bound_variable/0},

    {"rename propagates to fold iterator list expression",
     fun rename_propagates_to_fols_iterator_list_expression/0},

    {"rename propagates to fold body expression",
     fun rename_propagates_to_fold_body_expression/0},

    {"rename propagates to rcd field",
     fun rename_propagates_to_rcd_field/0},

    {"rename propagates to projection operand",
     fun rename_propagates_to_projection_operand/0},

    {"rename propagates to fixpoint operand",
     fun rename_propagates_to_fixpoint_operand/0},

    {"rename leaves error alone",
     fun rename_leaves_error_alone/0}
   ]
  }.


rename_leaves_str_alone() ->
  E = str( <<"blub">> ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_comparison_lhs() ->
  E = cmp( var( x ), var( y ) ),
  ?assertEqual( cmp( var( z ), var( y ) ), rename( E, x, z ) ).

rename_propagates_to_comparison_rhs() ->
  E = cmp( var( x ), var( y ) ),
  ?assertEqual( cmp( var( x ), var( z ) ), rename( E, y, z ) ).

rename_leaves_file_alone() ->
  E = file( <<"blub">> ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_leaves_true_alone() ->
  E = true(),
  ?assertEqual( E, rename( E, x, y ) ).

rename_leaves_false_alone() ->
  E = false(),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_cnd_if_expr() ->
  E1 = cnd( var( x ), true(), false() ),
  E2 = cnd( var( y ), true(), false() ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_cnd_then_expr() ->
  E1 = cnd( true(), var( x ), false() ),
  E2 = cnd( true(), var( y ), false() ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_cnd_else_expr() ->
  E1 = cnd( true(), true(), var( x ) ),
  E2 = cnd( true(), true(), var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_negation_operand() ->
  E1 = neg( var( x ) ),
  E2 = neg( var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_conjunction_lhs() ->
  E1 = conj( var( x ), var( y ) ),
  E2 = conj( var( z ), var( y ) ),
  ?assertEqual( E2, rename( E1, x, z ) ).

rename_propagates_to_conjunction_rhs() ->
  E1 = conj( var( x ), var( y ) ),
  E2 = conj( var( x ), var( z ) ),
  ?assertEqual( E2, rename( E1, y, z ) ).

rename_propagates_to_disjunction_lhs() ->
  E1 = disj( var( x ), var( y ) ),
  E2 = disj( var( z ), var( y ) ),
  ?assertEqual( E2, rename( E1, x, z ) ).

rename_propagates_to_disjunction_rhs() ->
  E1 = disj( var( x ), var( y ) ),
  E2 = disj( var( x ), var( z ) ),
  ?assertEqual( E2, rename( E1, y, z ) ).

rename_leaves_nonmatching_var_alone() ->
  E = var( x ),
  ?assertEqual( E, rename( E, y, z ) ).

matching_var_is_renamed() ->
  E1 = var( x ),
  E2 = var( y ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_lam_ntv_arg_lst() ->
  E1 = lam_ntv( [lam_ntv_arg( x, t_str() )], str( <<"blub">> ) ),
  E2 = lam_ntv( [{y, x, t_str()}], str( <<"blub">> ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_nonmatching_lam_ntv_arg_alone() ->
  E = lam_ntv( [lam_ntv_arg( z, t_str() )], str( <<"blub">> ) ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_lam_ntv_body() ->
  E1 = lam_ntv( [], var( x ) ),
  E2 = lam_ntv( [], var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_app_function() ->
  E1 = app( var( x ), [] ),
  E2 = app( var( y ), [] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_foreign_lambda_alone() ->
  E = lam_frn( f, [t_arg( x, t_str() )], t_rcd( [t_arg( a, t_str() )] ),
               l_bash(), <<"blub">> ),
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_app_e_bind_lst() ->
  E1 = app( var( f ), [e_bind( x, var( x ) )] ),
  E2 = app( var( f ), [e_bind( x, var( y ) )] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_future_alone() ->
  E = {fut, na, t_str(), na},
  ?assertEqual( E, rename( E, x, y ) ).

rename_propagates_to_list_elements() ->
  E1 = lst( t_str(), [var( x )] ),
  E2 = lst( t_str(), [var( y )] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_append_lhs() ->
  E1 = append( var( x ), var( y ) ),
  E2 = append( var( z ), var( y ) ),
  ?assertEqual( E2, rename( E1, x, z ) ).

rename_propagates_to_append_rhs() ->
  E1 = append( var( x ), var( y ) ),
  E2 = append( var( x ), var( z ) ),
  ?assertEqual( E2, rename( E1, y, z ) ).

rename_propagates_to_isnil_operand() ->
  E1 = isnil( var( x ) ),
  E2 = isnil( var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_for_bound_variable() ->
  E1 = for( t_str(), [e_bind( x, var( l ) )], var( y ) ),
  E2 = for( t_str(), [e_bind( z, var( l ) )], var( y ) ),
  ?assertEqual( E2, rename( E1, x, z ) ).

rename_propagates_to_for_list_expression() ->
  E1 = for( t_str(), [e_bind( x, var( l ) )], var( y ) ),
  E2 = for( t_str(), [e_bind( x, var( z ) )], var( y ) ),
  ?assertEqual( E2, rename( E1, l, z ) ).

rename_propagates_to_for_body_expression() ->
  E1 = for( t_str(), [e_bind( x, var( l ) )], var( y ) ),
  E2 = for( t_str(), [e_bind( x, var( l ) )], var( z ) ),
  ?assertEqual( E2, rename( E1, y, z ) ).

rename_propagates_to_accumulator_bound_argument() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  E2 = fold( e_bind( z, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertEqual( E2, rename( E1, x_acc, z ) ).

rename_propagates_to_fold_accumulator_init_expression() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  E2 = fold( e_bind( x_acc, var( z ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertEqual( E2, rename( E1, x0, z ) ).

rename_propagates_to_fold_iterator_bound_variable() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  E2 = fold( e_bind( x_acc, var( x0 ) ), e_bind( z, var( x_lst ) ), var( y ) ),
  ?assertEqual( E2, rename( E1, x, z ) ).

rename_propagates_to_fols_iterator_list_expression() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  E2 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( z ) ), var( y ) ),
  ?assertEqual( E2, rename( E1, x_lst, z ) ).

rename_propagates_to_fold_body_expression() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( y ) ),
  E2 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, var( x_lst ) ), var( z ) ),
  ?assertEqual( E2, rename( E1, y, z ) ).

rename_propagates_to_rcd_field() ->
  E1 = rcd( [e_bind( x, var( x ) )] ),
  E2 = rcd( [e_bind( x, var( y ) )] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_projection_operand() ->
  E1 = proj( x, var( x ) ),
  E2 = proj( x, var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_propagates_to_fixpoint_operand() ->
  E1 = fix( var( x ) ),
  E2 = fix( var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_leaves_error_alone() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( E, rename( E, x, y ) ).


subst_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"substitution leaves str alone",   fun subst_leaves_str_alone/0},
    {"substitution leaves file alone",  fun subst_leaves_file_alone/0},
    {"substitution leaves true alone",  fun subst_leaves_true_alone/0},
    {"substitution leaves false alone", fun subst_leaves_false_alone/0},

    {"substitution propagates to comparison lhs",
     fun substitution_propagates_to_comparison_lhs/0},

    {"substitution propagates to comparison rhs",
     fun substitution_propagates_to_comparison_rhs/0},

    {"substitution propagates to condition if expression",
     fun subst_propagates_to_cnd_if_expr/0},

    {"substitution propagates to condition then expression",
     fun subst_propagates_to_cnd_then_expr/0},

    {"substitution propagates to condition else expression",
     fun subst_propagates_to_cnd_else_expr/0},

    {"substitution propagates to negation operand",
     fun substitution_propagates_to_negation_operand/0},

    {"substitution propagates to conjunction lhs",
     fun substitution_propagates_to_conjunction_lhs/0},

    {"substitution propagates to conjunction rhs",
     fun substitution_propagates_to_conjunction_rhs/0},

    {"substitution propagates to disjunction lhs",
     fun substitution_propagates_to_disjunction_lhs/0},

    {"substitution propagates to disjunction rhs",
     fun substitution_propagates_to_disjunction_rhs/0},

    {"substitution leaves non-matching variable alone",
     fun subst_leaves_nonmatching_var_alone/0},

    {"matching variable is substituted",
     fun matching_var_is_substituted/0},

    {"substitution propagates to native lambda body",
     fun subst_propagates_to_lam_ntv_body/0},

    {"substitution is shadowed by bound variables",
     fun subst_shadowed_by_bound_var/0},

    {"substitution is capture avoiding",
     fun subst_is_capture_avoiding/0},

    {"substitution retains order of native lambda arguments",
     fun subst_retains_order_of_lam_ntv_arg_lst/0},

    {"substitution leaves foreign lambda alone",
     fun substitution_leaves_foreign_lambda_alone/0},

    {"substitution propagates to application function",
     fun subst_propagates_to_app_function/0},

    {"substitution propagates to application arguments",
     fun subst_propagates_to_e_bind_lst/0},

    {"substitution leaves futures alone",
     fun substitution_leaves_futures_alone/0},

    {"substitution leaves empty list alone",
     fun substitution_leaves_empty_list_alone/0},

    {"substitution propagates to list elements",
     fun substitution_propagates_to_list_elements/0},

    {"substitution propagates to append lhs",
     fun substitution_propagates_to_append_lhs/0},

    {"substitution propagates to append rhs",
     fun substitution_propagates_to_append_rhs/0},

    {"substitution propagates to isnil operand",
     fun substitution_propagates_to_isnil_operand/0},

    {"substitution propagates to record fields",
     fun substitution_propagates_to_record_fields/0},

    {"substitution propagates to propjection operand",
     fun substitution_propagates_to_projection_operand/0},

    {"substitution propagates to fixpoint operand",
     fun substitution_propagates_to_fixpoint_operand/0},

    {"substitution propagates to for body",
     fun substitution_propagates_to_for_body/0},

    {"substitution propagates to for list expression",
     fun substitution_propagates_to_for_list_expression/0},

    {"for iterator shadows substitution",
     fun for_iterator_shadows_substitution/0},

    {"for iterator is capture avoiding",
     fun for_iterator_is_capture_avoiding/0},

    {"for iterator retains order of list bindings",
     fun for_iterator_retains_order_of_list_bindings/0},

    {"substitution propagates to fold body",
     fun substitution_propagates_to_fold_body/0},

    {"substitution propagates to accumulator init expression",
     fun substitution_propagates_to_accumulator_init_expression/0},

    {"substitution propagates to list expression",
     fun substitution_propagates_to_list_expression/0},

    {"fold accumulator shadows substitution",
     fun fold_accumulator_shadows_substitution/0},

    {"fold accumulator is capture avoiding",
     fun fold_accumulator_is_capture_avoiding/0},

    {"fold list expression shadows substitution",
     fun fold_list_expression_shadows_substitution/0},

    {"fold list expression is capture avoiding",
     fun fold_list_expression_is_capture_avoiding/0},

    {"substitution leaves error alone",
     fun substitution_leaves_error_alone/0}
   ]
  }.


subst_leaves_str_alone() ->
  E = str( <<"blub">> ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_file_alone() ->
  E = file( <<"blub">> ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_true_alone() ->
  E = true(),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

subst_leaves_false_alone() ->
  E = false(),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

substitution_propagates_to_comparison_lhs() ->
  E1 = cmp( var( x ), var( y ) ),
  E2 = cmp( str( <<"blub">> ), var( y ) ),
  ?assertEqual( E2, subst( E1, x, str( <<"blub">> ) ) ).

substitution_propagates_to_comparison_rhs() ->
  E1 = cmp( var( x ), var( y ) ),
  E2 = cmp( var( x ), str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E1, y, str( <<"blub">> ) ) ).

subst_propagates_to_cnd_if_expr() ->
  E1 = cnd( var( x ), true(), false() ),
  E2 = cnd( var( y ), true(), false() ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_cnd_then_expr() ->
  E1 = cnd( true(), var( x ), false() ),
  E2 = cnd( true(), var( y ), false() ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_cnd_else_expr() ->
  E1 = cnd( true(), true(), var( x ) ),
  E2 = cnd( true(), true(), var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_negation_operand() ->
  E1 = neg( var( x ) ),
  E2 = neg( false() ),
  ?assertEqual( E2, subst( E1, x, false() ) ).

substitution_propagates_to_conjunction_lhs() ->
  E1 = conj( var( x ), var( y ) ),
  E2 = conj( false(), var( y ) ),
  ?assertEqual( E2, subst( E1, x, false() ) ).

substitution_propagates_to_conjunction_rhs() ->
  E1 = conj( var( x ), var( y ) ),
  E2 = conj( var( x ), false() ),
  ?assertEqual( E2, subst( E1, y, false() ) ).

substitution_propagates_to_disjunction_lhs() ->
  E1 = disj( var( x ), var( y ) ),
  E2 = disj( false(), var( y ) ),
  ?assertEqual( E2, subst( E1, x, false() ) ).

substitution_propagates_to_disjunction_rhs() ->
  E1 = disj( var( x ), var( y ) ),
  E2 = disj( var( x ), false() ),
  ?assertEqual( E2, subst( E1, y, false() ) ).

subst_leaves_nonmatching_var_alone() ->
  E = var( x ),
  ?assertEqual( E, subst( E, y, var( z ) ) ).

matching_var_is_substituted() ->
  E1 = var( x ),
  E2 = var( y ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_lam_ntv_body() ->
  E1 = lam_ntv( [], var( x ) ),
  E2 = lam_ntv( [], var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_shadowed_by_bound_var() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ),
  ?assertMatch( {lam_ntv, na, [{X, x, 'Str'}], {var, na, X}},
                subst( E, x, var( y ) ) ).

subst_is_capture_avoiding() ->
  E = lam_ntv( [lam_ntv_arg( x, t_str() )], var( y ) ),
  ?assertNotMatch( {lam_ntv, na, [{X, x, 'Str'}], {var, na, X}},
                   subst( E, y, var( x ) ) ).

subst_retains_order_of_lam_ntv_arg_lst() ->
  ?assertMatch( {lam_ntv, na, [{X, x, 'Str'},
                               {_, y, 'File'}], {var, na, X}},
                subst( e_lam1(), a, var( b ) ) ).

substitution_leaves_foreign_lambda_alone() ->
  E = lam_frn( f, [], t_rcd( [t_arg( a, t_str() )] ), l_bash(), <<"blub">> ),
  ?assertMatch( E, subst( E, x, var( y ) ) ).

subst_propagates_to_app_function() ->
  E1 = app( var( x ), [] ),
  E2 = app( var( y ), [] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

subst_propagates_to_e_bind_lst() ->
  E1 = app( var( f ), [e_bind( x, var( x ) )] ),
  E2 = app( var( f ), [e_bind( x, var( y ) )] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_leaves_futures_alone() ->
  E = {fut, na, t_str(), na},
  ?assertEqual( E, subst( E, x, var( y ) ) ).

substitution_leaves_empty_list_alone() ->
  E = lst( t_str(), [] ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).

substitution_propagates_to_list_elements() ->
  E1 = lst( t_str(), [var( x )] ),
  E2 = lst( t_str(), [var( y )] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_append_lhs() ->
  E1 = append( var( x ), var( y ) ),
  E2 = append( var( z ), var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( z ) ) ).

substitution_propagates_to_append_rhs() ->
  E1 = append( var( x ), var( y ) ),
  E2 = append( var( x ), var( z ) ),
  ?assertEqual( E2, subst( E1, y, var( z ) ) ).

substitution_propagates_to_isnil_operand() ->
  E1 = isnil( var( x ) ),
  E2 = isnil( var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_record_fields() ->
  E1 = rcd( [e_bind( x, var( x ) )] ),
  E2 = rcd( [e_bind( x, var( y ) )] ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_projection_operand() ->
  E1 = proj( x, var( x ) ),
  E2 = proj( x, var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_fixpoint_operand() ->
  E1 = fix( var( x ) ),
  E2 = fix( var( y ) ),
  ?assertEqual( E2, subst( E1, x, var( y ) ) ).

substitution_propagates_to_for_body() ->
  E = for( t_str(), [e_bind( x, var( x_lst ) )], var( y ) ),
  ?assertMatch( {for, na, 'Str', [{_, {var, na, x_lst}}], {var, na, z}},
                subst( E, y, var( z ) ) ).

substitution_propagates_to_for_list_expression() ->
  E = for( t_str(), [e_bind( x, var( x_lst ) )], var( y ) ),
  ?assertMatch( {for, na, 'Str', [{_, {var, na, z}}], {var, na, y}},
                subst( E, x_lst, var( z ) ) ).

for_iterator_shadows_substitution() ->
  E = for( t_str(), [e_bind( x, var( x_lst ) )], var( x ) ),
  ?assertMatch( {for, na, 'Str', [{X, {var, na, x_lst}}], {var, na, X}},
                subst( E, x, var( z ) ) ).

for_iterator_is_capture_avoiding() ->
  E = for( t_str(), [e_bind( x, var( x_lst ) )], var( y ) ),
  ?assertNotMatch( {for, na, 'Str', [{X, {var, na, x_lst}}], {var, na, X}},
                   subst( E, y, var( x ) ) ).

for_iterator_retains_order_of_list_bindings() ->
  E = for( t_str(), [e_bind( x, var( x_lst ) ),
                     e_bind( y, var( y_lst ) )], var( z ) ),

  ?assertMatch( {for, na, 'Str', [{_, {var, na, x_lst}},
                                  {_, {var, na, y_lst}}], {str, na, <<"bla">>}},
                subst( E, z, str( <<"bla">> ) ) ).

substitution_propagates_to_fold_body() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertMatch( {fold, na, {_, {var, na, x0}},
                           {_, {var, na, x_lst}}, {var, na, z}},
                subst( E, y, var( z ) ) ).

substitution_propagates_to_accumulator_init_expression() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertMatch( {fold, na, {_, {var, na, z}},
                           {_, {var, na, x_lst}}, {var, na, y}},
                subst( E, x0, var( z ) ) ).

substitution_propagates_to_list_expression() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertMatch( {fold, na, {_, {var, na, x0}}, {_, {var, na, z}}, {var, na, y}},
                subst( E, x_lst, var( z ) ) ).

fold_accumulator_shadows_substitution() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( x_acc ) ),
  ?assertMatch( {fold, na,
                       {X0, {var, na, x0}},
                       {_, {var, na, x_lst}},
                       {var, na, X0}},
                subst( E, x_acc, var( z ) ) ).

fold_accumulator_is_capture_avoiding() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( x ) ),
  ?assertMatch( {fold, na,
                       {_, {var, na, x0}},
                       {X, {var, na, x_lst}},
                       {var, na, X}},
                subst( E, x, var( z ) ) ).

fold_list_expression_shadows_substitution() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertNotMatch( {fold, na,
                          {X0, {var, na, x0}},
                          {_, {var, na, x_lst}},
                          {var, na, X0}},
                subst( E, y, var( x0 ) ) ).

fold_list_expression_is_capture_avoiding() ->
  E = fold( e_bind( x_acc, var( x0 ) ),
            e_bind( x, var( x_lst ) ), var( y ) ),
  ?assertNotMatch( {fold, na,
                          {_, {var, na, x0}},
                          {X, {var, na, x_lst}},
                          {var, na, X}},
                subst( E, y, var( x ) ) ).

substitution_leaves_error_alone() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( E, subst( E, x, var( y ) ) ).


gensym_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"gensym adds unique number to var",
     fun gensym_adds_unique_number_to_var/0},

    {"gensym replaces unique number instead of appending",
     fun gensym_replaces_unique_number_instead_of_appending/0}
   ]
  }.

gensym_adds_unique_number_to_var() ->
  X = gensym( blub ),
  [A, _] = string:tokens( atom_to_list( X ), "$" ),
  ?assertEqual( "blub", A ).

gensym_replaces_unique_number_instead_of_appending() ->
  X1 = gensym( blub ),
  X2 = gensym( X1 ),
  [A, _] = string:tokens( atom_to_list( X2 ), "$" ),
  ?assertEqual( "blub", A ).

%%====================================================================
%% Evaluation contexts
%%====================================================================

in_hole_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"inserting in the empty context returns the original expression",
     fun insert_in_empty_ctx_returns_original_expr/0},

    {"inserting leaves string literal unchanged",
     fun inserting_leaves_string_literal_unchanged/0},

    {"inserting traverses comparison lhs",
     fun inserting_traverses_comparison_lhs/0},

    {"inserting traverses comparison rhs",
     fun inserting_traverses_comparison_rhs/0},

    {"inserting leaves file unchanged",
     fun inserting_leaves_file_unchanged/0},

    {"inserting leaves true unchanged",
     fun inserting_leaves_true_unchanged/0},

    {"inserting leaves false unchanged",
     fun inserting_leaves_false_unchanged/0},

    {"inserting leaves error unchanged",
     fun inserting_leaves_error_unchanged/0},

    {"inserting traverses conditional's if expression",
     fun insert_traverses_cnd_if_expr/0},

    {"inserting traverses negation operand",
     fun inserting_traverses_negation_operand/0},

    {"inserting traverses conjunction lhs",
     fun inserting_traverses_conjunction_lhs/0},

    {"inserting traverses conjunction rhs",
     fun inserting_traverses_conjunction_rhs/0},

    {"inserting traverses disjunction lhs",
     fun inserting_traverses_disjunction_lhs/0},

    {"inserting traverses disjunction rhs",
     fun inserting_traverses_disjunction_rhs/0},

    {"inserting leaves variable unchanged",
     fun inserting_leaves_variable_unchanged/0},

    {"inserting leaves native function unchanged",
     fun inserting_leaves_native_function_unchanged/0},

    {"inserting leaves foreign function unchanged",
     fun inserting_leaves_foreign_function_unchanged/0},

    {"inserting traverses application function position",
     fun inserting_traverses_application_function_position/0},

    {"inserting traverses application argument bindings",
     fun inserting_traverses_application_argument_bindings/0},

    {"inserting leaves future unchanged",
     fun inserting_leaves_future_unchanged/0},

    {"inserting traverses list elements",
     fun inserting_traverses_list_elements/0},

    {"inserting traverses list append lhs",
     fun inserting_traverses_list_append_lhs/0},

    {"inserting traverses list append rhs",
     fun inserting_traverses_list_append_rhs/0},

    {"inserting traverses isnil operand",
     fun inserting_traverses_isnil_operand/0},

    {"inserting traverses for list expressions",
     fun inserting_traverses_for_list_expressions/0},

    {"inserting traverses fold list expression",
     fun inserting_traverses_fold_list_expression/0},

    {"inserting traverses record fields",
     fun inserting_traverses_record_fields/0},

    {"inserting traverses projection operand",
     fun inserting_traverses_projection_operand/0},

    {"inserting traverses fixpoint operand",
     fun inserting_traverses_fixpoint_operand/0}

   ]
  }.

insert_in_empty_ctx_returns_original_expr() ->
  E = str( <<"blub">> ),
  ?assertEqual( E, in_hole( E, hole ) ).

inserting_leaves_string_literal_unchanged() ->
  ?assertEqual( str( <<"blub">> ), in_hole( true(), str( <<"blub">> ) ) ).

inserting_traverses_comparison_lhs() ->
  E1 = str( <<"bla">> ),
  Ctx = {cmp, na, hole, str( <<"blub">> )},
  ?assertEqual( cmp( E1, str( <<"blub">> ) ), in_hole( E1, Ctx ) ).

inserting_traverses_comparison_rhs() ->
  E1 = str( <<"blub">> ),
  Ctx = {cmp, na, str( <<"bla">> ), hole},
  ?assertEqual( cmp( str( <<"bla">> ), E1 ), in_hole( E1, Ctx ) ).

inserting_leaves_file_unchanged() ->
  E = file( <<"bla.txt">> ),
  ?assertEqual( E, in_hole( true(), E ) ).

inserting_leaves_true_unchanged() ->
  ?assertEqual( true(), in_hole( str( <<"blub">> ), true() ) ).

inserting_leaves_false_unchanged() ->
  ?assertEqual( false(), in_hole( str( <<"blub">> ), false() ) ).

inserting_leaves_error_unchanged() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( E, in_hole( str( <<"blub">> ), E ) ).

insert_traverses_cnd_if_expr() ->
  E1 = true(),
  Ctx = {cnd, na, hole, str( <<"bla">> ), str( <<"blub">> )},
  E2 = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_negation_operand() ->
  E1 = true(),
  Ctx = {neg, na, hole},
  E2 = neg( E1 ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_conjunction_lhs() ->
  E1 = true(),
  Ctx = {conj, na, hole, false()},
  E2 = conj( E1, false() ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_conjunction_rhs() ->
  E1 = false(),
  Ctx = {conj, na, true(), hole},
  E2 = conj( true(), E1 ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_disjunction_lhs() ->
  E1 = true(),
  Ctx = {disj, na, hole, false()},
  E2 = disj( E1, false() ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_disjunction_rhs() ->
  E1 = false(),
  Ctx = {disj, na, true(), hole},
  E2 = disj( true(), E1 ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_leaves_variable_unchanged() ->
  ?assertEqual( var( x ), in_hole( str( <<"blub">> ), var( x ) ) ).

inserting_leaves_native_function_unchanged() ->
  ?assertEqual( e_lam1(), in_hole( true(), e_lam1() ) ).

inserting_leaves_foreign_function_unchanged() ->
  Ctx = lam_frn( f, [], t_rcd( [t_arg( a, t_str() )] ), l_bash(), <<"blub">> ),
  ?assertEqual( Ctx, in_hole( str( <<"blub">> ), Ctx ) ).

inserting_traverses_application_function_position() ->
  E1 = var( f ),
  Ctx = app( hole, [] ),
  E2 = app( var( f ), [] ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_application_argument_bindings() ->
  E1 = var( x ),
  Ctx = {app, na, var( f ), [{x, hole}]},
  E2 = app( var( f ), [e_bind( x, E1 )] ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_leaves_future_unchanged() ->
  E = {fut, na, t_str(), na},
  ?assertEqual( E, in_hole( true(), E ) ).

inserting_traverses_list_elements() ->
  E1 = var( x ),
  Ctx = {cons, na, t_str(), hole, null( t_str() )},
  E2 = lst( t_str(), [E1] ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_list_append_lhs() ->
  E1 = var( x ),
  Ctx = {append, na, hole, var( y )},
  E2 = append( E1, var( y ) ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_list_append_rhs() ->
  E1 = var( x ),
  Ctx = {append, na, var( y ), hole},
  E2 = append( var( y ), E1 ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_isnil_operand() ->
  E1 = var( x ),
  Ctx = {isnil, na, hole},
  ?assertEqual( isnil( E1 ), in_hole( E1, Ctx ) ).

inserting_traverses_for_list_expressions() ->
  E1 = var( l ),
  Ctx = {for, na, 'Str', [{x, hole}], var( x )},
  ?assertEqual( for( t_str(), [e_bind( x, E1 )], var( x ) ), in_hole( E1, Ctx ) ).

inserting_traverses_fold_list_expression() ->
  E1 = var( l ),
  Ctx = {fold, na, {x_acc, var( x_acc )}, {x, hole}, var( x )},
  E2 = fold( e_bind( x_acc, var( x_acc ) ), e_bind( x, var( l ) ), var( x ) ),
  ?assertEqual( E2, in_hole( E1, Ctx ) ).

inserting_traverses_record_fields() ->
  E1 = var( x ),
  Ctx = {rcd, na, [{a, hole}]},
  ?assertEqual( rcd( [e_bind( a, E1 )] ), in_hole( E1, Ctx ) ).

inserting_traverses_projection_operand() ->
  E1 = var( x ),
  Ctx = {proj, na, a, hole},
  ?assertEqual( proj( a, E1 ), in_hole( E1, Ctx ) ).

inserting_traverses_fixpoint_operand() ->
  E1 = var( x ),
  Ctx = {fix, na, hole},
  ?assertEqual( fix( E1 ), in_hole( E1, Ctx ) ).


find_context_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"string string is no redex",
     fun string_is_no_redex/0},

    {"comparison with two value operands is redex",
     fun comparison_with_two_value_operands_is_redex/0},

    {"find_context traverses comparison lhs",
     fun find_context_traverses_comparison_lhs/0},

    {"find_context traverses comparison rhs",
     fun find_context_traverses_comparison_rhs/0},

    {"file is no redex",
     fun file_is_no_redex/0},
      
    {"true is no redex",
     fun true_is_no_redex/0},
      
    {"false is no redex",
     fun false_is_no_redex/0},

    {"conditional with true if expression is redex",
     fun cnd_with_true_if_expr_is_redex/0},

    {"conditional with false if expression is redex",
     fun cnd_with_false_if_expr_is_redex/0},

    {"find_context traverses conditional's if expression",
     fun find_context_traverses_cnd_if_expr/0},

    {"negation with value operand is redex",
     fun negation_with_value_operand_is_redex/0},

    {"find_context traverses negation operand",
     fun find_context_traverses_negation_operand/0},

    {"conjunction with value operands is redex",
     fun conjunction_with_value_operands_is_redex/0},

    {"find_context traverses conjunction lhs",
     fun find_context_traverses_conjunction_lhs/0},

    {"find_context traverses conjunction rhs",
     fun find_context_traverses_conjunction_rhs/0},

    {"disjunction with value operands is redex",
     fun disjunction_with_value_operands_is_redex/0},

    {"find_context traverses disjunction lhs",
     fun find_context_traverses_disjunction_lhs/0},

    {"find_context traverses disjunction rhs",
     fun find_context_traverses_disjunction_rhs/0},

    {"var is no redex",
     fun var_is_no_redex/0},

    {"native function is no redex",
     fun lam_ntv_is_no_redex/0},

    {"foreign function is no redex",
     fun foreign_function_is_no_redex/0},

    {"application with native function is redex",
     fun app_with_lam_ntv_is_redex/0},

    {"application with foreign function and no arguments is redex",
     fun application_with_foreign_function_and_no_arguments_is_redex/0},

    {"application with foreign function and value argument is redex",
     fun application_with_foreign_function_and_value_argument_is_redex/0},

    {"application with foreign function traverses arguments",
     fun application_with_foreign_function_traverses_arguments/0},

    {"find_context traverses application's function position",
     fun find_context_traverses_app_fn_pos/0},

    {"future is no redex",
     fun future_is_no_redex/0},

    {"empty list is no redex",
     fun empty_list_is_no_redex/0},

    {"find_context traverses list elements",
     fun find_context_traverses_list_elements/0},

    {"list append with value operands is redex",
     fun list_append_with_value_operands_is_redex/0},

    {"list append with literal list operands is redex",
     fun list_append_with_literal_list_operands_is_redex/0},

    {"find_context traverses list append lhs",
     fun find_context_traverses_list_append_lhs/0},

    {"find_context traverses list append rhs",
     fun find_context_traverses_list_append_rhs/0},

    {"isnil with value operand is redex",
     fun isnil_with_value_operand_is_redex/0},

    {"isnil with literal list operand is redex",
     fun isnil_with_literal_list_operand_is_redex/0},

    {"find_context traverses isnil operand",
     fun find_context_traverses_isnil_operand/0},

    {"find_context traverses record fields",
     fun find_context_traverses_record_fields/0},

    {"projection with value operand is redex",
     fun projection_with_value_operand_is_redex/0},

    {"projection with literal record operand is redex",
     fun projection_with_literal_record_operand_is_redex/0},

    {"find_context traverses projection operand",
     fun find_context_traverses_projection_operand/0},

    {"fixpoint operator with value operand is redex",
     fun fixpoint_operator_with_value_operand_is_redex/0},

    {"find_context traverses fixpoint operand",
     fun find_context_traverses_fixpoint_operand/0},

    {"for with value list expression is redex",
     fun for_with_value_list_expression_is_redex/0},

    {"for with literal list expression is redex",
     fun for_with_literal_list_expression_is_redex/0},

    {"find_context traverses for list expression",
     fun find_context_traverses_for_list_expression/0},

    {"fold with value list expression is redex",
     fun fold_with_value_list_expression_is_redex/0},

    {"fold with literal list expression is redex",
     fun fold_with_literal_list_expression_is_redex/0},

    {"find_context traverses fold list expression",
     fun find_context_traverses_fold_list_expression/0},

    {"error is redex",
     fun error_is_redex/0}
   ]}.
    

string_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( str( <<"blub">> ) ) ).

comparison_with_two_value_operands_is_redex() ->
  E = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_comparison_lhs() ->
  ECnd = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  E = cmp( ECnd, str( <<"z">> ) ),
  Ctx = {cmp, na, hole, str( <<"z">> )},
  ?assertEqual( {ok, ECnd, Ctx}, find_context( E ) ).

find_context_traverses_comparison_rhs() ->
  ECnd = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  E = cmp( str( <<"z">> ), ECnd ),
  Ctx = {cmp, na, str( <<"z">> ), hole},
  ?assertEqual( {ok, ECnd, Ctx}, find_context( E ) ).

file_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( file( <<"blub">> ) ) ).

true_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( true() ) ).

false_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( false() ) ).

cnd_with_true_if_expr_is_redex() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

cnd_with_false_if_expr_is_redex() ->
  E = cnd( false(), str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_cnd_if_expr() ->
  E = cnd( true(), true(), false() ),
  Ctx = cnd( hole, str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

negation_with_value_operand_is_redex() ->
  E = neg( true() ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_negation_operand() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = neg( hole ),
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

conjunction_with_value_operands_is_redex() ->
  E = conj( true(), false() ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_conjunction_lhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {conj, na, hole, false()},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

find_context_traverses_conjunction_rhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {conj, na, true(), hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

disjunction_with_value_operands_is_redex() ->
  E = disj( true(), false() ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_disjunction_lhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {disj, na, hole, false()},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

find_context_traverses_disjunction_rhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {disj, na, true(), hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

var_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( var( x ) ) ).

lam_ntv_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( e_lam1() ) ).

foreign_function_is_no_redex() ->
  E = lam_frn( f, [], t_rcd( [t_arg( a, t_str() )] ), l_bash(), <<"blub">> ),
  ?assertEqual( no_ctx, find_context( E ) ).

app_with_lam_ntv_is_redex() ->
  E = e_app_id(),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

application_with_foreign_function_and_no_arguments_is_redex() ->
  E = app(
        lam_frn( f, [], t_rcd( [t_arg( a, t_str() )] ), l_bash(), <<"blub">> ),
        [] ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

application_with_foreign_function_and_value_argument_is_redex() ->
  E = app(
        lam_frn( f, [t_arg( x, t_bool() )], t_rcd( [t_arg( a, t_str() )] ),
                 l_bash(), <<"blub">> ),
        [e_bind( x, true() )] ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

application_with_foreign_function_traverses_arguments() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {app, na,
          {lam_frn, na, f, [t_arg( x, t_bool() )],
                    t_rcd( [t_arg( a, t_str() )] ), l_bash(), <<"blub">>},
          [{x, hole}]},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

find_context_traverses_app_fn_pos() ->
  E = cnd( true(), e_lam_const(), e_lam_const() ),
  Ctx = app( hole, [] ),
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

future_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( {fut, na, t_str(), na} ) ).

empty_list_is_no_redex() ->
  ?assertEqual( no_ctx, find_context( lst( t_str(), [] ) ) ).

find_context_traverses_list_elements() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {cons, na, t_str(), hole, null( t_str() )},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

list_append_with_value_operands_is_redex() ->
  E = append( lst( t_str(), [] ), lst( t_str(), [] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

list_append_with_literal_list_operands_is_redex() ->
  E = append( lst( t_bool(), [cmp( str( <<"bla">> ), str( <<"blub">> ) )] ),
              lst( t_bool(), [cmp( true(), false() )] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_list_append_lhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {append, na, hole, var( x )},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

find_context_traverses_list_append_rhs() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {append, na, var( x ), hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

isnil_with_value_operand_is_redex() ->
  E = isnil( lst( t_str(), [] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

isnil_with_literal_list_operand_is_redex() ->
  E = isnil( lst( t_bool(), [cmp( str( <<"bla">> ), str( <<"blub">> ) )] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_isnil_operand() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {isnil, na, hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

find_context_traverses_record_fields() ->
  E = cnd( true(), str( <<"bla">> ), str( <<"blub">> ) ),
  Ctx = {rcd, na, [{a, hole}]},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

projection_with_value_operand_is_redex() ->
  E = proj( x, rcd( [e_bind( x, str( <<"blub">> ) )] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

projection_with_literal_record_operand_is_redex() ->
  E = proj( x, rcd( [e_bind( x, cmp( true(), false() ) )] ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_projection_operand() ->
  E = cnd( true(), rcd( [e_bind( x, str( <<"bla">> ) )] ),
                   rcd( [e_bind( x, str( <<"blub">> ) )] ) ),
  Ctx = {proj, na, x, hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

fixpoint_operator_with_value_operand_is_redex() ->
  E = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )], str( <<"blub">> ) ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_fixpoint_operand() ->
  E = cnd( true(),
           lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                    str( <<"bla">> ) ),
           lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                    str( <<"blub">> ) ) ),
  Ctx = {fix, na, hole},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

for_with_value_list_expression_is_redex() ->
  E = for( t_str(), [e_bind( x, lst( t_str(), [str( <<"bla">> )] ) )], var( x ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

for_with_literal_list_expression_is_redex() ->
  E = for( t_bool(), [e_bind( x, lst( t_bool(), [neg( true() )] ) )], var( x ) ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).

find_context_traverses_for_list_expression() ->
  E = append( lst( t_str(), [str( <<"bla">> )] ), lst( t_str(), [str( <<"blub">> )] ) ),
  Ctx = {for, na, t_str(), [{x, hole}], var( x )},
  ?assertEqual( {ok, E, Ctx}, find_context( in_hole( E, Ctx ) ) ).

fold_with_value_list_expression_is_redex() ->
  ELst = lst( t_bool(), [true()] ),
  EFold = fold( e_bind( x_acc, true() ), e_bind( x, ELst ), var( x ) ),
  ?assertEqual( {ok, EFold, hole}, find_context( EFold ) ).


fold_with_literal_list_expression_is_redex() ->
  ELst = lst( t_bool(), [neg( true() )] ),
  EFold = fold( e_bind( x_acc, true() ), e_bind( x, ELst ), var( x ) ),
  ?assertEqual( {ok, EFold, hole}, find_context( EFold ) ).

find_context_traverses_fold_list_expression() ->
  ELst = cnd( true(), lst( t_bool(), [true()] ), lst( t_bool(), [false()] ) ),
  Ctx = {fold, na, {x_acc, true()}, {x, hole}, var( x )},
  ?assertEqual( {ok, ELst, Ctx}, find_context( in_hole( ELst, Ctx ) ) ).

error_is_redex() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( {ok, E, hole}, find_context( E ) ).


subst_fut_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"subst_fut leaves string literal alone",
     fun subst_fut_leaves_string_literal_alone/0},

    {"subst_fut leaves file literal alone",
     fun subst_fut_leaves_file_literal_alone/0},

    {"subst_fut leaves true alone",
     fun subst_fut_leaves_true_alone/0},

    {"subst_fut leaves false alone",
     fun subst_fut_leaves_false_alone/0},

    {"subst_fut alters matching future",
     fun subst_fut_alters_matching_future/0},

    {"subst_fut leaves non-matching future alone",
     fun subst_fut_leaves_nonmatching_future_alone/0},

    {"subst_fut traverses comparison lhs",
     fun subst_fut_traverses_comparison_lhs/0},

    {"subst_fut traverses comparison rhs",
     fun subst_fut_traverses_comparison_rhs/0},

    {"subst_fut traverses condition if expression",
     fun subst_fut_traverses_condition_if_term/0},

    {"subst_fut traverses condition then expression",
     fun subst_fut_traverses_condition_then_expression/0},

    {"subst_fut traverses condition else expression",
     fun subst_fut_traverses_condition_else_expression/0},

    {"subst_fut traverses negation",
     fun subst_fut_traverses_negation/0},

    {"subst_fut traverses conjunction lhs",
     fun subst_fut_traverses_conjunction_lhs/0},

    {"subst_fut traverses conjunction rhs",
     fun subst_fut_traverses_conjunction_rhs/0},

    {"subst_fut traverses disjunction lhs",
     fun subst_fut_traverses_disjunction_lhs/0},

    {"subst_fut traverses disjunction rhs",
     fun subst_fut_traverses_disjunction_rhs/0},

    {"subst_fut leaves variable alone",
     fun subst_fut_leaves_variable_alone/0},

    {"subst_fut leaves native lambda alone",
     fun subst_fut_leaves_native_lambda_alone/0},

    {"subst_fut leaves foreign lambda alone",
     fun subst_fut_leaves_foreign_lambda_alone/0},

    {"subst_fut traverses application function position",
     fun subst_fut_traverses_application_function_position/0},

    {"subst_fut traverses application argument bindings",
     fun subst_fut_traverses_application_argument_bindings/0},

    {"subst_fut leaves null alone",
     fun subst_fut_leaves_null_alone/0},

    {"subst_fut traverses cons lhs",
     fun subst_fut_traverses_cons_lhs/0},

    {"subst_fut traverses cons rhs",
     fun subst_fut_traverses_cons_rhs/0},

    {"subst_fut traverses append lhs",
     fun subst_fut_traverses_append_lhs/0},

    {"subst_fut traverses append rhs",
     fun subst_fut_traverses_append_rhs/0},

    {"subst_fut traverses isnil operand",
     fun subst_fut_traverses_isnil_operand/0},

    {"subst_fut traverses record fields",
     fun subst_fut_traverses_record_fields/0},

    {"subst_fut traverses projection operand",
     fun subst_fut_traverses_projection_operand/0},

    {"subst_fut traverses fixpoint operand",
     fun subst_fut_traverses_fixpoint_operand/0},

    {"subst_fut leaves error alone",
     fun subst_fut_leaves_error_alone/0},

    {"subst_fut traverses for list expression",
     fun subst_fut_traverses_for_list_expression/0},

    {"subst_fut traverses fold list expression",
     fun subst_fut_traverses_fold_list_expression/0}
   ]
  }.

subst_fut_leaves_string_literal_alone() ->
  E = str( <<"blub">> ),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, A, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_file_literal_alone() ->
  E = file( <<"blub.txt">> ),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, A, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_true_alone() ->
  E = true(),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, A, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_false_alone() ->
  E = false(),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, A, file( <<"idx.tar">> ) ) ).

subst_fut_alters_matching_future() ->
  Info = 12,
  E1 = {fut, Info, t_file(), <<"1234">>},
  E2 = file( Info, <<"idx.tar">> ),
  A = <<"1234">>,
  ?assertEqual( E2, subst_fut( E1, A, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_nonmatching_future_alone() ->
  E = {fut, na, t_file(), <<"123">>},
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, A, file( <<"idx.tar">> ) ) ).

subst_fut_traverses_comparison_lhs() ->
  E1 = cmp( {fut, na, t_str(), <<"1234">>}, str( <<"bla">> ) ),
  E2 = cmp( str( <<"blub">> ), str( <<"bla">> ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"blub">> ) ) ).

subst_fut_traverses_comparison_rhs() ->
  E1 = cmp( str( <<"bla">> ), {fut, na, t_str(), <<"1234">>} ),
  E2 = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"blub">> ) ) ).

subst_fut_traverses_condition_if_term() ->
  E1 = cnd( {fut, na, t_bool(), <<"1234">>}, str( <<"bla">>), str( <<"blub">> ) ),
  E2 = cnd( true(), str( <<"bla">>), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_traverses_condition_then_expression() ->
  E1 = cnd( true(), {fut, na, t_str(), <<"1234">>}, str( <<"blub">> ) ),
  E2 = cnd( true(), str( <<"bla">>), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"bla">>) ) ).

subst_fut_traverses_condition_else_expression() ->
  E1 = cnd( true(), str( <<"bla">> ), {fut, na, t_str(), <<"1234">>} ),
  E2 = cnd( true(), str( <<"bla">>), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"blub">>) ) ).

subst_fut_traverses_negation() ->
  E1 = neg( {fut, na, t_bool(), <<"1234">>} ),
  E2 = neg( true() ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_traverses_conjunction_lhs() ->
  E1 = conj( {fut, na, t_bool(), <<"1234">>}, false() ),
  E2 = conj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_traverses_conjunction_rhs() ->
  E1 = conj( true(), {fut, na, t_bool(), <<"1234">>} ),
  E2 = conj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, false() ) ).

subst_fut_traverses_disjunction_lhs() ->
  E1 = disj( {fut, na, t_bool(), <<"1234">>}, false() ),
  E2 = disj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_traverses_disjunction_rhs() ->
  E1 = disj( true(), {fut, na, t_bool(), <<"1234">>} ),
  E2 = disj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, false() ) ).

subst_fut_leaves_variable_alone() ->
  E = var( x ),
  ?assertEqual( E, subst_fut( E, <<"1234">>, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_native_lambda_alone() ->
  E = e_lam_const(),
  ?assertEqual( E, subst_fut( E, <<"1234">>, file( <<"idx.tar">> ) ) ).

subst_fut_leaves_foreign_lambda_alone() ->
  E = e_lam_greet(),
  ?assertEqual( E, subst_fut( E, <<"1234">>, file( <<"idx.tar">> ) ) ).

subst_fut_traverses_application_function_position() ->
  E1 = app( cnd( {fut, na, t_bool(), <<"1234">>}, var( f ), var( g ) ), [] ),
  E2 = app( cnd( true(), var( f ), var( g ) ), [] ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_traverses_application_argument_bindings() ->
  E1 = app( var( z ), [e_bind( x, {fut, na, t_str(), <<"1234">>} )] ),
  E2 = app( var( z ), [e_bind( x, str( <<"bla">> ) )] ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"bla">> ) ) ).

subst_fut_leaves_null_alone() ->
  E = null( t_str() ),
  ?assertEqual( E, subst_fut( E, <<"1234">>, file( <<"idx.tar">> ) ) ).

subst_fut_traverses_cons_lhs() ->
  E1 = cons( t_str(), {fut, na, t_str(), <<"1234">>}, null( t_str() ) ),
  E2 = cons( t_str(), str( <<"bla">> ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"bla">> ) ) ).

subst_fut_traverses_cons_rhs() ->
  E1 = cons( t_str(), var( x ), cons( t_str(), {fut, na, t_str(), <<"1234">>}, null( t_str() ) ) ),
  E2 = cons( t_str(), var( x ), cons( t_str(), str( <<"bla">> ), null( t_str() ) ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, str( <<"bla">> ) ) ).

subst_fut_traverses_append_lhs() ->
  E1 = append( {fut, na, t_lst( t_str() ), <<"1234">>}, null( t_str() ) ),
  E2 = append( null( t_str() ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).

subst_fut_traverses_append_rhs() ->
  E1 = append( null( t_str() ), {fut, na, t_lst( t_str() ), <<"1234">>} ),
  E2 = append( null( t_str() ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).

subst_fut_traverses_isnil_operand() ->
  E1 = isnil( {fut, na, t_lst( t_str() ), <<"1234">>} ),
  E2 = isnil( null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).

subst_fut_traverses_record_fields() ->
  E1 = rcd( [e_bind( a, {fut, na, t_lst( t_str() ), <<"1234">>} )] ),
  E2 = rcd( [e_bind( a, null( t_str() ) )] ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).

subst_fut_traverses_projection_operand() ->
  E1 = proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), <<"1234">>} ),
  E2 = proj( a, rcd( [e_bind( a, str( <<"blub">> ) )] ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, rcd( [e_bind( a, str( <<"blub">> ) )] ) ) ).

subst_fut_traverses_fixpoint_operand() ->
  E1 = fix( cnd( {fut, na, t_bool(), <<"1234">>}, var( f ), var( g ) ) ),
  E2 = fix( cnd( true(), var( f ), var( g ) ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, true() ) ).

subst_fut_leaves_error_alone() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( E, subst_fut( E, <<"1234">>, file( <<"idx.tar">> ) ) ).

subst_fut_traverses_for_list_expression() ->
  E1 = for( t_str(), [e_bind( x, {fut, na, t_lst( t_str() ), <<"1234">>} )], var( x ) ),
  E2 = for( t_str(), [e_bind( x, null( t_str() ) )], var( x ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).

subst_fut_traverses_fold_list_expression() ->
  E1 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, {fut, na, t_lst( t_str() ), <<"1234">>} ), var( x ) ),
  E2 = fold( e_bind( x_acc, var( x0 ) ), e_bind( x, null( t_str() ) ), var( x ) ),
  ?assertEqual( E2, subst_fut( E1, <<"1234">>, null( t_str() ) ) ).


set_info_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"set info on error expression",
     fun set_info_on_error_expression/0}
   ]}.

set_info_on_error_expression() ->
  E1 = err( na, t_str(), <<"blub">> ),
  E2 = err( 50, t_str(), <<"blub">> ),
  ?assertEqual( E2, set_info( E1, 50 ) ).
