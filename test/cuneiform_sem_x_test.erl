%% -*- erlang -*-
%%
%% cf_client
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
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
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

-define( CUNEIFORM_SEM, cuneiform_sem_stx ).


%%====================================================================
%% Includes
%%====================================================================

-include_lib( "eunit/include/eunit.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( ?CUNEIFORM_SEM, [step/1] ).

-import( cuneiform_lang, [t_str/0, t_bool/0, t_fn/3, t_rcd/1] ).
-import( cuneiform_lang, [lam_ntv_arg/2, e_bind/2, t_arg/2] ).
-import( cuneiform_lang, [l_bash/0] ).
-import( cuneiform_lang, [
                          str/1, true/0, false/0, cnd/3, var/1, lam_frn/5,
                          lam_ntv/2, app/2, cmp/2, neg/1, conj/2, disj/2,
                          lst/2, append/2, isnil/1, for/3, fold/3, err/2,
                          rcd/1, proj/2, fix/1, null/1, cons/2
                         ] ).

 
%%====================================================================
%% Expression definitions
%%====================================================================

e_lam_const() ->
  lam_ntv( [], str( <<"blub">> ) ).

e_lam_id() ->
  lam_ntv( [lam_ntv_arg( x, t_str() )], var( x ) ).

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

    {"append nil and list reduces to list",
     fun append_nil_and_list_reduces_to_list/0},

    {"append cons list1 and list2 reduces to cons append list1 and list2",
     fun append_cons_list1_and_list2_reduces_to_cons_append_list1_and_list2/0},

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
     fun fixpoint_operator_inserts_function_definition/0},

    {"for over empty list reduces to empty list",
     fun for_over_empty_list_reduces_to_empty_list/0},

    {"folding over empty list reduces to initial accumulator",
     fun folding_over_empty_list_reduces_to_initial_accumulator/0},

    {"foreign function application reduces to future",
     fun foreign_function_application_reduces_to_future/0},

    {"nested error reduces to error",
     fun nested_error_reduces_to_error/0}
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

negation_with_true_operand_reduces_to_false() ->
  ?assertEqual(
    {ok, false(), []},
    step( neg( true() ) ) ).

negation_with_false_operand_reduces_to_true() ->
  ?assertEqual(
    {ok, true(), []},
    step( neg( false() ) ) ).

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

nat_app_without_arg_reduces_to_lam_body() ->
  E1 = app( e_lam_const(), [] ),
  E2 = str( <<"blub">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

nat_app_with_single_arg_reduces_to_empty_application() ->
  E1 = e_app_id(),
  E2 = app( lam_ntv( [], str( <<"blub">> ) ), [] ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

append_nil_and_list_reduces_to_list() ->
  E2 = lst( t_bool(), [false(), true()] ),
  E1 = append( null( t_bool() ), E2 ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

append_cons_list1_and_list2_reduces_to_cons_append_list1_and_list2() ->
  L2 = lst( t_bool(), [false(), true()] ),
  E1 = append( lst( t_bool(), [true(), false()] ), L2 ),
  E2 = cons( true(), append( lst( t_bool(), [false()] ), L2 ) ),
  ?assertEqual(
    {ok, E2, []},
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

projection_reduces_to_record_field() ->
  E2 = str( <<"bla">> ),
  E1 = proj( a, rcd( [e_bind( a, E2 )] ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

map_reduces_to_list() ->
  E1 = for( t_bool(), [e_bind( x, lst( t_bool(), [true(), false()] ) )], neg( var( x ) ) ),
  E2 = cons( neg( true() ),
             for( t_bool(), [e_bind( x, lst( t_bool(), [false()] ) )], neg( var( x ) ) ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

zip_reduces_to_list() ->
  E1 = for( t_bool(),
            [e_bind( x, lst( t_bool(), [true(), false()] ) ),
             e_bind( y, lst( t_bool(), [false(), false()] ) )],
            disj( var( x ), var( y ) ) ),
  E2 = cons( disj( true(), false() ),
             for( t_bool(),
                  [e_bind( x, lst( t_bool(), [false()] ) ),
                   e_bind( y, lst( t_bool(), [false()] ) )],
                  disj( var( x ), var( y ) ) ) ),

  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

fold_reduces() ->
  E1 = fold( e_bind( x_acc, str( <<"0">> ) ),
             e_bind( x, lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
             var( x ) ),
  E2 = fold( e_bind( x_acc, str( <<"1">> ) ),
             e_bind( x, lst( t_str(), [str( <<"2">> )] ) ),
             var( x ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

fixpoint_operator_reduces() ->
  E1 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                     str( <<"blub">> ) ) ),
  E2 = lam_ntv( [], str( <<"blub">> ) ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

fixpoint_operator_inserts_function_definition() ->
  E1 = fix( lam_ntv( [lam_ntv_arg( f, t_fn( ntv, [], t_str() ) )],
                    var( f ) ) ),
  E2 = lam_ntv( [], E1 ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

for_over_empty_list_reduces_to_empty_list() ->
  E1 = for( t_str(), [e_bind( x, null( t_str() ) )], var( x ) ),
  E2 = null( t_str() ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

folding_over_empty_list_reduces_to_initial_accumulator() ->
  E1 = fold( e_bind( acc, str( <<"bla">> ) ), e_bind( x, null( t_str() ) ), var( x ) ),
  E2 = str( <<"bla">> ),
  ?assertEqual(
    {ok, E2, []},
    step( E1 ) ).

foreign_function_application_reduces_to_future() ->
  E1 = app( lam_frn( f, [], t_rcd( [t_arg( out, t_str() )]), l_bash(), <<"blub">> ), [] ),
  ?assertMatch(
    {ok, {fut, na, {'Rcd', [{out, 'Str'}]}, _}, [_]},
    step( E1 ) ).

nested_error_reduces_to_error() ->
  Err = err( t_bool(), <<"blub">> ),
  E1 = neg( Err ),
  ?assertEqual(
    {ok, Err, []},
    step( E1 ) ).