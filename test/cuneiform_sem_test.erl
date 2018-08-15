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

-module( cuneiform_sem_test ).
-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_sem, [is_value/1, subst_fut/2, set_info/2] ).

-import( cuneiform_lang, [
                          t_str/0, t_file/0, t_bool/0, t_fn/3, t_arg/2, t_rcd/1,
                          t_lst/1
                         ] ).
-import( cuneiform_lang, [
                          str/1, file/1, true/0, false/0, cnd/3, var/1, file/2,
                          lam_ntv/2, app/2, cmp/2, neg/1, conj/2, disj/2,
                          lam_frn/5, lst/2, append/2, isnil/1, for/3, fold/3,
                          rcd/1, proj/2, fix/1, assign/3, null/1, cons/2, err/2,
                          err/3, typed_bind/3, e_bind/2, l_bash/0
                         ] ).


e_lam1() ->
  lam_ntv( [t_arg( x, t_str() ),
            t_arg( y, t_file() )], var( x ) ).


e_lam_first() ->
  lam_ntv( [t_arg( x, t_str() ),
            t_arg( y, t_str() )], var( x ) ).

e_lam_const() ->
  lam_ntv( [], str( <<"blub">> ) ).

e_lam_id() ->
  lam_ntv( [t_arg( x, t_str() )], var( x ) ).

e_app_id() ->
  app( e_lam_id(), [e_bind( x, str( <<"blub">> ) )] ).

e_lam_greet() ->
  lam_frn( greet, [t_arg( person, t_str() )], t_rcd( [t_arg( out, t_str() )] ), l_bash(), <<"out=\"hello $person\"">> ).



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
    {"error is no value",       fun error_is_no_value/0}
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
  E = fold( typed_bind( x_acc, t_str(), str( <<"0">> ) ),
            typed_bind( x, t_str(), lst( t_str(), [str( <<"1">> ), str( <<"2">> )] ) ),
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
  ELam = lam_ntv( [t_arg( f, t_fn( ntv, [], t_str() ) )],
                  str( <<"bla">> ) ),
  E = fix( ELam ),
  ?assertNot( is_value( E ) ).

error_is_no_value() ->
  ?assertEqual( false, is_value( err( t_str(), <<"blub">> ) ) ).


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
  E = str( <<"bla">> ),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_file_literal_alone() ->
  E = file( <<"blub.txt">> ),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_true_alone() ->
  E = true(),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_false_alone() ->
  E = false(),
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_alters_matching_future() ->
  Info = 12,
  E1 = {fut, Info, t_file(), <<"1234">>},
  E2 = file( Info, <<"idx.tar">> ),
  A = <<"1234">>,
  ?assertEqual( E2, subst_fut( E1, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_nonmatching_future_alone() ->
  E = {fut, na, t_file(), <<"123">>},
  A = <<"1234">>,
  ?assertEqual( E, subst_fut( E, [{A, file( <<"idx.tar">> )}] ) ).

subst_fut_traverses_comparison_lhs() ->
  E1 = cmp( {fut, na, t_str(), <<"1234">>}, str( <<"bla">> ) ),
  E2 = cmp( str( <<"blub">> ), str( <<"bla">> ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, str( <<"blub">> )}] ) ).

subst_fut_traverses_comparison_rhs() ->
  E1 = cmp( str( <<"bla">> ), {fut, na, t_str(), <<"1234">>} ),
  E2 = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, str( <<"blub">> )}] ) ).

subst_fut_traverses_condition_if_term() ->
  E1 = cnd( {fut, na, t_bool(), <<"1234">>}, str( <<"bla">>), str( <<"blub">> ) ),
  E2 = cnd( true(), str( <<"bla">>), str( <<"blub">> ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_traverses_negation() ->
  E1 = neg( {fut, na, t_bool(), <<"1234">>} ),
  E2 = neg( true() ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_traverses_conjunction_lhs() ->
  E1 = conj( {fut, na, t_bool(), <<"1234">>}, false() ),
  E2 = conj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_traverses_conjunction_rhs() ->
  E1 = conj( true(), {fut, na, t_bool(), <<"1234">>} ),
  E2 = conj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, false()}] ) ).

subst_fut_traverses_disjunction_lhs() ->
  E1 = disj( {fut, na, t_bool(), <<"1234">>}, false() ),
  E2 = disj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_traverses_disjunction_rhs() ->
  E1 = disj( true(), {fut, na, t_bool(), <<"1234">>} ),
  E2 = disj( true(), false() ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, false()}] ) ).

subst_fut_leaves_variable_alone() ->
  E = var( x ),
  ?assertEqual( E, subst_fut( E, [{<<"1234">>, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_native_lambda_alone() ->
  E = e_lam_const(),
  ?assertEqual( E, subst_fut( E, [{<<"1234">>, file( <<"idx.tar">> )}] ) ).

subst_fut_leaves_foreign_lambda_alone() ->
  E = e_lam_greet(),
  ?assertEqual( E, subst_fut( E, [{<<"1234">>, file( <<"idx.tar">> )}] ) ).

subst_fut_traverses_application_function_position() ->
  E1 = app( cnd( {fut, na, t_bool(), <<"1234">>}, var( f ), var( g ) ), [] ),
  E2 = app( cnd( true(), var( f ), var( g ) ), [] ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_traverses_application_argument_bindings() ->
  EF = cons( str( <<"blub">> ), null( t_str() ) ),
  E1 = app( var( z ), [e_bind( x, {fut, na, t_lst( t_str() ), <<"1234">>} )] ),
  E2 = app( var( z ), [e_bind( x, EF )] ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, EF}] ) ).

subst_fut_leaves_null_alone() ->
  E = null( t_str() ),
  ?assertEqual( E, subst_fut( E, [{<<"1234">>, file( <<"idx.tar">> )}] ) ).

subst_fut_traverses_cons_lhs() ->
  E1 = cons( {fut, na, t_str(), <<"1234">>}, null( t_str() ) ),
  E2 = cons( str( <<"bla">> ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, str( <<"bla">> )}] ) ).

subst_fut_traverses_cons_rhs() ->
  E1 = cons( var( x ), cons( {fut, na, t_str(), <<"1234">>}, null( t_str() ) ) ),
  E2 = cons( var( x ), cons( str( <<"bla">> ), null( t_str() ) ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, str( <<"bla">> )}] ) ).

subst_fut_traverses_append_lhs() ->
  E1 = append( {fut, na, t_lst( t_str() ), <<"1234">>}, null( t_str() ) ),
  E2 = append( null( t_str() ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).

subst_fut_traverses_append_rhs() ->
  E1 = append( null( t_str() ), {fut, na, t_lst( t_str() ), <<"1234">>} ),
  E2 = append( null( t_str() ), null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).

subst_fut_traverses_isnil_operand() ->
  E1 = isnil( {fut, na, t_lst( t_str() ), <<"1234">>} ),
  E2 = isnil( null( t_str() ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).

subst_fut_traverses_record_fields() ->
  E1 = rcd( [e_bind( a, {fut, na, t_lst( t_str() ), <<"1234">>} )] ),
  E2 = rcd( [e_bind( a, null( t_str() ) )] ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).

subst_fut_traverses_projection_operand() ->
  E1 = proj( a, {fut, na, t_rcd( [t_arg( a, t_str() )] ), <<"1234">>} ),
  E2 = proj( a, rcd( [e_bind( a, str( <<"blub">> ) )] ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, rcd( [e_bind( a, str( <<"blub">> ) )] )}] ) ).

subst_fut_traverses_fixpoint_operand() ->
  E1 = fix( cnd( {fut, na, t_bool(), <<"1234">>}, var( f ), var( g ) ) ),
  E2 = fix( cnd( true(), var( f ), var( g ) ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, true()}] ) ).

subst_fut_leaves_error_alone() ->
  E = err( t_str(), <<"blub">> ),
  ?assertEqual( E, subst_fut( E, [{<<"1234">>, file( <<"idx.tar">> )}] ) ).

subst_fut_traverses_for_list_expression() ->
  E1 = for( t_str(), [typed_bind( x, t_str(), {fut, na, t_lst( t_str() ), <<"1234">>} )], var( x ) ),
  E2 = for( t_str(), [typed_bind( x, t_str(), null( t_str() ) )], var( x ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).

subst_fut_traverses_fold_list_expression() ->
  E1 = fold( typed_bind( x_acc, t_str(), var( x0 ) ),
             typed_bind( x, t_str(), {fut, na, t_lst( t_str() ), <<"1234">>} ),
             var( x ) ),
  E2 = fold( typed_bind( x_acc, t_str(), var( x0 ) ),
             typed_bind( x, t_str(), null( t_str() ) ), var( x ) ),
  ?assertEqual( E2, subst_fut( E1, [{<<"1234">>, null( t_str() )}] ) ).


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
