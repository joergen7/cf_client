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

-module( cuneiform_lang_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_lang, [expand_closure/2, assign/2, r_var/2, r_rcd/1] ).
-import( cuneiform_lang, [t_str/0, t_rcd/1, t_file/0, t_bool/0, t_lst/1,
                          t_fn/2] ).
-import( cuneiform_lang, [alet/2, lst/2] ).
-import( cuneiform_lang, [var/1, app/2, lam/2, proj/2, str/1, err/2, rcd/1,
                          fix/1, fut/1, true/0, false/0, file/1, cmp/2, conj/2,
                          disj/2, neg/1, isnil/1, cnd/3, null/1, cons/2, hd/2,
                          tl/2, append/2, for/3, fold/3] ).
-import( cuneiform_lang, [l_bash/0, l_elixir/0, l_erlang/0, l_java/0,
                          l_javascript/0, l_matlab/0, l_octave/0, l_perl/0,
                          l_python/0, l_r/0, l_racket/0, l_awk/0, l_gnuplot/0] ).
-import( cuneiform_lang, [subst/3, protect_expr/1] ).
-import( cuneiform_lang, [is_alpha_equivalent/2, free_vars/1] ).


%%====================================================================
%% Language constructors
%%====================================================================

lang_constructor_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    fun l_bash_returns_atom/0,
    fun l_elixir_returns_atom/0,
    fun l_erlang_returns_atom/0,
    fun l_java_returns_atom/0,
    fun l_javascript_returns_atom/0,
    fun l_matlab_returns_atom/0,
    fun l_octave_returns_atom/0,
    fun l_perl_returns_atom/0,
    fun l_python_returns_atom/0,
    fun l_r_returns_atom/0,
    fun l_racket_returns_atom/0,
    fun l_awk_returns_atom/0,
    fun l_gnuplot_returns_atom/0
   ]
  }.

l_bash_returns_atom()       -> ?assertEqual( 'Bash', l_bash() ).
l_elixir_returns_atom()     -> ?assertEqual( 'Elixir', l_elixir() ).
l_erlang_returns_atom()     -> ?assertEqual( 'Erlang', l_erlang() ).
l_java_returns_atom()       -> ?assertEqual( 'Java', l_java() ).
l_javascript_returns_atom() -> ?assertEqual( 'Javascript', l_javascript() ).
l_matlab_returns_atom()     -> ?assertEqual( 'Matlab', l_matlab() ).
l_octave_returns_atom()     -> ?assertEqual( 'Octave', l_octave() ).
l_perl_returns_atom()       -> ?assertEqual( 'Perl', l_perl() ).
l_python_returns_atom()     -> ?assertEqual( 'Python', l_python() ).
l_r_returns_atom()          -> ?assertEqual( 'R', l_r() ).
l_racket_returns_atom()     -> ?assertEqual( 'Racket', l_racket() ).
l_awk_returns_atom()        -> ?assertEqual( 'Awk', l_awk() ).
l_gnuplot_returns_atom()    -> ?assertEqual( 'Gnuplot', l_gnuplot() ).


%%====================================================================
%% Type constructors
%%====================================================================

type_constructor_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    fun t_str_returns_type/0,
    fun t_file_returns_type/0,
    fun t_bool_returns_type/0,
    fun t_rcd_returns_type/0,
    fun t_lst_returns_type/0,
    fun t_fn_returns_type/0
   ]
  }.

t_str_returns_type()  -> ?assertEqual( 'Str', t_str() ).
t_file_returns_type() -> ?assertEqual( 'File', t_file() ).
t_bool_returns_type() -> ?assertEqual( 'Bool', t_bool() ).

t_rcd_returns_type()  ->
  ?assertEqual( {'Rcd', [{a, 'Str'}, {b, 'File'}]},
                t_rcd( [{a, t_str()}, {b, t_file()}] ) ).

t_lst_returns_type() ->
  ?assertEqual( {'Lst', 'Str'}, t_lst( t_str() ) ).

t_fn_returns_type() ->
  ?assertEqual( {'Fn', [{x, 'Str'}, {y, 'File'}], 'Bool'},
                t_fn( [{x, t_str()}, {y, t_file()}], t_bool() ) ).



%%====================================================================
%% Expression constructors
%%====================================================================

%%====================================================================
%% Syntactic Sugar
%%====================================================================

%%====================================================================
%% Pattern Constructors, Assignments, and Expansion
%%====================================================================

expand_closure_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
     fun assign_variable_pattern/0,
     fun last_assignment_binds_innermost/0,
     fun empty_record_pattern_is_neutral/0,
     fun assignment_resolution_propagates_to_record_fields/0,
     fun ambiguous_variable_binding_returns_no_error/0
   ]
  }.

assign_variable_pattern() ->
  AssignLst = [assign( r_var( x, t_str() ), var( y ) )],
  EBody = var( z ),
  Closure = app( lam( [{x, t_str()}], {ntv, var( z )} ),
                 [{x, var( y )}] ),
  ?assertEqual( Closure, expand_closure( AssignLst, EBody ) ).

last_assignment_binds_innermost() ->
  AssignLst = [assign( r_var( x1, t_str() ), var( y1 ) ),
               assign( r_var( x2, t_str() ), var( y2 ) )],
  EBody = var( z ),
  Closure = app( lam( [{x1, t_str()}],
                      {ntv, app( lam( [{x2, t_str()}],
                                      {ntv, var( z )} ),
                                 [{x2, var( y2 )}] )} ),
                 [{x1, var( y1 )}] ),
  ?assertEqual( Closure, expand_closure( AssignLst, EBody ) ).

empty_record_pattern_is_neutral() ->
  AssignLst = [assign( r_rcd( [] ), var( x ) )],
  EBody = var( y ),
  ?assertEqual( EBody, expand_closure( AssignLst, EBody ) ).

assignment_resolution_propagates_to_record_fields() ->
  AssignLst = [assign( r_rcd( [{a, r_var( x, t_str() )}] ),
               var( y ) )],
  EBody = var( z ),
  Closure = app( lam( [{x, t_str()}],
                      {ntv, var( z )} ),
                 [{x, proj( a, var( y ) )}] ),
  ?assertEqual( Closure, expand_closure( AssignLst, EBody ) ).

ambiguous_variable_binding_returns_no_error() ->
  AssignLst = [assign( r_rcd( [{a, r_var( x, t_str() )},
                               {b, r_var( x, t_str() )}] ),
                       var( m ) )],
  EBody = var( x ),
  Closure = alet( [{x, t_str(), proj( a, var( m ) )},
                   {x, t_str(), proj( b, var( m ) )}], EBody ),
  ?assertEqual( Closure,
                expand_closure( AssignLst, EBody ) ).


%%====================================================================
%% Name Helpers
%%====================================================================

%%====================================================================
%% Validators
%%====================================================================

%%====================================================================
%% Contract Predicates
%%====================================================================

%%====================================================================
%% Renaming and Substitution
%%====================================================================

subst_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"subst free var",
     fun subst_free_var/0},
    {"subst unrelated var",
     fun subst_unrelated_var/0},
    {"subst ntv lam traverses body",
     fun subst_ntv_lam_traverses_body/0},
    {"subst ntv lam shadows",
     fun subst_ntv_lam_shadows/0},
    {"subst ntv lam cannot capture",
     fun subst_ntv_lam_cannot_capture/0},
    {"subst frn function no effect",
     fun subst_frn_lam_no_effect/0},
    {"subst app traverses function position",
     fun subst_app_traverses_function_position/0},
    {"subst app traverses argument position",
     fun subst_app_traverses_argument_position/0},
    {"subst fix traverses",
     fun subst_fix_traverses/0},
    {"subst fut no effect",
     fun subst_fut_no_effect/0},
    {"subst str no effect",
     fun subst_str_no_effect/0},
    {"subst file no effect",
     fun subst_file_no_effect/0},
    {"subst true no effect",
     fun subst_true_no_effect/0},
    {"subst false no effect",
     fun subst_false_no_effect/0},
    {"subst cmp traverses lhs",
     fun subst_cmp_traverses_lhs/0},
    {"subst cmp traverses rhs",
     fun subst_cmp_traverses_rhs/0},
    {"subst conj traverses lhs",
     fun subst_conj_traverses_lhs/0},
    {"subst conj traverses rhs",
     fun subst_conj_traverses_rhs/0},
    {"subst disj traverses lhs",
     fun subst_disj_traverses_lhs/0},
    {"subst disj traverses rhs",
     fun subst_disj_traverses_rhs/0},
    {"subst neg traverses",
     fun subst_neg_traverses/0},
    {"subst isnil traverses",
     fun subst_isnil_traverses/0},
    {"subst cnd traverses if posiiton",
     fun subst_cnd_traverses_if_position/0},
    {"subst cnd traverses then posiiton",
     fun subst_cnd_traverses_then_position/0},
    {"subst cnd traverses else posiiton",
     fun subst_cnd_traverses_else_position/0},
    {"subst null no effect",
     fun subst_null_no_effect/0},
    {"subst cons traverses lhs",
     fun subst_cons_traverses_lhs/0},
    {"subst cons traverses rhs",
     fun subst_cons_traverses_rhs/0},
    {"subst hd traverses lhs",
     fun subst_hd_traverses_lhs/0},
    {"subst hd traverses rhs",
     fun subst_hd_traverses_rhs/0},
    {"subst tl traverses lhs",
     fun subst_tl_traverses_lhs/0},
    {"subst tl traverses rhs",
     fun subst_tl_traverses_rhs/0},
    {"subst append traverses lhs",
     fun subst_append_traverses_lhs/0},
    {"subst append traverses rhs",
     fun subst_append_traverses_rhs/0},
    {"subst for traverses list_binding",
     fun subst_for_traverses_list_binding/0},
    {"subst for traverses body",
     fun subst_for_traverses_body/0},
    {"subst for list binding shadows",
     fun subst_for_list_binding_shadows/0},
    {"subst for list binding cannot capture",
     fun subst_for_list_binding_cannot_capture/0},
    {"subst fold traverses acc binding",
     fun subst_fold_traverses_acc_binding/0},
    {"subst fold traverses list binding",
     fun subst_fold_traverses_list_binding/0},
    {"subst fold traverses body",
     fun subst_fold_traverses_body/0},
    {"subst fold acc binding shadows",
     fun subst_fold_acc_binding_shadows/0},
    {"subst fold acc binding cannot capture",
     fun subst_fold_acc_binding_cannot_capture/0},
    {"subst fold list binding shadows",
     fun subst_fold_list_binding_shadows/0},
    {"subst fold list binding cannot capture",
     fun subst_fold_list_binding_cannot_capture/0},
    {"subst rcd propagates",
     fun subst_rcd_propagates/0},
    {"subst proj propagates",
     fun subst_proj_propagates/0},
    {"subst error no effect",
     fun subst_error_no_effect/0}]}.


subst_free_var() ->
  E0 = var( x ),
  E1 = str( <<"blub">> ),
  ?assertEqual( E1, subst( E0, x, E1 ) ).

subst_unrelated_var() ->
  E0 = var( x ),
  E1 = str( <<"blub">> ),
  ?assertEqual( E0, subst( E0, y, E1 ) ).

subst_ntv_lam_traverses_body() ->
  E0 = lam( [], {ntv, var( x )} ),
  E1 = str( <<"blub">> ),
  E2 = lam( [], {ntv, E1} ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_ntv_lam_shadows() ->
  E0 = lam( [{x, t_str()}], {ntv, var( x )} ),
  E1 = str( <<"blub">> ),
  ?assert( is_alpha_equivalent( E0, subst( E0, x, E1 ) ) ).

subst_ntv_lam_cannot_capture() ->
  E0 = lam( [{x, t_str()}], {ntv, var( y )} ),
  E1 = lam( [{x, t_str()}], {ntv, var( x )} ),
  E2 = subst( E0, y, var( x ) ),
  ?assertNot( is_alpha_equivalent( E1, E2 ) ).

subst_frn_lam_no_effect() ->
  E0 = lam( [{x, t_str()}],
            {frn, <<"f">>,
                  t_rcd( [{y, t_str()}] ),
                  l_bash(), <<"bla">>} ),
  ?assertEqual( E0, subst( E0, y, str( <<"blub">> ) ) ).

subst_app_traverses_function_position() ->
  E0 = app( var( f ), [] ),
  E1 = lam( [], {ntv, str( <<"blub">> )} ),
  E2 = app( E1, [] ),
  ?assertEqual( E2, subst( E0, f, E1 ) ).

subst_app_traverses_argument_position() ->
  ELam = lam( [{x, t_str()}], {ntv, var( x )} ),
  E0 = app( ELam, [{x, var( y )}] ),
  E1 = str( <<"blub">> ),
  E2 = app( ELam, [{x, E1}] ),
  ?assert( is_alpha_equivalent( E2, subst( E0, y, E1 ) ) ).

subst_fix_traverses() ->
  E0 = fix( lam( [{f, t_fn( [], t_str() )}], {ntv, var( x )} ) ),
  E1 = str( <<"blub">> ),
  E2 = fix( lam( [{f, t_fn( [], t_str() )}], {ntv, E1} ) ),
  ?assert( is_alpha_equivalent( E2, subst( E0, x, E1 ) ) ).

subst_fut_no_effect() ->
  E0 = fut( app( lam( [{x, t_bool()}],
                      {frn, <<"f">>,
                            t_rcd( [{y, t_bool()}] ),
                            l_elixir(),
                            <<"bla">>} ),
                 [{x, true()}] ) ),
  ?assertEqual( E0, subst( E0, x, false() ) ).

subst_str_no_effect() ->
  E0 = str( <<"blub">> ),
  ?assertEqual( E0, subst( E0, x, false() ) ).

subst_file_no_effect() ->
  E0 = file( <<"blub.txt">> ),
  ?assertEqual( E0, subst( E0, x, false() ) ).

subst_true_no_effect() ->
  E0 = true(),
  ?assertEqual( E0, subst( E0, x, false() ) ).

subst_false_no_effect() ->
  E0 = false(),
  ?assertEqual( E0, subst( E0, x, false() ) ).

subst_cmp_traverses_lhs() ->
  E0 = cmp( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = cmp( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_cmp_traverses_rhs() ->
  E0 = cmp( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = cmp( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_conj_traverses_lhs() ->
  E0 = conj( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = conj( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_conj_traverses_rhs() ->
  E0 = conj( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = conj( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_disj_traverses_lhs() ->
  E0 = disj( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = disj( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_disj_traverses_rhs() ->
  E0 = disj( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = disj( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_neg_traverses() ->
  E0 = neg( var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = neg( E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_isnil_traverses() ->
  E0 = isnil( var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = isnil( E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_cnd_traverses_if_position() ->
  E0 = cnd( var( x ), str( <<"bla">> ), str( <<"blub">> ) ),
  E1 = str( <<"foo">> ),
  E2 = cnd( E1, str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_cnd_traverses_then_position() ->
  E0 = cnd( str( <<"bla">> ), var( x ), str( <<"blub">> ) ),
  E1 = str( <<"foo">> ),
  E2 = cnd( str( <<"bla">> ), E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_cnd_traverses_else_position() ->
  E0 = cnd( str( <<"bla">> ), str( <<"blub">> ), var( x ) ),
  E1 = str( <<"foo">> ),
  E2 = cnd( str( <<"bla">> ), str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_null_no_effect() ->
  E0 = null( t_str() ),
  ?assertEqual( E0, subst( E0, y, str( <<"blub">> ) ) ).

subst_cons_traverses_lhs() ->
  E0 = cons( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = cons( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_cons_traverses_rhs() ->
  E0 = cons( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = cons( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_hd_traverses_lhs() ->
  E0 = hd( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = hd( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_hd_traverses_rhs() ->
  E0 = hd( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = hd( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_tl_traverses_lhs() ->
  E0 = tl( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = tl( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_tl_traverses_rhs() ->
  E0 = tl( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = tl( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_append_traverses_lhs() ->
  E0 = append( var( x ), str( <<"blub">> ) ),
  E1 = str( <<"bla">> ),
  E2 = append( E1, str( <<"blub">> ) ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_append_traverses_rhs() ->
  E0 = append( str( <<"blub">> ), var( x ) ),
  E1 = str( <<"bla">> ),
  E2 = append( str( <<"blub">> ), E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_for_traverses_list_binding() ->
  E0 = for( t_str(), [{x, t_str(), var( l )}], var( x ) ),
  E1 = lst( t_str(), [str( <<"bla">> ), str( <<"blub">> )] ),
  E2 = for( t_str(), [{x, t_str(), E1}], var( x ) ),
  ?assert( is_alpha_equivalent( E2, subst( E0, l, E1 ) ) ).

subst_for_traverses_body() ->
  E0 = for( t_str(), [{x, t_str(), null( t_str() )}], var( y ) ),
  E1 = str( <<"blub">> ),
  E2 = for( t_str(), [{x, t_str(), null( t_str() )}], E1 ),
  ?assert( is_alpha_equivalent( E2, subst( E0, y, E1 ) ) ).

subst_for_list_binding_shadows() ->
  E0 = for( t_str(), [{x, t_str(), null( t_str() )}], var( x ) ),
  ?assert( is_alpha_equivalent( E0, subst( E0, x, var( y ) ) ) ).

subst_for_list_binding_cannot_capture() ->
  E0 = for( t_str(), [{x, t_str(), null( t_str() )}], var( y ) ),
  E1 = for( t_str(), [{x, t_str(), null( t_str() )}], var( x ) ),
  ?assert( not is_alpha_equivalent( E1, subst( E0, y, var( x ) ) ) ).

subst_fold_traverses_acc_binding() ->
  E0 = fold( {acc, t_str(), var( s )},
             {x, t_str(), null( t_str() )},
             var( x ) ),
  E1 = str( <<"blub">> ),
  E2 = fold( {acc, t_str(), E1},
             {x, t_str(), null( t_str() )},
             var( x ) ),
  ?assert( is_alpha_equivalent( E2, subst( E0, s, E1 ) ) ).

subst_fold_traverses_list_binding() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), var( l )},
             var( x ) ),
  E1 = null( t_str() ),
  E2 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), E1},
             var( x ) ),
  ?assert( is_alpha_equivalent( E2, subst( E0, l, E1 ) ) ).

subst_fold_traverses_body() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( y ) ),
  E1 = str( <<"blub">> ),
  E2 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             E1 ),
  ?assert( is_alpha_equivalent( E2, subst( E0, y, E1 ) ) ).

subst_fold_acc_binding_shadows() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( acc ) ),
  E1 = str( <<"blub">> ),
  ?assert( is_alpha_equivalent( E0, subst( E0, y, E1 ) ) ).

subst_fold_acc_binding_cannot_capture() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( y ) ),
  E2 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( acc ) ),
  ?assertNot( is_alpha_equivalent( E2, subst( E0, y, var( acc ) ) ) ).

subst_fold_list_binding_shadows() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( x ) ),
  E1 = str( <<"blub">> ),
  ?assert( is_alpha_equivalent( E0, subst( E0, y, E1 ) ) ).

subst_fold_list_binding_cannot_capture() ->
  E0 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( y ) ),
  E2 = fold( {acc, t_str(), str( <<"bla">> )},
             {x, t_str(), null( t_str() )},
             var( x ) ),
  ?assertNot( is_alpha_equivalent( E2, subst( E0, y, var( x ) ) ) ).

subst_rcd_propagates() ->
  E0 = rcd( [{a, var( x )}] ),
  E1 = str( <<"blub">> ),
  E2 = rcd( [{a, E1}] ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_proj_propagates() ->
  E0 = proj( a, var( x ) ),
  E1 = rcd( [{a, str( <<"blub">> )}] ),
  E2 = proj( a, E1 ),
  ?assertEqual( E2, subst( E0, x, E1 ) ).

subst_error_no_effect() ->
  E0 = err( t_str(), <<"my message">> ),
  ?assertEqual( E0, subst( E0, y, str( <<"blub">> ) ) ).







%%====================================================================
%% Expression Analysis
%%====================================================================

is_alpha_equivalent_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"is_alpha_equivalent lam identity functions",
     fun is_alpha_equivalent_lam_identity_functions/0},
    {"is_alpha_equivalent lam free var not bound var",
     fun is_alpha_equivalent_lam_free_var_not_bound_var/0},
    {"is_alpha_equivalent for renamed list binding",
     fun is_alpha_equivalent_for_renamed_list_binding/0},
    {"is_alpha_equivalent for free var not bound var",
     fun is_alpha_equivalent_for_free_var_not_bound_var/0},
    {"is_alpha_equivalent null",
     fun is_alpha_equivalent_null/0}
   ]
  }.

is_alpha_equivalent_lam_identity_functions() ->
  E1 = lam( [{x, t_str()}], {ntv, var( x )} ),
  E2 = lam( [{y, t_str()}], {ntv, var( y )} ),
  ?assert( is_alpha_equivalent( E1, E2 ) ).

is_alpha_equivalent_lam_free_var_not_bound_var() ->
  E1 = lam( [{x, t_str()}], {ntv, var( x )} ),
  E2 = lam( [{y, t_str()}], {ntv, var( x )} ),
  ?assertNot( is_alpha_equivalent( E1, E2 ) ).

is_alpha_equivalent_for_renamed_list_binding() ->
  E1 = for( t_str(), [{x, t_str(), null( t_str() )}], var( x ) ),
  E2 = for( t_str(), [{y, t_str(), null( t_str() )}], var( y ) ),
  ?assert( is_alpha_equivalent( E1, E2 ) ).

is_alpha_equivalent_for_free_var_not_bound_var() ->
  E1 = for( t_str(), [{x, t_str(), null( t_str() )}], var( x ) ),
  E2 = for( t_str(), [{y, t_str(), null( t_str() )}], var( x ) ),
  ?assertNot( is_alpha_equivalent( E1, E2 ) ).

is_alpha_equivalent_null() ->
  E0 = null( t_str() ),
  ?assert( is_alpha_equivalent( E0, E0 ) ).





protect_expr_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"protect_expr fold", fun protect_expr_fold/0}
   ]
  }.

protect_expr_fold() ->
  E1 = {cmp,na,{fix,na,{app,na,{var,na,'\000'},[{'',{append,na,{cmp,na,{var,na,'\000'},{lam,na,[],{ntv,{fix,na,{for,na,'Str',[],{fold,na,{'','Str',{var,na,''}},{'','File',{app,1,{cnd,na,{cmp,na,{proj,na,'\207',{false,{<<>>,1}}},{tl,{<<>>,1},{true,3},{false,{<<53>>,1}}}},{var,na,''},{str,na,<<>>}},[{'',{fix,{<<5>>,1},{str,na,<<>>}}}]}},{null,na,{'Fn',[{'','Str'},{'>','Str'},{'','Str'}],'Bool'}}}}}}}},{var,na,'\000'}}}]}},{var,na,'\000'}},
  F1 = free_vars( E1 ),
  E2 = protect_expr( E1 ),
  F2 = free_vars( E2 ),
  ?assertEqual( F1, F2 ).