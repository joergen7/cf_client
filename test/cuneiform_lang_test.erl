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

-import( cuneiform_lang, [expand_closure/2, assign/2, assign/3, r_var/2,
                          r_rcd/1] ).
-import( cuneiform_lang, [t_str/0, t_rcd/1, t_file/0, t_bool/0, t_lst/1,
                          t_fn/2] ).
-import( cuneiform_lang, [alet/2, lst/2, asc/2] ).
-import( cuneiform_lang, [lst/3, alet/3, asc/3] ).
-import( cuneiform_lang, [var/1, app/2, lam/2, proj/2, str/1, err/2, rcd/1,
                          fix/1, fut/1, true/0, false/0, file/1, cmp/2, conj/2,
                          disj/2, neg/1, isnil/1, cnd/3, null/1, cons/2, hd/2,
                          tl/2, append/2, for/3, fold/3] ).
-import( cuneiform_lang, [var/2, lam/3, app/3, fix/2, fut/2, str/2, file/2,
                          true/1, false/1, cmp/3, conj/3, disj/3, neg/2,
                          isnil/2, cnd/4, null/2, cons/3, hd/3, tl/3, append/3,
                          for/4, fold/4, rcd/2, proj/3, err/3] ).
-import( cuneiform_lang, [l_bash/0, l_elixir/0, l_erlang/0, l_java/0,
                          l_javascript/0, l_matlab/0, l_octave/0, l_perl/0,
                          l_python/0, l_r/0, l_racket/0, l_awk/0,
                          l_gnuplot/0] ).
-import( cuneiform_lang, [subst/3, protect_expr/1, rename/3] ).
-import( cuneiform_lang, [is_alpha_equivalent/2, expr_free_vars/1] ).
-import( cuneiform_lang, [validate_lang/1, validate_info/1] ).


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
    {"t_str returns type",
     fun t_str_returns_type/0},
    {"t_file returns type",
     fun t_file_returns_type/0},
    {"t_bool returns type",
     fun t_bool_returns_type/0},
    {"t_rcd returns type",
     fun t_rcd_returns_type/0},
    {"t_rcd fails for invalid xt list",
     fun t_rcd_fails_for_invalid_xt_list/0},
    {"t_rcd fails for invalid xt",
     fun t_rcd_fails_for_invalid_xt/0},
    {"t_rcd fails for invalid x",
     fun t_rcd_fails_for_invalid_x/0},
    {"t_rcd fails for invalid t",
     fun t_rcd_fails_for_invalid_t/0},
    {"t_lst returns type",
     fun t_lst_returns_type/0},
    {"t_lst fails for invalid type",
     fun t_lst_fails_for_invalid_type/0},
    {"t_fn returns type",
     fun t_fn_returns_type/0},
    {"t_fn fails for invalid xt list",
     fun t_fn_fails_for_invalid_xt_list/0},
    {"t_fn fails for invalid xt",
     fun t_fn_fails_for_invalid_xt/0},
    {"t_fn fails for invalid x",
     fun t_fn_fails_for_invalid_x/0},
    {"t_fn fails for invalid t",
     fun t_fn_fails_for_invalid_t/0},
    {"t_fn fails for invalid return type",
     fun t_fn_fails_for_invalid_return_type/0}
   ]
  }.

t_str_returns_type()  ->
  ?assertEqual( 'Str', t_str() ).

t_file_returns_type() ->
  ?assertEqual( 'File', t_file() ).

t_bool_returns_type() ->
  ?assertEqual( 'Bool', t_bool() ).

t_rcd_returns_type()  ->
  ?assertEqual( {'Rcd', [{a, 'Str'}, {b, 'File'}]},
                t_rcd( [{a, t_str()}, {b, t_file()}] ) ).

t_rcd_fails_for_invalid_xt_list() ->
  ?assertError( {bad_xt_lst, 5}, t_rcd( 5 ) ).

t_rcd_fails_for_invalid_xt() ->
  ?assertError( {bad_xt, 5}, t_rcd( [5] ) ).

t_rcd_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, t_rcd( [{5, t_str()}] ) ).

t_rcd_fails_for_invalid_t() ->
  ?assertError( {bad_type, 5}, t_rcd( [{a, 5}] ) ).

t_lst_returns_type() ->
  ?assertEqual( {'Lst', 'Str'}, t_lst( t_str() ) ).

t_lst_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5}, t_lst( 5 ) ).

t_fn_returns_type() ->
  ?assertEqual( {'Fn', [{x, 'Str'}, {y, 'File'}], 'Bool'},
                t_fn( [{x, t_str()}, {y, t_file()}], t_bool() ) ).

t_fn_fails_for_invalid_xt_list() ->
  ?assertError( {bad_xt_lst, 5}, t_fn( 5, t_str() ) ).

t_fn_fails_for_invalid_xt() ->
  ?assertError( {bad_xt, 5}, t_fn( [5], t_str() ) ).

t_fn_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, t_fn( [{5, t_bool()}], t_str() ) ).

t_fn_fails_for_invalid_t() ->
  ?assertError( {bad_type, 5}, t_fn( [{x, 5}], t_str() ) ).

t_fn_fails_for_invalid_return_type() ->
  ?assertError( {bad_type, 5}, t_fn( [{x, t_bool()}], 5 ) ).


%%====================================================================
%% Expression constructors
%%====================================================================

expr_constructor_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"var returns expr",
     fun var_returns_expr/0},
    {"var fails for invalid info",
     fun var_fails_for_invalid_info/0},
    {"var fails for invalid variable name",
     fun var_fails_for_invalid_variable_name/0},
    {"lam ntv returns expr",
     fun lam_ntv_returns_expr/0},
    {"lam frn returns expr",
     fun lam_frn_returns_expr/0},
    {"lam fails for invalid info",
     fun lam_fails_for_invalid_info/0},
    {"lam fails for invalid xt list",
     fun lam_fails_for_invalid_xt_list/0},
    {"lam fails for invalid xt",
     fun lam_fails_for_invalid_xt/0},
    {"lam fails for invalid x",
     fun lam_fails_for_invalid_x/0},
    {"lam fails for invalid t",
     fun lam_fails_for_invalid_t/0},
    {"lam fails for invalid body",
     fun lam_fails_for_invalid_body/0},
    {"lam ntv fails for invalid body expr",
     fun lam_ntv_fails_for_invalid_body_expr/0},
    {"lam frn fails for invalid function name",
     fun lam_frn_fails_for_invalid_function_name/0},
    {"lam frn fails for invalid return type",
     fun lam_frn_fails_for_invalid_return_type/0},
    {"lam frn fails for invalid lang",
     fun lam_frn_fails_for_invalid_lang/0},
    {"lam frn fails for invalid script",
     fun lam_frn_fails_for_invalid_script/0},
    {"app returns expr",
     fun app_returns_expr/0},
    {"app fails for invalid info",
     fun app_fails_for_invalid_info/0},
    {"app fails for invalid function expr",
     fun app_fails_for_invalid_function_expr/0},
    {"app fails for invalid xe list",
     fun app_fails_for_invalid_xe_list/0},
    {"app fails for invalid xe",
     fun app_fails_for_invalid_xe/0},
    {"app fails for invalid x",
     fun app_fails_for_invalid_x/0},
    {"app fails for invalid e",
     fun app_fails_for_invalid_e/0},
    {"fix returns expr",
     fun fix_returns_expr/0},
    {"fix fails for invalid info",
     fun fix_fails_for_invalid_info/0},
    {"fix fails for invalid operand",
     fun fix_fails_for_invalid_operand/0},
    {"fut returns expr",
     fun fut_returns_expr/0},
    {"fut fails for invalid info",
     fun fut_fails_for_invalid_info/0},
    {"fut fails for invalid operand",
     fun fut_fails_for_invalid_operand/0},
    {"str returns expr",
     fun str_returns_expr/0},
    {"str fails for invalid info",
     fun str_fails_for_invalid_info/0},
    {"str fails for invalid content",
     fun str_fails_for_invalid_content/0},
    {"file returns expr",
     fun file_returns_expr/0},
    {"file fails for invalid info",
     fun file_fails_for_invalid_info/0},
    {"file fails for invalid content",
     fun file_fails_for_invalid_content/0},
    {"true returns expr",
     fun true_returns_expr/0},
    {"true fails for invalid info",
     fun true_fails_for_invalid_info/0},
    {"false returns expr",
     fun false_returns_expr/0},
    {"false fails for invalid info",
     fun false_fails_for_invalid_info/0},
    {"cmp returns expr",
     fun cmp_returns_expr/0},
    {"cmp fails for invalid info",
     fun cmp_fails_for_invalid_info/0},
    {"cmp fails for invalid lhs",
     fun cmp_fails_for_invalid_lhs/0},
    {"cmp fails for invalid rhs",
     fun cmp_fails_for_invalid_rhs/0},
    {"conj returns expr",
     fun conj_returns_expr/0},
    {"conj fails for invalid info",
     fun conj_fails_for_invalid_info/0},
    {"conj fails for invalid lhs",
     fun conj_fails_for_invalid_lhs/0},
    {"conj fails for invalid rhs",
     fun conj_fails_for_invalid_rhs/0},
    {"disj returns expr",
     fun disj_returns_expr/0},
    {"disj fails for invalid info",
     fun disj_fails_for_invalid_info/0},
    {"disj fails for invalid lhs",
     fun disj_fails_for_invalid_lhs/0},
    {"disj fails for invalid rhs",
     fun disj_fails_for_invalid_rhs/0},
    {"neg returns expr",
     fun neg_returns_expr/0},
    {"neg fails for invalid info",
     fun neg_fails_for_invalid_info/0},
    {"neg fails for invalid operand",
     fun neg_fails_for_invalid_operand/0},
    {"isnil returns expr",
     fun isnil_returns_expr/0},
    {"isnil fails for invalid info",
     fun isnil_fails_for_invalid_info/0},
    {"isnil fails for invalid operand",
     fun isnil_fails_for_invalid_operand/0},
    {"cnd returns expr",
     fun cnd_returns_expr/0},
    {"cnd fails for invalid info",
     fun cnd_fails_for_invalid_info/0},
    {"cnd fails for invalid condition expr",
     fun cnd_fails_for_invalid_condition_expr/0},
    {"cnd fails for invalid then expr",
     fun cnd_fails_for_invalid_then_expr/0},
    {"cnd fails for invalid else expr",
     fun cnd_fails_for_invalid_else_expr/0},
    {"null returns expr",
     fun null_returns_expr/0},
    {"null fails for invalid info",
     fun null_fails_for_invalid_info/0},
    {"null fails for invalid type",
     fun null_fails_for_invalid_type/0},
    {"cons returns expr",
     fun cons_returns_expr/0},
    {"cons fails for invalid info",
     fun cons_fails_for_invalid_info/0},
    {"cons fails for invalid lhs",
     fun cons_fails_for_invalid_lhs/0},
    {"cons fails for invalid rhs",
     fun cons_fails_for_invalid_rhs/0},
    {"hd returns expr",
     fun hd_returns_expr/0},
    {"hd fails for invalid info",
     fun hd_fails_for_invalid_info/0},
    {"hd fails for invalid lhs",
     fun hd_fails_for_invalid_lhs/0},
    {"hd fails for invalid rhs",
     fun hd_fails_for_invalid_rhs/0},
    {"tl returns expr",
     fun tl_returns_expr/0},
    {"tl fails for invalid info",
     fun tl_fails_for_invalid_info/0},
    {"tl fails for invalid lhs",
     fun tl_fails_for_invalid_lhs/0},
    {"tl fails for invalid rhs",
     fun tl_fails_for_invalid_rhs/0},
    {"append returns expr",
     fun append_returns_expr/0},
    {"append fails for invalid info",
     fun append_fails_for_invalid_info/0},
    {"append fails for invalid lhs",
     fun append_fails_for_invalid_lhs/0},
    {"append fails for invalid rhs",
     fun append_fails_for_invalid_rhs/0},
    {"for returns expr",
     fun for_returns_expr/0},
    {"for fails for invalid info",
     fun for_fails_for_invalid_info/0},
    {"for fails for invalid type",
     fun for_fails_for_invalid_type/0},
    {"for fails for invalid xte list",
     fun for_fails_for_invalid_xte_list/0},
    {"for fails for invalid xte",
     fun for_fails_for_invalid_xte/0},
    {"for fails for invalid x",
     fun for_fails_for_invalid_x/0},
    {"for fails for invalid t",
     fun for_fails_for_invalid_t/0},
    {"for fails for invalid e",
     fun for_fails_for_invalid_e/0},
    {"for fails for invalid body expr",
     fun for_fails_for_invalid_body_expr/0},
    {"fold returns expr",
     fun fold_returns_expr/0},
    {"fold fails for invalid info",
     fun fold_fails_for_invalid_info/0},
    {"fold fails for invalid acc xte",
     fun fold_fails_for_invalid_acc_xte/0},
    {"fold fails for invalid acc x",
     fun fold_fails_for_invalid_acc_x/0},
    {"fold fails for invalid acc t",
     fun fold_fails_for_invalid_acc_t/0},
    {"fold fails for invalid acc e",
     fun fold_fails_for_invalid_acc_e/0},
    {"fold fails for invalid list xte",
     fun fold_fails_for_invalid_list_xte/0},
    {"fold fails for invalid list x",
     fun fold_fails_for_invalid_list_x/0},
    {"fold fails for invalid list t",
     fun fold_fails_for_invalid_list_t/0},
    {"fold fails for invalid list e",
     fun fold_fails_for_invalid_list_e/0},
    {"fold fails for invalid body expr",
     fun fold_fails_for_invalid_body_expr/0},
    {"rcd returns expr",
     fun rcd_returns_expr/0},
    {"rcd fails for invalid info",
     fun rcd_fails_for_invalid_info/0},
    {"rcd fails for invalid xe list",
     fun rcd_fails_for_invalid_xe_list/0},
    {"rcd fails for invalid xe",
     fun rcd_fails_for_invalid_xe/0},
    {"rcd fails for invalid x",
     fun rcd_fails_for_invalid_x/0},
    {"rcd fails for invalid e",
     fun rcd_fails_for_invalid_e/0},
    {"proj returns expr",
     fun proj_returns_expr/0},
    {"proj fails for invalid info",
     fun proj_fails_for_invalid_info/0},
    {"proj fails for invalid field name",
     fun proj_fails_for_invalid_field_name/0},
    {"proj fails for invalid operand",
     fun proj_fails_for_invalid_operand/0},
    {"err run returns expr",
     fun err_run_returns_expr/0},
    {"err stagein returns expr",
     fun err_stagein_returns_expr/0},
    {"err stageout returns expr",
     fun err_stageout_returns_expr/0},
    {"err user returns expr",
     fun err_user_returns_expr/0},
    {"err fails for invalid info",
     fun err_fails_for_invalid_info/0},
    {"err fails for invalid type",
     fun err_fails_for_invalid_type/0},
    {"err fails for invalid reason",
     fun err_fails_for_invalid_reason/0},
    {"err run fails for invalid node",
     fun err_run_fails_for_invalid_node/0},
    {"err run fails for invalid appid",
     fun err_run_fails_for_invalid_appid/0},
    {"err run fails for invalid lamname",
     fun err_run_fails_for_invalid_lamname/0},
    {"err run fails for invalid script",
     fun err_run_fails_for_invalid_script/0},
    {"err run fails for invalid output",
     fun err_run_fails_for_invalid_output/0},
    {"err stagein fails for invalid node",
     fun err_stagein_fails_for_invalid_node/0},
    {"err stagein fails for invalid appid",
     fun err_stagein_fails_for_invalid_appid/0},
    {"err stagein fails for invalid lamname",
     fun err_stagein_fails_for_invalid_lamname/0},
    {"err stagein fails for invalid file list",
     fun err_stagein_fails_for_invalid_file_list/0},
    {"err stagein fails for invalid file",
     fun err_stagein_fails_for_invalid_file/0},
    {"err stageout fails for invalid node",
     fun err_stageout_fails_for_invalid_node/0},
    {"err stageout fails for invalid appid",
     fun err_stageout_fails_for_invalid_appid/0},
    {"err stageout fails for invalid lamname",
     fun err_stageout_fails_for_invalid_lamname/0},
    {"err stageout fails for invalid file list",
     fun err_stageout_fails_for_invalid_file_list/0},
    {"err stageout fails for invalid file",
     fun err_stageout_fails_for_invalid_file/0},
    {"err user fails for invalid msg",
     fun err_user_fails_for_invalid_msg/0}
   ]  
  }.

var_returns_expr() ->
  ?assertEqual( {var, na, x}, var( na, x ) ).

var_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, var( y, x ) ).

var_fails_for_invalid_variable_name() ->
  ?assertError( {bad_atom, 5}, var( na, 5 ) ).

lam_ntv_returns_expr() ->
  ?assertEqual( {lam, na, [{x, 'Str'}], {ntv, {var, na, x}}},
                lam( na, [{x, t_str()}], {ntv, var( x )} ) ).

lam_frn_returns_expr() ->
  Body = {frn, f, t_rcd( [{y, t_str()}] ), l_bash(), <<"blub">>},
  ?assertEqual( {lam, na, [{x, 'Str'}], Body},
                lam( na, [{x, t_str()}], Body ) ).

lam_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, lam( y, [], {ntv, var( x )} ) ).

lam_fails_for_invalid_xt_list() ->
  ?assertError( {bad_xt_lst, 5}, lam( na, 5, {ntv, var( x )} ) ).

lam_fails_for_invalid_xt() ->
  ?assertError( {bad_xt, 5}, lam( na, [5], {ntv, var( x )} ) ).

lam_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, lam( na, [{5, t_str()}], {ntv, var( x )} ) ).

lam_fails_for_invalid_t() ->
  ?assertError( {bad_type, 5}, lam( na, [{x, 5}], {ntv, var( x )} ) ).

lam_fails_for_invalid_body() ->
  ?assertError( {bad_body, 5}, lam( na, [], 5 ) ).

lam_ntv_fails_for_invalid_body_expr() ->
  ?assertError( {bad_expr, 5}, lam( na, [], {ntv, 5} ) ).

lam_frn_fails_for_invalid_function_name() ->
  Body = {frn, 5, t_rcd( [{y, t_str()}] ), l_bash(), <<"blub">>},
  ?assertError( {bad_atom, 5}, lam( na, [], Body ) ).

lam_frn_fails_for_invalid_return_type() ->
  Body = {frn, f, 5, l_bash(), <<"blub">>},
  ?assertError( {bad_type, 5}, lam( na, [], Body ) ).

lam_frn_fails_for_invalid_lang() ->
  Body = {frn, f, t_rcd( [{y, t_str()}] ), 5, <<"blub">>},
  ?assertError( {bad_lang, 5}, lam( na, [], Body ) ).

lam_frn_fails_for_invalid_script() ->
  Body = {frn, f, t_rcd( [{y, t_str()}] ), l_bash(), 5},
  ?assertError( {bad_binary, 5}, lam( na, [], Body ) ).

app_returns_expr() ->
  E = app( na, lam( [{x, t_str()}], {ntv, var( x )} ),
               [{x, str( <<"bla">> )}] ),
  ?assertEqual( {app, na, {lam, na, [{x, 'Str'}], {ntv, {var, na, x}}},
                          [{x, {str, na, <<"bla">>}}]},
                E ).

app_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, app( y, var( f ), [] ) ).

app_fails_for_invalid_function_expr() ->
  ?assertError( {bad_expr, 5}, app( na, 5, [] ) ).

app_fails_for_invalid_xe_list() ->
  ?assertError( {bad_xe_lst, 5}, app( na, var( f ), 5 ) ).

app_fails_for_invalid_xe() ->
  ?assertError( {bad_xe, 5}, app( na, var( f ), [5] ) ).

app_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, app( na, var( f ), [{5, true()}] ) ).

app_fails_for_invalid_e() ->
  ?assertError( {bad_expr, 5}, app( na, var( f ), [{x, 5}] ) ).

fix_returns_expr() ->
  ?assertEqual( {fix, na, {true, na}}, fix( na, true() ) ).

fix_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, fix( y, true() ) ).

fix_fails_for_invalid_operand() ->
  ?assertError( {bad_expr, 5}, fix( na, 5 ) ).

fut_returns_expr() ->
  ?assertEqual( {fut, na, {true, na}}, fut( na, true() ) ).

fut_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, fut( y, true() ) ).

fut_fails_for_invalid_operand() ->
  ?assertError( {bad_expr, 5}, fut( na, 5 ) ).

str_returns_expr() ->
  ?assertEqual( {str, na, <<"blub">>}, str( na, <<"blub">> ) ).

str_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, str( y, <<"blub">> ) ).

str_fails_for_invalid_content() ->
  ?assertError( {bad_binary, 5}, str( na, 5 ) ).

file_returns_expr() ->
  ?assertEqual( {file, na, <<"blub">>}, file( na, <<"blub">> ) ).

file_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, file( y, <<"blub">> ) ).

file_fails_for_invalid_content() ->
  ?assertError( {bad_binary, 5}, file( na, 5 ) ).

true_returns_expr() ->
  ?assertEqual( {true, na}, true( na ) ).

true_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, true( y ) ).

false_returns_expr() ->
  ?assertEqual( {false, na}, false( na ) ).

false_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, false( y ) ).

cmp_returns_expr() ->
  ?assertEqual( {cmp, na, {true, na}, {false, na}},
                cmp( na, true(), false() ) ).

cmp_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, cmp( y, true(), false() ) ).

cmp_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, cmp( na, 5, false() ) ).

cmp_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, cmp( na, true(), 5 ) ).

conj_returns_expr() ->
  ?assertEqual( {conj, na, {true, na}, {false, na}},
                conj( na, true(), false() ) ).

conj_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, conj( y, true(), false() ) ).

conj_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, conj( na, 5, false() ) ).

conj_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, conj( na, true(), 5 ) ).

disj_returns_expr() ->
  ?assertEqual( {disj, na, {true, na}, {false, na}},
                disj( na, true(), false() ) ).

disj_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, disj( y, true(), false() ) ).

disj_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, disj( na, 5, false() ) ).

disj_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, disj( na, true(), 5 ) ).

neg_returns_expr() ->
  ?assertEqual( {neg, na, {true, na}}, neg( na, true() ) ).

neg_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, neg( y, true() ) ).

neg_fails_for_invalid_operand() ->
  ?assertError( {bad_expr, 5}, neg( na, 5 ) ).

isnil_returns_expr() ->
  ?assertEqual( {isnil, na, {null, na, t_bool()}},
                isnil( na, null( t_bool() ) ) ).

isnil_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, isnil( y, null( t_bool() ) ) ).

isnil_fails_for_invalid_operand() ->
  ?assertError( {bad_expr, 5}, isnil( na, 5 ) ).

cnd_returns_expr() ->
  ?assertEqual( {cnd, na, {true, na},
                          {str, na, <<"bla">>},
                          {str, na, <<"blub">>}},
                cnd( na, true(), str( <<"bla">> ), str( <<"blub">> ) ) ).

cnd_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                cnd( y, true(), str( <<"bla">> ), str( <<"blub">> ) ) ).

cnd_fails_for_invalid_condition_expr() ->
  ?assertError( {bad_expr, 5},
                cnd( na, 5, str( <<"bla">> ), str( <<"blub">> ) ) ).

cnd_fails_for_invalid_then_expr() ->
  ?assertError( {bad_expr, 5}, cnd( na, true(), 5, str( <<"blub">> ) ) ).

cnd_fails_for_invalid_else_expr() ->
  ?assertError( {bad_expr, 5}, cnd( na, true(), str( <<"bla">> ), 5 ) ).

null_returns_expr() ->
  ?assertEqual( {null, na, 'Str'}, null( na, t_str() ) ).

null_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, null( y, t_str() ) ).

null_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5}, null( na, 5 ) ).

cons_returns_expr() ->
  ?assertEqual( {cmp, na, {true, na}, {false, na}},
                cmp( na, true(), false() ) ).

cons_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, cons( y, true(), null( t_bool() ) ) ).

cons_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, cons( na, 5, null( t_bool() ) ) ).

cons_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, cons( na, true(), 5 ) ).

hd_returns_expr() ->
  ?assertEqual( {hd, na, {null, na, 'Bool'}, {false, na}},
                hd( na, null( t_bool() ), false() ) ).

hd_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, hd( y, null( t_bool() ), false() ) ).

hd_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, hd( na, 5, false() ) ).

hd_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, hd( na, null( t_bool() ), 5 ) ).

tl_returns_expr() ->
  ?assertEqual( {tl, na, {null, na, 'Bool'}, {false, na}},
                tl( na, null( t_bool() ), false() ) ).

tl_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, tl( y, null( t_bool() ), false() ) ).

tl_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, tl( na, 5, false() ) ).

tl_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, tl( na, null( t_bool() ), 5 ) ).

append_returns_expr() ->
  ?assertEqual( {append, na, {null, na, 'Bool'}, {null, na, 'Bool'}},
                append( na, null( t_bool() ), null( t_bool() ) ) ).

append_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, append( y, null( t_bool() ), null( t_bool() ) ) ).

append_fails_for_invalid_lhs() ->
  ?assertError( {bad_expr, 5}, append( na, 5, null( t_bool() ) ) ).

append_fails_for_invalid_rhs() ->
  ?assertError( {bad_expr, 5}, append( na, null( t_bool() ), 5 ) ).

for_returns_expr() ->
  ?assertEqual( {for, na, 'Bool', [{x, 'Bool', {null, na, 'Bool'}}],
                          {neg, na, {var, na, x}}},
                for( na, t_bool(), [{x, t_bool(), null( t_bool() )}],
                         neg( var( x ) ) ) ).

for_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                for( y, t_bool(), [{x, t_bool(), null( t_bool() )}],
                        neg( var( x ) ) ) ).

for_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5},
                for( na, 5, [{x, t_bool(), null( t_bool() )}],
                         neg( var( x ) ) ) ).

for_fails_for_invalid_xte_list() ->
  ?assertError( {bad_xte_lst, 5},
                for( na, t_bool(), 5, neg( var( x ) ) ) ).

for_fails_for_invalid_xte() ->
  ?assertError( {bad_xte, 5},
                for( na, t_bool(), [5], neg( var( x ) ) ) ).

for_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5},
                for( na, t_bool(), [{5, t_bool(), null( t_bool() )}],
                         neg( var( x ) ) ) ).

for_fails_for_invalid_t() ->
  ?assertError( {bad_type, 5},
                for( na, t_bool(), [{x, 5, null( t_bool() )}],
                         neg( var( x ) ) ) ).

for_fails_for_invalid_e() ->
  ?assertError( {bad_expr, 5},
                for( na, t_bool(), [{x, t_bool(), 5}],
                         neg( var( x ) ) ) ).

for_fails_for_invalid_body_expr() ->
  ?assertError( {bad_expr, 5},
                for( na, t_bool(), [{x, t_bool(), null( t_bool() )}], 5 ) ).

fold_returns_expr() ->
  ?assertEqual( {fold, na, {acc, 'Bool', {true, na}},
                           {x, 'Bool', {null, na, 'Bool'}},
                           {conj, na, {var, na, acc}, {var, na, x}}},
                fold( na, {acc, t_bool(), true()},
                          {x, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                fold( y, {acc, t_bool(), true()},
                         {x, t_bool(), null( t_bool() )},
                         conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_acc_xte() ->
  ?assertError( {bad_xte, 5},
                fold( na, 5,
                          {x, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_acc_x() ->
  ?assertError( {bad_atom, 5},
                fold( na, {5, t_bool(), true()},
                          {x, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_acc_t() ->
  ?assertError( {bad_type, 5},
                fold( na, {acc, 5, true()},
                          {x, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_acc_e() ->
  ?assertError( {bad_expr, 5},
                fold( na, {acc, t_bool(), 5},
                          {x, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_list_xte() ->
  ?assertError( {bad_xte, 5},
                fold( na, {acc, t_bool(), true()},
                          5,
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_list_x() ->
  ?assertError( {bad_atom, 5},
                fold( na, {acc, t_bool(), true()},
                          {5, t_bool(), null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_list_t() ->
  ?assertError( {bad_type, 5},
                fold( na, {acc, t_bool(), true()},
                          {x, 5, null( t_bool() )},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_list_e() ->
  ?assertError( {bad_expr, 5},
                fold( na, {acc, t_bool(), true()},
                          {x, t_bool(), 5},
                          conj( var( acc ), var( x ) ) ) ).

fold_fails_for_invalid_body_expr() ->
  ?assertError( {bad_expr, 5},
                fold( na, {acc, t_bool(), true()},
                          {x, t_bool(), null( t_bool() )},
                          5 ) ).

rcd_returns_expr() ->
  ?assertEqual( {rcd, na, [{a, {str, na, <<"blub">>}}]},
                rcd( na, [{a, str( <<"blub">> )}] ) ).

rcd_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, rcd( y, [{a, str( <<"blub">> )}] ) ).

rcd_fails_for_invalid_xe_list() ->
  ?assertError( {bad_xe_lst, 5}, rcd( na, 5 ) ).

rcd_fails_for_invalid_xe() ->
  ?assertError( {bad_xe, 5}, rcd( na, [5] ) ).

rcd_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, rcd( na, [{5, str( <<"blub">> )}] ) ).

rcd_fails_for_invalid_e() ->
  ?assertError( {bad_expr, 5}, rcd( na, [{a, 5}] ) ).

proj_returns_expr() ->
  ?assertEqual( {proj, na, a, {rcd, na, [{a, {str, na, <<"blub">>}}]}},
                proj( na, a, rcd( [{a, str( <<"blub">> )}] ) ) ).

proj_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, proj( y, a, rcd( [{a, str( <<"blub">> )}] ) ) ).

proj_fails_for_invalid_field_name() ->
  ?assertError( {bad_atom, 5}, proj( na, 5, rcd( [{a, str( <<"blub">> )}] ) ) ).

proj_fails_for_invalid_operand() ->
  ?assertError( {bad_expr, 5}, proj( na, a, 5 ) ).

err_run_returns_expr() ->
  Reason = {run, <<"a@b">>, <<"ef12">>, f, <<"bla">>, <<"blub">>},
  ?assertEqual( {err, na, 'Bool', Reason},
                err( na, t_bool(), Reason ) ).

err_stagein_returns_expr() ->
  Reason = {stagein, <<"a@b">>, <<"ef12">>, f, [<<"a.txt">>]},
  ?assertEqual( {err, na, 'Bool', Reason},
                err( na, t_bool(), Reason ) ).

err_stageout_returns_expr() ->
  Reason = {stageout, <<"a@b">>, <<"ef12">>, f, [<<"a.txt">>]},
  ?assertEqual( {err, na, 'Bool', Reason},
                err( na, t_bool(), Reason ) ).

err_user_returns_expr() ->
  ?assertEqual( {err, na, 'Bool', {user, <<"blub">>}},
                err( na, t_bool(), {user, <<"blub">>} ) ).

err_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, err( y, t_bool(), {user, <<"blub">>} ) ).

err_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5}, err( na, 5, {user, <<"blub">>} ) ).

err_fails_for_invalid_reason() ->
  ?assertError( {bad_reason, 5}, err( na, t_bool(), 5 ) ).

err_run_fails_for_invalid_node() ->
  R = {run, 5, <<"ef12">>, f, <<"bla">>, <<"blub">>},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_run_fails_for_invalid_appid() ->
  R = {run, <<"a@b">>, 5, f, <<"bla">>, <<"blub">>},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_run_fails_for_invalid_lamname() ->
  R = {run, <<"a@b">>, <<"ef12">>, 5, <<"bla">>, <<"blub">>},
  ?assertError( {bad_atom, 5}, err( na, t_bool(), R ) ).

err_run_fails_for_invalid_script() ->
  R = {run, <<"a@b">>, <<"ef12">>, f, 5, <<"blub">>},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_run_fails_for_invalid_output() ->
  R = {run, <<"a@b">>, <<"ef12">>, f, <<"bla">>, 5},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_stagein_fails_for_invalid_node() ->
  R = {stagein, 5, <<"ef12">>, f, [<<"bla.txt">>]},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_stagein_fails_for_invalid_appid() ->
  R = {stagein, <<"a@b">>, 5, f, [<<"bla.txt">>]},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_stagein_fails_for_invalid_lamname() ->
  R = {stagein, <<"a@b">>, <<"ef12">>, 5, [<<"bla.txt">>]},
  ?assertError( {bad_atom, 5}, err( na, t_bool(), R ) ).

err_stagein_fails_for_invalid_file_list() ->
  R = {stagein, <<"a@b">>, <<"ef12">>, f, 5},
  ?assertError( {bad_file_lst, 5}, err( na, t_bool(), R ) ).

err_stagein_fails_for_invalid_file() ->
  R = {stagein, <<"a@b">>, <<"ef12">>, f, [5]},
  ?assertError( {bad_file_lst, [5]}, err( na, t_bool(), R ) ).

err_stageout_fails_for_invalid_node() ->
  R = {stageout, 5, <<"ef12">>, f, [<<"bla.txt">>]},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_stageout_fails_for_invalid_appid() ->
  R = {stageout, <<"a@b">>, 5, f, [<<"bla.txt">>]},
  ?assertError( {bad_binary, 5}, err( na, t_bool(), R ) ).

err_stageout_fails_for_invalid_lamname() ->
  R = {stageout, <<"a@b">>, <<"ef12">>, 5, [<<"bla.txt">>]},
  ?assertError( {bad_atom, 5}, err( na, t_bool(), R ) ).

err_stageout_fails_for_invalid_file_list() ->
  R = {stageout, <<"a@b">>, <<"ef12">>, f, 5},
  ?assertError( {bad_file_lst, 5}, err( na, t_bool(), R ) ).

err_stageout_fails_for_invalid_file() ->
  R = {stageout, <<"a@b">>, <<"ef12">>, f, [5]},
  ?assertError( {bad_file_lst, [5]}, err( na, t_bool(), R ) ).

err_user_fails_for_invalid_msg() ->
  ?assertError( {bad_binary, 5}, err( na, t_bool(), {user, 5} ) ).


%%====================================================================
%% Syntactic Sugar
%%====================================================================

sugar_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"lst returns cons chain",
     fun lst_returns_cons_chain/0},
    {"lst no elements returns null",
     fun lst_no_elements_returns_null/0},
    {"lst fails for invalid info",
     fun lst_fails_for_invalid_info/0},
    {"lst fails for invalid type",
     fun lst_fails_for_invalid_type/0},
    {"lst fails for invalid element list",
     fun lst_fails_for_invalid_element_list/0},
    {"lst fails for invalid element",
     fun lst_fails_for_invalid_element/0},
    {"alet returns expr",
     fun alet_returns_expr/0},
    {"alet fails for invalid info",
     fun alet_fails_for_invalid_info/0},
    {"alet fails for invalid xte list",
     fun alet_fails_for_invalid_xte_list/0},
    {"alet fails for invalid xte",
     fun alet_fails_for_invalid_xte/0},
    {"alet fails for invalid x",
     fun alet_fails_for_invalid_x/0},
    {"alet fails for invalid t",
     fun alet_fails_for_invalid_t/0},
    {"alet fails for invalid e",
     fun alet_fails_for_invalid_e/0},
    {"alet fails for invalid body expr",
     fun alet_fails_for_invalid_body_expr/0},
    {"asc returns expr",
     fun asc_returns_expr/0},
    {"asc fails for invalid info",
     fun asc_fails_for_invalid_info/0},
    {"asc fails for invalid expr",
     fun asc_fails_for_invalid_expr/0},
    {"asc fails for invalid type",
     fun asc_fails_for_invalid_type/0}
   ]
  }.

lst_returns_cons_chain() ->
  ?assertEqual( {cons, na, {true, na}, {cons, na, {false, na}, {null, na, 'Bool'}}},
                lst( na, t_bool(), [true(), false()] ) ).

lst_no_elements_returns_null() ->
  ?assertEqual( {null, na, 'Bool'}, lst( na, t_bool(), [] ) ).

lst_fails_for_invalid_info() ->
  ?assertError( {bad_info, y}, lst( y, t_bool(), [true(), false()] ) ).

lst_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5}, lst( na, 5, [true(), false()] ) ).

lst_fails_for_invalid_element_list() ->
  ?assertError( {bad_element_lst, 5}, lst( na, t_bool(), 5 ) ).

lst_fails_for_invalid_element() ->
  ?assertError( {bad_expr, 5}, lst( na, t_bool(), [5] ) ).

alet_returns_expr() ->
  ?assertEqual( {app, na, {lam, na, [{x, 'Str'}], {ntv, {var, na, x}}}, [{x, {str, na, <<"bla">>}}]},
                alet( na, [{x, t_str(), str( <<"bla">> )}], var( x ) ) ).

alet_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                alet( y, [{x, t_str(), str( <<"bla">> )}], var( x ) ) ).

alet_fails_for_invalid_xte_list() ->
  ?assertError( {bad_xte_lst, 5},
                alet( na, 5, var( x ) ) ).

alet_fails_for_invalid_xte() ->
  ?assertError( {bad_xte, 5},
                alet( na, [5], var( x ) ) ).

alet_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5},
                alet( na, [{5, t_str(), str( <<"bla">> )}], var( x ) ) ).

alet_fails_for_invalid_t() ->
  ?assertError( {bad_type, 5},
                alet( na, [{x, 5, str( <<"bla">> )}], var( x ) ) ).

alet_fails_for_invalid_e() ->
  ?assertError( {bad_expr, 5},
                alet( na, [{x, t_str(), 5}], var( x ) ) ).

alet_fails_for_invalid_body_expr() ->
  ?assertError( {bad_expr, 5},
                alet( na, [{x, t_str(), str( <<"bla">> )}], 5 ) ).

asc_returns_expr() ->
  ?assertMatch( {app, na, {lam, na, [{X, 'Str'}], {ntv, {var, na, X}}},
                          [{X, {str, na, <<"bla">>}}]} when is_atom( X ),
                asc( na, str( <<"bla">> ), t_str() ) ).

asc_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                asc( y, str( <<"bla">> ), t_str() ) ).

asc_fails_for_invalid_expr() ->
  ?assertError( {bad_expr, 5},
                asc( na, 5, t_str() ) ).

asc_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5},
                asc( na, str( <<"bla">> ), 5 ) ).


sugar_abbrev_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"lst2 abbreviates lst3",
     fun lst2_abbreviates_lst3/0},
    {"let2 abbreviates let3",
     fun let2_abbreviates_let3/0},
    {"asc2 abbreviates asc3",
     fun asc2_abbreviates_asc3/0}
   ]
  }.

lst2_abbreviates_lst3() ->
  ?assertEqual( lst( na, t_str(), [] ),
                lst( t_str(), [] ) ).

let2_abbreviates_let3() ->
  ?assertEqual( alet( na, [{x, t_str(), str( <<"bla">> )}], var( x ) ),
                alet( [{x, t_str(), str( <<"bla">> )}], var( x ) ) ).

asc2_abbreviates_asc3() ->
  ?assertEqual( asc( na, str( <<"bla">> ), t_str() ),
                asc( str( <<"bla">> ), t_str() ) ).

%%====================================================================
%% Pattern Constructors, Assignments, and Expansion
%%====================================================================

pattern_constructor_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"r_var returns pattern",
     fun r_var_returns_pattern/0},
    {"r_var fails for invalid x",
     fun r_var_fails_for_invalid_name/0},
    {"r_var fails for invalid t",
     fun r_var_fails_for_invalid_type/0},
    {"r_rcd returns pattern",
     fun r_rcd_returns_pattern/0},
    {"r_rcd fails for invalid xr list",
     fun r_rcd_fails_for_invalid_xr_list/0},
    {"r_rcd fails for invalid xr",
     fun r_rcd_fails_for_invalid_xr/0},
    {"r_rcd fails for invalid x",
     fun r_rcd_fails_for_invalid_x/0},
    {"r_rcd fails for invalid r",
     fun r_rcd_fails_for_invalid_r/0}
   ]
  }.

r_var_returns_pattern() ->
  ?assertEqual( {r_var, x, 'Str'}, r_var( x, t_str() ) ).

r_var_fails_for_invalid_name() ->
  ?assertError( {bad_atom, 5}, r_var( 5, t_str() ) ).

r_var_fails_for_invalid_type() ->
  ?assertError( {bad_type, 5}, r_var( x, 5 ) ).

r_rcd_returns_pattern() ->
  ?assertEqual( {r_rcd, [{a, {r_var, x, 'Str'}}]},
                r_rcd( [{a, r_var( x, t_str() )}] ) ).

r_rcd_fails_for_invalid_xr_list() ->
  ?assertError( {bad_xr_lst, 5}, r_rcd( 5 ) ).

r_rcd_fails_for_invalid_xr() ->
  ?assertError( {bad_xr, 5}, r_rcd( [5] ) ).

r_rcd_fails_for_invalid_x() ->
  ?assertError( {bad_atom, 5}, r_rcd( [{5, r_var( x, t_str() )}] ) ).

r_rcd_fails_for_invalid_r() ->
  ?assertError( {bad_pattern, 5}, r_rcd( [{a, 5}] ) ).


assign_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"assign returns assign",
     fun assign_returns_assign/0},
    {"assign fails for invalid info",
     fun assign_fails_for_invalid_info/0},
    {"assign fails for invalid pattern",
     fun assign_fails_for_invalid_pattern/0},
    {"assign fails for invalid expr",
     fun assign_fails_for_invalid_expr/0}
   ]
  }.

assign_returns_assign() ->
  ?assertEqual( {assign, na, {r_var, x, 'Str'}, {str, na, <<"bla">>}},
                assign( na, r_var( x, t_str() ), str( <<"bla">> ) ) ).

assign_fails_for_invalid_info() ->
  ?assertError( {bad_info, y},
                assign( y, r_var( x, t_str() ), str( <<"bla">> ) ) ).

assign_fails_for_invalid_pattern() ->
  ?assertError( {bad_pattern, 5},
                assign( na, 5, str( <<"bla">> ) ) ).

assign_fails_for_invalid_expr() ->
  ?assertError( {bad_expr, 5},
                assign( na, r_var( x, t_str() ), 5 ) ).



expand_closure_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [fun assign_variable_pattern/0,
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

validate_info_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"validate_info na returns info",
     fun validate_info_na_returns_info/0},
    {"validate_info pos integer returns info",
     fun validate_info_pos_integer_returns_info/0},
    {"validate_info fails for zero",
     fun validate_info_fails_for_zero/0},
    {"validate_info fails for neg integer",
     fun validate_info_fails_for_neg_integer/0},
    {"validate_info binary pos integer pair returns info",
     fun validate_info_binary_pos_integer_pair_returns_info/0},
    {"validate_info fails for invalid filename",
     fun validate_info_fails_for_invalid_filename/0},
    {"validate_info fails for filename zero pair",
     fun validate_info_fails_for_filename_zero_pair/0},
    {"validate_info fails for filename neg integer pair",
     fun validate_info_fails_for_filename_neg_integer_pair/0}
   ]
  }.

validate_info_na_returns_info() ->
  ?assertEqual( na, validate_info( na ) ).

validate_info_pos_integer_returns_info() ->
  ?assertEqual( 1, validate_info( 1 ) ).

validate_info_fails_for_zero() ->
  ?assertError( {bad_info, 0}, validate_info( 0 ) ).

validate_info_fails_for_neg_integer() ->
  ?assertError( {bad_info, -1}, validate_info( -1 ) ).

validate_info_binary_pos_integer_pair_returns_info() ->
  Info = {<<"bla.cfl">>, 1},
  ?assertEqual( Info, validate_info( Info ) ).

validate_info_fails_for_invalid_filename() ->
  ?assertError( {bad_info, {"bla.txt", 1}}, validate_info( {"bla.txt", 1} ) ).

validate_info_fails_for_filename_zero_pair() ->
  ?assertError( {bad_info, {<<"bla.txt">>, 0}},
                validate_info( {<<"bla.txt">>, 0} ) ).

validate_info_fails_for_filename_neg_integer_pair() ->
  ?assertError( {bad_info, {<<"bla.txt">>, -1}},
                validate_info( {<<"bla.txt">>, -1} ) ).


validate_lang_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"validate_lang awk returns lang",
     fun validate_lang_awk_returns_lang/0},
    {"validate_lang bash returns lang",
     fun validate_lang_bash_returns_lang/0},
    {"validate_lang elixir returns lang",
     fun validate_lang_elixir_returns_lang/0},
    {"validate_lang erlang returns lang",
     fun validate_lang_erlang_returns_lang/0},
    {"validate_lang gnuplot returns lang",
     fun validate_lang_gnuplot_returns_lang/0},
    {"validate_lang java returns lang",
     fun validate_lang_java_returns_lang/0},
    {"validate_lang javascript returns lang",
     fun validate_lang_javascript_returns_lang/0},
    {"validate_lang matlab returns lang",
     fun validate_lang_matlab_returns_lang/0},
    {"validate_lang octave returns lang",
     fun validate_lang_octave_returns_lang/0},
    {"validate_lang perl returns lang",
     fun validate_lang_perl_returns_lang/0},
    {"validate_lang python returns lang",
     fun validate_lang_python_returns_lang/0},
    {"validate_lang r returns lang",
     fun validate_lang_r_returns_lang/0},
    {"validate_lang racket returns lang",
     fun validate_lang_racket_returns_lang/0},
    {"validate_lang fails for invalid lang",
     fun validate_lang_fails_for_invalid_lang/0}
   ]
  }.

validate_lang_awk_returns_lang() ->
  ?assertEqual( 'Awk', validate_lang( l_awk() ) ).

validate_lang_bash_returns_lang() ->
  ?assertEqual( 'Bash', validate_lang( l_bash() ) ).

validate_lang_elixir_returns_lang() ->
  ?assertEqual( 'Elixir', validate_lang( l_elixir() ) ).

validate_lang_erlang_returns_lang() ->
  ?assertEqual( 'Erlang', validate_lang( l_erlang() ) ).

validate_lang_gnuplot_returns_lang() ->
  ?assertEqual( 'Gnuplot', validate_lang( l_gnuplot() ) ).

validate_lang_java_returns_lang() ->
  ?assertEqual( 'Java', validate_lang( l_java() ) ).

validate_lang_javascript_returns_lang() ->
  ?assertEqual( 'Javascript', validate_lang( l_javascript() ) ).

validate_lang_matlab_returns_lang() ->
  ?assertEqual( 'Matlab', validate_lang( l_matlab() ) ).

validate_lang_octave_returns_lang() ->
  ?assertEqual( 'Octave', validate_lang( l_octave() ) ).

validate_lang_perl_returns_lang() ->
  ?assertEqual( 'Perl', validate_lang( l_perl() ) ).

validate_lang_python_returns_lang() ->
  ?assertEqual( 'Python', validate_lang( l_python() ) ).

validate_lang_r_returns_lang() ->
  ?assertEqual( 'R', validate_lang( l_r() ) ).

validate_lang_racket_returns_lang() ->
  ?assertEqual( 'Racket', validate_lang( l_racket() ) ).

validate_lang_fails_for_invalid_lang() ->
  ?assertError( {bad_lang, 5}, validate_lang( 5 ) ).


%%====================================================================
%% Contract Predicates
%%====================================================================

%%====================================================================
%% Renaming and Substitution
%%====================================================================

rename_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [{"rename rcd traverses field",
     fun rename_rcd_traverses_field/0},
    {"rename proj traverses operand",
     fun rename_proj_traverses_operand/0},
    {"rename err no effect",
     fun rename_err_no_effect/0}
   ]
  }.

rename_rcd_traverses_field() ->
  E1 = rcd( [{a, var( x )}] ),
  E2 = rcd( [{a, var( y )}] ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_proj_traverses_operand() ->
  E1 = proj( a, var( x ) ),
  E2 = proj( a, var( y ) ),
  ?assertEqual( E2, rename( E1, x, y ) ).

rename_err_no_effect() ->
  E = err( t_str(), {user, <<"bla">>} ),
  ?assertEqual( E, rename( E, x, y ) ).


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
            {frn, f,
                  t_rcd( [{y, t_str()}] ),
                  l_awk(), <<"bla">>} ),
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
                      {frn, f,
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
  E0 = err( t_str(), {user, <<"my message">>} ),
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





free_vars_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"free_vars for subtracts list binding",
     fun free_vars_for_subtracts_list_binding/0},
    {"free_vars for free var in list binding",
     fun free_vars_for_free_var_in_list_binding/0},
    {"free_vars fold subtracts acc binding",
     fun free_vars_fold_subtracts_acc_binding/0},
    {"free_vars fold subtracts list binding",
     fun free_vars_fold_subtracts_list_binding/0},
    {"free_vars fold free var in acc",
     fun free_vars_fold_free_var_in_acc/0},
    {"free_vars fold free var in list binding",
     fun free_vars_fold_free_var_in_list_binding/0}
   ]
  }.

free_vars_for_subtracts_list_binding() ->
  E = for( t_str(), [{y, t_str(), null( t_str() )}], var( y ) ),
  ?assertEqual( [], expr_free_vars( E ) ).

free_vars_for_free_var_in_list_binding() ->
  E = {hd,na,{proj,na,'',{cnd,na,{disj,na,{fix,na,{for,na,'Str',[{'','Str',{var,na,''}}],{var,na,''}}},{var,na,'\000'}},{var,na,'\000'},{var,na,'\000'}}},{var,na,'\000'}},
  ?assertEqual( ['', '\000'], expr_free_vars( E ) ).

free_vars_fold_subtracts_acc_binding() ->
  E = fold( {x, t_str(), str( <<"blub">> )}, {y, t_str(), null( t_str() )}, var( x ) ),
  ?assertEqual( [], expr_free_vars( E ) ).

free_vars_fold_subtracts_list_binding() ->
  E = fold( {x, t_str(), str( <<"blub">> )}, {y, t_str(), null( t_str() )}, var( y ) ),
  ?assertEqual( [], expr_free_vars( E ) ).

free_vars_fold_free_var_in_acc() ->
  E = {cmp,na,{fix,na,{app,na,{var,na,'\000'},[{'',{append,na,{cmp,na,{var,na,'\000'},{lam,na,[],{ntv,{fix,na,{for,na,'Str',[],{fold,na,{'','Str',{var,na,''}},{'','File',{app,1,{cnd,na,{cmp,na,{proj,na,'\207',{false,{<<>>,1}}},{tl,{<<>>,1},{true,3},{false,{<<53>>,1}}}},{var,na,''},{str,na,<<>>}},[{'',{fix,{<<5>>,1},{str,na,<<>>}}}]}},{null,na,{'Fn',[{'','Str'},{'>','Str'},{'','Str'}],'Bool'}}}}}}}},{var,na,'\000'}}}]}},{var,na,'\000'}},
  ?assertEqual( ['','\000'], expr_free_vars( E ) ).

free_vars_fold_free_var_in_list_binding() ->
  E = fold( {x, t_str(), str( <<"blub">> )}, {y, t_str(), var( y )}, str( <<"bla">> ) ),
  ?assertEqual( [y], expr_free_vars( E ) ).


