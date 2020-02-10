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
%%0
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

-module( cuneiform_format_test ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "eunit/include/eunit.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_format, [format_type/1, format_info/1, format_expr/1] ).

-import( cuneiform_lang, [l_bash/0] ).
-import( cuneiform_lang, [t_str/0, t_file/0, t_bool/0, t_fn/2, t_lst/1,
                          t_rcd/1] ).
-import( cuneiform_lang, [var/1, lam/2, app/2, fix/1, fut/2, str/1, file/1,
                          true/0, false/0, cmp/2, conj/2, disj/2, neg/1,
                          isnil/1, cnd/3, null/1, cons/2, hd/2, append/2,
                          for/3, fold/3, rcd/1, proj/2, err/2, close/2] ).

format_type_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"format type Str", fun format_type_str/0},
    {"format type File", fun format_type_file/0},
    {"format type Bool", fun format_type_bool/0},
    {"format type Fn 0", fun format_type_fn_0/0},
    {"format type Fn 1", fun format_type_fn_1/0},
    {"format type Fn 2", fun format_type_fn_2/0},
    {"format type Lst", fun format_type_lst/0},
    {"format type Rcd 1", fun format_type_rcd_1/0},
    {"format type Rcd 2", fun format_type_rcd_2/0}
   ]
  }.

format_type_str() ->
  ?assertEqual( "Str", format_type( t_str() ) ).

format_type_file() ->
  ?assertEqual( "File", format_type( t_file() ) ).

format_type_bool() ->
  ?assertEqual( "Bool", format_type( t_bool() ) ).

format_type_fn_0() ->
  T = t_fn( [], t_str() ),
  ?assertEqual( "Fn() -> Str", format_type( T ) ).

format_type_fn_1() ->
  T = t_fn( [{x, t_bool()}], t_str() ),
  ?assertEqual( "Fn( x : Bool ) -> Str", format_type( T ) ).

format_type_fn_2() ->
  T = t_fn( [{x, t_bool()}, {y, t_file()}], t_str() ),
  ?assertEqual( "Fn( x : Bool, y : File ) -> Str", format_type( T ) ).

format_type_lst() ->
  T = t_lst( t_bool() ),
  ?assertEqual( "[Bool]", format_type( T ) ).

format_type_rcd_1() ->
  T = t_rcd( [{a, t_str()}] ),
  ?assertEqual( "<a : Str>", format_type( T ) ).

format_type_rcd_2() ->
  T = t_rcd( [{a, t_str()}, {b, t_bool()}] ),
  ?assertEqual( "<a : Str, b : Bool>", format_type( T ) ).

format_info_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"format info na", fun format_info_na/0},
    {"format info line", fun format_info_line/0},
    {"format info file line", fun format_info_file_line/0}
   ]
  }.

format_info_na() ->
  I = na,
  ?assertEqual( "[na]", format_info( I ) ).

format_info_line() ->
  I = 1,
  ?assertEqual( "line 1", format_info( I ) ).

format_info_file_line() ->
  I = {<<"script.cfl">>, 20},
  ?assertEqual( "in script.cfl line 20", format_info( I ) ).





format_expr_test_() ->
  {foreach,
   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"format expr var", fun format_expr_var/0},
    {"format expr ntv lam", fun format_expr_ntv_lam/0},
    {"format expr frn lam", fun format_expr_frn_lam/0},
    {"format expr fn app 0", fun format_expr_fn_app_0/0},
    {"format expr fn app 1", fun format_expr_fn_app_1/0},
    {"format expr fn app 2", fun format_expr_fn_app_2/0},
    {"format expr fix", fun format_expr_fix/0},
    {"format expr fut", fun format_expr_fut/0},
    {"format expr str", fun format_expr_str/0},
    {"format expr file", fun format_expr_file/0},
    {"format expr true", fun format_expr_true/0},
    {"format expr false", fun format_expr_false/0},
    {"format expr cmp", fun format_expr_cmp/0},
    {"format expr conj", fun format_expr_conj/0},
    {"format expr disj", fun format_expr_disj/0},
    {"format expr neg", fun format_expr_neg/0},
    {"format expr isnil", fun format_expr_isnil/0},
    {"format expr cnd", fun format_expr_cnd/0},
    {"format expr null", fun format_expr_null/0},
    {"format expr cons", fun format_expr_cons/0},
    {"format expr cons cons", fun format_expr_cons_cons/0},
    {"format expr hd", fun format_expr_hd/0},
    {"format expr tl", fun format_expr_tl/0},
    {"format expr append", fun format_expr_append/0},
    {"format expr for", fun format_expr_for_1/0},
    {"format expr for", fun format_expr_for_2/0},
    {"format expr fold", fun format_expr_fold/0},
    {"format expr rcd 1", fun format_expr_rcd_1/0},
    {"format expr rcd 2", fun format_expr_rcd_2/0},
    {"format expr proj", fun format_expr_proj/0},
    {"format expr err user", fun format_expr_err_user/0},
    {"format expr err run", fun format_expr_err_run/0},
    {"format expr close", fun format_expr_close/0}
   ]
  }.

format_expr_var() ->
  E = var( x ),
  ?assertEqual( "x", format_expr( E ) ).

format_expr_ntv_lam() ->
  E = lam( [], {ntv, str( <<"bla">> )} ),
  ?assertEqual( "*lam*", format_expr( E ) ).

format_expr_frn_lam() ->
  E = lam( [], {frn, f, t_str(), l_bash(), <<"bla">>} ),
  ?assertEqual( "*lam*", format_expr( E ) ).

format_expr_fn_app_0() ->
  E = app( var( f ), [] ),
  ?assertEqual( "f()", format_expr( E ) ).

format_expr_fn_app_1() ->
  E = app( var( f ), [{x, str( <<"bla">> )}] ),
  ?assertEqual( "f( x = \"bla\" )", format_expr( E ) ).

format_expr_fn_app_2() ->
  E = app( var( f ), [{x, str( <<"bla">> )}, {y, str( <<"blub">> )}] ),
  ?assertEqual( "f( x = \"bla\", y = \"blub\" )", format_expr( E ) ).

format_expr_fix() ->
  E = fix( var( f ) ),
  ?assertEqual( "*fix*", format_expr( E ) ).

format_expr_fut() ->
  E = fut( t_str(), <<"1234">> ),
  ?assertEqual( "*fut*", format_expr( E ) ).

format_expr_str() ->
  E = str( <<"bla">> ),
  ?assertEqual( "\"bla\"", format_expr( E ) ).

format_expr_file() ->
  E = file( <<"bla.txt">> ),
  ?assertEqual( "'bla.txt'", format_expr( E ) ).

format_expr_true() ->
  E = true(),
  ?assertEqual( "true", format_expr( E ) ).

format_expr_false() ->
  E = false(),
  ?assertEqual( "false", format_expr( E ) ).

format_expr_cmp() ->
  E = cmp( str( <<"bla">> ), str( <<"blub">> ) ),
  ?assertEqual( "( \"bla\" == \"blub\" )", format_expr( E ) ).

format_expr_conj() ->
  E = conj( true(), false() ),
  ?assertEqual( "( true and false )", format_expr( E ) ).

format_expr_disj() ->
  E = disj( true(), false() ),
  ?assertEqual( "( true or false )", format_expr( E ) ).

format_expr_neg() ->
  E = neg( true() ),
  ?assertEqual( "not true", format_expr( E ) ).

format_expr_isnil() ->
  E = isnil( var( l ) ),
  ?assertEqual( "isnil l", format_expr( E ) ).

format_expr_cnd() ->
  E = cnd( var( e1 ), var( e2 ), var( e3 ) ),
  ?assertEqual( "if e1 then e2 else e3 end", format_expr( E ) ).

format_expr_null() ->
  E = null( t_str() ),
  ?assertEqual( "[: Str]", format_expr( E ) ).

format_expr_cons() ->
  E = cons( str( <<"bla">> ), null( t_str() ) ),
  ?assertEqual( "[\"bla\" : Str]", format_expr( E ) ).

format_expr_cons_cons() ->
  E = cons( str( <<"bla">> ), cons( str( <<"blub">> ), null( t_str() ) ) ),
  ?assertEqual( "[\"bla\", \"blub\" : Str]", format_expr( E ) ).

format_expr_hd() ->
  E = hd( var( l ), str( <<"bla">> ) ),
  ?assertEqual( "hd l default \"bla\" end", format_expr( E ) ).

format_expr_tl() ->
  E = hd( var( l ), null( t_str() ) ),
  ?assertEqual( "hd l default [: Str] end", format_expr( E ) ).

format_expr_append() ->
  E = append( null( t_str() ), null( t_str() ) ),
  ?assertEqual( "( [: Str] + [: Str] )", format_expr( E ) ).

format_expr_for_1() ->
  E = for( t_str(), [{x, t_str(), var( l )}], var( x ) ),
  ?assertEqual( "for x : Str <- l do x : Str end", format_expr( E ) ).

format_expr_for_2() ->
  E = for( t_str(), [{x, t_str(), var( l )}, {y, t_bool(), var( m )}], var( x ) ),
  ?assertEqual( "for x : Str <- l, y : Bool <- m do x : Str end", format_expr( E ) ).

format_expr_fold() ->
  E = fold( {acc, t_str(), str( <<"blub">> )}, {x, t_str(), var( l )}, var( x ) ),
  ?assertEqual( "fold acc : Str = \"blub\", x : Str <- l do x end", format_expr( E ) ).

format_expr_rcd_1() ->
  E = rcd( [{a, str( <<"bla">> )}] ),
  ?assertEqual( "<a = \"bla\">", format_expr( E ) ).

format_expr_rcd_2() ->
  E = rcd( [{a, str( <<"bla">> )}, {b, true()}] ),
  ?assertEqual( "<a = \"bla\", b = true>", format_expr( E ) ).

format_expr_proj() ->
  E = proj( a, var( r ) ),
  ?assertEqual( "( r | a )", format_expr( E ) ).

format_expr_err_user() ->
  E = err( t_str(), {user, <<"kaboom">>} ),
  ?assertEqual( "error \"kaboom\" : Str", format_expr( E ) ).

format_expr_err_run() ->
  R = {run, <<"minion">>, <<"1234ef">>, f, <<"blub">>, <<"bla">>},
  E = err( t_str(), R ),
  ?assertEqual( "*err*", format_expr( E ) ).

format_expr_close() ->
  E = close( var( x ), #{} ),
  ?assertEqual( "*close*", format_expr( E ) ).