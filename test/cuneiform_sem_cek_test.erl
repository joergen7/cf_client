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

-module( cuneiform_sem_cek_test ).


%%====================================================================
%% Includes
%%====================================================================

-include_lib( "eunit/include/eunit.hrl" ).

%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_sem_cek, [split_zip/1, bind_all/3] ).

-import( cuneiform_lang, [
                          null/1, t_str/0, typed_bind/3, cons/2, str/1, t_lst/1,
                          app/2, lam_ntv/2, t_arg/2, e_bind/2
                         ] ).


split_zip_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"all lists empty returns null",
     fun all_lists_empty_returns_null/0},

    {"some list empty returns null",
     fun some_list_empty_returns_null/0},

    {"some list unevaluated returns norule",
     fun some_list_unevaluated_returns_norule/0},

    {"all lists non-empty splits head",
     fun all_lists_nonempty_splits_head/0}
   ]}.

all_lists_empty_returns_null() ->
  EBindLst = [typed_bind( a, t_str(), null( t_str() ) ),
              typed_bind( b, t_str(), null( t_str() ) )],
  ?assertEqual( null, split_zip( EBindLst ) ).

some_list_empty_returns_null() ->
  EBindLst = [typed_bind( a, t_str(), null( t_str() ) ),
              typed_bind( b, t_str(), cons( str( <<"bla">> ), null( t_str() ) ) )],
  ?assertEqual( null, split_zip( EBindLst ) ).

some_list_unevaluated_returns_norule() ->
  EBindLst = [typed_bind( a, t_str(), cons( str( <<"bla">> ), null( t_str() ) ) ),
              typed_bind( b, t_str(), {fut, na, t_lst( t_str() )} )],
  ?assertEqual( stuck, split_zip( EBindLst ) ).

all_lists_nonempty_splits_head() ->
  EBindLst = [typed_bind( a, t_str(), cons( str( <<"bla">> ), null( t_str() ) ) ),
              typed_bind( b, t_str(), cons( str( <<"blub">> ), null( t_str() ) ) )],
  ?assertEqual( {[typed_bind( a, t_str(), str( <<"bla">> ) ),
                  typed_bind( b, t_str(), str( <<"blub">> ) )],
                 [typed_bind( a, t_str(), null( t_str() ) ),
                  typed_bind( b, t_str(), null( t_str() ) )]},
                split_zip( EBindLst ) ).



bind_all_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"empty binding list returns body",
     fun empty_binding_list_returns_body/0},

    {"non-empty binding produces application",
     fun nonempty_binding_produces_application/0}
   ]}.

empty_binding_list_returns_body() ->
  EBody = str( <<"bla">> ),
  ?assertEqual( EBody, bind_all( na, [], EBody ) ).


nonempty_binding_produces_application() ->
  EBody = str( <<"bla">> ),
  E2 = app( lam_ntv( [t_arg( x, t_str() )], EBody ),
            [e_bind( x, str( <<"x">> ) )] ),
  ?assertEqual( E2, bind_all( na, [typed_bind( x, t_str(), str( <<"x">> ) )], EBody ) ).
