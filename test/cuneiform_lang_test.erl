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
%% @version 0.1.2
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cuneiform_lang_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cuneiform_lang, [r_var/2, r_rcd/1, r_bind/2] ).
-import( cuneiform_lang, [t_str/0] ).
-import( cuneiform_lang, [assign/2, create_closure/2] ).
-import( cuneiform_lang, [lam_ntv_arg/2, e_bind/2] ).
-import( cuneiform_lang, [var/1, app/2, lam_ntv/2, proj/2] ).

create_closure_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"assign variable pattern",         fun assign_variable_pattern/0},
    {"last assignment binds innermost", fun last_assignment_binds_innermost/0},
    {"empty record pattern is neutral", fun empty_record_pattern_is_neutral/0},
    {"assignment resolution propagates to record fields",
     fun assignment_resolution_propagates_to_record_fields/0}
   ]
  }.

assign_variable_pattern() ->
  AssignLst = [assign( r_var( x, t_str() ), var( y ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [lam_ntv_arg( x, t_str() )],
                          var( z ) ),
                 [e_bind( x, var( y ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).

last_assignment_binds_innermost() ->
  AssignLst = [assign( r_var( x1, t_str() ), var( y1 ) ),
               assign( r_var( x2, t_str() ), var( y2 ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [lam_ntv_arg( x1, t_str() )],
                          app( lam_ntv( [lam_ntv_arg( x2, t_str() )],
                                        var( z ) ),
                               [e_bind( x2, var( y2 ) )] ) ),
                 [e_bind( x1, var( y1 ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).

empty_record_pattern_is_neutral() ->
  AssignLst = [assign( r_rcd( [] ), var( x ) )],
  EBody = var( y ),
  ?assertEqual( {ok, EBody}, create_closure( AssignLst, EBody ) ).

assignment_resolution_propagates_to_record_fields() ->
  AssignLst = [assign( r_rcd( [r_bind( a, r_var( x, t_str() ) )] ),
               var( y ) )],
  EBody = var( z ),
  Closure = app( lam_ntv( [lam_ntv_arg( x, t_str() )],
                          var( z ) ),
                 [e_bind( x, proj( a, var( y ) ) )] ),
  ?assertEqual( {ok, Closure}, create_closure( AssignLst, EBody ) ).
