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

-import( cuneiform_format, [format_type/1] ).

-import( cuneiform_lang, [t_str/0, t_file/0, t_bool/0, t_fn/2] ).

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
    {"format type Fn 2", fun format_type_fn_2/0}
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