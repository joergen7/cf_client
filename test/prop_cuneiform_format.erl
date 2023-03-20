%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2013 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @copyright 2013
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( prop_cuneiform_format ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform_lang.hrl" ).
-include( "cuneiform_type.hrl" ).

-import( cuneiform_format, [format_info/1, format_type/1, format_expr/1,
                            format_error/1] ).

prop_format_info_handles_all_info() ->
  ?FORALL( Info, info(),
    begin
      is_list( format_info( Info ) )
    end ).

prop_format_type_handles_all_t() ->
  ?FORALL( T, t(),
    begin
      is_list( format_type( T ) )
    end ).

% prop_format_expr_handles_all_e() ->
%   ?FORALL( E, e(),
%     begin
%       is_list( format_expr( E ) )
%     end ).

% prop_format_error_handles_all_type_error() ->
%   ?FORALL( TE, type_error(),
%     begin
%       is_list( format_error( {error, type, TE} ) )
%     end ).
