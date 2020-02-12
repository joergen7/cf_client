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
%% @copyright 2013-2020
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( prop_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform_lang.hrl" ).


-import( cuneiform_lang, [validate_assign/1,
                          validate_expr/1,
                          validate_lang/1,
                          validate_pattern/1,
                          validate_reason/1,
                          validate_type/1] ).


%%==========================================================
%% Properties
%%==========================================================





%% Types

prop_validate_type_always_returns_original() ->
  ?FORALL( T, t(),
    begin
      T =:= validate_type( T )
    end ).

%% Langs

prop_validate_lang_always_returns_original() ->
  ?FORALL( L, l(),
    begin
      L =:= validate_lang( L )
    end ).

%% Patterns

prop_validate_pattern_always_returns_original() ->
  ?FORALL( R, r(),
    begin
      R =:= validate_pattern( R )
    end ).

%% Reasons

prop_validate_reason_always_returns_original() ->
  ?FORALL( Reason, reason(),
    begin
      Reason =:= validate_reason( Reason )
    end ).



