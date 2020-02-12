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

-module( prop_cuneiform_type ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform_lang.hrl" ).

-import( cuneiform_lang, [validate_type/1] ).

-import( cuneiform_type, [is_type_comparable/1,
                          is_type_equivalent/2] ).

%% Type Comparability

prop_is_type_comparable_total() ->
  ?FORALL( Z, oneof( [t(), term()] ),
    begin
      is_boolean( is_type_comparable( Z ) )
    end ).

%% Type Equivalence

prop_is_type_equivalent_reflexive() ->
  ?FORALL( T, t(),
    begin
      is_type_equivalent( T, T )
    end ).

prop_is_type_equivalent_symmetric() ->
  ?FORALL( T1, t(),
    ?FORALL( T2, t(),
      begin
        is_type_equivalent( T1, T2 ) =:= is_type_equivalent( T2, T1 )
      end ) ).

