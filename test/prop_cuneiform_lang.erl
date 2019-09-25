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

-module( prop_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform.hrl" ).


-import( cuneiform_lang, [expr_vars/1,
                          expr_free_vars/1,
                          expr_size/1,
                          is_alpha_equivalent/2,
                          is_assign/1,
                          is_expr/1,
                          is_lang/1,
                          is_pattern/1,
                          is_reason/1,
                          is_type/1,
                          is_value/1,
                          protect_expr/1,
                          rename/3,
                          subst/3,
                          validate_assign/1,
                          validate_expr/1,
                          validate_lang/1,
                          validate_pattern/1,
                          validate_reason/1,
                          validate_type/1] ).


%%==========================================================
%% Properties
%%==========================================================

%% Assignments

prop_validate_assign_always_returns_original() ->
  ?FORALL( A, assign(),
    begin
      A =:= validate_assign( A )
    end ).

prop_is_assign_total() ->
  ?FORALL( Z, oneof( [assign(), term()] ), 
    begin
      is_boolean( is_assign( Z ) )
    end ).

prop_is_assign_true_for_assign() ->
  ?FORALL( A, assign(),
    begin
      is_assign( A )
    end ).


%% Expressions

prop_validate_expr_always_returns_original() ->
  ?FORALL( E, e(),
    begin
      E =:= validate_expr( E )
    end ).

prop_is_expr_total() ->
  ?FORALL( Z, oneof( [e(), term()] ), 
    begin
      is_boolean( is_expr( Z ) )
    end ).

prop_is_expr_true_for_expr() ->
  ?FORALL( E, e(),
    begin
      collect( expr_category( E ), is_expr( E ) )
    end ).

%% Types

prop_validate_type_always_returns_original() ->
  ?FORALL( T, t(),
    begin
      T =:= validate_type( T )
    end ).

prop_is_type_total() ->
  ?FORALL( Z, oneof( [t(), term()] ),
    begin
      is_boolean( is_type( Z ) )
    end ).

prop_is_type_true_for_type() ->
  ?FORALL( T, t(),
    begin
      collect( type_category( T ), is_type( T ) )
    end ).

%% Langs

prop_validate_lang_always_returns_original() ->
  ?FORALL( L, l(),
    begin
      L =:= validate_lang( L )
    end ).

prop_is_lang_total() ->
  ?FORALL( Z, oneof( [l(), term()] ),
    begin
      is_boolean( is_lang( Z ) )
    end ).

prop_is_lang_true_for_lang() ->
  ?FORALL( L, l(),
    begin
      is_lang( L )
    end ).

%% Patterns

prop_validate_pattern_always_returns_original() ->
  ?FORALL( R, r(),
    begin
      R =:= validate_pattern( R )
    end ).

prop_is_pattern_total() ->
  ?FORALL( Z, oneof( [r(), term()] ),
    begin
      is_boolean( is_pattern( Z ) )
    end ).

prop_is_pattern_true_for_pattern() ->
  ?FORALL( R, r(),
    begin
      is_pattern( R )
    end ).

%% Reasons

prop_validate_reason_always_returns_original() ->
  ?FORALL( Reason, reason(),
    begin
      Reason =:= validate_reason( Reason )
    end ).

prop_is_reason_total() ->
  ?FORALL( Z, oneof( [reason(), term()] ),
    begin
      is_boolean( is_reason( Z ) )
    end ).

prop_is_reason_true_for_reason() ->
  ?FORALL( R, reason(),
    begin
      is_reason( R )
    end ).


%% Renaming

prop_rename_renamed_var_never_appears_in_produced_expr() ->
  ?FORALL( E1, e(),
    ?FORALL( X1, x(),
      ?FORALL( X2, ?SUCHTHAT( X, x(), X =/= X1 ),
        begin
          E2 = rename( E1, X1, X2 ),
          not lists:member( X1, expr_vars( E2 ) )
        end ) ) ).

%% Protecting Expressions

prop_protect_expr_produces_alpha_equivalent_expr() ->
  ?FORALL( E1, e(),
    begin
      is_alpha_equivalent( E1, protect_expr( E1 ) )
    end ).

prop_protect_expr_never_alters_free_vars() ->
  ?FORALL( E, e(),
    begin
      expr_free_vars( E ) =:= expr_free_vars( protect_expr( E ) )
    end ).

prop_protect_expr_alters_all_bound_vars() ->
  ?FORALL( E1, e(),
    begin
      E2 = protect_expr( E1 ),
      BoundLst1 = expr_vars( E1 )--expr_free_vars( E1 ),
      BoundLst2 = expr_vars( E2 )--expr_free_vars( E2 ),
      XLst = BoundLst1++BoundLst2,
      length( XLst ) =:= length( lists:usort( XLst ) )
    end ).

%% Substitution

prop_subst_var_leaves_expr_size_constant() ->
  ?FORALL( E1, e(),
    ?FORALL( X, x(),
      ?FORALL( E2, {var, info(), x()},
        begin
          E3 = subst( E1, X, E2 ),
          expr_size( E3 ) =:= expr_size( E1 )
        end ) ) ).

prop_subst_increases_expr_size_by_multiples_of_insert_size() ->
  ?FORALL( E1, e(),
    ?FORALL( X, x(),
      ?FORALL( E2, ?SUCHTHAT( E, e(), expr_category( E ) =/= var ),
        begin
          E3 = subst( E1, X, E2 ),
          S1 = expr_size( E1 ),
          S2 = expr_size( E2 ),
          S3 = expr_size( E3 ),
          DeltaS = S3-S1,
          collect(
            DeltaS div S2,
            ( DeltaS >= 0 ) andalso ( ( DeltaS rem S2 ) =:= 0 ) )
        end ) ) ).

prop_var_is_neutral_element_of_subst() ->
  ?FORALL( E1, e(),
    ?FORALL( X, x(),
      ?FORALL( E2, {var, info(), x()},
        begin
          E3 = subst( E1, X, E2 ),
          is_alpha_equivalent( E1, E3 )
        end ) ) ).


%% Alpha Equivalence

prop_is_alpha_equivalent_reflexive() ->
  ?FORALL( E, e(),
    begin
      is_alpha_equivalent( E, E )
    end ).

prop_is_alpha_equivalent_symmetric() ->
  ?FORALL( E1, e(),
    ?FORALL( E2, oneof( [e(),
                         ?LET( {E, X1, X2}, {E1, x(), x()},
                           rename( E, X1, X2 ) )
                        ] ),
      begin
        is_alpha_equivalent( E1, E2 ) =:= is_alpha_equivalent( E2, E1 )
      end ) ).

%% Variable Names

prop_expr_vars_always_produces_list_of_x() ->
  ?FORALL( E, e(),
    begin
      V = expr_vars( E ),
      case is_list( V ) of
        true  -> collect( length( V ), lists:all( fun is_binary/1, V ) );
        false -> false
      end
    end ).

prop_expr_vars_always_produces_sorted_list_with_no_duplicates() ->
  ?FORALL( E, e(),
    begin
      V = expr_vars( E ),
      V =:= lists:usort( V )
    end ).


%% Free Variables

prop_expr_free_vars_always_produces_list_of_x() ->
  ?FORALL( E, e(),
    begin
      FV = expr_free_vars( E ),
      case is_list( FV ) of
        true  -> collect( length( FV ), lists:all( fun is_binary/1, FV ) );
        false -> false
      end
    end ).

prop_expr_free_vars_always_produces_sorted_list_with_no_duplicates() ->
  ?FORALL( E, e(),
    begin
      FV = expr_free_vars( E ),
      FV =:= lists:usort( FV )
    end ).

%% Expression Size

prop_expr_size_never_negative() ->
  ?FORALL( E, e(),
    begin
      expr_size( E ) >= 0
    end ).

%% Values

prop_is_value_total() ->
  ?FORALL( Z, oneof( [e(), term()] ),
  begin
    is_boolean( is_value( Z ) )
  end ).



%%==========================================================
%% Helpers
%%==========================================================

-spec type_category( T :: t() ) -> atom().

type_category( T ) when is_atom( T ) -> T;
type_category( T ) when is_tuple( T ) -> element( 1, T ).


-spec expr_category( E :: e() ) -> atom().

expr_category( E ) -> element( 1, E ).