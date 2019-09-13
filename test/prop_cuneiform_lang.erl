-module( prop_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform.hrl" ).


-import( cuneiform_lang, [expr_vars/1,
                          expr_free_vars/1,
                          expr_size/1,
                          is_alpha_equivalent/2,
                          is_expr/1,
                          is_lang/1,
                          is_pattern/1,
                          is_reason/1,
                          is_type/1,
                          is_value/1,
                          protect_expr/1,
                          rename/3,
                          subst/3,
                          validate_expr/1,
                          validate_pattern/1,
                          validate_reason/1,
                          validate_type/1] ).


%%==========================================================
%% Properties
%%==========================================================

%% Contract Predicates

%% Expressions

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

prop_validate_expr_mirrors_is_expr() ->
  ?FORALL( Z, oneof( [e(), term()] ),
    begin
      V =
        try validate_expr( Z ) of
          Z -> true
        catch
          error:_ -> false
        end,
      I = is_expr( Z ),
      V =:= I
    end ).

%% Types

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

prop_validate_type_mirrors_is_type() ->
  ?FORALL( Z, oneof( [t(), term()] ),
    begin
      V =
        try validate_type( Z ) of
          Z -> true
        catch
          error:_ -> false
        end,
      I = is_type( Z ),
      V =:= I
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

prop_validate_pattern_mirrors_is_type() ->
  ?FORALL( Z, oneof( [r(), term()] ),
    begin
      V =
        try validate_pattern( Z ) of
          Z -> true
        catch
          error:_ -> false
        end,
      I = is_pattern( Z ),
      V =:= I
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

prop_validate_reason_mirrors_is_reason() ->
  ?FORALL( Z, oneof( [reason(), term()] ),
    begin
      V =
        try validate_reason( Z ) of
          Z -> true
        catch
          error:_ -> false
        end,
      I = is_reason( Z ),
      V =:= I
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

%% Expression Analysis

%% Alpha Equivalence

prop_is_alpha_equivalent_symmetric() ->
  ?FORALL( E1, e(),
    ?FORALL( E2, oneof( [e(),
                         ?LET( {E, X1, X2}, {E1, x(), x()},
                           rename( E, X1, X2 ) )
                        ] ),
      case is_alpha_equivalent( E1, E2 ) of
        true ->
          collect( if E1 =:= E2 -> equal; true -> alpha_eq end,
                   is_alpha_equivalent( E2, E1 ) );
        false ->
          collect( neq, true )
      end ) ).

%% Variables

prop_expr_vars_always_produces_list_of_atom() ->
  ?FORALL( E, e(),
    begin
      V = expr_vars( E ),
      case is_list( V ) of
        true  -> collect( length( V ), lists:all( fun is_atom/1, V ) );
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

prop_expr_free_vars_always_produces_list_of_atom() ->
  ?FORALL( E, e(),
    begin
      FV = expr_free_vars( E ),
      case is_list( FV ) of
        true  -> collect( length( FV ), lists:all( fun is_atom/1, FV ) );
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

prop_is_value_total_for_expr() ->
  ?FORALL( E, e(),
  begin
    I = is_value( E ),
    collect( case I of true -> value; false -> nonvalue end,
             is_boolean( I ) )
  end ).

%%==========================================================
%% Helpers
%%==========================================================

-spec type_category( T :: t() ) -> atom().

type_category( T ) when is_atom( T ) -> T;
type_category( T ) when is_tuple( T ) -> element( 1, T ).


-spec expr_category( E :: e() ) -> atom().

expr_category( E ) -> element( 1, E ).