-module( prop_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform.hrl" ).


-import( cuneiform_lang, [expr_vars/1,
                          expr_free_vars/1,
                          expr_size/1,
                          is_alpha_equivalent/2,
                          is_expr/1,
                          is_type/1,
                          protect_expr/1,
                          rename/3,
                          subst/3] ).
-import( gen_cuneiform_lang, [e/0, t/0, x/0, info/0] ).


%%==========================================================
%% Properties
%%==========================================================

%% Contract Predicates

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

%% Renaming

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
          collect( DeltaS div S2, ( DeltaS >= 0 ) andalso ( ( DeltaS rem S2 ) =:= 0 ) )
        end ) ) ).

%%==========================================================
%% Helpers
%%==========================================================

-spec type_category( T :: t() ) -> atom().

type_category( T ) when is_atom( T ) -> T;
type_category( T ) when is_tuple( T ) -> element( 1, T ).


-spec expr_category( E :: e() ) -> atom().

expr_category( E ) -> element( 1, E ).