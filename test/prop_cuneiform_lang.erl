-module( prop_cuneiform_lang ).
-include_lib( "proper/include/proper.hrl" ).

-import( cuneiform_lang, [free_vars/1,
                          is_alpha_equivalent/2,
                          is_expr/1,
                          is_type/1,
                          protect_expr/1,
                          rename/3] ).
-import( gen_cuneiform_lang, [e/0, t/0, x/0] ).


%%==========================================================
%% Properties
%%==========================================================

prop_is_expr_total() ->
  ?FORALL( Z, oneof( [e(), term()] ), 
    begin
      is_boolean( is_expr( Z ) )
    end ).

prop_is_expr_true_for_expr() ->
  ?FORALL( E, e(),
    begin
      collect( element( 1, E ), is_expr( E ) )
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

prop_rename_produces_alpha_equivalent_expr() ->
  ?FORALL( E1, e(),
    ?FORALL( X1, x(),
      ?FORALL( X2, x(),
        begin
          is_alpha_equivalent( E1, rename( E1, X1, X2 ) )
        end ) ) ).

prop_protect_expr_never_alters_free_vars() ->
  ?FORALL( E, e(),
    begin
      free_vars( E ) =:= free_vars( protect_expr( E ) )
    end ).

%%==========================================================
%% Helpers
%%==========================================================

type_category( X ) when is_atom( X ) -> X;
type_category( X ) when is_tuple( X ) -> element( 1, X ).