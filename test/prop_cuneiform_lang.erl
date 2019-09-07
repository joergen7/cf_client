-module( prop_cuneiform_lang ).
-include_lib( "proper/include/proper.hrl" ).

-import( cuneiform_lang, [is_expr/1, is_type/1] ).
-import( gen_expr, [e/0, t/0] ).

prop_is_expr_total() ->
  ?FORALL( Z, term(), 
    begin
      collect( is_expr( Z ), is_boolean( is_expr( Z ) ) )
    end ).

prop_is_expr_true_for_expr() ->
  ?FORALL( E, e(), is_expr( E ) =:= true ).

prop_is_type_total() ->
  ?FORALL( Z, term(),
    begin
      collect( is_type( Z ), is_boolean( is_type( Z ) ) )
    end ).

prop_is_type_true_for_type() ->
  ?FORALL( T, t(),
    begin
      is_type( T ) =:= true
    end ).