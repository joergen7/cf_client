-module( gen_expr ).

-include_lib( "proper/include/proper.hrl" ).

-export( [e/0, t/0] ).

e() ->
  {var, na, atom()}.

t() ->
  'Str'.