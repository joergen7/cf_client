-module( gen_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-export( [e/0, t/0, xt_lst/0] ).



e() ->
  oneof( [{var, na, atom()},
          lam2()
         ] ).

t() ->
  ?LAZY( oneof( ['Str',
                 'File',
                 'Bool',
                 %{'Fn', vector( range( 0, 3 ), {x(), t()} ), t()},
                 {'Lst', t()}
                 %{'Rcd', vector( range( 0, 3 ), {x(), t()} )}
                ] ) ).

x() ->
  oneof( [x, y, z] ).

lam1() ->
  {ok, N} = proper_gen:pick( range( 0, 1 ) ),
  {lam, na, vector( N, {x, 'Str'} ), {ntv, {var, na, x}}}.

lam2() ->
  {lam, na, xt_lst(), {ntv, {var, na, x}}}.

xt_lst() ->
  resize( 3, list( {x(), 'Str'} ) ).