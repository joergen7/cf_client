-module( gen_cuneiform_lang ).

-include_lib( "proper/include/proper.hrl" ).

-export( [e/0, t/0, x/0] ).

%%==========================================================
%% Exported Generators
%%==========================================================

e() ->
  frequency( [{5, {var, info(), x()}},
              {4, ?LAZY( {lam, info(), xt_lst(), {ntv, e()}} )},
              {5, ?LAZY( {lam, info(), xt_lst(), {frn, s(), t(), l(), s()}} )},
              {1, ?LAZY( {app, info(), e(), xe_lst()} )},
              {4, ?LAZY( {fix, info(), e()} )},
              {4, ?LAZY( {fut, info(), e()} )},
              {5, {str, info(), s()}},
              {5, {file, info(), s()}},
              {5, {true, info()}},
              {5, {false, info()}},
              {3, ?LAZY( {cmp, info(), e(), e()} )},
              {3, ?LAZY( {conj, info(), e(), e()} )},
              {3, ?LAZY( {disj, info(), e(), e()} )},
              {4, ?LAZY( {neg, info(), e()} )},
              {4, ?LAZY( {isnil, info(), e()} )},
              {2, ?LAZY( {cnd, info(), e(), e(), e()} )},
              {5, {null, info(), t()}},
              {3, ?LAZY( {cons, info(), e(), e()} )},
              {3, ?LAZY( {hd, info(), e(), e()} )},
              {3, ?LAZY( {tl, info(), e(), e()} )},
              {3, ?LAZY( {append, info(), e(), e()} )},
              {1, ?LAZY( {for, info(), t(), xte_lst(), e()} )},
              {2, ?LAZY( {fold, info(), {x(), t(), e()}, {x(), t(), e()}, e()} )},
              {2, ?LAZY( {rcd, info(), xe_lst()} )},
              {4, ?LAZY( {proj, info(), x(), e()} )},
              {5, {err, info(), t(), reason()}}
             ] ).

t() ->
  frequency( [{5, 'Str'},
              {5, 'File'},
              {5, 'Bool'},
              {1, ?LAZY( {'Fn', xt_lst(), t()} )},
              {4, ?LAZY( {'Lst', t()} )},
              {2, ?LAZY( {'Rcd', xt_lst()} )}
             ] ).

%%==========================================================
%% Internal Generators
%%==========================================================

info() ->
  oneof( [na,
          pos_integer(),
          {binary(), pos_integer()}] ).

xt_lst() ->
  resize( 3, list( {x(), 'Str'} ) ).

xe_lst() ->
  resize( 3, list( {x(), e()} ) ).

xte_lst() ->
  resize( 3, list( {x(), t(), e()} ) ).

x() ->
  atom().

s() ->
  binary().

l() ->
  oneof( ['Awk',
          'Bash',
          'Elixir',
          'Erlang',
          'Gnuplot',
          'Java',
          'Javascript',
          'Matlab',
          'Octave',
          'Perl',
          'Python',
          'R',
          'Racket'
         ] ).

reason() ->
  oneof( [{run, binary(), binary(), binary(), binary(), binary()},
          {stagein, binary(), binary(), binary(), list( binary() )},
          {stageout, binary(), binary(), binary(), list( binary() )},
          {user, binary()}
         ] ).