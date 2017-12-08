-module( cuneiform_type ).

-include( "cuneiform.hrl" ).

-export( [type/1] ).

-spec type( E :: e() ) -> {ok, t()} | {error, type_error()}.

type( E ) ->
  type( #{}, E ).


-spec type( Gamma :: #{ x() => t() }, E :: e() ) -> {ok, t()} | {error, type_error()}.

type( _Gamma, {str, _Info, _S} ) ->
  {ok, cuneiform_lang:t_str()};

type( _Gamma, {file, _Info, _S, _Hash} ) ->
  {ok, cuneiform_lang:t_file()};

type( _Gamma, {true, _Info} ) ->
  {ok, cuneiform_lang:t_bool()};

type( _Gamma, {false, _Info} ) ->
  {ok, cuneiform_lang:t_bool()};

type( _Gamma, {cmp, _Info, _E1, _E2} ) ->
  {ok, cuneiform_lang:t_bool()};

type( _Gamma, _E ) -> error( bad_expr ).