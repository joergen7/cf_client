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

type( _Gamma, {cmp, Info, E1, E2} ) ->
  case type( E1 ) of % TODO: pull in Gamma

    {ok, 'Str'} ->
      case type( E2 ) of % TODO: pull in Gamma

        {ok, 'Str'} ->
          {ok, cuneiform_lang:t_bool()};

        {ok, T2} ->
          {error, {type_mismatch, Info, {cuneiform_lang:t_str(), T2}}};

        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {cuneiform_lang:t_str(), T1}}};

    {error, Reason1} ->
      {error, Reason1}
  end;

type( Gamma, {lam_ntv, _Info, [], EBody} ) ->
  {ok, TBody} = type( Gamma, EBody ), % what if body untypable
  {ok, cuneiform_lang:t_fn( ntv, [], TBody )};

type( Gamma, {lam_ntv, Info, [{XIn, XOut, TX}|LamNtvArgLst], EBody} ) ->
  % TODO: what if body untypable
  {ok, {'Fn', ntv, TArgLst, TRet}} = % TODO exchange XOut and XIn
    type( Gamma#{ XOut => TX }, {lam_ntv, Info, LamNtvArgLst, EBody} ),
  TArgLst1 = [cuneiform_lang:t_arg( XIn, TX )|TArgLst],
  {ok, cuneiform_lang:t_fn( ntv, TArgLst1, TRet )};

type( Gamma, {var, Info, X} ) ->
  case maps:is_key( X, Gamma ) of
    true  -> {ok, maps:get( X, Gamma )};
    false -> {error, {unbound, Info, X}}
  end;

type( _Gamma, _E ) -> error( bad_expr ).