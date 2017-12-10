-module( cuneiform_type ).

-include( "cuneiform.hrl" ).

-import( cuneiform_lang, [t_str/0, t_file/0, t_bool/0, t_fn/3, t_arg/2] ).

-export( [type/1] ).

-spec type( E :: e() ) -> {ok, t()} | {error, type_error()}.

type( E ) ->
  type( #{}, E ).


-spec type( Gamma :: #{ x() => t() }, E :: e() ) -> {ok, t()} | {error, type_error()}.

type( _Gamma, {str, _Info, _S} ) ->
  {ok, t_str()};

type( _Gamma, {file, _Info, _S, _Hash} ) ->
  {ok, t_file()};

type( _Gamma, {true, _Info} ) ->
  {ok, t_bool()};

type( _Gamma, {false, _Info} ) ->
  {ok, t_bool()};

type( Gamma, {cmp, Info, E1, E2} ) ->
  case type( Gamma, E1 ) of

    {ok, 'Str'} ->
      case type( Gamma, E2 ) of

        {ok, 'Str'} ->
          {ok, t_bool()};

        {ok, T2} ->
          {error, {type_mismatch, Info, {t_str(), T2}}};

        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_str(), T1}}};

    {error, Reason1} ->
      {error, Reason1}
  end;

type( Gamma, {lam_ntv, _Info, [], EBody} ) ->
  case type( Gamma, EBody ) of

    {ok, TBody} ->
      {ok, t_fn( ntv, [], TBody )};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {lam_ntv, Info, [{XIn, XOut, TX}|LamNtvArgLst], EBody} ) ->
  case type( Gamma#{ XIn => TX }, {lam_ntv, Info, LamNtvArgLst, EBody} ) of

    {ok, {'Fn', ntv, TArgLst, TRet}} ->
      TArgLst1 = [t_arg( XOut, TX )|TArgLst],
      {ok, t_fn( ntv, TArgLst1, TRet )};
    
    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {var, Info, X} ) ->
  case maps:is_key( X, Gamma ) of
    true  -> {ok, maps:get( X, Gamma )};
    false -> {error, {unbound_var, Info, X}}
  end;

type( Gamma, {neg, Info, E} ) ->
  case type( Gamma, E ) of

    {ok, 'Bool'} ->
      {ok, t_bool()};

    {ok, T} ->
      {error, {type_mismatch, Info, {t_bool(), T}}};

    {error, Reason} ->
      {error, Reason}

  end;

type( Gamma, {cnd, Info, E1, E2, E3} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of

        {ok, T2} ->
          case type( Gamma, E3 ) of
            {ok, T2}         -> {ok, T2};
            {ok, T3}         -> {error, {type_mismatch, Info, {T2, T3}}};
            {error, Reason3} -> {error, Reason3}
          end;
        
        {error, Reason2} ->
          {error, Reason2}

      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {conj, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {type_mismatch, Info, {t_bool(), T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( Gamma, {disj, Info, E1, E2} ) ->

  case type( Gamma, E1 ) of

    {ok, 'Bool'} ->
      case type( Gamma, E2 ) of
        {ok, 'Bool'}     -> {ok, t_bool()};
        {ok, T2}         -> {error, {type_mismatch, Info, {t_bool(), T2}}};
        {error, Reason2} -> {error, Reason2}
      end;

    {ok, T1} ->
      {error, {type_mismatch, Info, {t_bool(), T1}}};

    {error, Reason1} ->
      {error, Reason1}

  end;

type( _Gamma, E ) -> error( {bad_expr, E} ).