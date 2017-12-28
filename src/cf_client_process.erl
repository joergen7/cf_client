-module( cf_client_process ).
-behaviour( cre_client ).

-include_lib( "cuneiform.hrl" ).


%%====================================================================
%% Exports
%%====================================================================

%% CRE client callbacks
-export( [init/1, is_value/2, recv/4, step/2] ).

%% API functions
-export( [start_link/1, start_link/2] ).

-import( cuneiform_lang, [e_bind/2, true/0, false/0, str/1, file/1, lst/2,
                          t_bool/0, t_str/0, t_file/0, rcd/1] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( F ) when is_function( F, 0 ) ->
  case F() of
    {ok, CreName} -> start_link( CreName );
    {error, E}    -> {error, E}
  end;

start_link( CreName ) ->
  cre_client:start_link( CreName, ?MODULE, [] ).


start_link( ClientName, F ) when is_function( F, 0 ) ->
  case F() of
    {ok, CreName} -> start_link( ClientName, CreName );
    {error, E}    -> {error, E}
  end;

start_link( Clientname, CreName ) ->
  cre_client:start_link( Clientname, CreName, ?MODULE, [] ).


%%====================================================================
%% CRE client callback implementations
%%====================================================================

-spec init( ClientArg :: _ ) -> [].

init( _ClientArg ) -> [].


-spec is_value( E :: e(), UsrInfo :: _ ) -> boolean().

is_value( E, _ ) ->
  cuneiform_sem:is_value( E ).


-spec recv( E, A, Delta, UsrInfo ) -> e()
when E       :: e(),
     A       :: e(),
     Delta   :: e(),
     UsrInfo :: _.

recv( E, Request, Reply, _UsrInfo ) ->

  #{ app_id := AppId } = Reply,
  E1 = effi_reply_to_expr( Request, Reply ),

  cuneiform_sem:subst_fut( E, AppId, E1 ).


-spec step( E, UsrInfo ) -> Result
when E       :: e(),
     UsrInfo :: _,
     Result  :: {ok, e()}
              | {ok_send, e(), e()}
              | norule.

step( E, _UsrInfo ) ->
  case cuneiform_sem:find_context( E ) of

    {ok, E1, Ctx} ->
      case E1 of

        {app, Info, {lam_frn, _, _, _, _, _, _}, _} ->

          EffiRequest = app_to_effi_request( E1 ),
          #{ app_id := AppId } = EffiRequest,

          E2 = {fut, Info, AppId},
          E3 = cuneiform_sem:in_hole( E2, Ctx ),

          {ok_send, E3, EffiRequest};

        _ ->
          E2 = cuneiform_sem:reduce( E1 ),
          E3 = cuneiform_sem:in_hole( E2, Ctx ),
          {ok, E3}

      end;

    no_ctx ->
      norule

  end.


-spec app_to_effi_request( A :: e() ) -> #{ atom() => _ }.

app_to_effi_request(
  {app, _,
        {lam_frn, _, FName, ArgLst, {'Rcd', RetLst}, Lang, Body},
        EBindLst} ) ->

  ConvertExpr =
    fun
      ConvertExpr( {true, _} )            -> <<"true">>;
      ConvertExpr( {false, _} )           -> <<"false">>;
      ConvertExpr( {str, _, B} )          -> B;
      ConvertExpr( {file, _, B, _} )      -> B;
      ConvertExpr( {null, _, _} )         -> [];
      ConvertExpr( {cons, _, _, E1, E2} ) -> [ConvertExpr( E1 )|ConvertExpr( E2 )]
    end,

  ConvertTArg =
    fun
      ( {X, 'Bool'} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                             arg_type => <<"Bool">>,
                             is_list  => false };

      ( {X, 'Str'} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                           arg_type => <<"Str">>,
                           is_list  => false };

      ( {X, 'File'} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                            arg_type => <<"File">>,
                            is_list  => false };

      ( {X, {'Lst', 'Bool'}} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                                     arg_type => <<"Bool">>,
                                     is_list  => true };

      ( {X, {'Lst', 'Str'}} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                                    arg_type => <<"Str">>,
                                    is_list  => true };

      ( {X, {'Lst', 'File'}} ) -> #{ arg_name => atom_to_binary( X, utf8 ),
                                     arg_type => <<"File">>,
                                     is_list  => true }
    end,

  BinaryToHexString =
    fun( X ) when is_binary( X ) ->
      lists:flatten( [io_lib:format( "~2.16.0b", [B] ) || <<B>> <= X] )
    end,

  ArgTypeLst = [ConvertTArg( TArg ) || TArg <- ArgLst],

  RetTypeLst = [ConvertTArg( TArg ) || TArg <- RetLst],

  Lambda = #{ lambda_name  => atom_to_binary( FName, utf8 ),
              arg_type_lst => ArgTypeLst,
              ret_type_lst => RetTypeLst,
              lang         => atom_to_binary( Lang, utf8 ),
              script       => Body },

  ArgBindLst = [#{ arg_name => atom_to_binary( X, utf8 ),
                   value    => ConvertExpr( E ) }
                || {X, E} <- EBindLst],

  Hash = crypto:hash( sha512, io_lib:format( "~w~w", [Lambda, ArgBindLst] ) ),

  AppId = list_to_binary( BinaryToHexString( Hash ) ),

  #{ app_id       => AppId,
     lambda       => Lambda,
     arg_bind_lst => ArgBindLst }.


effi_reply_to_expr( Request, Reply ) ->

  #{ lambda := Lambda } = Request,
  #{ ret_type_lst := RetTypeLst } = Lambda,

  RTL = [{ArgName, ArgType, IsList}
         || #{ arg_name := ArgName,
               arg_type := ArgType,
               is_list  := IsList } <- RetTypeLst],

  Convert =
    fun( RetBind ) ->

      #{ arg_name := N,
         value    := V } = RetBind,

      {N, T, L} = lists:keyfind( N, 1, RTL ),

      X = binary_to_atom( N, utf8 ),

      case L of

        false ->
          case T of
            <<"Bool">> ->
              case V of
                <<"true">>  -> e_bind( X, true() );
                <<"false">> -> e_bind( X, false() )
              end;
            <<"Str">> ->
              e_bind( X, str( V ) );
            <<"File">> ->
              e_bind( X, file( V ) )
          end;

        true ->
          case T of
            <<"Bool">> ->
              e_bind( X, lst( t_bool(), [case Y of
                                           <<"true">>  -> true();
                                           <<"false">> -> false()
                                         end || Y <- V] ) );
            <<"Str">> ->
              e_bind( X, lst( t_str(), [str( Y ) || Y <- V] ) );
            <<"File">> ->
              e_bind( X, lst( t_file(), [file( Y ) || Y <- V] ) )
          end

      end
    end,

  #{ extended_script := ExtendedScript,
     result          := Result } = Reply,

  case Result of

    #{ status := <<"ok">>, ret_bind_lst := RetBindLst } ->
      rcd( [Convert( RetBind ) || RetBind <- RetBindLst] );

    #{ status := <<"error">>, output := Output } ->
      {err, na, ExtendedScript, Output}

  end.



