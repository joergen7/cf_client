%% -*- erlang -*-
%%
%% cf_client: Cuneiform client implementation
%%
%% Copyright 2015-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.7
%% @copyright 2015-2019
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_client_effi ).

-export( [app_to_effi_request/1, effi_reply_to_expr/2, reconstruct_type/1] ).

-include( "cuneiform.hrl" ).

-import( cuneiform_lang, [e_bind/2, true/0, false/0, str/1, file/1, lst/2,
                          t_bool/0, t_str/0, t_file/0, rcd/1, t_lst/1, t_rcd/1,
                          t_arg/2] ).



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
      ConvertExpr( {cons, _, E1, E2} ) -> [ConvertExpr( E1 )|ConvertExpr( E2 )]
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

  Hash = crypto:hash( sha224, io_lib:format( "~w~w", [Lambda, ArgBindLst] ) ),

  AppId = list_to_binary( BinaryToHexString( Hash ) ),

  #{ app_id       => AppId,
     lambda       => Lambda,
     arg_bind_lst => ArgBindLst }.


-spec effi_reply_to_expr( Request, Reply ) -> e()
when Request :: #{ atom() => _ },
     Reply   :: #{ atom() => _ }.

effi_reply_to_expr( Request, Reply ) ->

  #{ app_id := AppId,
     lambda := Lambda } = Request,
  #{ lambda_name  := LamName,
     ret_type_lst := RetTypeLst } = Lambda,

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

  #{ result := Result } = Reply,
  T = reconstruct_type( RetTypeLst ),

  case Result of

    #{ status       := <<"ok">>,
       ret_bind_lst := RetBindLst } ->
      rcd( [Convert( RetBind ) || RetBind <- RetBindLst] );

    #{ status          := <<"error">>,
       stage           := <<"run">>,
       extended_script := ExtendedScript,
       output          := Output } ->
      {err, na, T, {run, AppId, LamName, ExtendedScript, Output}};

    #{ status   := <<"error">>,
       stage    := <<"stagein">>,
       file_lst := FileLst } ->
      {err, na, T, {stagein, AppId, LamName, FileLst}};

    #{ status   := <<"error">>,
       stage    := <<"stageout">>,
       file_lst := FileLst } ->
      {err, na, T, {stageout, AppId, LamName, FileLst}}

  end.


-spec reconstruct_type( RetTypeLst :: [#{ atom() => _ }] ) -> t().

reconstruct_type( RetTypeLst ) ->

  F =
    fun( TypeInfo ) ->

      #{ arg_name := ArgName,
         arg_type := ArgType,
         is_list := IsList } = TypeInfo,

      X = binary_to_atom( ArgName, utf8 ),

      BaseType =
        case ArgType of
          <<"Str">>  -> t_str();
          <<"File">> -> t_file();
          <<"Bool">> -> t_bool()
        end,

      case IsList of
        true  -> t_arg( X, t_lst( BaseType ) );
        false -> t_arg( X, BaseType )
      end

    end,

  t_rcd( [F( TypeInfo ) || TypeInfo <- RetTypeLst] ).