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
%% @copyright 2013-2020
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_client_effi ).

-export( [app_to_effi_request/2,
          effi_reply_to_expr/2,
          reconstruct_type/1] ).

-export( [convert_expr/1] ).

-include( "cuneiform_lang.hrl" ).

-import( cuneiform_lang, [true/0, false/0, str/1, file/1, lst/2, t_bool/0,
                          t_str/0, t_file/0, rcd/1, t_lst/1, t_rcd/1] ).



-spec app_to_effi_request( H :: binary(), A :: e() ) -> #{ atom() => _ }.

app_to_effi_request( H, A ) ->

  {app, _, {lam, _, ArgLst, {frn, FName, {'Rcd', RetLst}, Lang, Body}}, EBindLst} = A,

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

  ArgTypeLst = [ConvertTArg( TArg ) || TArg <- ArgLst],

  RetTypeLst = [ConvertTArg( TArg ) || TArg <- RetLst],

  Lambda = #{ lambda_name  => atom_to_binary( FName, utf8 ),
              arg_type_lst => ArgTypeLst,
              ret_type_lst => RetTypeLst,
              lang         => atom_to_binary( Lang, utf8 ),
              script       => Body },

  ArgBindLst = [#{ arg_name => atom_to_binary( X, utf8 ),
                   value    => convert_expr( E ) }
                || {X, E} <- EBindLst],

  #{ app_id       => H,
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
                <<"true">>  -> {X, true()};
                <<"false">> -> {X, false()}
              end;
            <<"Str">> ->
              {X, str( V )};
            <<"File">> ->
              {X, file( V )}
          end;

        true ->
          case T of
            <<"Bool">> ->
              {X, lst( t_bool(), [case Y of
                                    <<"true">>  -> true();
                                    <<"false">> -> false()
                                  end || Y <- V] )};
            <<"Str">> ->
              {X, lst( t_str(), [str( Y ) || Y <- V] )};
            <<"File">> ->
              {X, lst( t_file(), [file( Y ) || Y <- V] )}
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
       node            := Node,
       stage           := <<"run">>,
       extended_script := ExtendedScript,
       output          := Output } ->
      {err, na, T, {run, Node, AppId, LamName, ExtendedScript, Output}};

    #{ status   := <<"error">>,
       node     := Node,
       stage    := <<"stagein">>,
       file_lst := FileLst } ->
      {err, na, T, {stagein, Node, AppId, LamName, FileLst}};

    #{ status   := <<"error">>,
       node     := Node,
       stage    := <<"stageout">>,
       file_lst := FileLst } ->
      {err, na, T, {stageout, Node, AppId, LamName, FileLst}}

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
        true  -> {X, t_lst( BaseType )};
        false -> {X, BaseType}
      end

    end,

  t_rcd( [F( TypeInfo ) || TypeInfo <- RetTypeLst] ).


-spec convert_expr( e() ) -> _.

convert_expr( {true, _} ) ->
  <<"true">>;

convert_expr( {false, _} ) ->
  <<"false">>;

convert_expr( {str, _, B} ) ->
  B;

convert_expr( {file, _, B} ) ->
  B;

convert_expr( {null, _, _} ) ->
  [];
 
convert_expr( {cons, _, E1, E2} ) ->
  [convert_expr( E1 )|convert_expr( E2 )].
