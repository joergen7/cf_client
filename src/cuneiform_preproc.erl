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

-module( cuneiform_preproc ).


%%====================================================================
%% Exports
%%====================================================================

-export( [visit_from/3, visit_fold/7, visit_cnd/6, visit_for/5] ).


%%====================================================================
%% Includes
%%====================================================================

-include( "cuneiform.hrl" ).


%%====================================================================
%% Imports
%%====================================================================

-import( cuneiform_lang, [typed_bind/3, create_closure/2, fold/4, cnd/4,
                          for/4] ).


%%====================================================================
%% Preprocessor functions
%%====================================================================

-spec visit_from( Id, T, E ) -> typed_bind()
when Id :: {id, _, S :: string()},
     T  :: t(),
     E  :: e().

visit_from( {id, _, S}, T, E ) ->
  typed_bind( list_to_atom( S ), T, E ).


-spec visit_fold( Fold, Id, T, E, From, DefLst, EBody ) -> e()
when Fold   :: {fold, L :: pos_integer(), _},
     Id     :: {id, _, S :: string()},
     T      :: t(),
     E      :: e(),
     From   :: typed_bind(),
     DefLst :: [assign()],
     EBody  :: e().

visit_fold( {fold, L, _}, {id, _, S}, T, E, From, DefLst, EBody ) ->

  AccBind = typed_bind( list_to_atom( S ), T, E ),

  case create_closure( DefLst, EBody ) of
    {ok, C}         -> fold( L, AccBind, From, C );
    {error, Reason} -> throw( Reason )
  end.


-spec visit_cnd( Cnd, EIf, DefLstThen, EThen, DefLstElse, EElse ) -> e()
when Cnd        :: {cnd, L :: pos_integer(), _},
     EIf        :: e(),
     DefLstThen :: [assign()],
     EThen      :: e(),
     DefLstElse :: [assign()],
     EElse      :: e().

visit_cnd( {cnd, L, _}, EIf, DefLstThen, EThen, DefLstElse, EElse ) ->
  case create_closure( DefLstThen, EThen ) of

    {error, R2} ->
      throw( R2 );

    {ok, E2} ->
      case create_closure( DefLstElse, EElse ) of

        {error, R3} ->
          throw( R3 );

        {ok, E3} ->
          cnd( L, EIf, E2, E3 )

      end

  end.


-spec visit_for( For, FromLst, DefLst, E, TRet ) -> e()
when For     :: {for, L :: pos_integer(), _},
     FromLst :: [typed_bind()],
     DefLst  :: [assign()],
     E       :: e(),
     TRet    :: t().

visit_for( {for, L, _}, FromLst, DefLst, E, TRet ) ->
  case create_closure( DefLst, E ) of
    {ok, C} ->
      for( L, TRet, FromLst, C );
    {error, Reason} ->
      throw( Reason )
  end.