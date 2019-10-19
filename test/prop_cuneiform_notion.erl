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

-module( prop_cuneiform_notion ).

-include_lib( "proper/include/proper.hrl" ).

-include( "cuneiform.hrl" ).

-import( cuneiform_lang, [is_expr/1] ).
-import( cuneiform_notion, [notion/1] ).


%%====================================================================
%% Properties
%%====================================================================

prop_notion_returns_expr_or_fails_norule() ->
  ?FORALL( E, e(),
    begin
      try
        is_expr( notion( E ) )
      catch
        error:{norule, E} -> true;
        error:_           -> false
      end
    end ).

prop_notion_alters_redex() ->
  ?FORALL( E, e(),
    begin
      try
        notion( E ) =/= E
      catch
        error:{norule, E} -> true
      end
    end ).

prop_notion_value_fails() ->
  ?FORALL( V, v(),
    begin
      try
        notion( V ),
        false
      catch
        error:{norule, V} -> true
      end
    end ).

prop_notion_nonexpr_fails() ->
  ?FORALL( Z, ?SUCHTHAT( Z, term(), not is_expr( Z ) ),
    begin
      try
        notion( Z ),
        false
      catch
        error:_ -> true
      end
    end ).