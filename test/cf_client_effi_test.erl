%% -*- erlang -*-
%%
%% cf_client
%%
%% Copyright 2015-2018 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.6
%% @copyright 2015-2018 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_client_effi_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cf_client_effi, [effi_reply_to_expr/2, reconstruct_type/1] ).
-import( cuneiform_lang, [rcd/1, e_bind/2, str/1, t_bool/0, t_arg/2, t_rcd/1] ).

result_decode_test() ->

  EffiRequest =
    #{ app_id       => <<"bdfb373d289eaa0da5a8ad5b8dcca263c5fc977d433b3adeb100c041117e959ea6060c9fc1fb6c3ec8d892dcb047efa7cdeab564600a1f4d4160d0d5a96856ec">>,
       arg_bind_lst => [#{ arg_name => <<"person">>,
                           value    => <<"Jorgen">>}],
       lambda       => #{ arg_type_lst => [#{arg_name => <<"person">>,
                                             arg_type => <<"Str">>,
                                             is_list  => false}],
                          lambda_name  => <<"greet">>,
                          lang         => <<"Bash">>,
                          ret_type_lst => [#{ arg_name => <<"out">>,
                                              arg_type => <<"Str">>,
                                              is_list => false}],
                          script       => <<"\n  out=\"Hello $person\"\n">> } },

  EffiResult =
    #{ app_id => <<"bdfb373d289eaa0da5a8ad5b8dcca263c5fc977d433b3adeb100c041117e959ea6060c9fc1fb6c3ec8d892dcb047efa7cdeab564600a1f4d4160d0d5a96856ec">>,
       result => #{ ret_bind_lst => [#{ arg_name => <<"out">>,
                                        value    => <<"Hello Jorgen">> }],
                    status       => <<"ok">> },
                    stat         => #{ duration => <<"9103976">>,
                                       t_start  => <<"1515225807640943499">> } },

  E = rcd( [e_bind( out, str( <<"Hello Jorgen">> ) )] ),

  ?assertEqual( E, effi_reply_to_expr( EffiRequest, EffiResult ) ).

reconstruct_type_test_() ->
  {foreach,

   fun() -> ok end,
   fun( _ ) -> ok end,

   [
    {"reconstruct singleton boolean",
     fun reconstruct_singleton_boolean/0}
   ]}.

reconstruct_singleton_boolean() ->
  
  RetTypeLst = [#{ arg_name => <<"a">>,
                   arg_type => <<"Bool">>,
                   is_list  => false }],

  T = t_rcd( [t_arg( a, t_bool() )] ),

  ?assertEqual( T, reconstruct_type( RetTypeLst ) ).