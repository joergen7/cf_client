-module( cf_client_effi_test ).

-include_lib( "eunit/include/eunit.hrl" ).

-import( cf_client_effi, [effi_reply_to_expr/2] ).
-import( cuneiform_lang, [rcd/1, e_bind/2, str/1] ).

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