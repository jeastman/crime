-module(crc16_tests).

-include_lib("eunit/include/eunit.hrl").

%% Check crc against a known crc value
crc_test() ->
    Expected = 40679,
    ?assertEqual(Expected, crc16:crc16("test string")).
