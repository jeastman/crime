-module(objectid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ENTERPRISENUM, 0).

%% eunit Tests

build_with_enum_test() ->
    TestString = "data string",
    TestNum = 96,
    Crc = 27447,
    Length = length(TestString),
    Obj = objectid:build_objectid(TestNum, TestString),
    CmpString = list_to_binary(TestString),
    ?assertEqual(Obj, <<0:8, TestNum:24, 
                        0:8, Length:8, Crc:16, CmpString/binary>>).

build_without_enum_test() ->
    TestString = "data string",
    Crc = 17183,
    Length = length(TestString),
    Obj = objectid:build_objectid(TestString),
    CmpString = list_to_binary(TestString),
    ?assertEqual(Obj, <<0:8, ?ENTERPRISENUM:24, 
                        0:8, Length:8, Crc:16, CmpString/binary>>).

build_with_badarg_test() ->
    TooLong = "12345678901234567890123456789012345",
    ?assertEqual({error, badarg}, 
                 objectid:build_objectid(TooLong)).

build_with_badarg2_test() ->
    TooLong = "12345678901234567890123456789012345",
    ?assertEqual({error, badarg}, 
                objectid:build_objectid(?ENTERPRISENUM, TooLong)).

base16_test() ->
    TestString = "data string",
    Obj = objectid:build_objectid(TestString),
    Encode = objectid:to_base16(Obj), 
    ?assertEqual(Obj, objectid:from_base16(Encode)).
