%% Support for CDMI Object ID
%%
%% Object IDs are used to identify objects in CDMI. Object IDs are intended
%% to be globally unique values that have a specific structure. The native
%% format of an object ID is a variable length byte sequence with a maximum
%% size of 40 bytes. This leaves an implementer up to 32 bytes for data
%% that can be used for whatever purpose is needed.
%%
%% Refer to clause 5.10 of the CDMI specification
%% for more information.
%%
-module(objectid).
-export([build_objectid/1, build_objectid/2, to_base16/1, from_base16/1]).


%% The SNMP Enterprise Number for your organization in network byte
%% order. See RFC 2578 and
%% http://www.iana.org/assignments/enterprise-numbers
%%
%% This reference implementation uses a value of 0.
-define(ENTERPRISENUM, 0).

%% @doc Build an object ID based on our own enterprise number. Data is
%% expected to be wither a string or a binary.
%%
%% @spec build_objectid(Data::{string() | binary()}) -> binary() | {error, atom()}
build_objectid(Data) ->
    build_objectid(?ENTERPRISENUM, Data).

%% @doc Build an object ID given an enterprise number and data as string.
%%
%% @spec build_objectid(Enum::integer(), Data::string()) -> binary() | {error, atom()}
build_objectid(Enum, Data) when is_list(Data) ->
    build_objectid(Enum, list_to_binary(Data));

%% @doc Build an object ID given an enterprise number and data as a
%% binary. We ensure here that the Data is not more than 32 bytes. The
%% object ID is composed of a number of fields:
%% 
%% +----------+------------+-----------+--------+-------+-----------+
%% |     0    | 1 | 2 | 3  |     4     |   5    | 6 | 7 | 8 |..| 39 |
%% +----------+------------+-----------+--------+-------+-----------+
%% | Reserved | Enterprise | Reserverd | Length |  CRC  | Opaque    |
%% | (zero)   | Number     | (zero)    |        |       | Data      |
%% +----------+------------+-----------+--------+-------+-----------+            
%%
%% @spec build_objectid(Enum::integer(), Data::binary()) -> binary() | {error, atom()}
build_objectid(Enum, Data) when is_binary(Data) ->
    Length = size(Data),
    case (Length =< 32) of
        true ->
            Bin = <<0:8, Enum:24, 0:8, Length:8, 0:16, Data/binary>>,
            Crc = crc16:crc16(binary_to_list(Bin)),
            io:format("CRC: ~p~n", [Crc]),
            <<0:8, Enum:24, 0:8, Length:8, Crc:16, Data/binary>>;
        false ->
            {error, badarg}
    end.

%% @doc Convert an object ID to a Base16 encoded string.
%% @spec to_base16(Bin::binary()) -> string()
to_base16(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || 
                      X <- binary_to_list(Bin)]).

%% @doc Convert an encoded object ID to its binary form.
%% @spec from_base16(Encoded::string()) -> binary() 
from_base16(Encoded) ->
    from_base16(Encoded, []).
from_base16([], Acc) ->
    list_to_binary(lists:reverse(Acc));
from_base16([X,Y | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    from_base16(T, [V | Acc]).
