%% @author John Eastman <john.eastman@gmail.com>

%% @doc Callbacks for the crime_web application.

-module(crime_web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for crime_web.
start(_Type, _StartArgs) ->
    crime_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for crime_web.
stop(_State) ->
    ok.
