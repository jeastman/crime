%% @author John Eastman <john.eastman@gmail.com>

%% @doc CDMI site root resource.

-module(crime_web_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_cdmi_root/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/cdmi-container", to_cdmi_root}], ReqData, Context}.

to_cdmi_root(ReqData, State) ->
    {"{\"objectURI\": \"/\", \"objectID\": \"ACDMIID\", \"parentURI\": \"/\", \"capabilitiesURI\": \"/cdmi_capabilities\", \"completionStatus\": \"Complete\", \"metadata\": {}, \"childrenrange\": \"0-0\", \"children\": [\"cdmi_capabilities/\"]}", ReqData, State}.
