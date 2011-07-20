%% @author John Eastman <john.eastman@gmail.com>
%% @doc CDMI Capabilities Resource.

-module(crime_capabilities_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_capabilities/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/cdmi-capabilities", to_capabilities}], ReqData, Context}.

to_capabilities(ReqData, State) ->
    {"{\"objectURI\": \"/cdmi_capabilities\", \"objectID\": \"ACDMIID\", \"parentURI\": \"/\", \"capabilities\": {\"security_https_transport\": \"false\", \"cdmi_read_metadata\": \"true\", \"cdmi_list_children\": \"true\"}, \"childrenrange\": \"\", \"children\": []}", ReqData, State}.
