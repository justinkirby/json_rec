-module(json_rec_model).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{new,1}];
behaviour_info(_Other) ->
    undefined.




