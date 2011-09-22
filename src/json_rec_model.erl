-module(rest_model).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init,1},
     {new,1},
     {rec,1}
    ];
behaviour_info(_Other) ->
    undefined.




