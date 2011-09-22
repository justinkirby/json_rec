%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011,
%%% @doc
%%% Assuming a record  of `-record(simple, {one, two})' in mod_fake
%%% Usage example:
%%% ```
%%%   Rec = mod_fake:new(<<"simple">>),
%%%   Json = mochijson2:decode("{'one':1,'two':2}"),
%%%   SimpleRec = json_rec:to_rec(Json,mod_fake,Rec)
%%%
%%% '''
%%%
%%% The above code will take the json and transform it into the
%%% specified record. Trying to match the field of the record with the
%%% key in the json. If a match fails, then json_rec will fall back to
%%% using proplists
%%%
%%% The module MUST export module:new/1. new/1 should take a binary and return a record. Example:
%%% ```
%%% -module(mod_fake).
%%% -export([new/1]).
%%% -record(simple, {one,two}).
%%% new(<<"simple">>) -> #simple{};
%%% new(_) -> undefined.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(json_rec).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(simple, {
          one,
          two
          }).

-record(simplet2l, {
          two
          }).

-record(deep, {
          simple,
          second = []
         }).


-compile({parse_transform, exprecs}).
-export_records([simple,simplet2l,deep]).

-endif.

-include("vest_types.hrl").
-export([to_rec/3,
         to_json/2
         ]).

to_json(Record, Module) ->
    Fields = Module:'#info-'(fields,Record),
    Pl = rec_keys(Fields,Record,Module,[]),
    {struct, Pl}.

rec_keys([], _Record, _Module, Acc) -> Acc;
rec_keys([Field|Rest],Record,Module,Acc) ->
    Value = Module:'#get-'(Field,Record),
    Key = list_to_binary(atom_to_list(Field)),
    JsonValue = field_value(Value,Module,[]),
    rec_keys(Rest, Record, Module,[{Key,JsonValue}|Acc]).

field_value(Value, Module, _Acc) when is_tuple(Value) ->
    case Module:rec(Value) of
        true ->
            to_json(Value,Module);
        false ->
            Value
    end;
field_value([],_Module, Acc)  -> Acc;
field_value([Value|Rest], Module, Acc) ->
    NewValue = case field_value(Value,Module,[]) of
                   IsRec when is_tuple(IsRec),
			      is_atom(element(1,Value)) ->
                       %% this returned a record, so get the first element fromthe rec tuple and do: {struct, atom
                       {struct, [{list_to_binary(atom_to_list(element(1,Value))),IsRec}]};
		   IsTuple when is_tuple(IsTuple) ->
		       tuple_to_list(IsTuple);
                   NotRec ->
		       NotRec
               end,
    field_value(Rest, Module,[NewValue|Acc]);
field_value(Value,_Module,_Acc) ->
    Value.




%% @spec to_rec(_Json, Module, Record) -> tuple()
%% @doc
%% Take the result from mochijson2:decode/1 and transform it into a
%% record, or proplist.
%%
%% _Json MUST the result of mochijson2:decode/1.
%% Module is a module that refers to a specific module which exports new/1.
%% Rec is the initial empty record #record_name{} or `module:new(<<"record_name">>)'
%%
%% NOTE: it is up to you to export and define module:new/1
-spec to_rec(_Json :: json_dict(), Module :: atom(), undefined) ->
                    proplist();
            (_Json :: json_dict(), Module :: atom(), Rec :: tuple() ) ->
                    Rec :: tuple().

to_rec({struct, Pl} = _Json, Module, undefined) ->
    pl(Pl, Module);

to_rec({struct, Pl} = _Json, Module, Rec) ->
    keys_rec(Pl, Module, Rec).

keys_rec([], _Module, Rec) -> Rec;
keys_rec([{Key, {struct, Pl}}|Rest], Module, Rec) ->
    Field = list_to_atom(binary_to_list(Key)),
    Value = case Module:new(Key) of
                undefined ->
                    %% this is not a sub record, so just pl it
                    pl(Pl,Module);
                SubRec ->
                    %% we have a new record, go back go the topproplist
                    to_rec({struct,Pl}, Module, SubRec)
            end,
    UpRec = Module:'#set-'([{Field,Value}],Rec),
    keys_rec(Rest, Module, UpRec);

keys_rec([{Key, Value}|Rest], Module, Rec) ->
    Field = list_to_atom(binary_to_list(Key)),
    NewValue = to_value(Value,Module),
    NewRec = Module:'#set-'([{Field,NewValue}],Rec),
    keys_rec(Rest,Module,NewRec).

pl(P, Module) ->
    pl(P,Module,[]).
pl([],_M,[H]) -> H;
pl([],_M,Acc) -> Acc;
pl([{Key, {struct,Pl}}|Rest], Module, Acc) ->
    Value = case Module:new(Key) of
                undefined ->
                    {Key, pl(Pl, Module, [])};
                Rec ->
                    to_rec({struct, Pl}, Module, Rec)
            end,
    pl(Rest, Module, [Value|Acc]);
pl([{Key,Value}|Rest], Module, Acc) ->
    pl(Rest, Module, [{Key,Value}|Acc]).

to_value(V, Module) ->
    to_value(V, Module, []).

to_value({struct, Pl}, Module, _Acc) ->
    pl(Pl,Module);
to_value([], _Module, Acc) -> Acc;
to_value([H|T],Module, Acc) ->
    to_value(T,Module,[to_value(H,Module,[])|Acc]);
to_value(V,_Module,_Acc) -> V.






-ifdef(TEST).

new(<<"simple">>) ->
    '#new-simple'();
new(<<"simplet2l">>) ->
    '#new-simplet2l'();
new(<<"deep">>) ->
    '#new-deep'();
new(_RecName) -> undefined.

rec(#simple{}) -> true;
rec(#simplet2l{}) -> true;
rec(#deep{}) -> true;
rec(_Rec) ->  false.


simple_json_data() ->
    ["{\"one\":1,\"two\":2}",
     #simple{ one = 1, two = 2}].

simple_json_t2l_data() ->
    ["{\"two\":[1,2,3]}",
     #simplet2l{ two = {1,2,3}}].
unknown_json_data() ->
    ["{\"one\":1,\"two\":2}",
     [{<<"two">>, 2},{<<"one">>,1}]].

deep_json_data() ->
    Simple = "{\"simple\":{\"one\":1,\"two\":2}",
    Deep = Simple++"}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2}
            }].

deep_deep_json_data() ->
    Simple = "\"simple\":{\"one\":1,\"two\":2}",
    Deep = "{"++Simple++",\"second\":[{"++Simple ++ "},{" ++ Simple ++ "},{" ++ Simple ++"}]}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2},
            second  = [ #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2}
                      ]
          }
     ].


simple_test() ->
    [Json, Rec] = simple_json_data(),
    NewRec = ?MODULE:to_rec(mochijson2:decode(Json),?MODULE,?MODULE:new(<<"simple">>)),
    ?assertEqual(Rec, NewRec).

deep_test() ->
    [Json, Rec] = deep_json_data(),
    NewRec = ?MODULE:to_rec(mochijson2:decode(Json),?MODULE,?MODULE:new(<<"deep">>)),
    ?assertEqual(Rec, NewRec).

deep_deep_test()  ->
    [Json, Rec] = deep_deep_json_data(),
    New = ?MODULE:to_rec(mochijson2:decode(Json),?MODULE,?MODULE:new(<<"deep">>)),
    ?assertEqual(Rec, New).

unknown_test() ->
    [Json, Rec] = unknown_json_data(),
    New = ?MODULE:to_rec(mochijson2:decode(Json),?MODULE,?MODULE:new(<<"unknown">>)),
    ?assertEqual(Rec, New).

to_json_simple_test() ->
    [_Json, Rec] = simple_json_data(),

    Conv = ?MODULE:to_json(Rec, ?MODULE),
    Sjson= lists:flatten(mochijson2:encode(Conv)),

    New = ?MODULE:to_rec(mochijson2:decode(Sjson),?MODULE,?MODULE:new(<<"simple">>)),
    ?assertEqual(Rec,New).

to_json_deep_test() ->
    [_Json, Rec] = deep_json_data(),
    Conv = ?MODULE:to_json(Rec,?MODULE),
    Sjson = lists:flatten(mochijson2:encode(Conv)),


    New = ?MODULE:to_rec(mochijson2:decode(Sjson), ?MODULE, ?MODULE:new(<<"deep">>)),
    ?assertEqual(Rec,New).

to_json_deep_deep_test() ->
    [_Json, Rec] = deep_deep_json_data(),
    Conv = ?MODULE:to_json(Rec,?MODULE),
    Sjson = lists:flatten(mochijson2:encode(Conv)),
    New = ?MODULE:to_rec(mochijson2:decode(Sjson), ?MODULE, ?MODULE:new(<<"deep">>)),
    ?assertEqual(Rec,New).

to_json_tuple_to_list_test() ->
    [Json, Rec] = simple_json_t2l_data(),
    Conv = ?MODULE:to_json(Rec,?MODULE),
    Sjson = lists:flatten(mochijson2:encode(Conv)),
    ?assertEqual(Json,Sjson).

-endif.
