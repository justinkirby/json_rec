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

-export([
         to_rec/3,
         to_json/2
        ]).

-include("json_rec_types.hrl").


%% note: I am using tuple() for record, since this is a generic record
-spec to_json(Record :: tuple(), Module :: [atom()]) -> {struct, proplist()};
             (Record :: tuple(), Module :: atom())  -> {struct, proplist()}.

to_json(Record, Module) when is_list(Module) ->
    Fields = module_rec_fields(Module,Record),
    Pl = rec_keys(Fields, Record, Module, []),
    {struct, Pl};

to_json(Record, Module) ->
    Fields = module_rec_fields([Module],Record),
    Pl = rec_keys(Fields,Record,[Module],[]),
    {struct, Pl}.


rec_keys([], _Record, _Module, Acc) -> Acc;
rec_keys([Field|Rest],Record,Module,Acc) ->
    Value = module_get(Module, Field, Record),
    Key = list_to_binary(atom_to_list(Field)),
    JsonValue = field_value(Value,Module,[]),
    rec_keys(Rest, Record, Module,[{Key,JsonValue}|Acc]).

field_value(Value, Module, _Acc) when is_tuple(Value) ->
    case module_has_rec(Module, Value, false) of
        false ->
            Value;
        _M when is_atom(_M) ->
            to_json(Value,Module)
    end;
field_value(Value, _Module, _Acc) when is_atom(Value) ->
    list_to_binary(atom_to_list(Value));

field_value([],_Module, Acc)  -> lists:reverse(Acc);
field_value([{_,_}|_] = Pl, Module, Acc) ->
    %% it is a proplist, make it a dict
    {struct, [{Key, Value} || {Key, V2} <- Pl,
                          begin
                              Value = field_value(V2, Module, Acc),
                              true
                          end]};

field_value([Value|Rest], Module, Acc) ->
    NewValue = case field_value(Value,Module,[]) of
                   IsRec when is_tuple(IsRec),
                              is_atom(element(1,Value)) ->
                       %% this returned a record, so get the first
                       %% element from the rec tuple and do: {struct,
                       %% atom
                       {struct, [{list_to_binary(atom_to_list(element(1,Value))),IsRec}]};
                   %% IsTuple when is_tuple(IsTuple) ->
                   %%     tuple_to_list(IsTuple);
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
-spec to_rec(_Json :: json_dict(), Module :: atom() | [atom()], undefined) ->
                    proplist();
            (_Json :: json_dict(), Module :: atom() | [atom()], Rec :: tuple() ) ->
                    Rec :: tuple().

to_rec({struct, Pl} = _Json, Module, undefined) when is_list(Module) ->
    pl(Pl, Module);
to_rec({struct, Pl} = _Json, Module, undefined) ->
    pl(Pl, [Module]);

to_rec({struct, Pl} = _Json, Module, Rec) when is_list(Module) ->
    keys_rec(Pl, Module, Rec);
to_rec({struct, Pl} = _Json, Module, Rec) ->
    keys_rec(Pl, [Module], Rec).

keys_rec([], _Module, Rec) -> Rec;
keys_rec([{Key, {struct, Pl}}|Rest], Module, Rec) ->
    Field = list_to_atom(binary_to_list(Key)),
    Value = case module_new(Module, Key, undefined) of
                undefined ->
                    %% this is not a sub record, so just pl it
                    pl(Pl,Module);
                SubRec ->
                    %% we have a new record, go back go the topproplist
                    to_rec({struct,Pl}, Module, SubRec)
            end,
    UpRec = module_set(Module, {Field,Value}, Rec),
    keys_rec(Rest, Module, UpRec);

keys_rec([{Key, Value}|Rest], Module, Rec) ->
    Field = list_to_atom(binary_to_list(Key)),
    NewValue = to_value(Value,Module),
    NewRec = module_set(Module, {Field, NewValue}, Rec),
    keys_rec(Rest,Module,NewRec).

pl(P, Module) ->
    pl(P,Module,[]).
pl([],_M,[H]) -> H;
pl([],_M,Acc) -> Acc;
pl([{Key, {struct,Pl}}|Rest], Module, Acc) ->
    Value = case module_new(Module,Key,undefined) of
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



module_new([], _Key, Rec) ->
    Rec;
module_new([H|T], Key, Rec) ->
    case H:new(Key) of
        undefined ->
            module_new(T,Key,Rec);
        SubRec ->
            SubRec
    end.


module_has_rec(Ms, Rec) ->
    module_has_rec(Ms, Rec, throw).

module_has_rec([],_Rec, throw) -> throw(did_not_find_module);
module_has_rec([],_Rec, V) -> V;
module_has_rec([M|T],Rec, Act) ->
    case M:'#is_record-'(Rec) of
        false ->
            module_has_rec(T,Rec, Act);
        true  ->
            M
    end.



module_set(Ms, Kv, Rec) ->
    M = module_has_rec(Ms,Rec),
    M:'#set-'([Kv],Rec).

module_rec_fields(Ms, Rec ) ->
    M = module_has_rec(Ms,Rec),
    M:'#info-'(element(1, Rec)).

module_get(Ms, Field, Rec) ->
    M = module_has_rec(Ms, Rec),
    M:'#get-'(Field,Rec).
