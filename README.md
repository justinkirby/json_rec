####Making erlang record <~> json conversion easy.

Detailed info at [exprecs, making json usable](http://blogs.openaether.org/?p=253 "json_rec")

Quick example:
--
    -module(book).

    -export([new/1]).

    -record(book, {
          style      :: atom(),
          count      :: integer(),
          available  :: boolean(),
          pages      :: integer(),
          excerpt    :: string(),
          author     :: string()
         }). 

    %% the exprecs export of the record interface
    -compile({parse_transform, exprecs}).
    -export_records([book]).

    %% here we provide a mapping of the json key to a record.
    new(<<"book">>) ->
        '#new-book'();

    %% if the key is unknown, return undefined.
    new(_RecName) ->
        undefined.

At this point we can take the following json and transform it into the #book{} record.

    {
      "style": "fiction",
      "count": 1,
      "available": true,
      "pages": 42, 
      "excerpt": "Good bye and thanks for all the fish.",
      "author":"Adams, Douglas"
    }
We can get a #book{} record from the above with


    -spec json_to_rec(Json :: string()) -> #book{}.
    json_to_rec(Json) ->
        ErlJson = mochijson2:decode(Json),
        Record = book:new(<<"book">>),
        json_rec:to_rec(ErlJson, book, Record).
