-ifndef(json_rec_types).
-define(json_rec_types, true).

-type proplist() :: list({term(),term()}).

-type json_dict() :: {struct, proplist()}.
-type json() :: json_dict()|list(term()).



-endif.
