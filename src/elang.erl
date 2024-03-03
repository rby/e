-module(elang).

%% API exports
%% TODO remove lexemes or actually moved completely outside of this file
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format(standard_error, "Args: ~p~n", [Args]),
    {ok, IO} =
        case Args of
            [] -> {ok, standard_io};
            [FileName | _] -> file:open(FileName, [read, {encoding, utf8}])
        end,

    case parser:parse(IO) of
        {ok, Parsed} -> io:format("Parsed:~n~p~n", [Parsed]);
        {error, Reason} -> io:format("Failed:~n~p~n", [Reason])
    end,

    ok =
        if
            IO /= standard_io -> file:close(IO);
            true -> ok
        end,

    erlang:halt(0).
