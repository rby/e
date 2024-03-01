-module(elang).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%% API exports
%% TODO remove lexemes or actually moved completely outside of this file
-export([main/1, lexemes/1]).

-define(seps, [$\t, $;, $\s]).
%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    {ok, IO} =
        case Args of
            [] -> {ok, standard_io};
            [FileName | _] -> file:open(FileName, [read, {encoding, utf8}])
        end,
    {ok, Data} = file:read(IO, 10),

    io:format("Data: ~p~n", [Data]),

    erlang:halt(0).
%% Internal functions
%%====================================================================
%%
%% tokenizer
%%
scan(IO) ->
    scan(IO, 0, []).

scan(IO, PrevLine, Acc) ->
    case file:read_line(IO) of
        {ok, Data} ->
            Line = PrevLine + 1,

            Tokens = scan_tokens(lexemes(string:trim(Data)), Line);
        eof ->
            something
    end.

scan_tokens(Tokens, Line) ->
    %% TODO
    Tokens.

lexemes(String) ->
    lexemes(String, [], 0).
lexemes("", Acc, EndPosition) ->
    {ok, lists:reverse(Acc), EndPosition};
lexemes(String, Acc, Col) ->
    {Leading, Trailing} = string:take(String, ?seps, true),
    io:format("Leading=~p, Trailing=~p, ~n", [Leading, Trailing]),
    {Ignore, Remain} = string:take(Trailing, ?seps),
    NewCol = Col + length(Leading) + length(Ignore),

    if
        Leading /= "" ->
            Acc2 =
                case string:find(Ignore, ";") of
                    nomatch -> [{Leading, Col} | Acc];
                    SemiColRem -> [{';', NewCol - length(SemiColRem)} | [{Leading, Col} | Acc]]
                end,

            lexemes(Remain, Acc2, NewCol);
        true ->
            %% just ignore Leading
            lexemes(Remain, Acc, NewCol)
    end.

-ifdef(TEST).

lexemes_test_() ->
    [
    ?_assertMatch(
       {ok,
        [
         {"def", 0},
         {"x", 4},
         {":=", 6},
         {"3", 9},
         {';', 10},
         {"y", 12},
         {":=", 14},
         {"4", 17}
        ],
        18},
       lexemes("def x := 3; y := 4")
      )
    %% ideally this should work too
    % ?_assertMatch(
    %    {ok,
    %     [
    %      {"def", 0},
    %      {"x", 4},
    %      {":=", 6},
    %      {"3", 9},
    %      {';', 10},
    %      {"y", 12},
    %      {":=", 14},
    %      {"4", 17}
    %     ],
    %     18},
    %    lexemes("def x:=3;y:=4")
    %   )
    ].

-endif.
