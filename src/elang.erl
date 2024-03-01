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

    {ok, _Parsed} = parse(IO),

    ok =
        if
            IO /= standard_io -> file:close(IO);
            true -> ok
        end,

    erlang:halt(0).
%% Internal functions
%%====================================================================
%%
%%
parse(IO) ->
    case scan(IO) of
        {ok, Tokens} ->
            case elang_parser:parse(Tokens) of
                {ok, Result} ->
                    io:format("Parsed:~n~p~n", [Result]),
                    {ok, Result};
                {error, Err} ->
                    io:format("Failed:~n~p~n", [Err]),
                    {error, Err}
            end;
        {error, Err} ->
            {error, Err}
    end.
%% tokenizer
%%
scan(IO) ->
    scan(IO, 0, []).

scan(IO, PrevLine, Acc) ->
    case file:read_line(IO) of
        {ok, Data} ->
            Line = PrevLine + 1,
            case lexemes(string:trim(Data)) of
                {ok, Lexemes, EndPosition} ->
                    Tokens = scan_tokens(Lexemes, Line),
                    io:format("~p~n", [{Tokens, EndPosition}]),
                    Acc2 = case Acc of 
                               [] -> [Tokens];
                               [P | PP] -> [Tokens | [{'\n', Line} | [ P | PP]]]
                           end,
                    scan(IO, Line, Acc2);
                Other ->
                    io:format("Failed with ~p~n", [Other]),
                    {error, Other}
            end;
        eof ->
            {ok, lists:reverse(lists:flatten(Acc))}
    end.

scan_tokens(Tokens, Line) ->
    lists:map(
        fun({Token, Col}) ->
            Position = {Line, Col},
            Token2 = process(Token),
            case Token2 of
                {Cat, Sym} -> {Cat, Position, Sym};
                _ -> {Token2, Position}
            end
        end,
        Tokens
    ).

process(';') ->
    ';';
process(":=") ->
    ':=';
process("_") ->
    '_';
process(X) ->
    Identity = fun(Y) -> Y end,
    Regs = [
        {"[+-]?[0-9][0-9_]*", integer, fun erlang:list_to_integer/1},
        {"[+-]?([0-9]*[.])?[0-9]+", float64, fun erlang:list_to_float/1},
        {"[_a-zA-Z]?[a-zA-Z_0-9]*", identifier, Identity}
    ],
    case
        lists:dropwhile(
            fun({Re, _, _}) ->
                case re:run(X, Re) of
                    match -> false;
                    {match, _} -> false;
                    _ -> true
                end
            end,
            Regs
        )
    of
        [{_, Cat, Tr} | _] ->
            {Cat, Tr(X)};
        _ ->
            X
    end.

lexemes(String) ->
    lexemes(String, false).
lexemes(String, Reverse) ->
    Res = lexemes(String, [], 0),
    if Reverse ->
           {ok, Tokens, EndP} = Res,
           {ok, lists:reverse(Tokens), EndP};
       true ->
           Res
    end.

lexemes("", Acc, EndPosition) ->
    {ok, Acc, EndPosition};
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
            lexemes("def x := 3; y := 4", true)
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
