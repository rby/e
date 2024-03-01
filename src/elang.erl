-module(elang).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%% API exports
%% TODO remove lexemes or actually moved completely outside of this file
-export([main/1, lexemes/1]).

-define(Spaces, #{$\t => 1, $\s => 1}).
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
                    Acc2 =
                        case Acc of
                            [] -> [Tokens];
                            [P | PP] -> [Tokens | [{'\n', Line} | [P | PP]]]
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

process(Atom) when is_atom(Atom) -> Atom;
process(F) when is_float(F) -> {float64, F};
process(I) when is_integer(I) -> {integer, I};
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
    if
        Reverse ->
            {ok, Tokens, EndP} = Res,
            {ok, lists:reverse(Tokens), EndP};
        true ->
            Res
    end.

lexemes("", Acc, EndPosition) ->
    {ok, Acc, EndPosition};
lexemes([$; | Rest], Acc, Col) ->
    lexemes(Rest, [{';', Col} | Acc], Col + 1);
lexemes([$. | Rest], Acc, Col) ->
    lexemes(Rest, [{'.', Col} | Acc], Col + 1);
lexemes([$& | Rest], Acc, Col) ->
    lexemes(Rest, [{'&', Col} | Acc], Col + 1);
lexemes(":=" ++ Rest, Acc, Col) ->
    lexemes(Rest, [{':=', Col} | Acc], Col + 2);
lexemes([$: | Rest], Acc, Col) ->
    lexemes(Rest, [{':', Col} | Acc], Col + 1);
lexemes(String = [$+, D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    lexemes(Rest, [{Number, Col} | Acc], Col2);
lexemes(String = [$-, D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    lexemes(Rest, [{Number, Col} | Acc], Col2);
lexemes(String = [D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    lexemes(Rest, [{Number, Col} | Acc], Col2);
lexemes([$+ | Rest], Acc, Col) ->
    lexemes(Rest, [{'+', Col} | Acc], Col + 1);
lexemes([$? | Rest], Acc, Col) ->
    lexemes(Rest, [{'?', Col} | Acc], Col + 1);
lexemes([$[ | Rest], Acc, Col) ->
    lexemes(Rest, [{'[', Col} | Acc], Col + 1);
lexemes([$] | Rest], Acc, Col) ->
    lexemes(Rest, [{']', Col} | Acc], Col + 1);
lexemes([${ | Rest], Acc, Col) ->
    lexemes(Rest, [{'{', Col} | Acc], Col + 1);
lexemes([$} | Rest], Acc, Col) ->
    lexemes(Rest, [{'}', Col} | Acc], Col + 1);
lexemes([$( | Rest], Acc, Col) ->
    lexemes(Rest, [{'(', Col} | Acc], Col + 1);
lexemes([$) | Rest], Acc, Col) ->
    lexemes(Rest, [{')', Col} | Acc], Col + 1);
lexemes([$, | Rest], Acc, Col) ->
    lexemes(Rest, [{',', Col} | Acc], Col + 1);
lexemes("<-" ++ Rest, Acc, Col) ->
    lexemes(Rest, [{'<-', Col} | Acc], Col + 3);
lexemes("def" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'def', Col} | Acc], Col + 4);
lexemes("var" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'var', Col} | Acc], Col + 4);
lexemes("try" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'try', Col} | Acc], Col + 4);
lexemes("if" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'if', Col} | Acc], Col + 3);
lexemes("else" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'else', Col} | Acc], Col + 5);
lexemes("escape" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'escape', Col} | Acc], Col + 7);
lexemes("catch" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'catch', Col} | Acc], Col + 6);
lexemes("finally" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'finally', Col} | Acc], Col + 8);
lexemes("method" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'method', Col} | Acc], Col + 7);
lexemes("match" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'match', Col} | Acc], Col + 6);
lexemes("implements" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{'implements', Col} | Acc], Col + 11);
lexemes("/**" ++ Rest, Acc, Col) ->
    lexemes(Rest, [{'/**', Col} | Acc], Col + 3);
lexemes("*/" ++ Rest, Acc, Col) ->
    lexemes(Rest, [{'*/', Col} | Acc], Col + 2);
lexemes(String = [D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Rest, Col2} = lexemes_number(String, Col),
    lexemes(Rest, [{Number, Col} | Acc], Col2);
lexemes([S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, Acc, Col + 1);
lexemes(String, Acc, Col) ->
    %% rollout all this to the most boring state
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

lexemes_number([$+ | Rest], Col) ->
    lexemes_number(Rest, Col + 1, []);
lexemes_number([$- | Rest], Col) ->
    Res = lexemes_number(Rest, Col + 1, []),
    io:format("Res = ~p~n", [Res]),
    {ok, Num, Col2, String} = Res,
    {ok, -Num, Col2, String};
lexemes_number([D | Rest], Col) when D >= $0, D =< $9 ->
    lexemes_number(Rest, Col + 1, [D]).
lexemes_number([D | Rest], Col, NumStr) when D >= $0, D =< $9 ->
    lexemes_number(Rest, Col + 1, [D | NumStr]);
lexemes_number([$. | Rest], Col, NumStr) ->
    lexemes_float(Rest, Col + 1, [$. | NumStr]);
lexemes_number(String, Col, NumStr) ->
    {ok, list_to_integer(lists:reverse(NumStr)), Col, String}.
lexemes_float([D | Rest], Col, NumStr) when D >= $0, D =< $9 ->
    lexemes_float(Rest, Col + 1, [D | NumStr]);
lexemes_float(String, Col, NumStr) ->
    {ok, list_to_float(lists:reverse(NumStr)), Col, String}.

-ifdef(TEST).

lexemes_number_test_() ->
    [
        ?_assertMatch({ok, -23.34, 1 + 6, " def"}, lexemes_number("-23.34 def", 1)),
        ?_assertMatch({ok, 2334, 1 + 5, " def"}, lexemes_number("+2334 def", 1)),
        ?_assertMatch({ok, 4, 1, ";"}, lexemes_number("4;", 0))
    ].
lexemes_test_() ->
    [
        ?_assertMatch(
            {ok,
                [
                    {'def', 0},
                    {"x", 4},
                    {':=', 6},
                    {3, 9},
                    {';', 10},
                    {"y", 12},
                    {':=', 14},
                    {4, 17}
                ],
                18},
            lexemes("def x := 3; y := 4", true)
        ),
        ?_assertMatch(
            {ok, [{'&', 0}, {"x", 1}, {':=', 3}, {4, 6}, {';', 7}], 8},
            lexemes("&x := 4;", true)
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
