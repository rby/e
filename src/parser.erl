-module(parser).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1, parse/2]).

-define(Spaces, #{$\t => 1, $\s => 1}).
-define(IdentifierChars, "_" ++ lists:seq($a, $z) ++ lists:seq($0, $9) ++ lists:seq($A, $Z)).

%%
%% Parses from an open IODevice
-spec parse(IO :: io:device()) -> Result when
      Result :: {ok, term()} | {error, string()}.
parse(IO) -> parse(IO, false).
parse(IO, Verbose) ->
    case scan(IO) of
        {ok, Tokens} ->
            case elang_parser:parse(Tokens) of
                {ok, Result} ->
                    Verbose andalso io:format("Parsed:~n~p~n", [Result]),
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
    scan(IO, 0, [], "", none).

scan(IO, PrevLine, Acc, Input, Cont) ->
    %% we should call this only on certain condition
    %% which is when we consumed everything
    Res =
        case Input of
            "" ->
                case file:read_line(IO) of
                    {ok, LineRead} -> {ok, string:chomp(LineRead), PrevLine + 1};
                    eof -> eof
                end;
            %% previous data to continue to consume on the same line.
            _ ->
                {ok, Input, PrevLine}
        end,

    case Res of
        {ok, Data, Line} ->
            case Cont of
                none ->
                    case lexemes(Data) of
                        {ok, Lexemes, _} ->
                            Tokens = scan_tokens(Lexemes, Line),
                            Acc2 =
                                case Acc of
                                    [] -> [Tokens];
                                    _ -> [Tokens | Acc]
                                end,
                            scan(IO, Line, Acc2, "", none);
                        {more, Acc2, _Col, Cont} ->
                            scan(IO, Line, Acc2, "", {cont, Cont});
                        Other ->
                            io:format(standard_error, "Failed with ~p~n", [Other]),
                            {error, Other}
                    end;
                {cont, C} ->
                    case C(Data, Acc, 0) of
                        {ok, Acc2, Remain, _Col} ->
                            scan(IO, Line, Acc2, Remain, none);
                        {more, Acc2, _Col, Cont2} ->
                            scan(IO, Line, Acc2, "", {cont, Cont2})
                    end
            end;
        eof ->
            Tokens = lists:reverse(lists:flatten(Acc)),
            io:format(standard_error, "Tokens:~n---~n~p~n---~n", [Tokens]),
            {ok, Tokens}
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
process(Term = {Cat, _Sym}) when is_atom(Cat) -> Term;
%% TODO remove
process("_") ->
    '_';
process(X) ->
    Identity = fun(Y) -> Y end,
    %% TODO those cases will not happen anymore we parse
    %% we should parse all in lexemes
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
    %% this one tricky we should return something like
    %% {more,...}
    %% 😩
    ConsumeComment = fun(Data, Accum, ColArg) ->
        case read_until(Data, "*/", ColArg) of
            {ok, Comment, Remain, Col2} ->
                {ok, [{{text, Comment}, Col2} | Accum], Remain, Col2};
            {more, Comment, Col2} ->
                {more, [{{text, Comment}, Col2} | Accum], Col2}
        end
    end,
    case ConsumeComment(Rest, [{'/**', Col} | Acc], Col + 3) of
        {ok, Acc2, Rest2, Col2} -> lexemes(Rest2, Acc2, Col2);
        {more, Acc2, Col2} -> {more, Acc2, Col2, ConsumeComment}
    end;
lexemes("*/" ++ Rest, Acc, Col) ->
    lexemes(Rest, [{'*/', Col} | Acc], Col + 2);
lexemes(String = [$" | _], Acc, Col) ->
    {ok, Str, Col2, Rest} = lexemes_string(String, Col + 1),
    %% XXX should be {string, Str} ! really
    lexemes(Rest, [{Str, Col} | Acc], Col2);
lexemes(String = [$' | _], Acc, Col) ->
    {ok, Char, Col2, Rest} = lexemes_char(String, Col),
    lexemes(Rest, [{Char, Col} | Acc], Col2);
lexemes([$_, S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, [{$_, Col} | Acc], Col + 2);
lexemes([$_, $; | Rest], Acc, Col) ->
    lexemes(Rest, [{$;, Col + 1}, {$_, Col} | Acc], Col + 2);
lexemes(String = [S | _], Acc, Col) when S == $_; S >= $a, S =< $z; S >= $A, S =< $Z ->
    {ok, Id, Col2, Rest} = lexemes_identifier(String, Col),
    lexemes(Rest, [{Id, Col} | Acc], Col2);
lexemes(String = [D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Rest, Col2} = lexemes_number(String, Col),
    lexemes(Rest, [{Number, Col} | Acc], Col2);
lexemes([S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    lexemes(Rest, Acc, Col + 1);
lexemes(String, Acc, Col) ->
    erlang:error(io_lib:format("unexepected input: \"~p\"~nAcc: ~p~nCol:~p~n", [String, Acc, Col])).

read_until(String, To, Col) ->
    case string:find(String, To) of
        nomatch ->
            {more, String, Col + length(String)};
        Suffix ->
            Comment = string:slice(String, 0, length(String) - length(Suffix)),
            {ok, Comment, Suffix, Col + length(Comment)}
    end.

lexemes_identifier(S, Col) ->
    case string:take(S, ?IdentifierChars) of
        {Lead, Tail} -> {ok, Lead, Col + length(Lead), Tail}
    end.

lexemes_string([$" | String], Col) ->
    case string:take(String, [$"], true) of
        {_, []} -> {error, String};
        {Lead, [$" | Tail]} -> {ok, Lead, Col + length(Lead) + 1, Tail}
    end.
lexemes_char([$', $\\, S, $' | Rest], Col) ->
    case S of
        $s -> {ok, $\s, Col + 4, Rest};
        $r -> {ok, $\r, Col + 4, Rest};
        $t -> {ok, $\t, Col + 4, Rest};
        $n -> {ok, $\n, Col + 4, Rest};
        $b -> {ok, $\b, Col + 4, Rest};
        _ -> {error, not_char}
    end;
lexemes_char([$', S, $' | Rest], Col) ->
    {ok, S, Col + 3, Rest};
lexemes_char(_, _) ->
    {error, not_char}.

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
lexemes_identifier_test_() ->
    [
        ?_assertMatch({ok, "_abC3", 5, " := bc"}, lexemes_identifier("_abC3 := bc", 0)),
        ?_assertMatch({ok, "abE__34", 7, ""}, lexemes_identifier("abE__34", 0))
    ].
lexemes_char_test_() ->
    [
        ?_assertMatch({ok, $a, 3, " ; bc"}, lexemes_char("'a' ; bc", 0)),
        ?_assertMatch({ok, $\s, 4, " ; bc"}, lexemes_char("'\\s' ; bc", 0))
    ].
lexemes_string_test_() ->
    [
        ?_assertMatch({ok, "abc", 4, " some a"}, lexemes_string("\"abc\" some a", 0))
    ].
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
    ].

-endif.
