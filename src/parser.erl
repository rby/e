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
-spec scan(IO :: io:device()) -> Return when
    Return :: {ok, [term()]} | {error, string()}.
scan(IO) ->
    scan(IO, 0, [], "", none).

-spec scan(
    IO :: io:device(),
    PrevLine :: int,
    Acc :: [Token],
    Input :: string(),
    Cont :: function()
) -> {ok, [term()]} | {error, string()} when
    Line :: int,
    Column :: int,
    Position :: {Line, Column},
    Token :: {{Category, Symbol}, Position},
    Category :: atom(),
    Symbol :: string().
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
                    case tokens(Data) of
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

%% TODO This is really not needed if Tokens are correctly returned by lexemes
process(Atom) when is_atom(Atom) -> Atom;
process(F) when is_float(F) -> {float64, F};
process(I) when is_integer(I) -> {integer, I};
process(Term = {Cat, _Sym}) when is_atom(Cat) -> Term;
process(X) -> {unexpected, X}.

-spec tokens(String :: list()) -> {ok, Tokens, EndPosition} when
    Token :: {atom(), list() | atom(), integer()} | {error, list()},
    EndPosition :: integer(),
    Tokens :: [Token].
tokens(String) ->
    tokens(String, false).
tokens(String, Reverse) ->
    Res = tokens(String, [], 0),
    if
        Reverse ->
            {ok, Tokens, EndP} = Res,
            {ok, lists:reverse(Tokens), EndP};
        true ->
            Res
    end.

tokens("", Acc, EndPosition) ->
    {ok, Acc, EndPosition};
tokens([$; | Rest], Acc, Col) ->
    tokens(Rest, [{';', Col} | Acc], Col + 1);
tokens([$. | Rest], Acc, Col) ->
    tokens(Rest, [{'.', Col} | Acc], Col + 1);
tokens([$& | Rest], Acc, Col) ->
    tokens(Rest, [{'&', Col} | Acc], Col + 1);
tokens(":=" ++ Rest, Acc, Col) ->
    tokens(Rest, [{':=', Col} | Acc], Col + 2);
tokens([$: | Rest], Acc, Col) ->
    tokens(Rest, [{':', Col} | Acc], Col + 1);
tokens(String = [$+, D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    tokens(Rest, [{Number, Col} | Acc], Col2);
tokens(String = [$-, D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    tokens(Rest, [{Number, Col} | Acc], Col2);
tokens(String = [D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Col2, Rest} = lexemes_number(String, Col),
    tokens(Rest, [{Number, Col} | Acc], Col2);
tokens([$+ | Rest], Acc, Col) ->
    tokens(Rest, [{'+', Col} | Acc], Col + 1);
tokens([$? | Rest], Acc, Col) ->
    tokens(Rest, [{'?', Col} | Acc], Col + 1);
tokens([$[ | Rest], Acc, Col) ->
    tokens(Rest, [{'[', Col} | Acc], Col + 1);
tokens([$] | Rest], Acc, Col) ->
    tokens(Rest, [{']', Col} | Acc], Col + 1);
tokens([${ | Rest], Acc, Col) ->
    tokens(Rest, [{'{', Col} | Acc], Col + 1);
tokens([$} | Rest], Acc, Col) ->
    tokens(Rest, [{'}', Col} | Acc], Col + 1);
tokens([$( | Rest], Acc, Col) ->
    tokens(Rest, [{'(', Col} | Acc], Col + 1);
tokens([$) | Rest], Acc, Col) ->
    tokens(Rest, [{')', Col} | Acc], Col + 1);
tokens([$, | Rest], Acc, Col) ->
    tokens(Rest, [{',', Col} | Acc], Col + 1);
tokens("<-" ++ Rest, Acc, Col) ->
    tokens(Rest, [{'<-', Col} | Acc], Col + 3);
tokens("def" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'def', Col} | Acc], Col + 4);
tokens("var" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'var', Col} | Acc], Col + 4);
tokens("try" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'try', Col} | Acc], Col + 4);
tokens("if" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'if', Col} | Acc], Col + 3);
tokens("else" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'else', Col} | Acc], Col + 5);
tokens("escape" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'escape', Col} | Acc], Col + 7);
tokens("catch" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'catch', Col} | Acc], Col + 6);
tokens("finally" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'finally', Col} | Acc], Col + 8);
tokens("method" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'method', Col} | Acc], Col + 7);
tokens("match" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'match', Col} | Acc], Col + 6);
tokens("implements" ++ [S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{'implements', Col} | Acc], Col + 11);
tokens("/**" ++ Rest, Acc, Col) ->
    ConsumeComment = fun(Data, Accum, ColArg) ->
        case read_until(Data, "*/", ColArg) of
            {ok, Comment, Remain, Col2} ->
                {ok, [{{text, Comment}, Col2} | Accum], Remain, Col2};
            {more, Comment, Col2} ->
                {more, [{{text, Comment}, Col2} | Accum], Col2}
        end
    end,
    case ConsumeComment(Rest, [{'/**', Col} | Acc], Col + 3) of
        {ok, Acc2, Rest2, Col2} -> tokens(Rest2, Acc2, Col2);
        {more, Acc2, Col2} -> {more, Acc2, Col2, ConsumeComment}
    end;
tokens("*/" ++ Rest, Acc, Col) ->
    tokens(Rest, [{'*/', Col} | Acc], Col + 2);
tokens(String = [$" | _], Acc, Col) ->
    {ok, Str, Col2, Rest} = lexemes_string(String, Col + 1),
    tokens(Rest, [{{string, Str}, Col} | Acc], Col2);
tokens(String = [$' | _], Acc, Col) ->
    {ok, Char, Col2, Rest} = lexemes_char(String, Col),
    tokens(Rest, [{Char, Col} | Acc], Col2);
tokens([$_, S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, [{$_, Col} | Acc], Col + 2);
tokens([$_, $; | Rest], Acc, Col) ->
    tokens(Rest, [{$;, Col + 1}, {$_, Col} | Acc], Col + 2);
tokens(String = [S | _], Acc, Col) when S == $_; S >= $a, S =< $z; S >= $A, S =< $Z ->
    {ok, Id, Col2, Rest} = lexemes_identifier(String, Col),
    tokens(Rest, [{{identifier, Id}, Col} | Acc], Col2);
tokens(String = [D | _], Acc, Col) when D >= $0, D =< $9 ->
    {ok, Number, Rest, Col2} = lexemes_number(String, Col),
    tokens(Rest, [{Number, Col} | Acc], Col2);
tokens([S | Rest], Acc, Col) when is_map_key(S, ?Spaces) ->
    tokens(Rest, Acc, Col + 1);
tokens(String, Acc, Col) ->
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
%% TODO we should finish with either ';' or some space, or empty
lexemes_number(String, Col, NumStr) ->
    {ok, list_to_integer(lists:reverse(NumStr)), Col, String}.
lexemes_float([D | Rest], Col, NumStr) when D >= $0, D =< $9 ->
    lexemes_float(Rest, Col + 1, [D | NumStr]);
%% TODO we should finish with either ';' or some space, or empty
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
                    {{identifier, "x"}, 4},
                    {':=', 6},
                    {3, 9},
                    {';', 10},
                    {{identifier, "y"}, 12},
                    {':=', 14},
                    {4, 17}
                ],
                18},
            tokens("def x := 3; y := 4", true)
        ),
        ?_assertMatch(
            {ok, [{'&', 0}, {{identifier, "x"}, 1}, {':=', 3}, {4, 6}, {';', 7}], 8},
            tokens("&x := 4;", true)
        )
    ].

-endif.
