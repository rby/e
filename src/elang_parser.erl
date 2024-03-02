%% This is the Elang compiler

-module(elang_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 150).

-file(
    "/nix/store/4srfrvql4wdhrmfi8a9grsmd7wnvgn43-erlang-25.3.2.8/lib/erlang/lib/parsetools-2.4.1/include/yeccpre.hrl",
    0
).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-spec parse_and_scan(
    {function() | {atom(), atom()}, [_]}
    | {atom(), atom(), [_]}
) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try
        yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch
        error:Error:Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(
                        error,
                        {yecc_bug, ?CODE_VERSION, Desc},
                        Stacktrace
                    )
            catch
                _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw:{error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE, F, ArityOrArgs, _} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok, [{atom, _, Symbol}], _} = erl_scan:string(SymbolL),
            State =
                case ArityOrArgs of
                    [S, _, _, _, _, _, _] -> S;
                    _ -> state_is_unknown
                end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A}, _Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(
        State,
        '$end',
        States,
        Vstack,
        yecc_end(Line),
        [],
        {no_func, Line}
    );
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(
        State,
        '$end',
        States,
        Vstack,
        yecc_end(EndLocation),
        [],
        {no_func, EndLocation}
    ).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(
        State,
        element(1, Token),
        [State1 | States],
        [Token0 | Vstack],
        Token,
        Tokens,
        Tzr
    );
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F, _A}, _Location} = Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(
        State,
        '$end',
        [State1 | States],
        [Token0 | Vstack],
        yecc_end(Location),
        [],
        {no_func, Location}
    );
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(
        State,
        '$end',
        [State1 | States],
        [Token0 | Vstack],
        yecc_end(Location),
        [],
        {no_func, Location}
    ).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch
        _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch
        _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try
        erl_scan:location(Token)
    catch
        _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) ->
    io_lib:write_atom(A);
yecctoken2string1({integer, _, N}) ->
    io_lib:write(N);
yecctoken2string1({float, _, F}) ->
    io_lib:write(F);
yecctoken2string1({char, _, C}) ->
    io_lib:write_char(C);
yecctoken2string1({var, _, V}) ->
    io_lib:format("~s", [V]);
yecctoken2string1({string, _, S}) ->
    io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) ->
    io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) ->
    io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) ->
    "'.'";
yecctoken2string1({'$end', _}) ->
    [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-file("/Users/ramzi/code/personal/elang/src/elang_parser.erl", 186).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function, yeccpars2/7}).
yeccpars2(0 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158 = S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
    erlang:error({yecc_bug, "1.4", {missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function, yeccpars2_0/7}).
yeccpars2_0(S, '&', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'char', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'def', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'escape', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'float64', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'if', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'string', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'try', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function, yeccpars2_1/7}).
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_1_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function, yeccpars2_2/7}).
yeccpars2_2(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
    {ok, hd(Stack)};
yeccpars2_2(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function, yeccpars2_3/7}).
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_3_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function, yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_4_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
-compile({nowarn_unused_function, yeccpars2_5/7}).
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_5_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function, yeccpars2_6/7}).
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_6_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).
-compile({nowarn_unused_function, yeccpars2_7/7}).
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_7_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).
-compile({nowarn_unused_function, yeccpars2_8/7}).
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_8_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function, yeccpars2_9/7}).
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_9_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).
-compile({nowarn_unused_function, yeccpars2_10/7}).
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_10_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
-compile({nowarn_unused_function, yeccpars2_11/7}).
yeccpars2_11(S, '&', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, ';', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'char', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'def', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'escape', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'float64', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'if', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'string', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'try', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_11_(Stack),
    yeccgoto_seq_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
-compile({nowarn_unused_function, yeccpars2_12/7}).
yeccpars2_12(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'def', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function, yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_13_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function, yeccpars2_14/7}).
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_14_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).
-compile({nowarn_unused_function, yeccpars2_15/7}).
yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_15_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_16/7}).
-compile({nowarn_unused_function, yeccpars2_16/7}).
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_16_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).
-compile({nowarn_unused_function, yeccpars2_17/7}).
yeccpars2_17(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function, yeccpars2_18/7}).
yeccpars2_18(S, '*/', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function, yeccpars2_19/7}).
yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_19_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_20/7}).
-compile({nowarn_unused_function, yeccpars2_20/7}).
yeccpars2_20(S, '[', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '_', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'var', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).
-compile({nowarn_unused_function, yeccpars2_21/7}).
yeccpars2_21(S, '[', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, '_', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, 'var', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function, yeccpars2_22/7}).
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_22_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function, yeccpars2_23/7}).
yeccpars2_23(S, ':=', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_23_(Stack),
    yeccgoto_noun(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function, yeccpars2_24/7}).
yeccpars2_24(S, '(', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function, yeccpars2_25/7}).
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_25_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function, yeccpars2_26/7}).
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_26_(Stack),
    yeccgoto_e_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).
-compile({nowarn_unused_function, yeccpars2_27/7}).
yeccpars2_27(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_28: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_29/7}).
-compile({nowarn_unused_function, yeccpars2_29/7}).
yeccpars2_29(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function, yeccpars2_30/7}).
yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_30_(Stack),
    yeccgoto_hide_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_31: see yeccpars2_0

yeccpars2_32(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
-compile({nowarn_unused_function, yeccpars2_32/7}).
yeccpars2_cont_32(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_32(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_32(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_32(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
-compile({nowarn_unused_function, yeccpars2_33/7}).
yeccpars2_33(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_34: see yeccpars2_33

%% yeccpars2_35: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_36/7}).
-compile({nowarn_unused_function, yeccpars2_36/7}).
yeccpars2_36(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'finally', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_37: see yeccpars2_21

%% yeccpars2_38: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_39/7}).
-compile({nowarn_unused_function, yeccpars2_39/7}).
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_39_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function, yeccpars2_40/7}).
yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_40_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function, yeccpars2_41/7}).
yeccpars2_41(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function, yeccpars2_42/7}).
yeccpars2_42(S, ':', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function, yeccpars2_43/7}).
yeccpars2_43(S, '+', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_43_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function, yeccpars2_44/7}).
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_44_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).
-compile({nowarn_unused_function, yeccpars2_45/7}).
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_45_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function, yeccpars2_46/7}).
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_46_(Stack),
    yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_47: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_48/7}).
-compile({nowarn_unused_function, yeccpars2_48/7}).
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_48_(Stack),
    yeccgoto_ignore_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function, yeccpars2_49/7}).
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_49_(Stack),
    yeccgoto_noun(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function, yeccpars2_50/7}).
yeccpars2_50(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function, yeccpars2_51/7}).
yeccpars2_51(S, ':', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_52: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function, yeccpars2_53/7}).
yeccpars2_53(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _ | Nss] = Ss,
    NewStack = yeccpars2_53_(Stack),
    yeccgoto_var_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function, yeccpars2_54/7}).
yeccpars2_54(S, ']', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
-compile({nowarn_unused_function, yeccpars2_55/7}).
yeccpars2_55(S, ',', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_55_(Stack),
    yeccgoto_patterns(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_56: see yeccpars2_21

%% yeccpars2_57: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_58/7}).
-compile({nowarn_unused_function, yeccpars2_58/7}).
yeccpars2_58(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_58_(Stack),
    yeccgoto_such_that_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
-compile({nowarn_unused_function, yeccpars2_59/7}).
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_59_(Stack),
    yeccgoto_patterns(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
-compile({nowarn_unused_function, yeccpars2_60/7}).
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_60_(Stack),
    yeccgoto_list_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_62/7}).
-compile({nowarn_unused_function, yeccpars2_62/7}).
yeccpars2_62(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_62_(Stack),
    yeccgoto_cdr_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_63: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_64/7}).
-compile({nowarn_unused_function, yeccpars2_64/7}).
yeccpars2_64(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_64_(Stack),
    yeccgoto_final_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_65: see yeccpars2_0

yeccpars2_66(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_67/7}).
-compile({nowarn_unused_function, yeccpars2_67/7}).
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_67_(Stack),
    yeccgoto_finally_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_68/7}).
-compile({nowarn_unused_function, yeccpars2_68/7}).
yeccpars2_68(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_69: see yeccpars2_0

yeccpars2_70(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
-compile({nowarn_unused_function, yeccpars2_71/7}).
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_71_(Stack),
    yeccgoto_catch_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_72/7}).
-compile({nowarn_unused_function, yeccpars2_72/7}).
yeccpars2_72(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_72_(Stack),
    yeccgoto_match_bind_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_73/7}).
-compile({nowarn_unused_function, yeccpars2_73/7}).
yeccpars2_73(S, '(', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_74/7}).
-compile({nowarn_unused_function, yeccpars2_74/7}).
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_74_(Stack),
    yeccgoto_verb(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_75: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_76/7}).
-compile({nowarn_unused_function, yeccpars2_76/7}).
yeccpars2_76(S, ')', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).
-compile({nowarn_unused_function, yeccpars2_77/7}).
yeccpars2_77(S, ',', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_77_(Stack),
    yeccgoto_e_exprs(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_78: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_79/7}).
-compile({nowarn_unused_function, yeccpars2_79/7}).
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_79_(Stack),
    yeccgoto_e_exprs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_80/7}).
-compile({nowarn_unused_function, yeccpars2_80/7}).
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_80_(Stack),
    yeccgoto_send_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).
-compile({nowarn_unused_function, yeccpars2_81/7}).
yeccpars2_81(S, '(', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_82: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_83/7}).
-compile({nowarn_unused_function, yeccpars2_83/7}).
yeccpars2_83(S, ')', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_84/7}).
-compile({nowarn_unused_function, yeccpars2_84/7}).
yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_84_(Stack),
    yeccgoto_call_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_85: see yeccpars2_0

yeccpars2_86(S, ')', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).
-compile({nowarn_unused_function, yeccpars2_87/7}).
yeccpars2_87(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_88: see yeccpars2_0

yeccpars2_89(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_90/7}).
-compile({nowarn_unused_function, yeccpars2_90/7}).
yeccpars2_90(S, 'else', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_91/7}).
-compile({nowarn_unused_function, yeccpars2_91/7}).
yeccpars2_91(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_92: see yeccpars2_0

yeccpars2_93(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
-compile({nowarn_unused_function, yeccpars2_94/7}).
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _, _, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_94_(Stack),
    yeccgoto_if_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_95: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_96/7}).
-compile({nowarn_unused_function, yeccpars2_96/7}).
yeccpars2_96(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_96_(Stack),
    yeccgoto_assign_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).
-compile({nowarn_unused_function, yeccpars2_97/7}).
yeccpars2_97(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_98: see yeccpars2_0

yeccpars2_99(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
-compile({nowarn_unused_function, yeccpars2_100/7}).
yeccpars2_100(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_101: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_102/7}).
-compile({nowarn_unused_function, yeccpars2_102/7}).
yeccpars2_102(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_103: see yeccpars2_0

yeccpars2_104(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).
-compile({nowarn_unused_function, yeccpars2_105/7}).
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_105_(Stack),
    yeccgoto_escape_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_106/7}).
-compile({nowarn_unused_function, yeccpars2_106/7}).
yeccpars2_106(S, ':=', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_107/7}).
-compile({nowarn_unused_function, yeccpars2_107/7}).
yeccpars2_107(S, 'implements', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_107_(Stack),
    yeccgoto_ignore_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).
-compile({nowarn_unused_function, yeccpars2_108/7}).
yeccpars2_108(S, 'implements', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'match', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_108_(Stack),
    yeccgoto_noun(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
-compile({nowarn_unused_function, yeccpars2_109/7}).
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_109_(Stack),
    yeccgoto_behavior(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).
-compile({nowarn_unused_function, yeccpars2_110/7}).
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_110_(Stack),
    yeccgoto_behavior(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
-compile({nowarn_unused_function, yeccpars2_111/7}).
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_111_(Stack),
    yeccgoto_object_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_112/7}).
-compile({nowarn_unused_function, yeccpars2_112/7}).
yeccpars2_112(S, 'match', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_113: see yeccpars2_0

%% yeccpars2_114: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_115/7}).
-compile({nowarn_unused_function, yeccpars2_115/7}).
yeccpars2_115(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'match', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, 'method', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_116/7}).
-compile({nowarn_unused_function, yeccpars2_116/7}).
yeccpars2_116(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_117/7}).
-compile({nowarn_unused_function, yeccpars2_117/7}).
yeccpars2_117(S, 'match', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_118/7}).
-compile({nowarn_unused_function, yeccpars2_118/7}).
yeccpars2_118(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(S, 'method', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_118_(Stack),
    yeccgoto_emethods(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_119/7}).
-compile({nowarn_unused_function, yeccpars2_119/7}).
yeccpars2_119(S, '/**', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(S, 'method', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_119(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_120: see yeccpars2_33

-dialyzer({nowarn_function, yeccpars2_121/7}).
-compile({nowarn_unused_function, yeccpars2_121/7}).
yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_121_(Stack),
    yeccgoto_escript(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
-compile({nowarn_unused_function, yeccpars2_122/7}).
yeccpars2_122(S, '(', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_123: see yeccpars2_21

-dialyzer({nowarn_function, yeccpars2_124/7}).
-compile({nowarn_unused_function, yeccpars2_124/7}).
yeccpars2_124(S, ')', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_125/7}).
-compile({nowarn_unused_function, yeccpars2_125/7}).
yeccpars2_125(S, ':', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_126: see yeccpars2_0

yeccpars2_127(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_cont_32(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_128: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_129/7}).
-compile({nowarn_unused_function, yeccpars2_129/7}).
yeccpars2_129(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_130/7}).
-compile({nowarn_unused_function, yeccpars2_130/7}).
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _, _, _, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_130_(Stack),
    yeccgoto_emethod(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
-compile({nowarn_unused_function, yeccpars2_131/7}).
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_131_(Stack),
    yeccgoto_emethod(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_132/7}).
-compile({nowarn_unused_function, yeccpars2_132/7}).
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_132_(Stack),
    yeccgoto_emethods(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_133/7}).
-compile({nowarn_unused_function, yeccpars2_133/7}).
yeccpars2_133(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_134/7}).
-compile({nowarn_unused_function, yeccpars2_134/7}).
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_134_(Stack),
    yeccgoto_escript(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_135/7}).
-compile({nowarn_unused_function, yeccpars2_135/7}).
yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _ | Nss] = Ss,
    NewStack = yeccpars2_135_(Stack),
    yeccgoto_escript(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_136/7}).
-compile({nowarn_unused_function, yeccpars2_136/7}).
yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_136_(Stack),
    yeccgoto_escript(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_137/7}).
-compile({nowarn_unused_function, yeccpars2_137/7}).
yeccpars2_137(S, '?', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
    yeccerror(T).

%% yeccpars2_138: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_139/7}).
-compile({nowarn_unused_function, yeccpars2_139/7}).
yeccpars2_139(S, '}', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_140/7}).
-compile({nowarn_unused_function, yeccpars2_140/7}).
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _, _ | Nss] = Ss,
    NewStack = yeccpars2_140_(Stack),
    yeccgoto_matcher(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_141/7}).
-compile({nowarn_unused_function, yeccpars2_141/7}).
yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_141_(Stack),
    yeccgoto_auditors(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_142/7}).
-compile({nowarn_unused_function, yeccpars2_142/7}).
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _ | Nss] = Ss,
    NewStack = yeccpars2_142_(Stack),
    yeccgoto_object_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_112

-dialyzer({nowarn_function, yeccpars2_144/7}).
-compile({nowarn_unused_function, yeccpars2_144/7}).
yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _ | Nss] = Ss,
    NewStack = yeccpars2_144_(Stack),
    yeccgoto_object_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_145: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_146/7}).
-compile({nowarn_unused_function, yeccpars2_146/7}).
yeccpars2_146(S, '.', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, '<-', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, '=~', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _, _ | Nss] = Ss,
    NewStack = yeccpars2_146_(Stack),
    yeccgoto_define_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_147/7}).
-compile({nowarn_unused_function, yeccpars2_147/7}).
yeccpars2_147(S, '*/', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_148/7}).
-compile({nowarn_unused_function, yeccpars2_148/7}).
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_148_(Stack),
    yeccgoto_doc_comment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_149/7}).
-compile({nowarn_unused_function, yeccpars2_149/7}).
yeccpars2_149(S, 'text', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    NewStack = yeccpars2_149_(Stack),
    yeccgoto_texts(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_150/7}).
-compile({nowarn_unused_function, yeccpars2_150/7}).
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_150_(Stack),
    yeccgoto_texts(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
-compile({nowarn_unused_function, yeccpars2_151/7}).
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_151_(Stack),
    yeccgoto_doc_comment(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_152/7}).
-compile({nowarn_unused_function, yeccpars2_152/7}).
yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_152_(Stack),
    yeccgoto_slot_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
-compile({nowarn_unused_function, yeccpars2_153/7}).
yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_153_(Stack),
    yeccgoto_object_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
-compile({nowarn_unused_function, yeccpars2_154/7}).
yeccpars2_154(S, '_', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(S, 'identifier', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_155/7}).
-compile({nowarn_unused_function, yeccpars2_155/7}).
yeccpars2_155(S, 'implements', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_156/7}).
-compile({nowarn_unused_function, yeccpars2_156/7}).
yeccpars2_156(S, 'implements', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, 'match', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(S, '{', Ss, Stack, T, Ts, Tzr) ->
    yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_, _, _, _, T, _, _) ->
    yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_157/7}).
-compile({nowarn_unused_function, yeccpars2_157/7}).
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_ | Nss] = Ss,
    NewStack = yeccpars2_157_(Stack),
    yeccgoto_seq_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_158: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_159/7}).
-compile({nowarn_unused_function, yeccpars2_159/7}).
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
    [_, _ | Nss] = Ss,
    NewStack = yeccpars2_159_(Stack),
    yeccgoto_seq_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_assign_expr/7}).
-compile({nowarn_unused_function, yeccgoto_assign_expr/7}).
yeccgoto_assign_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_assign_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_auditors/7}).
-compile({nowarn_unused_function, yeccgoto_auditors/7}).
yeccgoto_auditors(107, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_112(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditors(108, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditors(155, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_112(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditors(156, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_behavior/7}).
-compile({nowarn_unused_function, yeccgoto_behavior/7}).
yeccgoto_behavior(108 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_behavior(112 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_behavior(143 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_behavior(156 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_call_expr/7}).
-compile({nowarn_unused_function, yeccgoto_call_expr/7}).
yeccgoto_call_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_catch_expr/7}).
-compile({nowarn_unused_function, yeccgoto_catch_expr/7}).
yeccgoto_catch_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_catch_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_cdr_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_cdr_pattern/7}).
yeccgoto_cdr_pattern(20 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(21 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(37 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(38 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(47 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(61 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(101 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(114 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cdr_pattern(123 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_define_expr/7}).
-compile({nowarn_unused_function, yeccgoto_define_expr/7}).
yeccgoto_define_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_define_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_doc_comment/7}).
-compile({nowarn_unused_function, yeccgoto_doc_comment/7}).
yeccgoto_doc_comment(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(11, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(12, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(28, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(31, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(52, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(57, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(63, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(65, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(69, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(75, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(78, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(82, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(85, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(88, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(92, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(95, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(98, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(103, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(113, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(115, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(118, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(119, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(126, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(128, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(138, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(145, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_doc_comment(158, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_e_expr/7}).
-compile({nowarn_unused_function, yeccgoto_e_expr/7}).
yeccgoto_e_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(11, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(28, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(31, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(52, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(57, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(63, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(65, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(69, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_70(70, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(75, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(78, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(82, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(85, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(88, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(92, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(95, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(98, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_99(99, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(103, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(113, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(126, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(145, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_expr(158, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_e_exprs/7}).
-compile({nowarn_unused_function, yeccgoto_e_exprs/7}).
yeccgoto_e_exprs(75, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_exprs(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_exprs(82, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_e_exprs(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_emethod/7}).
-compile({nowarn_unused_function, yeccgoto_emethod/7}).
yeccgoto_emethod(115, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emethod(118, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emethod(119 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_emethods/7}).
-compile({nowarn_unused_function, yeccgoto_emethods/7}).
yeccgoto_emethods(115, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emethods(118 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_escape_expr/7}).
-compile({nowarn_unused_function, yeccgoto_escape_expr/7}).
yeccgoto_escape_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escape_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_escript/7}).
-compile({nowarn_unused_function, yeccgoto_escript/7}).
yeccgoto_escript(108 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escript(112 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escript(143 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_escript(156 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_final_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_final_pattern/7}).
yeccgoto_final_pattern(20 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(21 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(37 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(38 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(47 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(61 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(101 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(114 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_final_pattern(123 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_finally_expr/7}).
-compile({nowarn_unused_function, yeccgoto_finally_expr/7}).
yeccgoto_finally_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_finally_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_hide_expr/7}).
-compile({nowarn_unused_function, yeccgoto_hide_expr/7}).
yeccgoto_hide_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hide_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_if_expr/7}).
-compile({nowarn_unused_function, yeccgoto_if_expr/7}).
yeccgoto_if_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_if_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_ignore_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_ignore_pattern/7}).
yeccgoto_ignore_pattern(20 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(21 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(37 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(38 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(47 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(61 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(101 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(114 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ignore_pattern(123 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_list_pattern/7}).
yeccgoto_list_pattern(20, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(21, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(35, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(37, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(38, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(47, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(56, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(61, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(101, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(114, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_pattern(123, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_bind_expr/7}).
-compile({nowarn_unused_function, yeccgoto_match_bind_expr/7}).
yeccgoto_match_bind_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match_bind_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_matcher/7}).
-compile({nowarn_unused_function, yeccgoto_matcher/7}).
yeccgoto_matcher(108 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matcher(112 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matcher(115, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matcher(117, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_133(133, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matcher(143 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_matcher(156 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_noun/7}).
-compile({nowarn_unused_function, yeccgoto_noun/7}).
yeccgoto_noun(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(20, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(21, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(35, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(37, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(38, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(47, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(50, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(56, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(61, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(101, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(114, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(123, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_noun(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_object_expr/7}).
-compile({nowarn_unused_function, yeccgoto_object_expr/7}).
yeccgoto_object_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(12 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_object_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_pattern/7}).
yeccgoto_pattern(20, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_106(106, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(21, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(35, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(37, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(38, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(47, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(56, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(61, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(101, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(114, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_137(137, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(123, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_55(55, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_patterns/7}).
-compile({nowarn_unused_function, yeccgoto_patterns/7}).
yeccgoto_patterns(47, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_patterns(123, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_send_expr/7}).
-compile({nowarn_unused_function, yeccgoto_send_expr/7}).
yeccgoto_send_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_send_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_expr/7}).
-compile({nowarn_unused_function, yeccgoto_seq_expr/7}).
yeccgoto_seq_expr(0, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_expr(28, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_29(29, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_expr(128, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_129(129, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_expr(138, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_139(139, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_slot_expr/7}).
-compile({nowarn_unused_function, yeccgoto_slot_expr/7}).
yeccgoto_slot_expr(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(11 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(31 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(63 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(65 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(75 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(85 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(88 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(92 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(103 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(113 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(126 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(128 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(138 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(145 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_slot_expr(158 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_such_that_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_such_that_pattern/7}).
yeccgoto_such_that_pattern(20 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(21 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(37 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(38 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(47 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(61 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(101 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(114 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_such_that_pattern(123 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_texts/7}).
-compile({nowarn_unused_function, yeccgoto_texts/7}).
yeccgoto_texts(18, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_texts(149 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_var_pattern/7}).
-compile({nowarn_unused_function, yeccgoto_var_pattern/7}).
yeccgoto_var_pattern(20 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(21 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(35 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(37 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(38 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(47 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(56 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(61 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(101 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(114 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_var_pattern(123 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_verb/7}).
-compile({nowarn_unused_function, yeccgoto_verb/7}).
yeccgoto_verb(33, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(34, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_verb(120, Cat, Ss, Stack, T, Ts, Tzr) ->
    yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline, yeccpars2_1_/1}).
-dialyzer({nowarn_function, yeccpars2_1_/1}).
-compile({nowarn_unused_function, yeccpars2_1_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 64).
yeccpars2_1_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function, yeccpars2_3_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 73).
yeccpars2_3_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function, yeccpars2_4_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 75).
yeccpars2_4_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_5_/1}).
-dialyzer({nowarn_function, yeccpars2_5_/1}).
-compile({nowarn_unused_function, yeccpars2_5_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 63).
yeccpars2_5_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_6_/1}).
-dialyzer({nowarn_function, yeccpars2_6_/1}).
-compile({nowarn_unused_function, yeccpars2_6_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 74).
yeccpars2_6_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_7_/1}).
-dialyzer({nowarn_function, yeccpars2_7_/1}).
-compile({nowarn_unused_function, yeccpars2_7_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 68).
yeccpars2_7_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_8_/1}).
-dialyzer({nowarn_function, yeccpars2_8_/1}).
-compile({nowarn_unused_function, yeccpars2_8_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 67).
yeccpars2_8_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function, yeccpars2_9_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 71).
yeccpars2_9_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_10_/1}).
-dialyzer({nowarn_function, yeccpars2_10_/1}).
-compile({nowarn_unused_function, yeccpars2_10_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 69).
yeccpars2_10_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_11_/1}).
-dialyzer({nowarn_function, yeccpars2_11_/1}).
-compile({nowarn_unused_function, yeccpars2_11_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 56).
yeccpars2_11_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function, yeccpars2_13_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 66).
yeccpars2_13_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_14_/1}).
-dialyzer({nowarn_function, yeccpars2_14_/1}).
-compile({nowarn_unused_function, yeccpars2_14_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 70).
yeccpars2_14_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_15_/1}).
-dialyzer({nowarn_function, yeccpars2_15_/1}).
-compile({nowarn_unused_function, yeccpars2_15_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 72).
yeccpars2_15_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_16_/1}).
-dialyzer({nowarn_function, yeccpars2_16_/1}).
-compile({nowarn_unused_function, yeccpars2_16_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 65).
yeccpars2_16_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_19_/1}).
-dialyzer({nowarn_function, yeccpars2_19_/1}).
-compile({nowarn_unused_function, yeccpars2_19_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 59).
yeccpars2_19_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function, yeccpars2_22_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 61).
yeccpars2_22_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function, yeccpars2_23_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 77).
yeccpars2_23_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_25_/1}).
-dialyzer({nowarn_function, yeccpars2_25_/1}).
-compile({nowarn_unused_function, yeccpars2_25_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 60).
yeccpars2_25_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_26_/1}).
-dialyzer({nowarn_function, yeccpars2_26_/1}).
-compile({nowarn_unused_function, yeccpars2_26_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 62).
yeccpars2_26_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_30_/1}).
-dialyzer({nowarn_function, yeccpars2_30_/1}).
-compile({nowarn_unused_function, yeccpars2_30_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 83).
yeccpars2_30_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {hide, ___2}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_39_/1}).
-dialyzer({nowarn_function, yeccpars2_39_/1}).
-compile({nowarn_unused_function, yeccpars2_39_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 101).
yeccpars2_39_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_40_/1}).
-dialyzer({nowarn_function, yeccpars2_40_/1}).
-compile({nowarn_unused_function, yeccpars2_40_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 102).
yeccpars2_40_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function, yeccpars2_43_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 103).
yeccpars2_43_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function, yeccpars2_44_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 99).
yeccpars2_44_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_45_/1}).
-dialyzer({nowarn_function, yeccpars2_45_/1}).
-compile({nowarn_unused_function, yeccpars2_45_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 100).
yeccpars2_45_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function, yeccpars2_46_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 104).
yeccpars2_46_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_48_/1}).
-dialyzer({nowarn_function, yeccpars2_48_/1}).
-compile({nowarn_unused_function, yeccpars2_48_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 108).
yeccpars2_48_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ignore_pattern
        end
        | __Stack
    ].

-compile({inline, yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function, yeccpars2_49_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 77).
yeccpars2_49_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function, yeccpars2_53_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 107).
yeccpars2_53_(__Stack0) ->
    [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {var_pattern, ___2, ___4}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_55_/1}).
-dialyzer({nowarn_function, yeccpars2_55_/1}).
-compile({nowarn_unused_function, yeccpars2_55_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 131).
yeccpars2_55_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_58_/1}).
-dialyzer({nowarn_function, yeccpars2_58_/1}).
-compile({nowarn_unused_function, yeccpars2_58_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 109).
yeccpars2_58_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {such_that_pattern, ___1, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_59_/1}).
-dialyzer({nowarn_function, yeccpars2_59_/1}).
-compile({nowarn_unused_function, yeccpars2_59_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 132).
yeccpars2_59_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_60_/1}).
-dialyzer({nowarn_function, yeccpars2_60_/1}).
-compile({nowarn_unused_function, yeccpars2_60_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 110).
yeccpars2_60_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {list_patterns, ___2}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_62_/1}).
-dialyzer({nowarn_function, yeccpars2_62_/1}).
-compile({nowarn_unused_function, yeccpars2_62_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 111).
yeccpars2_62_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {cdr_pattern, ___1, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_64_/1}).
-dialyzer({nowarn_function, yeccpars2_64_/1}).
-compile({nowarn_unused_function, yeccpars2_64_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 106).
yeccpars2_64_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {final_pattern, ___1, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_67_/1}).
-dialyzer({nowarn_function, yeccpars2_67_/1}).
-compile({nowarn_unused_function, yeccpars2_67_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 87).
yeccpars2_67_(__Stack0) ->
    [___9, ___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {finally, ___3, ___6, ___8}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_71_/1}).
-dialyzer({nowarn_function, yeccpars2_71_/1}).
-compile({nowarn_unused_function, yeccpars2_71_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 86).
yeccpars2_71_(__Stack0) ->
    [___9, ___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {catch_statement, ___3, ___6, ___8}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_72_/1}).
-dialyzer({nowarn_function, yeccpars2_72_/1}).
-compile({nowarn_unused_function, yeccpars2_72_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 81).
yeccpars2_72_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {'=~', ___1, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_74_/1}).
-dialyzer({nowarn_function, yeccpars2_74_/1}).
-compile({nowarn_unused_function, yeccpars2_74_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 78).
yeccpars2_74_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_77_/1}).
-dialyzer({nowarn_function, yeccpars2_77_/1}).
-compile({nowarn_unused_function, yeccpars2_77_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 134).
yeccpars2_77_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_79_/1}).
-dialyzer({nowarn_function, yeccpars2_79_/1}).
-compile({nowarn_unused_function, yeccpars2_79_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 135).
yeccpars2_79_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_80_/1}).
-dialyzer({nowarn_function, yeccpars2_80_/1}).
-compile({nowarn_unused_function, yeccpars2_80_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 89).
yeccpars2_80_(__Stack0) ->
    [___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {send, ___1, ___3, ___5}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_84_/1}).
-dialyzer({nowarn_function, yeccpars2_84_/1}).
-compile({nowarn_unused_function, yeccpars2_84_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 88).
yeccpars2_84_(__Stack0) ->
    [___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {call, ___1, ___3, ___5}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_94_/1}).
-dialyzer({nowarn_function, yeccpars2_94_/1}).
-compile({nowarn_unused_function, yeccpars2_94_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 84).
yeccpars2_94_(__Stack0) ->
    [___11, ___10, ___9, ___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {if_statement, ___3, ___6, ___10}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_96_/1}).
-dialyzer({nowarn_function, yeccpars2_96_/1}).
-compile({nowarn_unused_function, yeccpars2_96_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 80).
yeccpars2_96_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {':=', ___1, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_105_/1}).
-dialyzer({nowarn_function, yeccpars2_105_/1}).
-compile({nowarn_unused_function, yeccpars2_105_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 85).
yeccpars2_105_(__Stack0) ->
    [___10, ___9, ___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {escape, ___2, ___4, ___7, ___9}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_107_/1}).
-dialyzer({nowarn_function, yeccpars2_107_/1}).
-compile({nowarn_unused_function, yeccpars2_107_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 108).
yeccpars2_107_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ignore_pattern
        end
        | __Stack
    ].

-compile({inline, yeccpars2_108_/1}).
-dialyzer({nowarn_function, yeccpars2_108_/1}).
-compile({nowarn_unused_function, yeccpars2_108_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 77).
yeccpars2_108_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_109_/1}).
-dialyzer({nowarn_function, yeccpars2_109_/1}).
-compile({nowarn_unused_function, yeccpars2_109_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 97).
yeccpars2_109_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_110_/1}).
-dialyzer({nowarn_function, yeccpars2_110_/1}).
-compile({nowarn_unused_function, yeccpars2_110_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 96).
yeccpars2_110_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            ___1
        end
        | __Stack
    ].

-compile({inline, yeccpars2_111_/1}).
-dialyzer({nowarn_function, yeccpars2_111_/1}).
-compile({nowarn_unused_function, yeccpars2_111_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 92).
yeccpars2_111_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {object, "", ___2, [], ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_118_/1}).
-dialyzer({nowarn_function, yeccpars2_118_/1}).
-compile({nowarn_unused_function, yeccpars2_118_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 128).
yeccpars2_118_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_121_/1}).
-dialyzer({nowarn_function, yeccpars2_121_/1}).
-compile({nowarn_unused_function, yeccpars2_121_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 122).
yeccpars2_121_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {escript, [], nil}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_130_/1}).
-dialyzer({nowarn_function, yeccpars2_130_/1}).
-compile({nowarn_unused_function, yeccpars2_130_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 118).
yeccpars2_130_(__Stack0) ->
    [___10, ___9, ___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {emethod, "", ___2, ___4, ___7, ___9}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_131_/1}).
-dialyzer({nowarn_function, yeccpars2_131_/1}).
-compile({nowarn_unused_function, yeccpars2_131_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 119).
yeccpars2_131_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            setelement(2, ___2, ___1)
        end
        | __Stack
    ].

-compile({inline, yeccpars2_132_/1}).
-dialyzer({nowarn_function, yeccpars2_132_/1}).
-compile({nowarn_unused_function, yeccpars2_132_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 129).
yeccpars2_132_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_134_/1}).
-dialyzer({nowarn_function, yeccpars2_134_/1}).
-compile({nowarn_unused_function, yeccpars2_134_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 124).
yeccpars2_134_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {escript, ___2, nil}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_135_/1}).
-dialyzer({nowarn_function, yeccpars2_135_/1}).
-compile({nowarn_unused_function, yeccpars2_135_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 125).
yeccpars2_135_(__Stack0) ->
    [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {escript, ___2, ___3}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_136_/1}).
-dialyzer({nowarn_function, yeccpars2_136_/1}).
-compile({nowarn_unused_function, yeccpars2_136_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 123).
yeccpars2_136_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {escript, [], ___2}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_140_/1}).
-dialyzer({nowarn_function, yeccpars2_140_/1}).
-compile({nowarn_unused_function, yeccpars2_140_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 121).
yeccpars2_140_(__Stack0) ->
    [___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {match, ___2, ___4}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_141_/1}).
-dialyzer({nowarn_function, yeccpars2_141_/1}).
-compile({nowarn_unused_function, yeccpars2_141_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 114).
yeccpars2_141_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            ___2
        end
        | __Stack
    ].

-compile({inline, yeccpars2_142_/1}).
-dialyzer({nowarn_function, yeccpars2_142_/1}).
-compile({nowarn_unused_function, yeccpars2_142_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 93).
yeccpars2_142_(__Stack0) ->
    [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {object, "", ___2, ___3, ___4}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_144_/1}).
-dialyzer({nowarn_function, yeccpars2_144_/1}).
-compile({nowarn_unused_function, yeccpars2_144_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 91).
yeccpars2_144_(__Stack0) ->
    [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {object, "", underscore, ___3, ___4}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_146_/1}).
-dialyzer({nowarn_function, yeccpars2_146_/1}).
-compile({nowarn_unused_function, yeccpars2_146_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 82).
yeccpars2_146_(__Stack0) ->
    [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {def, ___2, ___4}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_148_/1}).
-dialyzer({nowarn_function, yeccpars2_148_/1}).
-compile({nowarn_unused_function, yeccpars2_148_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 137).
yeccpars2_148_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {comment, []}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_149_/1}).
-dialyzer({nowarn_function, yeccpars2_149_/1}).
-compile({nowarn_unused_function, yeccpars2_149_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 139).
yeccpars2_149_(__Stack0) ->
    [___1 | __Stack] = __Stack0,
    [
        begin
            [___1]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_150_/1}).
-dialyzer({nowarn_function, yeccpars2_150_/1}).
-compile({nowarn_unused_function, yeccpars2_150_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 140).
yeccpars2_150_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_151_/1}).
-dialyzer({nowarn_function, yeccpars2_151_/1}).
-compile({nowarn_unused_function, yeccpars2_151_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 138).
yeccpars2_151_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {comment, []}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_152_/1}).
-dialyzer({nowarn_function, yeccpars2_152_/1}).
-compile({nowarn_unused_function, yeccpars2_152_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 79).
yeccpars2_152_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            {slot, ___1}
        end
        | __Stack
    ].

-compile({inline, yeccpars2_153_/1}).
-dialyzer({nowarn_function, yeccpars2_153_/1}).
-compile({nowarn_unused_function, yeccpars2_153_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 94).
yeccpars2_153_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            setelement(2, ___2, ___1)
        end
        | __Stack
    ].

-compile({inline, yeccpars2_157_/1}).
-dialyzer({nowarn_function, yeccpars2_157_/1}).
-compile({nowarn_unused_function, yeccpars2_157_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 57).
yeccpars2_157_(__Stack0) ->
    [___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___2]
        end
        | __Stack
    ].

-compile({inline, yeccpars2_159_/1}).
-dialyzer({nowarn_function, yeccpars2_159_/1}).
-compile({nowarn_unused_function, yeccpars2_159_/1}).
-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 58).
yeccpars2_159_(__Stack0) ->
    [___3, ___2, ___1 | __Stack] = __Stack0,
    [
        begin
            [___1 | ___3]
        end
        | __Stack
    ].

-file("/Users/ramzi/code/personal/elang/src/elang_parser.yrl", 151).
