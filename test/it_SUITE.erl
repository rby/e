-module(it_SUITE).

-behaviour(ct_suite).

-export([all/0, compile_all/1]).
-include_lib("common_test/include/ct.hrl").

-define(ERROR, ?HI_VERBOSITY).

all() -> [compile_all].

compile_all(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, Files} = file:list_dir(DataDir),
    EFiles = [F || F <- Files, string:rstr(F, ".e") > 0],
    lists:foreach(
        fun(File) ->
            FullName = filename:join(DataDir, File),
            OutName = filename:join(DataDir, filename:basename(File, ".e")) ++ ".out",

            {ok, IO} = file:open(FullName, [read]),
            {ok, Out} = file:read_file(OutName),
            Out2 = unicode:characters_to_list(Out, utf8),
            Out3 =
                case lists:last(Out2) of
                    $. -> Out2;
                    _ -> Out2 ++ "."
                end,

            {ok, Tokens, _EndLine} = erl_scan:string(Out3),
            {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
            {value, Expected, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),

            %% how do I load the term in .out file?

            {ok, Parsed} = parser:parse(IO),
            (Parsed == Expected) orelse ct:fail({{exepected, Expected}, {got, Parsed}})
        end,
        EFiles
    ),
    ok.
