-module(glchat_util).

-include("glchat.hrl").

-compile(export_all).

%%====================================================================
%% UUIDs
%%====================================================================

parse_uuid(S) when is_list(S) andalso length(S) == 36 ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                                [TL, TM, THV, CSR, CSL, N])).

random_uuid() ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    <<(random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32,
     (random:uniform(4294967296) - 1):32>>.


%%====================================================================
%% Datetimes
%%====================================================================

datetime_now_diff(DT) ->
    datetime_diff(DT, calendar:universal_time()).

datetime_diff(DT2, DT1) ->
    S2 = calendar:datetime_to_gregorian_seconds(DT2),
    S1 = calendar:datetime_to_gregorian_seconds(DT1),
    S2 - S1.


datetime_now_delta(Secs) ->
    datetime_delta(calendar:universal_time(), Secs).

datetime_delta(DT, Secs) ->
    S = calendar:datetime_to_gregorian_seconds(DT),
    calendar:gregorian_seconds_to_datetime(S + Secs).

unix_timestamp() ->
    unix_timestamp(calendar:universal_time()).

unix_timestamp(DT) ->
    %LocalEpoch = calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}}),
    Epoch = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(Epoch).

%%====================================================================
%% Argument parsing
%%====================================================================

% I think this function will be used in our chat.

parse_args([], [], Values, _Args) ->
    {ok, lists:reverse(Values)};
parse_args([], Errors, _Values, _Args) ->
    {error, lists:reverse(Errors)};
parse_args([{PostKey, Type, Required}|Rest], Errors, Values, Args) ->
    {NewVals, NewErrors} = case {Required, (catch fetch_arg(Type, PostKey, Args))} of
                               {true, undefined} ->
                                   {Values, [{{error, missing}, {key, PostKey}}|Errors]};
                               {_, {ok, Val}} ->
                                   {[Val|Values], Errors};
                               {_, _Error} ->
                                   {Values, [{{error, invalid}, {key, PostKey}}|Errors]}
                           end,
    parse_args(Rest, NewErrors, NewVals, Args).


get_bin(Key, Args) ->
    case ?GV(Key, Args) of
        undefined -> undefined;
        StrBin when is_list(StrBin) ->
            list_to_binary(StrBin);
        Bin when is_binary(Bin) ->
            Bin
    end.    

fetch_arg(uuid, Key, Args) ->
    {ok, get_bin(Key, Args)};
    
fetch_arg(bin, Key, Args) ->
    {ok, get_bin(Key, Args)};

fetch_arg(int, Key, Args) ->
    Int = case ?GV(Key, Args) of
        undefined -> undefined;
        I when is_integer(I) -> I;
        List when is_list(List) -> list_to_integer(List);
        Bin when is_binary(Bin) -> list_to_integer(binary_to_list(Bin))
    end,
    {ok, Int};

fetch_arg(json, Key, Args) ->
    case ?GV(Key, Args) of
        undefined -> undefined;
        Bin ->
            JSON = jsonerl:decode(Bin),
            {ok, tuple_to_list(JSON)}
    end;

fetch_arg(jsonlist, Key, Args) ->
    case ?GV(Key, Args) of
        undefined -> undefined;
        Bin ->
            JSON = jsonerl:decode(Bin),
            true = is_list(JSON),
            {ok, JSON}
    end;

% fetch_arg(chat, Key, Args) ->
%     Code = get_bin(Key, Args),
%     case Code of 
%         undefined -> undefined;
%         <<>> ->
%             {error, invalid};
%         _ ->
%             case glchat_chat_sup:ensure_chat(Code) of
%                 none -> {error, invalid};
%                 Pid -> {ok, {Code, Pid}}
%             end
%     end;

fetch_arg(list, Key, Args) ->
    case ?GV(Key, Args) of
        undefined -> undefined;
        List ->
            true = is_list(List),
            {ok, List}
    end;


fetch_arg({list, raw}, KeyName, Args) ->
    Key = <<KeyName/binary, "[]">>,
    {ok, [V || {K, V} <- Args, K == Key]};

fetch_arg({list, Converter}, KeyName, Args) ->
    Key = KeyName ++ "[]",
    {ok, [Converter(V) || {K, V} <- Args, K == Key]}.


%%====================================================================
%% Misc
%%====================================================================

% find_object([], _Key, _Value) ->
%     {error, not_found};
% find_object([Obj|Rest], Key, Value) ->
%     case ?GV(Key, Obj) of
%         Value -> {ok, Obj};
%         _ -> find_object(Rest, Key, Value)
%     end.
             

    
