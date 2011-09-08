%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% Manage visibility of I'm Hungry chats 
%%% @end
%%% Created : 20 Jun 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_hunger).

-include("glchat.hrl").

-export([init/0, 
         start_hungry/1, stop_hungry/1, get_hungry/1,
         create_hungry_chat/2,
         join_hungry_chat/2,
         id_to_nick/1,
         test/0]).


init() ->
    ok.


-define(UN(ID), id_to_nick(ID, Conn)).
-define(UNS(IDs), [id_to_nick(U, Conn) || U <- IDs]).

-define(TRACE(Term), ok).
%-define(TRACE(Term), ?DBG(Term)).

-define(PUSH_RADIUS, 50 * 1000).

%% ----------------------------------------------------------- 
%% User hunger
%% ----------------------------------------------------------- 

get_hungry(UserID) ->
    make_hungry(UserID, none).

start_hungry(UserID) ->
    start_hungry(UserID, true).


start_hungry(UserID, MakeHungry) ->
    {_, Pid} = glchat_usercache:ensure_cache(UserID, MakeHungry),
    case MakeHungry of
        true ->
            make_hungry(UserID, MakeHungry);
        _ ->
            gen_server:call(Pid, state)
    end.
        

make_hungry(UserID, MakeHungry) ->
    Start = erlang:now(),

    Conn = glchat_mongo:get_conn(),

    {Nick, Location} = get_user_info(UserID, Conn),

    ?TRACE({get_hunger, MakeHungry, UserID, Nick}),
    ContactIDs = lists:sort([UserID | get_contact_ids(UserID, Conn)]),

    {AllFriends, HungryFriends, FriendInfo}
        = find_visible_friends(UserID, Conn, ContactIDs),

    ?TRACE({all_friends, ?UNS(AllFriends)}),
    ?TRACE({hungry_friends, ?UNS(HungryFriends)}),
    
    Chats = lists:usort(lists:flatten(
                          [find_hungry_chats(FID, Conn)
                           || FID <- [UserID|AllFriends]])),

    VisibleChats = [UUID || UUID <- Chats, 
                            lists:member(
                              chat_visible(UserID, get_participants(UUID), AllFriends), 
                              [existing, show])],

    ?TRACE({visible_chats, VisibleChats}),

    FriendIDs = [UID || {UID, _, _} <- FriendInfo],

    Updates0 = [{<<"friendIDs">>, {set, {array, FriendIDs}}}],
    Updates = case MakeHungry of
                  true -> [{<<"hungry">>, {set, true}}|Updates0];
                  _ -> Updates0
              end,
            
    ok = Conn:update("user_profiles_userprofile", 
                [{<<"user_id">>, {oid, UserID}}],
                Updates, []),

    case MakeHungry of
        true ->
            spawn(fun() -> 
                          ?TRACE({publishing_hungry_to, ?UNS(AllFriends)}),
                          glchat_apns:ensure_connection(),
                          Blob = {{<<"nickname">>, Nick}, {<<"location">>, Location}},
                          [begin
                               glchat_usercache:ensure_cache(FID, none),
                               publish(FID, {hungry, UserID, Blob}),
                               maybe_apns(FID, Nick, Location, find_friend_location(FID, FriendInfo))
                           end || FID <- AllFriends]
                  end);
        _ ->
            ok
    end,

    publish(UserID, {init, VisibleChats, HungryFriends}),

    %% Ridiculously inefficient way to do things.
    %% Maybe convert FriendInfo to a dict earlier?
    HungryFriendInfo = [{UID, {{<<"nickname">>, N}, {<<"location">>, Loc}}}
                        || {{oid, UID}, N, Loc} <- FriendInfo,
                           lists:member(UID, HungryFriends)],

    ?DBG({hungry_time, Nick, timer:now_diff(erlang:now(), Start) / 1000}),

    {VisibleChats, HungryFriendInfo}.

publish(UserID, Msg) ->
    gproc:send({p, l, {user_cache, UserID}}, Msg).


%% Awful N^2 crap.
find_friend_location(FriendID, FriendInfo) ->
    case [Loc || {{oid, UID}, _, Loc} <- FriendInfo,
                 UID =:= FriendID] of
        [] ->
            undefined;
        [Loc|_] ->
            Loc
    end.


stop_hungry(UserID) ->
    Conn = glchat_mongo:get_conn(),
    make_unhungry(UserID, Conn).

make_unhungry(UserID, Conn) ->
    ?TRACE({making_unhungry, ?UN(UserID)}),
    Conn:update("user_profiles_userprofile", 
                [{<<"user_id">>, {oid, UserID}}],
                [{<<"hungry">>, {set, false}}],
                []),

    [begin 
         glchat_usercache:ensure_cache(FID, none),
         publish(FID, {unhungry, UserID})
     end || FID <- get_friend_ids(UserID, Conn)],

    % Leave the chats *after* making them unhungry, so
    % they don't get notified of visible chats they just left
    [leave_hungry_chat(UserID, C) || C <- find_hungry_chat_uuids(UserID, Conn)],

    glchat_usercache:finish_cache(UserID),

    ok.    


get_participants(ChatUUID) ->
    case glchat_chat_sup:ensure_chat(ChatUUID) of
        none -> [];
        Chat -> lists:sort(gen_server:call(Chat, useridlist, 30000))
    end.


publish_chat(ChatUUID, Conn) ->            
    Participants = get_participants(ChatUUID),

    Settings = lists:usort(lists:flatten(
                             [publish_chat_for_participant(Participants, Part, ChatUUID, Conn) 
                              || Part <- Participants])),

    [publish(UserID, {Action, ChatUUID}) || {Action, UserID} <- Settings].


publish_chat_for_participant(ChatParticipants, UserID, ChatUUID, Conn) ->
    FriendIDs = get_mutual_friends(UserID, Conn),
    FriendSets = get_mutual_friend_sets(FriendIDs, Conn),
    publish_chat_to_friendsets(FriendSets, ChatParticipants, ChatUUID, []).

publish_chat_to_friendsets([], _, _, Acc) ->
    Acc;
publish_chat_to_friendsets([{Participant, Friends}|Rest], ChatParticipants, ChatUUID, Acc) ->
    NewAcc = case lists:member(Participant, ChatParticipants) of
                 true -> 
                     [{existing, Participant}|Acc];
                 false -> 
                     ShowHide = chat_visible(Participant, ChatParticipants, lists:sort(Friends)),
                     case ShowHide of
                         hide ->
                             case glchat_usercache:get_cache(Participant) of
                                 {ok, Cache} ->
                                     case gen_server:call(Cache, {visible, ChatUUID}) of
                                         true ->
                                             [{hide, Participant} | Acc];
                                         false ->
                                             Acc
                                     end;
                                 _ ->
                                     Acc
                             end;
                         Action ->
                             [{Action, Participant} | Acc]
                     end
             end,
    publish_chat_to_friendsets(Rest, ChatParticipants, ChatUUID, NewAcc).



%% ----------------------------------------------------------- 
%% Chat management
%% ----------------------------------------------------------- 

create_participant(UserID, ParticipantUUID, DisplayName, IsAdmin) ->
    SessionUUID = list_to_binary(glchat_util:format_uuid(glchat_util:random_uuid())),
    [{<<"uuid">>, ParticipantUUID},
     {<<"session_uuid">>, SessionUUID},
     {<<"gulu_user_id">>, {oid, UserID}},
     {<<"display_name">>, DisplayName},
     {<<"active">>, false},
     {<<"seen">>, glchat_util:unix_timestamp()},
     {<<"is_admin">>, IsAdmin},
     {<<"is_banned">>, false},
     {<<"last_message_sequence">>, -1}].

create_session_entry(Participant) ->
    [{<<"session">>, ?GV(<<"session_uuid">>, Participant)},
     {<<"participant">>, ?GV(<<"uuid">>, Participant)}].

    
create_hungry_chat(UserID, FriendID) ->
    UUID = list_to_binary(glchat_util:format_uuid(glchat_util:random_uuid())),
    Created = glchat_util:unix_timestamp(),
    Conn = glchat_mongo:get_conn(),
    
    {ParticipantUUID, DisplayName} = get_uuid_and_name(UserID, Conn),
    {FriendUUID, FriendName} = get_uuid_and_name(FriendID, Conn),
    Part1 = create_participant(UserID, ParticipantUUID, DisplayName, true),
    Part2 = create_participant(FriendID, FriendUUID, FriendName, false),

    {oid, _OID} = Conn:save(
                   "chat_chat",
                   [{<<"uuid">>, UUID}, {<<"created">>, Created}, 
                    {<<"hungry">>, true}, {<<"finished">>, false},
                    {<<"participants">>, [{ParticipantUUID, Part1}, {FriendUUID, Part2}]},
                    {<<"sessions">>, {array, [create_session_entry(Part1), create_session_entry(Part2)]}},
                    {<<"messages">>, {array, []}}, {<<"last_message_sequence">>, 0}]),
 
    glchat_chat_sup:ensure_chat(UUID),
    gproc:send({p, l, {user_cache, FriendID}}, {show, UUID}),
    gproc:send({p, l, {user_cache, UserID}}, {joinchat, UUID}),
    publish_chat(UUID, Conn),
    UUID.


join_hungry_chat(UserID, ChatUUID) ->
    Conn = glchat_mongo:get_conn(),
    ?TRACE({joining, ?UN(UserID), ChatUUID}),

    Chat = glchat_chat_sup:ensure_chat(ChatUUID),
    {ParticipantUUID, DisplayName} = get_uuid_and_name(UserID, Conn),
    Joined = case gen_server:call(Chat, {join, ParticipantUUID}, 30000) of
                 none -> 
                     Part = create_participant(UserID, ParticipantUUID, DisplayName, true),
                     Sess = create_session_entry(Part),
                     PartKey = <<"participants.", ParticipantUUID/binary>>,
                     Conn:update("chat_chat",
                                 [{<<"uuid">>, ChatUUID}],
                                 [{PartKey, {set, Part}},
                                  {<<"sessions">>, {push, Sess}}],
                                []),
                     gen_server:call(Chat, {join, ParticipantUUID}, 30000);
                 Other ->
                     Other
             end,

    case Joined of
        none -> 
            ?DBG({join_failed, ?UN(ParticipantUUID), ChatUUID});
        _ChatInfo ->
            publish_chat(ChatUUID, Conn)
    end,
    ok.


leave_hungry_chat(UserID, ChatUUID) ->
    Conn = glchat_mongo:get_conn(),
    ?DBG({leaving, ?UN(UserID), ChatUUID}),
    Chat = glchat_chat_sup:ensure_chat(ChatUUID),
    ParticipantUUID = get_uuid(UserID, Conn),

    %% Get the participant list *before* the call, because
    %% after it the chat might be closed and then we can't.
    Participants = get_participants(ChatUUID),

    case gen_server:call(Chat, {leave, ParticipantUUID}, 30000) of
        true ->
            publish_chat(ChatUUID, Conn);
        false ->
            [publish(UID, {hide, ChatUUID}) || UID <- Participants];
        _ ->
            ok
    end.
    


%% ----------------------------------------------------------- 
%% Visibility logic
%% ----------------------------------------------------------- 

chat_visible(Participant, Participants, Friends) ->
    case ordsets:is_element(Participant, Participants) of
        true -> existing;
        false ->
            Common = ordsets:intersection(Participants, Friends),
            CommonSize = ordsets:size(Common),
            TotalSize = ordsets:size(Participants),
            apply_visibility_rules(CommonSize, TotalSize)
    end.

% >= 66% to see a chat, < 50% to hide it
apply_visibility_rules(Common, Total) ->
    ShowRequisite = (Total * 2) div 3,
    case Common >= ShowRequisite andalso Common >= 2 of
        true -> show;
        false ->
            HideRequisite = Total div 2,
            case Common < HideRequisite orelse Common < 2 of
                true -> hide;
                false -> borderline
            end
    end.


%% ----------------------------------------------------------- 
%% Mongo
%% ----------------------------------------------------------- 

get_profile(UserID, Conn) ->
    get_profile(UserID, Conn, []).

get_profile(UserID, Conn, ExtraFields) ->
    Fields = [{<<"uuid">>, true} | [ {F, true} || F <- ExtraFields]],
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      Fields) of
        {ok, [_|_]=Doc} ->
            Doc;
        _Other ->
            none
    end.

get_uuid(UserID, Conn) ->
    case get_profile(UserID, Conn) of
        none -> none;
        Doc -> ?GV(<<"uuid">>, Doc)
    end.

get_uuid_and_name(UserID, Conn) ->
    case get_profile(UserID, Conn, [<<"nickname">>]) of
        none -> none;
        Doc -> {?GV(<<"uuid">>, Doc), ?GV(<<"nickname">>, Doc)}
    end.
    

get_contact_ids(UserID, Conn) ->
    {ok, Results} = Conn:find("contact_contact",
                              [{<<"user_id">>, {oid, UserID}},
                               {<<"gulu_user_id">>, {ne, null}}],
                              [{<<"gulu_user_id">>, true}],
                              0, 0),
    [UID || UID <- [?GV(<<"gulu_user_id">>, C) || C <- Results],
            UID /= {oid, UserID}].
    

get_mutual_friends(UserID, Conn) ->
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      [{<<"friendIDs">>, true}]) of
        {ok, [_|_]=Doc} ->
            case ?GV(<<"friendIDs">>, Doc) of
                {array, FriendObjs} ->
                    Friends = [UID || UID <- FriendObjs, 
                                      UID /= {oid, UserID}],
                    {ok, Docs} = Conn:find("user_profiles_userprofile",
                                           [{<<"user_id">>, {in, Friends}},
                                            {<<"hungry">>, true}],
                                           [{<<"user_id">>, true}], 0, 0),
                    HungryIDs = [?GV(<<"user_id">>, D) || D <- Docs],
                    [UID || {oid, UID} <- HungryIDs];
                undefined -> []
            end;
        _ ->
            none
    end.

get_mutual_friend_sets(UserIDs, Conn) ->
    {ok, Results} = Conn:find("user_profiles_userprofile",
                              [{<<"user_id">>, {in, [{oid, UID} || UID <- UserIDs]}}],
                              [{<<"user_id">>, true}, 
                               {<<"friendIDs">>, true}],
                              0, 0),
    [{U, [UID || {oid, UID} <- Fs]} 
     || {{oid, U}, {array, Fs}} 
            <- [{?GV(<<"user_id">>, P), 
                 ?GV(<<"friendIDs">>, P)} 
                || P <- Results]].


find_visible_friends(UserID, Conn, ContactIDs) ->
    {ok, Results} = Conn:find("user_profiles_userprofile",
                              [{<<"user_id">>, {in, ContactIDs}},
                               {<<"user_id">>, {ne, UserID}},
                               {'or', [{<<"_privacy">>, <<"public">>},
                                       {<<"_viewers.user_id">>, UserID}]}],
                              [{<<"user_id">>, true},
                               {<<"hungry">>, true},
                               {<<"nickname">>, true},
                               {<<"last_location">>, true}],
                              0, 0),
    AllFriends = lists:sort([UID || {oid, UID} <- [?GV(<<"user_id">>, R) || R <- Results]]),
    HungryFriends = [UID || 
                        {oid, UID} <- 
                            [?GV(<<"user_id">>, R) 
                             || R <- Results, ?GV(<<"hungry">>, R) == true]],
    FriendInfo = [{?GV(<<"user_id">>, R), ?GV(<<"nickname">>, R), munge_location(?GV(<<"last_location">>, R))}
                  || R <- Results],
    {AllFriends, HungryFriends, FriendInfo}.


munge_location(DBLocation) when is_list(DBLocation) ->
    Lat = ?GV(<<"latitude">>, DBLocation),
    Lng = ?GV(<<"longitude">>, DBLocation),
    case {Lat, Lng} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {_, _} -> [Lat, Lng]
    end;
munge_location(undefined) ->
    undefined;
munge_location(Other) ->
    ?DBG({ignoring_location, Other}),
    undefined.


find_hungry_chats(UserID, Conn) ->
    ParticipantUUID = get_uuid(UserID, Conn),
    {ok, Results} = Conn:find("chat_chat",
                              [{<<"sessions.participant">>, ParticipantUUID},
                               {<<"hungry">>, true}, {<<"finished">>, {ne, true}}],
                              [{<<"uuid">>, true}],
                              0, 0),
    
    [?GV(<<"uuid">>, C) || C <- Results].


find_hungry_chat_uuids(UserID, Conn) ->
    ParticipantUUID = get_uuid(UserID, Conn),
    {ok, Results} = Conn:find("chat_chat",
                              [{<<"sessions.participant">>, ParticipantUUID},
                               {<<"hungry">>, true}, {<<"finished">>, false}],
                              [{<<"uuid">>, true}],
                              0, 0),
    [?GV(<<"uuid">>, C) || C <- Results].



id_to_nick(<<>>) ->
    <<"no_id">>;
id_to_nick(UserID) ->
    id_to_nick(UserID, glchat_mongo:get_conn()).

id_to_nick(UserID, Conn) ->
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      [{<<"nickname">>, true}]) of
        {ok, [_|_]=Doc} ->
            Nickname = ?GV(<<"nickname">>, Doc),
            case Nickname of
                undefined ->
                    set_nickname(UserID, Conn);
                <<>> -> 
                    set_nickname(UserID, Conn);
                _ -> 
                    Nickname
            end;
        _ ->
            <<"???">>
    end.


get_user_info(UserID, Conn) ->
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      [{<<"nickname">>, true},
                       {<<"last_location">>, true}]) of
        {ok, [_|_]=Doc} ->
            {?GV(<<"nickname">>, Doc), munge_location(?GV(<<"last_location">>, Doc))};
        _ ->
            <<"???">>
    end.
    
set_nickname(UserID, Conn) ->
    case Conn:findOne("auth_user",
                      [{<<"_id">>, {oid, UserID}}],
                      [{<<"username">>, true}]) of
        {ok, [_|_]=Doc} ->
            Username = ?GV(<<"username">>, Doc),
            Conn:update("user_profiles_userprofile", 
                        [{<<"user_id">>, {oid, UserID}}],
                        [{<<"nickname">>, {set, Username}}],
                        []),
            Username;
        _ ->
            <<"???">>
    end.
    


get_friend_ids(UserID, Conn) ->
    case Conn:findOne("user_profiles_userprofile",
                      [{<<"user_id">>, {oid, UserID}}],
                      [{<<"friendIDs">>, true}]) of
        {ok, [_|_]=Doc} ->
            case ?GV(<<"friendIDs">>, Doc) of
                {array, FriendObjs} ->
                    [UID || {oid, UID} <- FriendObjs];
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% ----------------------------------------------------------- 
%% APNS related
%% ----------------------------------------------------------- 

decode_location(undefined) ->
    undefined;
decode_location([Lat, Lng]) ->
    {binary_to_number(Lat), binary_to_number(Lng)}.



maybe_apns(FriendID, Nick, undefined, _) ->
    send_apns(FriendID, Nick);
maybe_apns(FriendID, Nick, _, undefined) ->
    send_apns(FriendID, Nick);
maybe_apns(FriendID, Nick, Loc1, Loc2) ->
    case geo_wgs84:great_circle_distance(decode_location(Loc1), decode_location(Loc2)) of
        Distance when Distance > ?PUSH_RADIUS ->
            ?TRACE({skipping_apns, FriendID, Distance});
        Distance -> 
            ?TRACE({sending_apns, Loc1, Loc2, Distance}),
            send_apns(FriendID, Nick)
    end.

send_apns(FriendID, Nick) ->
    glchat_apns:send(FriendID, none, none, binary_to_list(Nick), " is hungry! Join them to eat?").


binary_to_number(B) ->
    list_to_number(binary_to_list(B)).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

%% ----------------------------------------------------------- 
%% Misc
%% ----------------------------------------------------------- 

test() ->
    ok = application:start(glchat),

    Conn = glchat_mongo:get_conn(),
    Conn:update("user_profiles_userprofile", 
                [{<<"_id">>, {exists, true}}], % erlmongo fail
                [{<<"hungry">>, {set, false}}], [multi]),

    Conn:remove("chat_chat", [{<<"hungry">>, true}]),

    Brend = <<"4e018cec794d400daa000034">>, % <<"c701823c-79b8-41fd-a417-3cdd5d4f95ad">>,
    Ryan = <<"4e016d46794d407a60000006">>, % <<"231e39b9-aa11-475c-920d-8f84007137da">>,
    Greg = <<"4e007680794d4005b20036b4">>, % <<"18408730-66a7-480d-98aa-d8a0707def20">>,
    Peter = <<"4e02d97a794d403daa000024">>, % <<"9cef164d-13b4-4950-a006-d3b1c680dd30">>,

    start_hungry(Brend),

    ?DBG('-----------------------------------------'),
    start_hungry(Ryan),

    ?DBG('-----------------------------------------'),
    ChatUUID = create_hungry_chat(Ryan, Brend),

    ?DBG('-----------------------------------------'),
    start_hungry(Greg),

    ?DBG('-----------------------------------------'),
    start_hungry(Peter),

    ?DBG('-----------------------------------------'),
    join_hungry_chat(Greg, ChatUUID),

    ?DBG('-----------------------------------------'),
    leave_hungry_chat(Greg, ChatUUID),

    ?DBG('-----------------------------------------'),
    stop_hungry(Ryan),

    ok.
