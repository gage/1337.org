%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% Participant list management
%%% @end
%%% Created : 13 Jun 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_participants).

-include("glchat.hrl").

-export([new/1,
         find/2, fetch/2, keys/1, get_field/3,
         add/2, ensure/2, remove/2,
         touch/2, untouch/2, seq/3,
         public_get/2, public_list/1, 
         hungry_list/1, userid_list/1,
         set_unseen_to_inactive/2,
         pushnotify/4,
         dispatch_idle/5,
         commit/1, commit/2
        ]).

-record(pstate, {
  parts
}).

new(PartList) ->
    Parts = dict:from_list(
              [{UUID, dict:from_list(P)} || {UUID, P} <- PartList]),
    #pstate{parts=Parts}.


find(UUID, #pstate{parts=Parts}) ->
    dict:find(UUID, Parts).

fetch(UUID, #pstate{parts=Parts}) ->
    dict:fetch(UUID, Parts).


keys(#pstate{parts=Parts}) ->
    dict:fetch_keys(Parts).


add(PartPL, #pstate{parts=Parts}=State) ->
    Part = dict:from_list(PartPL),
    UUID = dict:fetch(<<"uuid">>, Part),
    NewParts = dict:store(UUID, Part, Parts),
    State#pstate{parts=NewParts}.


ensure(PartPL, State) ->
    UUID = ?GV(<<"uuid">>, PartPL),
    case find(UUID, State) of
        {ok, _Existing} -> 
            touch(UUID, State);
        error ->
            NewState = touch(UUID, add(PartPL, State)),
            ChatUUID = get(chat_uuid),
            gproc:send({p, l, ChatUUID}, {push, ChatUUID, public_get(UUID, NewState)}),
            NewState
    end.


remove(PartPL, #pstate{parts=Parts}=State) ->
    UUID = dict:fetch(<<"uuid">>, PartPL),
    NewPartsA = set_field(UUID, <<"seen">>, glchat_util:unix_timestamp(), Parts, [write]),
    NewPartsB = set_field(UUID, <<"active">>, false, NewPartsA, [write]),
    NewParts = set_field(UUID, <<"is_gone">>, true, NewPartsB, [write, notify]),
    State#pstate{parts=NewParts}.


touch(UUID, #pstate{parts=Parts}=State) ->
    NewParts0 = set_field(UUID, <<"is_gone">>, false, Parts, [write]),
    NewParts = set_field(UUID, <<"seen">>, glchat_util:unix_timestamp(), NewParts0, [write]),
    State#pstate{parts=set_field(UUID, <<"active">>, true, NewParts, [write, notify])}.


untouch(UUID, #pstate{parts=Parts}=State) ->
    NewParts = set_field(UUID, <<"seen">>, glchat_util:unix_timestamp(), Parts, [write]),
    State#pstate{parts=set_field(UUID, <<"active">>, false, NewParts, [write, notify])}.


seq(UUID, Seq, #pstate{parts=Parts}=State) ->
    State#pstate{parts=set_field(UUID, <<"seen_seq">>, Seq, Parts, [write])}.



public_get(UUID, #pstate{parts=Parts}) ->
    Part = dict:fetch(UUID, Parts),
    [{<<"type">>, <<"participant">>} | munge_participant(Part)].

public_list(#pstate{parts=Parts}) ->
    [list_to_tuple(munge_participant(Part)) 
     || {_UUID, Part} <- dict:to_list(Parts)].

hungry_list(#pstate{parts=Parts}) ->
    [{UserID, Name} || 
        {{ok, {oid, UserID}}, Name} <-
            [{dict:find(<<"gulu_user_id">>, Part),
              dict:fetch(<<"display_name">>, Part)}
             || {_UUID, Part} <- dict:to_list(Parts), 
                dict:find(<<"is_gone">>, Part) /= {ok, true}]].

userid_list(#pstate{parts=Parts}) ->
    [UserID 
     || {ok, {oid, UserID}} 
            <- [dict:find(<<"gulu_user_id">>, Part) 
                || {_UUID, Part} <- dict:to_list(Parts), 
                   dict:find(<<"is_gone">>, Part) /= {ok, true}]].


get_field(UUID, Key, #pstate{parts=Parts}) ->
    get_field(UUID, Key, Parts);

get_field(UUID, Key, Parts) ->
    get_field(UUID, Key, Parts, undefined).

get_field(UUID, Key, Parts, Default) ->
    Part = dict:fetch(UUID, Parts),
    case dict:find(Key, Part) of
        {ok, Val} -> Val;
        error -> Default
    end.


set_unseen_to_inactive(Delta, #pstate{parts=Parts}=State) ->
    ModFun = fun(Part) -> 
                     UUID = dict:fetch(<<"uuid">>, Part),
                     Active = case gproc:lookup_pids({p, l, {participant_session, UUID}}) of
                                  [] -> false;
                                  _ -> true
                              end,
                     set_field(<<"active">>, Active, Part, [write, notify])
             end,
    NewParts = map_part_change(Delta, Parts, ModFun),
    State#pstate{parts=NewParts}.


dispatch_idle(_Delta, [], _Subject, _Extra, State) -> 
    State;
dispatch_idle(Delta, [Latest|_]=Messages, Subject, Extra, #pstate{parts=Parts}=State) ->
    LatestSeq = ?GV(<<"sequence">>, Latest),
    ModFun = fun(Part) -> 
                     case dict:find(<<"last_message_sequence">>, Part) of
                         {ok, X} when X >= LatestSeq -> Part;
                         {ok, X} when is_number(X) -> 
                             case determine_part_email(Part) of
                                 <<>> ->
                                     Part;
                                 null -> 
                                     Part;
                                 Email -> 
                                     LastSeq = round(X),
                                     DigestMessages = filter_messages(Messages, LastSeq),
                                     case DigestMessages of
                                         [] ->
                                             Part;
                                         _ ->
                                             SessionUUID = dict:fetch(<<"session_uuid">>, Part),
                                             SHA = string:to_lower(sha1:hexstring(binary_to_list(SessionUUID))),
                                             Content = glchat_email:digest_email(DigestMessages, Parts, 
                                                                                 [{sha, SHA}|Extra]),
                                             ReplyTo = <<SessionUUID/binary, "@gulumail.com">>,
                                             catch glchat_email:send_mail("chat@gulumail.com", Email,
                                                                          [{<<"Reply-To">>, ReplyTo}],
                                                                          Subject, Content),
                                             set_field(<<"last_message_sequence">>, LatestSeq, Part, [write])
                                     end
                             end;
                         Other ->
                             ?DBG({no_sequence, Other, Part})
                     end
             end,
    NewParts = map_part_change(Delta, Parts, ModFun),
    State#pstate{parts=NewParts}.


pushnotify(UUID, Content, Title, #pstate{parts=Parts}=State) ->
    ChatUUID = get(chat_uuid),
    Nick = unicode:characters_to_list(get_field(UUID, <<"display_name">>, Parts), utf8),
    SenderID = get_field(UUID, <<"gulu_user_id">>, Parts),
    StrContent = case unicode:characters_to_list(Content, utf8) of
                     Chars when length(Chars) > 128 ->
                         string:substr(Chars, 1, 125) ++ "...";
                     ShortMsg ->
                         ShortMsg
                 end,
    StrTitle = case Title of
                   none -> none;
                   B when is_binary(B) -> unicode:characters_to_list(Title, utf8)
               end,
    glchat_apns:ensure_connection(),
    [glchat_apns:send(UserID, ChatUUID, StrTitle, Nick, ": " ++ StrContent)
     || UserID <- userid_list(State),
        {oid, UserID} /= SenderID],
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================

broadcast_user(Part) ->
    ChatUUID = get(chat_uuid),
    PublicPart = [{<<"type">>, <<"participant">>} | munge_participant(Part)],
    gproc:send({p, l, ChatUUID}, {push, ChatUUID, PublicPart}).


set_field(UUID, Key, Val, Parts, Opts) ->
    NewPart = set_field(Key, Val, dict:fetch(UUID, Parts), Opts),
    dict:store(UUID, NewPart, Parts).


set_field(Key, Val, Part, Opts) ->
    OldVal = case dict:find(Key, Part) of
                 {ok, V} -> V;
                 error -> undefined
             end,

    case Val == OldVal of
        true -> 
            Part;
        _ ->
            NewPart0 = dict:store(Key, Val, Part),

            NewPart = case lists:member(write, Opts) of
                          true -> 
                              dict:append(changed, Key, NewPart0);
                          false -> NewPart0
                      end,

            case lists:member(notify, Opts) of
                true -> broadcast_user(NewPart);
                _ -> ok
            end,
            NewPart
    end.


commit(#pstate{parts=Parts}=State) ->
    commit(#pstate{parts=Parts}=State, []).

commit(#pstate{parts=Parts}=State, ExtraChanges) ->
    {NewParts, Changes} = 
        dict:fold(
          fun(UUID, Part, {NewParts, Changes}) ->
                  case dict:find(changed, Part) of
                      {ok, [_|_]=Fields} ->
                          NewPart = dict:store(changed, [], Part),
                          Sets = [{<<"participants.", UUID/binary, $., F/binary>>,
                                   {set, dict:fetch(F, Part)}}
                                  || F <- Fields],
                          {dict:store(UUID, NewPart, NewParts), [Sets|Changes]};
                  _ ->
                          {NewParts, Changes}
                  end
          end,
          {Parts, ExtraChanges}, Parts),

    case lists:flatten(Changes) of
        [] -> ok;
        FlatChanges ->
            ChatUUID = get(chat_uuid),
            glchat_mongo:commit_chat_changes(ChatUUID, FlatChanges)
    end,

    State#pstate{parts=NewParts}.
                      

map_part_change(Delta, Parts, ModFun) ->
    Now = glchat_util:unix_timestamp(),
    Limit = Now - Delta,
    dict:map(
      fun(_UUID, Part) ->
              case dict:find(<<"seen">>, Part) of
                  {ok, X} when is_number(X) andalso X =< Limit ->
                      ModFun(Part);
                  _ -> 
                      Part
              end
      end, Parts).


munge_participant(Participant) ->
    [{<<"uuid">>, dict:fetch(<<"session_uuid">>, Participant)},
     {<<"display_name">>, dict:fetch(<<"display_name">>, Participant)},
     {<<"user_id">>, case dict:find(<<"gulu_user_id">>, Participant) of 
                         {ok, {oid, OID}} -> OID;
                         {ok, OID} when is_binary(OID) -> OID;
                         _ -> undefined
                     end},
     {<<"active">>, case dict:find(<<"active">>, Participant) of {ok, Val} -> Val; error -> false end},
     {<<"is_admin">>, false}, %% XXX BGH TODO
     {<<"is_banned">>, false},  %% XXX BGH TODO
     {<<"is_gone">>, case dict:find(<<"is_gone">>, Participant) of
                         {ok, true} -> true;
                         _ -> false
                     end}
     ].


filter_messages(Messages, SinceSeq) ->
    [M || M <- Messages,
          ?GV(<<"sequence">>, M) > SinceSeq,
          ?GV(<<"type">>, M) /= <<"broadcast">>].


determine_part_email(Part) ->
    case dict:find(<<"gulu_contact_id">>, Part) of
        {ok, ID} ->
            glchat_mongo:get_contact_email(ID);
        _ ->
            case dict:find(<<"gulu_user_id">>, Part) of
                {ok, ID} ->
                    glchat_mongo:get_user_email(ID);
                _ ->
                    none
            end
    end.
