-module(glchat_participants).

-include("glchat.hrl").

-export([new/1, ensure/2, get_field/3, public_list/1, find/2]).

-record(pstate, {
  parts
}).

new(PartList) ->
    Parts = dict:from_list(
              [{UUID, dict:from_list(P)} || {UUID, P} <- PartList]),
    #pstate{parts=Parts}.

find(UUID, #pstate{parts=Parts}) ->
    dict:find(UUID, Parts).
    
% fetch(UUID, #pstate{parts=Parts}) ->
%     dict:fetch(UUID, Parts).
% keys(#pstate{parts=Parts}) ->
%     dict:fetch_keys(Parts).

add(PartPL, #pstate{parts=Parts}=State) ->
    Part = dict:from_list(PartPL),
    UUID = dict:fetch(<<"uuid">>, Part),
    NewParts = dict:store(UUID, Part, Parts),
    State#pstate{parts=NewParts}.

% It is an approach to let others know this user's status.
% touch(UUID, #pstate{parts=Parts}=State) ->
%     NewParts0 = set_field(UUID, <<"is_gone">>, false, Parts, [write]),
%     NewParts = set_field(UUID, <<"seen">>, glchat_util:unix_timestamp(), NewParts0, [write]),
%     State#pstate{parts=set_field(UUID, <<"active">>, true, NewParts, [write, notify])}.

ensure(PartPL, State) ->
    UUID = ?GV(<<"uuid">>, PartPL),
    case find(UUID, State) of
        {ok, _Existing} -> 
            % touch(UUID, State);
            ?DBG({already_in_chatroom}),
            State;
        error ->
            % NewState = touch(UUID, add(PartPL, State)),
            NewState = add(PartPL, State),
            ?DBG({join_chatroom}),
            ChatUUID = get(chat_uuid),
            gproc:send({p, l, ChatUUID}, {push, ChatUUID, public_get(UUID, NewState)}),
            NewState
    end.


public_get(UUID, #pstate{parts=Parts}) ->
    Part = dict:fetch(UUID, Parts),
    [{<<"type">>, <<"participant">>} | munge_participant(Part)].

% Get chat participants list from pstate
public_list(#pstate{parts=Parts}) ->
    [list_to_tuple(munge_participant(Part)) 
     || {_UUID, Part} <- dict:to_list(Parts)].

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

%%%===================================================================
%%% Internal functions
%%%===================================================================


% broadcast_user(Part) ->
%     ChatUUID = get(chat_uuid),
%     PublicPart = [{<<"type">>, <<"participant">>} | munge_participant(Part)],
%     gproc:send({p, l, ChatUUID}, {push, ChatUUID, PublicPart}).

% set_field(UUID, Key, Val, Parts, Opts) ->
%     NewPart = set_field(Key, Val, dict:fetch(UUID, Parts), Opts),
%     dict:store(UUID, NewPart, Parts).
% 
% set_field(Key, Val, Part, Opts) ->
%     OldVal = case dict:find(Key, Part) of
%                  {ok, V} -> V;
%                  error -> undefined
%              end,
% 
%     case Val == OldVal of
%         true -> 
%             Part;
%         _ ->
%             NewPart0 = dict:store(Key, Val, Part),
% 
%             NewPart = case lists:member(write, Opts) of
%                           true -> 
%                               dict:append(changed, Key, NewPart0);
%                           false -> NewPart0
%                       end,
% 
%             case lists:member(notify, Opts) of
%                 true -> broadcast_user(NewPart);
%                 _ -> ok
%             end,
%             NewPart
%     end.


munge_participant(Participant) ->
    [{<<"uuid">>, dict:fetch(<<"session_uuid">>, Participant)},
     % {<<"display_name">>, dict:fetch(<<"display_name">>, Participant)},
     {<<"user_id">>, case dict:find(<<"user_id">>, Participant) of 
                         {ok, {oid, OID}} -> OID;
                         {ok, OID} when is_binary(OID) -> OID;
                         _ -> undefined
                     end}
     % {<<"active">>, case dict:find(<<"active">>, Participant) of {ok, Val} -> Val; error -> false end},
     % {<<"is_admin">>, false}, %% XXX BGH TODO
     % {<<"is_banned">>, false},  %% XXX BGH TODO
     % {<<"is_gone">>, case dict:find(<<"is_gone">>, Participant) of
     %                     {ok, true} -> true;
     %                     _ -> false
     %                 end}
     ].
