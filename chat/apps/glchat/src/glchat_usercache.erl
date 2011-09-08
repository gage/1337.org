%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_usercache).

-behaviour(gen_server).

-include("glchat.hrl").

%% API
-export([start_link/1, ensure_cache/2, get_cache/1, finish_cache/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  userID,
  hungry=false,
  chats,
  friends,
  locations,
  expiryTimer=none
}).

-define(TRACE(Term), ok).
%-define(TRACE(Term), ?DBG(Term)).

-define(UN(ID), glchat_hunger:id_to_nick(ID)).

-define(EXPIRY, 10 * 60 * 1000).

%%%===================================================================
%%% API
%%%===================================================================

ensure_cache(UserID, Hungry) ->
    case gproc:lookup_pids({p, l, {user_cache, UserID}}) of
        [] ->
            case supervisor:start_child(glchat_usercache_sup, [UserID]) of
                {ok, Pid} -> 
                    gen_server:cast(Pid, {hungry, Hungry}),
                    {new, Pid};
                Other -> 
                    ?DBG({oh_noes,Other}),
                    none
            end;
        [Pid] ->
            gen_server:cast(Pid, {hungry, Hungry}),
            {existing, Pid};
        [Pid|_]=Multi ->
            ?DBG({multiple_caches, UserID, Multi}),
            {existing, Pid}
    end.

get_cache(UserID) ->
    case gproc:lookup_pids({p, l, {user_cache, UserID}}) of
        [Pid] ->
            {ok, Pid};
        _ ->
            error
    end.


finish_cache(UserID) ->
    [gen_server:cast(Cache, stop)
     || Cache <- gproc:lookup_pids({p, l, {user_cache, UserID}})].

start_link(UserID) ->
    gen_server:start_link(?MODULE, [UserID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([UserID]) ->
    gproc:reg({p, l, {user_cache, UserID}}),
    State = #state{userID=UserID, 
                   chats=gb_sets:empty(), 
                   friends=[]},
    {ok, update_expiry(State)}.

handle_call(state, _From, State) ->
    NewChats = [UUID || UUID <- gb_sets:to_list(State#state.chats),
                        glchat_chat_sup:ensure_chat(UUID) /= none],
    Reply = {NewChats, State#state.friends},
    {reply, Reply, State#state{chats=gb_sets:from_list(NewChats)}};

handle_call(hungry, _From, State) ->
    {reply, State#state.hungry, State};

handle_call({visible, Chat}, _From, State) ->
    {reply, gb_sets:is_member(Chat, State#state.chats), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------

handle_cast({hungry, none}, State) ->
    {noreply, State};
handle_cast({hungry, Hungry}, State) ->
    {noreply, State#state{hungry=Hungry}};

handle_cast(stop, State) ->
    ?TRACE({cache_finishing, State#state.userID}),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------

handle_info({init, Chats, Friends}, State) ->
    {noreply, State#state{
                chats=gb_sets:from_list(Chats), 
                friends=Friends}};

handle_info({hungry, FriendID, InfoBlob}, State) ->
    ?TRACE({hungry, FriendID, InfoBlob, State#state.userID}),
    Msg = [{<<"type">>, <<"start">>},
           {<<"data">>, {{FriendID, InfoBlob}}}],
    publish(Msg, State),
    NewFriends = [{FriendID, InfoBlob}|State#state.friends],
    {noreply, State#state{friends=NewFriends}};

handle_info({unhungry, FriendID}, State) ->
    ?TRACE({unhungry, glchat_hunger:id_to_nick(State#state.userID), glchat_hunger:id_to_nick(FriendID)}),
    Msg = [{<<"type">>, <<"stop">>},
           {<<"data">>, {{FriendID, <<"">>}}}],
    publish(Msg, State),
    NewFriends = [{U, N} || {U, N} <- State#state.friends,
                            U /= FriendID],
    {noreply, State#state{friends=NewFriends}};

handle_info({show, ChatUUID}, State) ->
    ?TRACE({showchat, ChatUUID}),
    Chat = glchat_chat_sup:ensure_chat(ChatUUID),
    true = is_pid(Chat),
    Parts = [{{<<"user_id">>, UID},
              {<<"display_name">>, Name}}
             || {UID, Name} <- gen_server:call(Chat, partlist)],
    Msg = [{<<"type">>, <<"showchat">>},
           {<<"data">>, {{<<"chat">>, ChatUUID},
                         {<<"participants">>, Parts}}}],
    publish(Msg, State),
    NewChats = gb_sets:add(ChatUUID, State#state.chats),
    {noreply, State#state{chats=NewChats}};

handle_info({hide, ChatUUID}, State) ->
    ?TRACE({hidechat, ChatUUID}),
    Msg = [{<<"type">>, <<"hidechat">>},
           {<<"data">>, {{<<"chat">>, ChatUUID}}}],
    publish(Msg, State),
    NewChats = gb_sets:delete_any(ChatUUID, State#state.chats),
    {noreply, State#state{chats=NewChats}};

handle_info({joinchat, ChatUUID}, State) ->
    NewChats = gb_sets:add(ChatUUID, State#state.chats),
    {noreply, State#state{chats=NewChats}};

handle_info({existing, ChatUUID}, State) ->
    handle_info({show, ChatUUID}, State);

handle_info(keepalive, State) ->
    {noreply, update_expiry(State)};

handle_info(expire, #state{hungry=false}=State) ->
    {stop, normal, State};
handle_info(expire, State) ->
    ?DBG({expiring, glchat_hunger:id_to_nick(State#state.userID)}),
    Msg = [{<<"type">>, <<"expire">>},
           {<<"data">>, {}}],
    publish(Msg, State),
    glchat_hunger:stop_hungry(State#state.userID),
    {noreply, State};

handle_info(Info, State) ->
    ?DBG({unexpected_info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ?DBG({dying, _Reason}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

publish(Msg, #state{userID=UserID}) ->
    gproc:send({p, l, {user_session, UserID}}, {push, <<"hungry">>, Msg}).


update_expiry(#state{expiryTimer=none}=State) ->
    {ok, TRef} = timer:send_after(?EXPIRY, expire),
    State#state{expiryTimer=TRef};
update_expiry(#state{expiryTimer=TRef}=State) ->
    catch timer:cancel(TRef),
    update_expiry(State#state{expiryTimer=none}).

