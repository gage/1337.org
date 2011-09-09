-module(glchat_chat).

-behaviour(gen_server).

-include("glchat.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% -record(state, {
%   uuid, 
%   messages,
%   participants,
%   sequence,
%   hungry,
%   title,
%   url
% }).

% Config
-define(IDLE_CHECK_TIME, 10000).

% Initial message buffer size
-define(BUFFER_SIZE, 50).


% Module shorthands
-define(PARTS, glchat_participants).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Code) ->
    gen_server:start_link(?MODULE, [Code], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([_Code]) ->
    % First we go to mongodb to query chat objects.
    implement_this.
    % case glchat_mongo:get_chat(Code) of
    %     {ok, _Info} ->
    %         gproc:reg({n, l, {chat, Code}}),
    % 
    %         {ok, Conf} = application:get_env(chat),
    %         % TODO
    %         {ok, Conf};
    % 
    %     {error, Error} ->
    %         ?DBG({chat_start_failed, Code, Error}),
    %         {stop, Error}
    % end.

% This is just an example
handle_call({join, _ParticipantUUID}, _From, State) ->
    % TODO
    {reply, State};
    
handle_call(Request, _From, State) ->
    ?DBG({unexpected_call, Request}),
    {reply, ok, State}.
    
handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    % Unexpected Input
    ?DBG({unexpected_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    % ?PARTS:commit(State#state.participants),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Participant-only method handlers
%%%===================================================================

% TODO

%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO