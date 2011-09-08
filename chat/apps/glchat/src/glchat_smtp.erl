%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%%
%%% @end
%%% Created : 10 Jun 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_smtp).

%-behaviour(gen_smtp_server_session).

-include("glchat.hrl").

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
	handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
	handle_other/3, code_change/3, terminate/2]).

-define(RELAY, true).

-record(state, {
  options = [] :: list()
}).

-type(error_message() :: {'error', string(), #state{}}).

-spec init(Hostname :: binary(), SessionCount :: non_neg_integer(), Address :: tuple(), Options :: list()) -> {'ok', string(), #state{}} | {'stop', any(), string()}.
init(Hostname, _SessionCount, _Address, Options) ->
    Banner = [Hostname, " ESMTP glchat_smtp"],
    State = #state{options = Options},
    {ok, Banner, State}.


-spec handle_HELO(Hostname :: binary(), State :: #state{}) -> {'ok', pos_integer(), #state{}} | {'ok', #state{}} | error_message().
handle_HELO(_Hostname, State) ->
	{ok, 655360, State}.

-spec handle_EHLO(Hostname :: binary(), Extensions :: list(), State :: #state{}) -> {'ok', list(), #state{}} | error_message().
handle_EHLO(_Hostname, Extensions, State) ->
	{ok, Extensions, State}.

-spec handle_MAIL(From :: binary(), State :: #state{}) -> {'ok', #state{}} | error_message().
handle_MAIL(_From, State) ->
	{ok, State}.

-spec handle_MAIL_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_MAIL_extension(_Extension, _State) ->
	error.

-spec handle_RCPT(To :: binary(), State :: #state{}) -> {'ok', #state{}} | {'error', string(), #state{}}.
handle_RCPT(_To, State) ->
	{ok, State}.

-spec handle_RCPT_extension(Extension :: binary(), State :: #state{}) -> {'ok', #state{}} | 'error'.
handle_RCPT_extension(_Extension, _State) ->
	error.

-spec handle_DATA(From :: binary(), To :: [binary(),...], Data :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_DATA(From, [To|_], Data, State) ->
	Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),

    ?DBG({email, From}),
    case glchat_parsemail:parse_to_addr(To) of
        {ok, ChatUUID, ParticipantUUID} ->
            glchat_parsemail:parse_async(
              Data, 
              fun(Result) ->
                      case Result of
                          {ok, [_|_]=Lines} ->
                              case glchat_chat_sup:ensure_chat(ChatUUID) of
                                  none -> 
                                      ?DBG({couldnt_get_chat_for_email, To, ChatUUID});
                                  Pid ->
                                      Message = list_to_binary(string:join(Lines, " ")),
                                      ?DBG({email_message, From, Message}),
                                      gen_server:call(Pid, {message, ParticipantUUID, Message, false})
                              end;
                          Other ->
                              Filename = list_to_binary(["emails/", integer_to_list(glchat_util:unix_timestamp()), "-", From]),
                              ?DBG({unparseable_mail, From, To, Filename, Other}),
                              ok = file:write_file(Filename, Data)
                      end
              end);
        _Other ->
            ?DBG({ignoring_unknown_to_addr, To}),
            ok
    end,
	{ok, Reference, State}.

-spec handle_RSET(State :: #state{}) -> #state{}.
handle_RSET(State) ->
	State.

-spec handle_VRFY(Address :: binary(), State :: #state{}) -> {'ok', string(), #state{}} | {'error', string(), #state{}}.
handle_VRFY(_Address, State) ->
	{error, "252 VRFY disabled by policy, just send some mail", State}.

-spec handle_other(Verb :: binary(), Args :: binary(), #state{}) -> {string(), #state{}}.
handle_other(Verb, _Args, State) ->
	{["500 Error: command not recognized : '", Verb, "'"], State}.


-spec code_change(OldVsn :: any(), State :: #state{}, Extra :: any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec terminate(Reason :: any(), State :: #state{}) -> {'ok', any(), #state{}}.
terminate(Reason, State) ->
	{ok, Reason, State}.

%%% Internal Functions %%%
