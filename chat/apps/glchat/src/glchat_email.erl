%%%-------------------------------------------------------------------
%%% @author Ubuntu <gulu@gulumail.gulu.com>
%%% @copyright (C) 2011, Ubuntu
%%% @doc
%%% Email templating
%%% @end
%%% Created : 15 Jun 2011 by Ubuntu <gulu@gulumail.gulu.com>
%%%-------------------------------------------------------------------
-module(glchat_email).

-export([prepare/0,
         digest_email/3,
         send_mail/5]).

-include("glchat.hrl").

prepare() ->
    erlydtl:compile(get_template("message_digest.html"), email_message_digest).

digest_email(Messages, Parts, Extra) ->
    Munged = [munge_email_message(M, Parts) || M <- Messages],
    {ok, Content} = email_message_digest:render([{messages, Munged}, {notgiven, undefined}|Extra]),
    list_to_binary(Content).


munge_email_message(Message, Parts) ->
    SenderUUID = ?GV(<<"sender">>, Message),
    [{sender, glchat_participants:get_field(SenderUUID, <<"display_name">>, Parts)},
     {content, ?GV(<<"content">>, Message)}].


get_template(Name) ->
    {ok, Conf} = application:get_env(http),
    BasePath = ?GV(templates, Conf),
    filename:absname(filename:join(BasePath,Name)).


send_mail(From, To, ExtraHeaders, Subject, Content) ->
    PrettyFrom = "Gulu <" ++ From ++ ">",
    Msg = mimemail:encode(
            {"text", "html", 
             [{<<"From">>, PrettyFrom},
              {<<"To">>, To},
              {<<"Subject">>, Subject}
              | ExtraHeaders],
             [], Content}),

    {ok, Conf} = application:get_env(mail),
    Relay = ?GV(relay, Conf),

    gen_smtp_client:send({From, [To], Msg}, [{relay, Relay}], 
                         fun(Result) -> log_mail_result(To, Result) end).


log_mail_result(To, {ok, Receipt}) ->
    ?DBG({mail_sent, To, Receipt});
log_mail_result(To, {error, Type, Message}) ->
    ?DBG({mail_error, To, Type, Message});
log_mail_result(To, {error, Reason}) ->
    ?DBG({mail_error, To, Reason}).


                     
