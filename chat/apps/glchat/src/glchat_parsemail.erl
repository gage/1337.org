%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@cogini.com>
%%% @copyright (C) 2011, Cogini
%%% @doc
%%% Parse incoming emails
%%% @end
%%% Created : 17 Jun 2011 by Brendon Hogger <brendonh@cogini.com>
%%%-------------------------------------------------------------------
-module(glchat_parsemail).

-export([parse_to_addr/1, parse_async/2, test/0]).

-include("glchat.hrl").

parse_to_addr(ToAddr) ->
    case binary:split(ToAddr, <<"@">>) of
        [SessionUUID, _] ->
            case (catch glchat_util:parse_uuid(binary_to_list(SessionUUID))) of
                UUID when is_binary(UUID) ->
                    case glchat_mongo:get_chat_by_session(SessionUUID) of
                        {ok, ChatUUID, ParticipantUUID} ->
                            {ok, ChatUUID, ParticipantUUID};
                        Other ->
                            Other
                    end;
                _Error ->
                    ?DBG({ignoring_sender, not_uuid, SessionUUID})
            end;
        _ ->
            ?DBG({ignoring_sender, wrong_format, ToAddr}),
            none
    end.


parse_async(Content, Callback) ->
    Self = self(),
    Pid = spawn(fun() -> do_parse_async(Content, Self) end),
    receive
        {Pid, Result} ->
            Callback(Result)
    end.
            

do_parse_async(Content, From) ->
    {ok, Conf} = application:get_env(glchat, mail),
    Parser = ?GV(parser, Conf),

    Port = erlang:open_port({spawn, Parser},[{packet, 4}, exit_status]),
    port_command(Port, Content),
    Result = loop(Port,[], 5000),
    From ! {self(), Result}.


loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} -> loop(Port, [NewData|Data], Timeout);
        {Port, {exit_status, 0}} -> {ok, lists:reverse(Data)};
        {Port, {exit_status, S}} -> {error, {exit_status, S}}
    after Timeout ->
            {error, timeout}
    end.
                   
                   


test() ->
    Data = <<"Received: from mail-ww0-f47.google.com (mail-ww0-f47.google.com [74.125.82.47])\r\n\tby ip-10-34-23-86.ec2.internal (Postfix) with ESMTPS id 674434E2B9\r\n\tfor <c98923c6-47af-40e5-8b5a-10f881a0dff5@gulumail.com>; Fri, 17 Jun 2011 07:23:19 +0000 (UTC)\r\nReceived: by wwk4 with SMTP id 4so2427286wwk.4\r\n        for <c98923c6-47af-40e5-8b5a-10f881a0dff5@gulumail.com>; Fri, 17 Jun 2011 00:23:17 -0700 (PDT)\r\nDKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;\r\n        d=gmail.com; s=gamma;\r\n        h=domainkey-signature:mime-version:in-reply-to:references:date\r\n         :message-id:subject:from:to:content-type;\r\n        bh=9tn8VVF7M07MiesYqHijPXwcX+VH5pAwWSLiX7aXFcM=;\r\n        b=ibF2E5f/9jkx0jEEHMPD4XZ4WNSy3CCln+BTO7vG3YT4HsjT5qjPdjJkThYLMacMtS\r\n         xzxOjGoEiMHqHNC+rnOlbOw52Hs814E9BHbO2PJy5WDWZeUqpjQy4Vw1fbfMWg/Nnbzl\r\n         hcFR2mi2Oza04QxdTWOiPQivaLxgPF5+95lCU=\r\nDomainKey-Signature: a=rsa-sha1; c=nofws;\r\n        d=gmail.com; s=gamma;\r\n        h=mime-version:in-reply-to:references:date:message-id:subject:from:to\r\n         :content-type;\r\n        b=bdW8+oxe1OEVr3TUiuYUm0fb3EAdumFb+3mOciRo9R2z0fvJnDKRrSL/kA/a/M5K9C\r\n         +U4Bza60DGVRaYAYuBJHDFDkVJH+GIbn7Uh/5yNIomGtN++tVvzwBtstTIlSynGPM0ie\r\n         rR9XxCx6ozBq3J77Ej7xmRChOo5v9dTut/LCY=\r\nMIME-Version: 1.0\r\nReceived: by 10.216.59.83 with SMTP id r61mr1785472wec.5.1308295397703; Fri,\r\n 17 Jun 2011 00:23:17 -0700 (PDT)\r\nReceived: by 10.216.17.211 with HTTP; Fri, 17 Jun 2011 00:23:17 -0700 (PDT)\r\nIn-Reply-To: <000001309c521ccc-df6934e0-e20e-4236-9b2f-a75499c5a773-000000@email.amazonses.com>\r\nReferences: <000001309c521ccc-df6934e0-e20e-4236-9b2f-a75499c5a773-000000@email.amazonses.com>\r\nDate: Fri, 17 Jun 2011 15:23:17 +0800\r\nMessage-ID: <BANLkTimKf8CC1c9cMbOMB0O4GytErcyn0w@mail.gmail.com>\r\nSubject: Re: Update: 9274c936-8903-4d52-b18f-e248fa7c5d1b\r\nFrom: Brendon Hogger <brendonh@gmail.com>\r\nTo: c98923c6-47af-40e5-8b5a-10f881a0dff5@gulumail.com\r\nContent-Type: multipart/alternative; boundary=000e0ce0d96071019a04a5e34284\r\n\r\n--000e0ce0d96071019a04a5e34284\r\nContent-Type: text/plain; charset=ISO-8859-1\r\n\r\nSome stuff goes in here.\r\n\r\nOn Fri, Jun 17, 2011 at 2:38 PM, <chat@gulumail.com> wrote:\r\n\r\n> *New Messages*\r\n>\r\n> *brendonh5*: Bump, give me a digest please.\r\n>\r\n\r\n--000e0ce0d96071019a04a5e34284\r\nContent-Type: text/html; charset=ISO-8859-1\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\nSome stuff goes in here.<br><br><div class=3D\"gmail_quote\">On Fri, Jun 17, =\r\n2011 at 2:38 PM,  <span dir=3D\"ltr\">&lt;<a href=3D\"mailto:chat@gulumail.com=\r\n\">chat@gulumail.com</a>&gt;</span> wrote:<br><blockquote class=3D\"gmail_quo=\r\nte\" style=3D\"border-left: 1px solid rgb(204, 204, 204); margin: 0pt 0pt 0pt=\r\n 0.8ex; padding-left: 1ex;\">\r\n<p><b>New Messages</b></p>\r\n\r\n<p>\r\n=20\r\n   <b>brendonh5</b>:\r\n   Bump, give me a digest please.\r\n   <br>\r\n=20\r\n</p>\r\n\r\n</blockquote></div><br>\r\n\r\n--000e0ce0d96071019a04a5e34284--">>,

    application:load(glchat),
    glchat_parsemail:parse_async(
     Data, 
     fun(Result) ->
             io:format("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~n"),
             case Result of 
                 {ok, Bits} ->
                     [io:format("~s~n", [L]) || L <- Bits];
                 Other ->
                     io:format("Error: ~p~n", [Other])
             end,
             io:format("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~n")
     end).
