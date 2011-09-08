-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(T), lager:info("~p", [T])).

-record(user, {
  participantUUID,
  userID
}).

-record(chat_chat, {
  docid,
  uuid,
  created,
  participants=[],
  messages=[]
}).

-record(participant, {
  docid,
  gulu_user,
  gulu_contact,
  uuid,
  session_uuid, 
  display_name,
  is_admin,
  is_banned,
  last_message_sequence
}).
