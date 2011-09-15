import uuid
import datetime
from django.db import models
from django.conf import settings
from django.contrib.auth.models import User
from pymongo.objectid import ObjectId
from django_mongodb_engine.contrib import MongoDBManager
from djangotoolbox.fields import ListField, DictField, EmbeddedModelField


class UnsafeSaveException(Exception):
    pass

class ChatParticipant(models.Model):
    """ Represents a participant in an event chat.
    	last_message_sequence - last sequence message that the member has read, -1 if never
    """
    user = models.ForeignKey(User, null=True, blank=True)
    # user uuid
    uuid = models.CharField(max_length=32, null=True, blank=True)
    session_uuid = models.CharField(max_length=32, null=True, blank=True)
    display_name = models.CharField(max_length=100, null=True, blank=True)
    # seen_seq = models.IntegerField(null=True, blank=True)
    # last_message_sequence = models.IntegerField(default=-1)
    # active = models.BooleanField(default=True)
    # seen = models.IntegerField(default=lambda: time.mktime(datetime.datetime.utcnow().timetuple()))
    # is_gone = models.BooleanField(default=False)

    def __unicode__(self):
        return "User: %s" % self.user

class Chatroom(models.Model):

    uuid = models.CharField(max_length=32)
    created = models.DateTimeField(default=datetime.datetime.utcnow)
    messages = ListField(models.CharField(max_length=10240, null=True, blank=True))
    participants = DictField(EmbeddedModelField('ChatParticipant'))
    finished = models.BooleanField(default=False)
    title = models.CharField( max_length=255 )
    objects = MongoDBManager()

    def get_absolute_url(self):
        return settings.CHAT_SERVER_DOMAIN + 'chat/room/%s' % self.title

    def save(self, *args, **kwargs):
        """ Generates a UUID on initial save. """
        if self.pk is None:
            self.uuid = "%s"% uuid.uuid4()
        else:
            raise UnsafeSaveException("Chats cannot be safely saved")
        super(Chatroom, self).save(*args, **kwargs)


    def join_chat_by_user(self, user):
        profile = user.get_profile()
        participant_info = {
            'uuid': str(profile.get_uuid()),
            'user_id': ObjectId(user.id),
            'display_name': user.get_profile().get_display_name()
        }
        
        participant = self.get_participant(participant_info['uuid'])
        if participant is not None:
            return {'uuid':participant.uuid, 'session_uuid':participant.session_uuid}
        
        participant_info.update({
                'session_uuid': str(uuid.uuid4()),
        })

        partUUID = participant_info['uuid']
        partKey = "participants.%s" % partUUID
        
        Chatroom.objects.raw_update(
            {'_id': ObjectId(self.pk)},
            {'$set': { partKey: participant_info },
             '$push': { 
                    "sessions": { 
                        "session": participant_info["session_uuid"],
                        "participant": partUUID 
        }}})

        return {'uuid':participant_info["uuid"], 'session_uuid':participant_info["session_uuid"]}
        
    def get_participant(self, uuid):
        """ Returns the ChatParticipant with `uuid` or None if not found. """
        return self.participants.get(uuid, None)