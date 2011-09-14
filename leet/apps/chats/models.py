import uuid
import datetime
from django.db import models
from django.conf import settings
from djangotoolbox.fields import ListField, DictField, EmbeddedModelField


class UnsafeSaveException(Exception):
    pass

class Chatroom(models.Model):

    uuid = models.CharField(max_length=32)
    created = models.DateTimeField(default=datetime.datetime.utcnow)
    messages = ListField(models.CharField(max_length=10240, null=True, blank=True))
    finished = models.BooleanField(default=False)
    title = models.CharField( max_length=255 )
    #objects = ChatroomManager()

    def get_absolute_url(self):
        return settings.CHAT_SERVER_DOMAIN + 'chat/room/%s' % self.title

    def save(self, *args, **kwargs):
        """ Generates a UUID on initial save. """
        if self.pk is None:
            self.uuid = "%s"% uuid.uuid4()
        else:
            raise UnsafeSaveException("Chats cannot be safely saved")
        super(Chatroom, self).save(*args, **kwargs)
