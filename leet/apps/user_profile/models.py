import uuid
import datetime
from django.db import models
from django.contrib.auth.models import User
from django.db.models.signals import post_save

from djangotoolbox.fields import ListField, DictField, EmbeddedModelField

class UserProfile(models.Model):
    """
    The information about our users.
    """
    
    # basic
    user        = models.ForeignKey(User, related_name='profile')
    hacker_name = models.CharField(max_length=40, null=True, blank=True)
    avatar      = models.ForeignKey('photos.Photo', null=True, blank=True)
    uuid        = models.CharField(max_length=40, null=True, blank=True)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    # info
    edu_school  = ListField(models.CharField(max_length=24, null=True))
    edu_dep     = ListField(models.CharField(max_length=24, null=True))
    
    
    
    def get_uuid(self):
        """ Returns this user's uuid.  Creates one if not available. """
        if not self.uuid:
            self.uuid = unicode(uuid.uuid4())
            self.save()
        return self.uuid
    
    def __unicode__(self):
        return self.user.username

def create_profile(sender, **kwargs):
    if kwargs['created'] == True:        
        UserProfile.objects.get_or_create(user=kwargs['instance'], uuid=unicode(uuid.uuid4()))
        
post_save.connect(create_profile, sender=User)