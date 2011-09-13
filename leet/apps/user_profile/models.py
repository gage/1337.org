import uuid
from django.db import models
from django.contrib.auth.models import User
from django.db.models.signals import post_save

class UserProfile(models.Model):
    """
    The information about our users.
    """
    user = models.ForeignKey(User)
    uuid = models.CharField(max_length=40, null=True, blank=True)
    
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