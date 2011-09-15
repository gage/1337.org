import datetime
from django.db import models
from django.db.models.signals import post_save


class Thread(models.Model):
    
    # basic
    user        = models.ForeignKey('auth.User', related_name='my_threads')
    title       = models.CharField(max_length=50, null=True, blank=True)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    def __unicode__(self):
        return self.title

def create_thread(sender, **kwargs):
    pass
        
post_save.connect(create_thread, sender=Thread)