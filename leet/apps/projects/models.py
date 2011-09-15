import datetime
from django.db import models
from django.db.models.signals import post_save


class Project(models.Model):
    """
    The information about Projects.
    """
    
    # basic
    user        = models.ForeignKey('auth.User', related_name='my_projects')
    title       = models.CharField(max_length=50, null=True, blank=True)
    logo        = models.ForeignKey('photos.Photo', null=True, blank=True)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    def __unicode__(self):
        return self.title

def create_project(sender, **kwargs):
    pass
        
post_save.connect(create_project, sender=Project)