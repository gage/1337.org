import datetime
from django.db import models
from django.db.models.signals import post_save
from djangotoolbox.fields import SetField, ListField


class Project(models.Model):
    """
    The information about Projects.
    """
    
    # basic
    user        = models.ForeignKey('auth.User', related_name='my_projects')
    title       = models.CharField(max_length=50)
    description = models.TextField(max_length=1024)
    members     = SetField(models.CharField( max_length=30, null=True))
    version     = models.CharField(max_length=50, null=True, blank=True)
    leader      = models.ForeignKey('auth.User', related_name='lead_projects')
    category    = models.ForeignKey('projects.Category', null=True, blank=True)
    logo        = models.ForeignKey('photos.Photo', null=True, blank=True)
    
    # info
    reference   = ListField(models.CharField( max_length=30, null=True))
    features    = ListField(models.CharField( max_length=140, null=True))
    screenshot  = ListField(models.CharField( max_length=30, null=True))
    target_user = models.CharField( max_length=50, null=True, blank=True)
    version     = models.CharField(max_length=50, null=True, blank=True)
    wanted      = models.TextField(max_length=1024, null=True, blank=True)
    
    git_url     = models.TextField(null=True, blank=True)
    is_tech     = models.BooleanField(default=False)
    
    
    # system
    score       = models.FloatField(default=0)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    
    def __unicode__(self):
        return self.title

def create_project(sender, **kwargs):
    pass
        
post_save.connect(create_project, sender=Project)