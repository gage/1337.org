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
    user            = models.ForeignKey('auth.User', related_name='my_profile')
    hacker_name     = models.CharField(max_length=40, null=True, blank=True)
    avatar          = models.ForeignKey('photos.Photo', null=True, blank=True)
    created         = models.DateTimeField(default=datetime.datetime.now)
    
    # info
    introduction    = models.TextField(max_length=1024, null=True, blank=True)
    edu             = ListField(models.CharField(max_length=30, null=True))
    experience      = ListField(models.CharField(max_length=30, null=True))
    score           = models.FloatField(default=0)
    skill           = models.ForeignKey('projects.Project', related_name='related_users', null=True, blank=True)
    
    def get_display_name(self):
        """ Returns user's hacker_name or username. """
        if self.hacker_name:
            return self.hacker_name
        else:
            return self.user.username
    
    def __unicode__(self):
        return self.user.username

def create_profile(sender, **kwargs):
    if kwargs['created'] == True:        
        UserProfile.objects.get_or_create(user=kwargs['instance'])
        
post_save.connect(create_profile, sender=User)


class School(models.Model):
    
    name = models.CharField(max_length=50)
    
    def __unicode__(self):
        return self.name
    
class Department(models.Model):
    
    name = models.CharField(max_length=50)
    
    def __unicode__(self):
        return self.name

class Edu(models.Model):
    
    school = models.ForeignKey('user_profile.School')
    dep    = models.ForeignKey('user_profile.Department')
    
    def __unicode__(self):
        return '%s %s' % (self.school, self.dep)

class Company(models.Model):
    
    name = models.CharField(max_length=50)
    
    def __unicode__(self):
        return self.name
    
class Experience(models.Model):
    
    company   = models.ForeignKey('user_profile.Company')
    job_title = models.CharField(max_length=50)

    def __unicode__(self):
        return '%s %s' % (self.company, self.job_title)