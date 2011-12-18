import datetime
from django.db import models
from django.db.models.signals import post_save
from djangotoolbox.fields import SetField, ListField, EmbeddedModelField
from django_mongodb_engine.contrib import MongoDBManager
from django.utils.translation import ugettext_lazy as _
from globals.models import UrlLink

class ProjectManager(MongoDBManager):
    pass

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
    leader      = models.ForeignKey('auth.User', related_name='lead_projects', null=True, blank=True)
    category    = models.ForeignKey('projects.Category', null=True, blank=True, related_name='all_projects')
    logo        = models.ForeignKey('photos.Photo', null=True, blank=True)
    
    # info
    reference   = ListField(models.CharField(max_length=30, null=True))
    features    = ListField(models.CharField(max_length=140, null=True))
    target_user = models.CharField(max_length=50, null=True, blank=True)
    version     = models.CharField(max_length=50, null=True, blank=True)
    wanted      = models.TextField(max_length=1024, null=True, blank=True)
    git_url     = models.TextField(null=True, blank=True)
    urls        = ListField(EmbeddedModelField('globals.UrlLink'))
    
    # media
    screenshot  = ListField(models.CharField( max_length=30, null=True))
    
    # system
    score       = models.FloatField(default=0)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    objects = ProjectManager()
    
    def __unicode__(self):
        return self.title

    def save(self, *args, **kwargs):
        leader = kwargs.pop('leader', None)
        if leader is None:
            self.leader = self.user
        super(Project, self).save(*args, **kwargs)
        
    def add_url_link(self, url, title):
        ul = UrlLink.objects.create(url=url, title=title)    
        self.urls.append(ul)
        self.save()
        return ul
        
    def delete_url_link(self, id):
        for url in self.urls:
            if url.id == id:
                self.urls.remove(url)
                self.save()
                return self.urls
        

class Media(models.Model):
    
    PHOTO = 1
    VIDEO = 2
    MEDIA_TYPE = [
        (PHOTO, _("Photo")),
        (VIDEO, _("Video")),
    ]
    
    project     = models.ForeignKey('projects.Project')
    photo       = models.ForeignKey('photos.Photo', null=True, blank=True)
#    video       = models.TextField(null=True)
    type        = models.IntegerField(null=True, blank=True, choices=MEDIA_TYPE)
    is_feature  = models.BooleanField(default=False)
    created     = models.DateTimeField(default=datetime.datetime.now)
    
    def __unicode__(self):
        return "%s %s" % (self.project.title, self.type)
    
    def is_photo(self):
        if self.type == PHOTO and self.photo:
            return True
        else:
            return False
    
    def is_video(self):
        if self.type == VIDEO and self.video:
            return True
        else:
            return False

   
class Category(models.Model):
    
    name        = models.CharField(max_length=50)
    parent      = models.ForeignKey('projects.Category', null=True, blank=True)