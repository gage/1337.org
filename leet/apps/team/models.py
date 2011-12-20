import datetime
from django.db import models
from django.contrib.auth.models import User
from globals.fields import SetField
from django_mongodb_engine.contrib import MongoDBManager
from pymongo.objectid import ObjectId

class TeamManager(MongoDBManager):
    def add_member(self, object_pk, user_pk):
           self.raw_update(
               {
                   '_id': ObjectId(object_pk),
                   'members': {'$ne': user_pk},
               },
               {
                   '$addToSet': {'members': user_pk},
               },
           )
           
    def remove_member(self, object_pk, user_pk):
        self.raw_update(
            {
                '_id': ObjectId(object_pk),
                'members': user_pk,
            },
            {
                '$pull': {'members': user_pk},
            },
        )
       
class Team(models.Model):
    """
    Team for projects.
    """
    
    name        = models.CharField(max_length=40)
    members     = SetField(models.CharField(max_length=30, null=True))
    # avatar      = models.ForeignKey('photos.Photo', null=True, blank=True)
    created     = models.DateTimeField(default=datetime.datetime.now)
    objects     = TeamManager()

    def __unicode__(self):
        return self.name
        
    def get_members(self):
        return User.objects.filter(id__in=self.members)
    
    def add_members(self, member_pk_list=[]):
        for member_pk in member_pk_list:
            self.__class__.objects.add_member(self.pk, member_pk)
            self.members.add(member_pk)
            
    def remove_members(self, member_pk_list=[]):
        for member_pk in member_pk_list:
            self.__class__.objects.remove_member(self.pk, member_pk)
            if member_pk in self.members:
                self.members.remove(member_pk)