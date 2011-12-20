from django.db import models
from django.contrib.auth.models import User
from django_mongodb_engine.contrib import MongoDBManager
from pymongo.objectid import ObjectId

from globals.fields import SetField


class UrlLink(models.Model):
    url         = models.TextField()
    title       = models.CharField(max_length=100, null=True, blank=True)

    def __unicode__(self):
        return "%s %s" % (self.url, self.title)
        

class BaseGroupManager(MongoDBManager):
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

class BaseGroup(models.Model):
    members     = SetField(models.CharField(max_length=30, null=True))
    
    class Meta:
        abstract = True
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
