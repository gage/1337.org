import datetime
from django.db import models
from django.contrib.auth.models import User
from globals.models import BaseGroupManager, BaseGroup

class TeamManager(BaseGroupManager):
    pass
       
class Team(BaseGroup):
    """
    Team for projects.
    """
    
    name        = models.CharField(max_length=40)
    # Inherit from BaseGroup
    # members     = SetField(models.CharField(max_length=30, null=True))
    created     = models.DateTimeField(default=datetime.datetime.now)
    objects     = TeamManager()

    def __unicode__(self):
        return self.name