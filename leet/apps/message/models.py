import datetime
from django.db import models
from django.contrib.auth.models import User
from globals.models import BaseGroupManager, BaseGroup

class MessageManager(BaseGroupManager):
    pass
       
class Message(BaseGroup):
    """
    Message for teams.
    In fact, this is a chatroom.
    """
    # Inherit from BaseGroup
    # members     = SetField(models.CharField(max_length=30, null=True))
    created     = models.DateTimeField(default=datetime.datetime.now)
    is_pm       = models.BooleanField(default=False)
    objects     = MessageManager()

    def __unicode__(self):
        cMembers = self.get_members().count()
        member_names = self.get_members().values_list('username', flat=True)[:3]
        rcMembers = cMembers - len(member_names)
        rtn = ', '.join(member_names)
        if rcMembers != 0:
            rtn += ' and other %s users.'% rcMembers
        return rtn
