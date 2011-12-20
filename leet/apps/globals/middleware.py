import uuid
from django.contrib.auth.models import User

class OneTimeUpdateMiddleware(object):
    """
    Check basic database setup of Timego.
    """
    def __init__(self):
        if not User.objects.filter(username='AnonymousUser').exists():
            User.objects.create_user(username='AnonymousUser', email='anonymous@site.com', \
                                     password=uuid.uuid4())
            print 'Create AnonymousUser'
        # from message.models import Message
        # from django.contrib.comments.models import Comment
        # message = Message.objects.all()[0]        
        # user_list = set(User.objects.all().exclude(username='AnonymousUser').values_list('id', flat=True))
        # message.add_members(user_list)
        # message.save()
        # print message.members