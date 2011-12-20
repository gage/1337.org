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
        