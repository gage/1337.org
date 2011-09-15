from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.core.management.color import no_style
from django.core.management.sql import sql_flush
from django.db import connections, models, DEFAULT_DB_ALIAS
from django.test import TestCase
from django.utils.encoding import smart_unicode


def install_contenttypes():
    """ Does a quick content types refresh.  Assumes no content types currently exist. """
    ctypes_collection = connections[DEFAULT_DB_ALIAS].get_collection('django_content_type')
    
    ContentType.objects.clear_cache()
    insert_us = []
    for app in models.get_apps():
        for klass in models.get_models(app):
            opts = klass._meta
            insert_us.append({
                'name': smart_unicode(opts.verbose_name_raw),
                'app_label': opts.app_label,
                'model': opts.object_name.lower(),
            })
    ctypes_collection.insert(insert_us)      


def create_anonymous_user():
    User.objects.create(username="AnonymousUser")


def fast_flush():
    """ All the flushing power, none of the mess. """
    connection = connections[DEFAULT_DB_ALIAS]
    sql_flush(no_style(), connection, only_django=True)
    

class MongoTestCase(TestCase):
    """ Mitigates some of the Mongo chaos. """
    
    def _fixture_setup(self):
        fast_flush()
        install_contenttypes()
        create_anonymous_user()
