import datetime
import urllib
import uuid, os

from django.core.files import File
from django.core.files.storage import DefaultStorage
from django.db import models
from django.utils.translation import ugettext_lazy as _
from django.conf import settings
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes.generic import GenericForeignKey
from django.contrib.auth.models import User
from django_mongodb_engine.contrib import MongoDBManager
from djangotoolbox.fields import ListField, SetField
from django.db.models.signals import post_save, post_delete

from imagekit.models import ImageModel
from imagekit.lib import Image

from django.dispatch import receiver

class PhotoManager(models.Manager):
    pass

class BasePhoto(ImageModel):
    """ Abstract photo base class """
    
    content_type    = models.ForeignKey(ContentType, null=True, blank=True)
    object_id       = models.TextField(null=True, blank=True)
    content_object  = GenericForeignKey()
    
    image           = models.ImageField(upload_to='photos')
    title           = models.CharField(max_length=255, null=True, blank=True)
    description     = models.TextField(null=True, blank=True)
    user            = models.ForeignKey(User, null=True, blank=True)
    created         = models.DateTimeField(default=datetime.datetime.now)
    
    def __unicode__(self):
        return "%s %s" % (self.pk, self.title)
    
    class Meta:
        abstract = True
        get_latest_by = 'created'
        verbose_name = _("photo")
        verbose_name_plural = _("photos")
        
    class IKOptions:
        spec_module = "photos.imagespecs"
        save_count_as = 'view_count'
        cache_dir = 'cache'
        cache_filename_format = "%(specname)s/%(filename)s.%(extension)s"
        storage = DefaultStorage()

    def attach(self, obj):
        """ Attaches this photo to obj, updates content_type and object_id """
        
        self.content_type = ContentType.objects.get_for_model(obj)
        self.object_id = obj.id
        self.save()
        
    def detach(self):
        self.content_type = None
        self.object_id = None
        self.save()
    
    def get_photo_url(self):
        if self.image:            
            return self.image.url
        else:
            return None
    
    def get_model_type_name(self):
        return _('Photo')


""" Non-abstract version of the BasePhoto class """
class Photo( BasePhoto ):
    def save(self, *args, **kwargs):
        if not self.user:
            default_user = User.objects.get(username='AnonymousUser')
            self.user = default_user
        super(BasePhoto, self).save(*args, **kwargs)

class InvalidImageException(Exception):
    pass
