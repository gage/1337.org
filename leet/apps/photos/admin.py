from django.contrib import admin
from django.contrib.contenttypes import generic
from django.forms import widgets
from django.db import models

from photos.models import *

class GenericPhotoInline(generic.GenericTabularInline):
    model = Photo


class PhotoAdmin(admin.ModelAdmin):
	""" Base photo admin class """
	list_display = ('pk', 'title', 'user', 'object_id', 'created', 'admin_thumbnail_view')
	list_filter = ['created', 'content_type']
	list_per_page = 10
	raw_id_fields = ('user',)

admin.site.register(Photo, PhotoAdmin)
