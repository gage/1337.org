__author__ = "Eric<eric.lee@geniecapital.com.tw>"
from django.db import models
from django.contrib.auth.models import User
import datetime


class Style(models.Model):
    """model for a css style display entry"""
    TYPE_ELEMENT = 'element'
    TYPE_BLOCK = 'block'
    TYPE_LAYOUT = 'layout'
    TYPE_CHOICES = [
        (TYPE_ELEMENT,  'element'),
        (TYPE_BLOCK, 'block'),
        (TYPE_LAYOUT, 'layout'),     
    ]
    
    name = models.CharField(max_length=25)
    type = models.CharField(max_length=25, null=True, blank=True)
    html = models.CharField(max_length=1000, null=True, blank=True)
    css  = models.CharField(max_length=1000, null=True, blank=True)
    description = models.CharField(max_length=1000, null=True, blank=True)
    photos = models.ForeignKey('photos.Photo', null=True, blank=True)
    
    file_name = models.CharField(max_length=100, null=True, blank=True)
    line_number = models.IntegerField(default=0, null=True, blank=True)
    creator = models.ForeignKey(User, null=True, blank=True)
    created = models.DateTimeField(default=datetime.datetime.now)
    updated = models.DateTimeField(auto_now=True)
    
    
