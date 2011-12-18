""" 1337 Job model"""
__author__ = "Eric Lee <eric.lee@geniecapital.com>"

import datetime

from django.db import models
from photos.models import Photo
from django.contrib.auth.models import User
from djangotoolbox.fields import SetField

class Job(models.Model):
    """
    Job information posted by 1337.org
    """
    #job info
    title = models.CharField(max_length=100)
    company = models.CharField(max_length=100,null=True, blank=True)
    city = models.CharField(max_length=100, null=True, blank=True)
    place_addr = models.CharField(max_length=255, null=True, blank=True)
    description = models.TextField(default='')
    benefits = models.TextField(default='', null=True, blank=True)
    #category = SetField(models.CharField(max_length=30, null=True, blank=True))
    num_opening = models.IntegerField(null=True, blank=True)
    photo = models.ForeignKey('photos.Photo', blank=True, null=True)
    
    #qualification
    min_qualifications = models.TextField(default='')
    preferred_qualifications = models.TextField(default='', null=True, blank=True) 
    skill_set = models.TextField(default='', null=True, blank=True)
    part_time = models.BooleanField(default=False)
    full_time = models.BooleanField(default=False)
    language = models.TextField(default='', null=True, blank=True)
    education = models.CharField(max_length=25, null=True, blank=True)
    req_working_years = models.IntegerField(default=1, null=True, blank=True)
    
    #apply info
    contact_info = models.TextField(default='', null=True, blank=True)
    created = models.DateTimeField(default=datetime.datetime.now)
    updated = models.DateTimeField(auto_now=True)
    other_info = models.TextField(default='', null=True, blank=True)
    
    
    
    
    
    
    
    