""" User profiles admin configuration """

__author__ = "Sean Cheng <sean.cheng@geniecapital.com>"

from django.contrib import admin
from user_profile.models import UserProfile

class UserProfileAdmin(admin.ModelAdmin):
    list_display = ('user', 'get_uuid')
    raw_id_fields = ('user', )
    list_per_page = 100
    
# Register our new admin class
admin.site.register(UserProfile, UserProfileAdmin)

