""" Team admin configuration """

from django.contrib import admin
from team.models import Team

class TeamAdmin(admin.ModelAdmin):
    fields = ('name', 'created')
    list_display = ('name', 'get_members', 'created')
    list_per_page = 100
    
# Register our new admin class
admin.site.register(Team, TeamAdmin)

