from django.contrib import admin
from projects.models import Project

class ProjectAdmin(admin.ModelAdmin):
    list_display = ('title', 'user', )
    raw_id_fields = ('user', )
    list_per_page = 100
    
# Register our new admin class
admin.site.register(Project, ProjectAdmin)

