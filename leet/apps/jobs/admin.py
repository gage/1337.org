from django.contrib import admin
from jobs.models import Job


class JobAdmin(admin.ModelAdmin):
    list_display = ('title', 'city', 'created', )
    #raw_id_fields = ('user', )
    list_per_page = 10
    
# Register our new admin class
admin.site.register(Job, JobAdmin)

