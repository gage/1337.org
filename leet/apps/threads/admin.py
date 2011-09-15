from django.contrib import admin
from threads.models import Thread

class ThreadAdmin(admin.ModelAdmin):
    list_display = ('title', 'user', )
    raw_id_fields = ('user', )
    list_per_page = 100
    
# Register our new admin class
admin.site.register(Thread, ThreadAdmin)

