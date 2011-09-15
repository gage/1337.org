from django.contrib import admin
from actstream.models import Action, Follow

class ActionAdmin(admin.ModelAdmin):
    list_display = ('actor','verb','target')
    list_editable = ('verb',)
    list_filter = ('timestamp',)
    search_fields = ('verb',)

class FollowAdmin(admin.ModelAdmin):
    list_display = ('__unicode__','user','actor')
    list_filter = ('user',)
    raw_id_fields = ( "user", )

admin.site.register(Action, ActionAdmin)
admin.site.register(Follow, FollowAdmin)
