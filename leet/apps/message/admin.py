""" Message admin configuration """

from django.contrib import admin
from message.models import Message

class MessageAdmin(admin.ModelAdmin):
    fields = ('created', )
    list_display = ('__unicode__', 'created')
    list_per_page = 100
    
# Register our new admin class
admin.site.register(Message, MessageAdmin)

