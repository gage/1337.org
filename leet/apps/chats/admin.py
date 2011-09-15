""" leet chats admin models """

from django.contrib import admin

from chats.models import Chatroom

    
admin.site.register( Chatroom )