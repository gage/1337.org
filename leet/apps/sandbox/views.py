from django.http import HttpResponse
from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required
from globals.utils import rt_notify
from chats.models import Chatroom

@login_required(login_url='/registration/accounts/login/')
def test_chat(request):
    user = request.user
    return render_to_response("sandbox_test_chat.html", {
    'user':user
    },context_instance=RequestContext( request ))
    
    
@login_required
def test_send_message(request):
    msg = request.POST.get('msg')
    clean_msg = msg.replace(' ', '')
    if not clean_msg:
        return HttpResponse('')
    chat_uuid = request.POST.get('chat_uuid')
    chatroom = Chatroom.objects.get(uuid=chat_uuid)
    recievers = []
    for uuid in chatroom.participants.keys():
        recievers.append(uuid)    
    user = request.user
    username = user.username
    rt_notify(recievers, {'msg':msg, 'username':username})
    return HttpResponse('')
