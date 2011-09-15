from chats.models import Chatroom
from django.http import HttpResponse
import json

def create_chatroom(request):
    if request.is_ajax():
        if request.method == 'POST':
            title = request.POST.get('title', None)
            is_test = request.POST.get('is_test', False)
            
            if is_test:
                try:
                    chatroom = Chatroom.objects.all()[0]
                except IndexError:
                    chatroom = Chatroom.objects.create(title=title)
            else:
                chatroom = Chatroom.objects.create(title=title)
            
            data={'title':chatroom.title,
                  'url':chatroom.get_absolute_url(),
                  'id':chatroom.id,
                  'uuid':chatroom.uuid}
            
        return HttpResponse(json.dumps({'status':1, 'data':data, 'msg':'success'}), mimetype="application/json")
    
    return HttpResponse(json.dumps({'status':0, 'msg':'failed'}), mimetype="application/json")