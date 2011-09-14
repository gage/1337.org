from django.shortcuts import render_to_response
from django.template.context import RequestContext
from django.core.urlresolvers import reverse
from django.contrib.auth.decorators import login_required

@login_required(login_url='/registration/accounts/login/')
def test_chat(request):
    user = request.user
    return render_to_response("sandbox_test_chat.html", {
    'user':user
    },context_instance=RequestContext( request ))
    