from django.shortcuts import  render_to_response
from django.template.context import RequestContext
from django.utils.translation import ugettext_lazy as _
from style_pattern.models import Style
from django.http import HttpResponse, Http404
import json

def show_all(request):
    if request.user.is_superuser:
        #style_set = Style.objects.all().order_by('created')
        style_set_block = Style.objects.filter(type=Style.TYPE_BLOCK).order_by('line_number')
        style_set_element = Style.objects.filter(type=Style.TYPE_ELEMENT).order_by('line_number')
        style_set_layout = Style.objects.filter(type=Style.TYPE_LAYOUT).order_by('line_number')
        style_set_null = Style.objects.filter(type__isnull=True).order_by('line_number')
        
        context = {'style_set_block': style_set_block,
                   'style_set_element': style_set_element,
                   'style_set_layout': style_set_layout,
                   'style_set_null': style_set_null,}
        return render_to_response( 'style_pattern.html', context, context_instance=RequestContext( request ) )
    else:
        raise Http404

def edit(request):
    if request.method == 'POST' and request.is_ajax() and request.user.is_superuser:
        target_id = request.POST.get('target_id')
        type = request.POST.get('type') #one of desp, html, css
        content = request.POST.get('content')
        
        try:
            style = Style.objects.get(id=target_id)
        except:
            return HttpResponse( json.dumps( {"ok":0} ), mimetype="application/json")
        
        if type == 'desp':
            style.description = content
        elif type == 'html':
            style.html = content
        
        style.creator = request.user
        style.save()
        return HttpResponse( json.dumps( {"ok":1, "new_content":content} ), mimetype="application/json")
        
    else:
        raise Http404
    
def delete(request):
    if request.method == 'POST' and request.is_ajax() and request.user.is_superuser:
        target_id = request.POST.get('target_id')
        try:
            style = Style.objects.get(id=target_id)
        except:
            return HttpResponse( json.dumps( {"ok":0,} ), mimetype="application/json")
        style.delete()
        return HttpResponse( json.dumps( {"ok":1,} ), mimetype="application/json")
        
    else:
        raise Http404
    
