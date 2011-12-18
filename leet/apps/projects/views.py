from django.shortcuts import render_to_response
from django.template.context import RequestContext
from projects.models import Project
from projects.forms import CreateProjectForm

def create(request):
    if request.method == 'POST':
        form = CreateProjectForm(request.POST)
        if form.is_valid():
            project = form.save()
    else:
        form = CreateProjectForm()
        
    return render_to_response("create_project.html", {
        'form': form,
    },context_instance=RequestContext( request ))
