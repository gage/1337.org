from django.conf.urls.defaults import *

urlpatterns = patterns('projects.views',
    url(r'^create/$', 'create', name="create-project"),
)

urlpatterns += patterns('projects.ajax',
)
