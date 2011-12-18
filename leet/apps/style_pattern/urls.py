from django.conf.urls.defaults import *

urlpatterns = patterns('style_pattern.views',
    url(r'^show_all/$', 'show_all', name="show-all"),
    url(r'^edit/$', 'edit', name="edit-style"),
    url(r'^delete/$', 'delete', name="delete-style"),
)
