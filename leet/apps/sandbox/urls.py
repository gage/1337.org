""" TEST URLs """

from django.conf.urls.defaults import *

urlpatterns = patterns('sandbox.views',
    url(r'^$', 'test_chat', name="sandbox-test-chat"),
)
