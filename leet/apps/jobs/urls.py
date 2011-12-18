
from django.conf.urls.defaults import *


urlpatterns = patterns('jobs.views',
    url(r'^(?P<job_id>\w+)/$', 'view_job', name="view-job"),
)