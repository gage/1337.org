from django.db import models

class UrlLink(models.Model):
    url         = models.TextField()
    title       = models.CharField(max_length=100, null=True, blank=True)

    def __unicode__(self):
        return "%s %s" % (self.url, self.title)