from djangotoolbox.fields import SetField as _SetField
from django.forms import Widget, CharField

class SetField(_SetField):
    def formfield(self, **kwargs):
        # This is a fairly standard way to set up some defaults
        # while letting the caller override them.
        defaults = {}
        defaults.update(kwargs)
        return CharField(defaults)
