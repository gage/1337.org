""" Gulu project module forms """

__author__ = "Jason Ke <jason.ke@geniecapital.com.tw>"
__contributors__ = ["Jason Ke <jason.ke@geniecapital.com.tw",]

from django import forms
from projects.models import Project

class CreateProjectForm(forms.ModelForm):
    """ Form used for creating new project """
    class Meta:
        model = Project
        exclude = ('members', 'reference', 'features', 'urls', 'screenshot')
