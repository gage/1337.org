from django.conf import settings
from django.core.management import call_command
from django.core.management.base import BaseCommand

class Command(BaseCommand):
    help = 'Runs the test suite for the apps in settings.LEET_APPS'

    def handle(self, *args, **options):
        call_command('test', settings=settings, *settings.LEET_APPS)