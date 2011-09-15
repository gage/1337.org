from testing.testcases import MongoTestCase
from projects.models import Project
from globals.models import UrlLink
from django.contrib.auth.models import User


class ProjectTest(MongoTestCase):
    
    def setUp(self):
        self.uesr1 = User.objects.create_user(username="user1", email="user1@test.com", password="password")
        self.user2 = User.objects.create_user(username="user2", email="user1@test.com", password="password")
        self.user3 = User.objects.create_user(username="user3", email="user1@test.com", password="password")
        self.p1 = Project.objects.create(user=self.uesr1, title="project1", description="project1")
          
    def test_add_url_link(self):
        self.p1.add_url_link(url="url1.com", title="url1")
        self.assertEqual(Project.objects.get(pk=self.p1.id).urls[0].url, 'url1.com')
        self.assertEqual(Project.objects.get(pk=self.p1.id).urls[0].title, 'url1')
        
    def test_delete_url_link(self):    
        url1 = self.p1.add_url_link(url="url1.com", title="url1")
        self.assertEqual(len(Project.objects.get(pk=self.p1.id).urls), 1)
        self.p1.delete_url_link(url1.id)
        self.assertEqual(len(Project.objects.get(pk=self.p1.id).urls), 0)