from testing.testcases import MongoTestCase
from projects.models import Project, UrlLink


class ProjectTest(MongoTestCase):
    
    def setUp(self):
        self.uesr1 = User.objects.create_user(username="user1", email="user1@test.com", password="password")
        self.user2 = User.objects.create_user(username="user2", email="user1@test.com", password="password")
        self.user3 = User.objects.create_user(username="user3", email="user1@test.com", password="password")
        self.p1 = Project.objects.create(user=self.uesr1, title="project1", description="project1")
          
    def test_url_link(self):
        self.p1.add_url_link(url="url1.com", title="url1")
        self.assertEqual(Project.objects.get(pk=self.p1.id).urls, [ul1])
