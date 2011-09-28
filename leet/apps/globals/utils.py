import json, threading
import urllib2, urllib
from django.conf import settings

def do_rt_notify(receivers, message):
    url = '%s/notify/' % settings.CHAT_SERVER_URL
    data = {'ids':json.dumps(receivers), 'message':json.dumps(message)}
    f = urllib2.urlopen(url, urllib.urlencode(data))
    result = json.loads(f.read())
    if result['success']:
        return True
    else:
        return False

def rt_notify(receivers, message):
    threading.Thread(target=do_rt_notify, args=(receivers, message,)).start()
