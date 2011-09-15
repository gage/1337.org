import Queue
from globals.threads import GuluGeneralThread
from photos.models import get_picture_from_url

class PhotoDownloadThread(GuluGeneralThread):
    MAX_NUM = 1
    thread_num = 0    
    Pool = Queue.Queue( 0 )
    
    def run(self):
        while True:          
            (img_url, photo) = self.Pool.get()
            try:
                get_picture_from_url(img_url, update_photo=photo)
            except:
                pass
            self.Pool.task_done()
            print 'Download Photo from %s to %s'%(img_url, photo)
