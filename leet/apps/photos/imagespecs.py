from imagekit.specs import ImageSpec
from imagekit import processors  

class Resize26x26(processors.Resize):
    width = 26
    height = 26
    crop = True

class Resize50x50(processors.Resize):
    width = 50
    height = 50
    crop = True

class Resize60x60(processors.Resize):
    width = 60
    height = 60
    crop = True

class Resize180x180(processors.Resize):
    width = 180
    height = 180
    crop = True

class Resize500x500(processors.Resize):
    width = 500
    height = 500
    crop = True

class EnhanceSmall(processors.Adjustment):
    contrast = 1.2
    sharpness = 1.1
    
class AdminThumbnail(ImageSpec):
    access_as = 'admin_thumbnail'
    processors = [Resize50x50, EnhanceSmall]

class Image26x26(ImageSpec):
    processors = [Resize26x26, EnhanceSmall]
    #pre_cache = True

class Image50x50(ImageSpec):
    processors = [Resize50x50, EnhanceSmall]
    #pre_cache = True

class Image60x60(ImageSpec):
    processors = [Resize60x60, EnhanceSmall]
    #pre_cache = True

class Image180x180(ImageSpec):
    processors = [Resize180x180]
    #pre_cache = False

class Image500x500(ImageSpec):
    processors = [Resize500x500]
    
