from basic.blog.models import Post
from dbindexer.api import register_index
register_index(Post, {'publish': 'year'}) 
register_index(Post, {'publish': 'month'}) 
register_index(Post, {'publish': 'day'})

register_index(Post, {'created': 'year'}) 
register_index(Post, {'created': 'month'}) 
register_index(Post, {'created': 'day'})

register_index(Post, {'modified': 'year'}) 
register_index(Post, {'modified': 'month'}) 
register_index(Post, {'modified': 'day'}) 