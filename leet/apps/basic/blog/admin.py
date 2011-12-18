from django.contrib import admin
from basic.blog.models import *


class CategoryAdmin(admin.ModelAdmin):
    prepopulated_fields = {'slug': ('title',)}
admin.site.register(Category, CategoryAdmin)

class PostAdmin(admin.ModelAdmin):
    list_display  = ('title', 'publish', 'status')
    list_filter   = ('publish', 'status')
    search_fields = ('title', 'body')
    prepopulated_fields = {'slug': ('title',)}
    
class CommonMedia:
    js = (
          'http://ajax.googleapis.com/ajax/libs/dojo/1.6.0/dojo/dojo.xd.js',
          '/static/js/editor.js',
    )
    css = {
           'all': ('/static/css/editor.css',),
    }

admin.site.register(Post, PostAdmin, Media = CommonMedia)


class BlogRollAdmin(admin.ModelAdmin):
    list_display = ('name', 'url', 'sort_order',)
    list_editable = ('sort_order',)
admin.site.register(BlogRoll)