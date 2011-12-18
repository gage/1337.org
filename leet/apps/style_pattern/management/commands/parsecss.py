import re
import os
from django.core.management.base import NoArgsCommand, BaseCommand, CommandError
from style_pattern.models import Style

class Command(BaseCommand):
    args = '<css_file>'
    help = 'parse css file and write style_pattern data'
    
    def handle(self, *args, **options):
        if len(args) == 0:
            file_path_ele = './gulu/apps/globals/static/less/element.less'
            file_path_block = './gulu/apps/globals/static/less/block.less'
            file_path_layout = './gulu/apps/globals/static/less/layout.less'
            
            self.handle_once(file_path_ele, **options)
            self.handle_once(file_path_block, **options)
            self.handle_once(file_path_layout, **options)
        elif len(args) == 1:
            self.handle_once(*args, **options)
        else: 
            raise CommandError('1 arguments required: css_file')
        
    def handle_once(self, *args, **options):
        
        file_path = os.path.abspath(args[0])
        css_file = open(file_path, 'r').read()
        
        pattern = "/\*\!(.*?)\*/$"
        pattern_style = "\@style:(.*?)\n"
        pattern_type = "\@type:(.*?)\n"
        pattern_desc = "\@description:(.*?)\n"
        
        matches = re.finditer(pattern, css_file, flags=re.MULTILINE | re.DOTALL)
        
        for m in matches:
            start = m.start()
            line_number = css_file.count('\n', 0, start) + 1
            head = m.group(1)
            style_list = re.findall(pattern_style, head, flags=re.MULTILINE | re.DOTALL)
            type = re.findall(pattern_type, head, flags=re.MULTILINE | re.DOTALL)
            desc = re.findall(pattern_desc, head, flags=re.MULTILINE | re.DOTALL)
            
            if len(style_list) < 1:
                self.stdout.write("error in parsing style_name.\n")
                exit()
            else:
                style_list = style_list[0].split(',')
            if len(type) < 1:
                type = None
            else:
                type = type[0].strip()
            
            if len(desc) < 1:
                desc = None
            else:
                desc = desc[0].strip()
            for style in style_list: 
                style_name = style.strip()
                try:
                    new_style = Style.objects.get(name=style_name)
                    if new_style.type != type:
                        print "same name in differnt types, name:%s, old:%s, new:%s\n" % (style_name, new_style.type, type)
                        print "change to new type if new type is not null or space"
                    if type:
                        new_style.type = type
                    new_style.description = desc    
                    new_style.file_name = args[0]
                    new_style.line_number = line_number
                    new_style.save()
                except:
                    if type:
                        new_style = Style.objects.create(name=style_name,
                                                         description = desc,
                                                         type = type,
                                                         file_name = args[0],
                                                         line_number = line_number,
                                                         )    
                    else:
                        new_style = Style.objects.create(name=style_name,
                                                         description = desc,
                                                         file_name = args[0],
                                                         line_number = line_number,
                                                         )        
        
        self.stdout.write("done.\n")
        
        
        
        
        