from django.template import Variable, Library, Node, TemplateSyntaxError, TemplateDoesNotExist
from django.template.loader import render_to_string
from django.contrib.auth.models import User
from django.contrib.contenttypes.models import ContentType
from django.utils.translation import ugettext_lazy as _

from actstream.models import is_following
from missions.models import MissionGroup, Mission
from globals.utils import gen_a_post
from review.models import Review
from post.models import Post
from events.models import Event

register = Library()

@register.inclusion_tag('inc_follow_btn.html', takes_context=True)
def follow_btn(context, content_object):
    user = context['request'].user
    if not user.is_anonymous():
        action = 'follow'
        if is_following(user, content_object):
            action = 'unfollow'

        content_type_id = ContentType.objects.get_for_model(content_object).pk

        return {
            'content_type_id' : content_type_id,
            'object_id' : content_object.pk,
            'action': action,
		} 

@register.inclusion_tag('inc_favorite_btn.html', takes_context=True)
def favorite_btn(context, content_object, style_class="btn_1 favorite"):
    user = context['request'].user
    btn_name = _("FAVORITE")
    btn_name_un = _("UNFAVORITE")
    if not user.is_anonymous():
        show_btn=True
        action = 'follow'
        if is_following(user, content_object):
            action = 'unfollow'

        content_type_id = ContentType.objects.get_for_model(content_object).pk
        if content_type_id == ContentType.objects.get_for_model(User).pk:
            btn_name = _("FOLLOW")
            btn_name_un = _("UNFOLLOW")
            if not content_object.get_profile().viewable_by(user):
                show_btn = False
        
        return {
            'content_type_id' : content_type_id,
            'object_id' : content_object.pk,
            'action': action,
            'show_btn': show_btn,
            'btn_class': style_class,
            'btn_name': btn_name,
            'btn_name_un': btn_name_un
        } 

class DummyMission():

    def __init__(self, mission):
        self.mission = mission
        
    def get_absolute_url(self):
        return self.mission.get_absolute_url()
       
    def get_display_name(self):
        return _("Mission - %(mission_title)s" % {'mission_title': self.mission.title}) 

class DummyMissionTask(DummyMission):

    def __init__(self, mission, task):
        DummyMission.__init__(self, mission)
        self.task = task

    def get_display_name(self):
        return _('Mission - %(mission_title)s\nTask %(task_seq_order)s - %(task_title)s'% {'mission_title': self.mission.title, 'task_seq_order': self.task.sequence_order, 'task_title':self.task.title})

class DisplayActionLabel(Node):
    def __init__(self, actor, varname=None):
        self.actor = Variable(actor)
        self.varname = varname
        
    def render(self, context):
        actor_instance = self.actor.resolve(context)
        try:
            user = Variable("request.user").resolve(context)
        except:
            user = None
        try:
            if user and user == actor_instance.user:
                result=" your "
            else:
                result = " %s's " % (actor_instance.user.get_full_name() or actor_instance.user.username)
        except ValueError:
            result = ""
        result += actor_instance.get_label()
        if self.varname is not None:
            context[self.varname] = result
            return ""
        else:
            return result

class DisplayAction(Node):
    def __init__(self, action, varname=None):
        self.action = Variable(action)
        self.varname = varname
    
#    @profile('DisplayAction.prof')
    def render(self, context):
        
        action_instance = self.action.resolve(context)
        request = context['request']

        actor               = action_instance.actor
        target              = action_instance.target
        target_content_type = action_instance.target_content_type

        ct_review  = ContentType.objects.get_for_model(Review)
        ct_mission = ContentType.objects.get_for_model(Mission)
        ct_post    = ContentType.objects.get_for_model(Post)
        ct_event   = ContentType.objects.get_for_model(Event)
        
        icon_group = None
        
        #=======================================================================
        # if the request user doesn't have permission to see this target, 
        # then return nothing
        #=======================================================================
        if hasattr(target, 'viewable_by'):
            if not target.viewable_by( request.user ):
                return ""

        #The following block is temporary used for excluding all mission group instances on user's wall
        if target == ContentType.objects.get_for_model( MissionGroup ).model:
            pps = None
            try:
                action_output = render_to_string(('activity/%(verb)s/action.html' % { 'verb':action_instance.verb.replace(' ','_') }),{ 'action':action_instance, 'pps':pps },context)
            except TemplateDoesNotExist:
                action_output = render_to_string(('activity/action.html'),{ 'action':action_instance, 'pps':pps },context)
            if self.varname is not None:
                context[self.varname] = action_output
                return ""
            else:
                return action_output

        #===========================================================================
        # arrange the parameters required by the inc_post_boilerplate.html
        #===========================================================================
        comment_target = target
        allow_comment = '1'
        
        avatar = actor.get_profile()
        
        if not avatar.public_comments:
            allow_comment="0"

        user_profile = avatar

        post_time = action_instance.timestamp

        #=======================================================================
        # Event
        #=======================================================================
        if target_content_type == ct_event:
            invitation = action_instance.target.get_invitation(request.user)
            if invitation:
                return render_to_string(('activity/event.html'),{ 'event': action_instance.target, 'invitation':invitation, 'request':request},context)
            else:
                return ""
            
        #=======================================================================
        # Review
        #=======================================================================
        if target_content_type == ct_review:
            #===================================================================
            # Review for Task Completion
            #===================================================================
            if target.task:
                task = target.task
                mission = task.mission
                pre_content_object = DummyMissionTask( mission, task )
                post_content_object = None
                todo_target = target.restaurant
            #===================================================================
            # Normal Review
            #===================================================================
            else:
                if hasattr(target, 'dish') and target.dish:
                    pre_content_object = target.dish
                    post_content_object = target.restaurant
                    todo_target = target.dish
                else:
                    pre_content_object = target.restaurant
                    post_content_object = None
                    todo_target = target.restaurant
            
            icon_group = target.score
            
            #content    
            if hasattr(target, 'content') and target.content:
                content = target.content
            else:
                content = None
              
            target_absolute_url="/restaurant/%s/photo_detail/%s/" % (target.restaurant.id, target.photo.id)
        #=======================================================================
        # Created Mission
        #=======================================================================
        elif target_content_type == ct_mission:
            pre_content_object = DummyMission( target )
            post_content_object = None
            
            content=target.description
            
            todo_target = target
            target_absolute_url = None
        
        #=======================================================================
        # Wall Post
        #=======================================================================
        elif target_content_type == ct_post:
            pre_content_object = None 
            post_content_object = None
            todo_target = None
            target_absolute_url = None
            content = target.content
            
            tag_list = target.get_all_tags_list()
            
        else:
            raise Exception("invlid type")
            

        
        if not target_content_type == ct_post:
            tag_list = None
        
        if target_content_type == ct_post:
            target_photo = target.get_photos()
        else:
            target_photo = [target.get_display_photo()]
        target_content_type_id = ContentType.objects.get_for_model(comment_target).id
        
        
        pps = gen_a_post(
            allow_comment=allow_comment,
            avatar=avatar,
        	user_profile=user_profile,
            pre_content_object=pre_content_object,
            post_content_object=post_content_object,
            content=content,
            post_time=post_time,
            todo_target=todo_target,
            target_photo=target_photo,
            target_absolute_url=target_absolute_url,
            target_content_type_id=target_content_type_id,
            comment_target=comment_target,
            request=request,
            tag_list = tag_list,
            icon_group = icon_group,
            action = action_instance,
		)
        
        try:
            action_output = render_to_string(('activity/%(verb)s/action.html' % { 'verb':action_instance.verb.replace(' ','_') }),{ 'action':action_instance, 'pps':pps },context)
        except TemplateDoesNotExist:
            action_output = render_to_string(('activity/action.html'),{ 'action':action_instance, 'pps':pps },context)
        if self.varname is not None:
            context[self.varname] = action_output
            return ""
        else:
            return action_output        
        
class DisplayActionShort(Node):
    def __init__(self, action, varname=None):
        self.action = Variable(action)
        self.varname = varname
        
    def render(self, context):
        
        action_instance = self.action.resolve(context)
        try:
            action_output = render_to_string(('activity/%(verb)s/action.html' % { 'verb':action_instance.verb.replace(' ','_') }),{ 'hide_actor':True, 'action':action_instance },context)
        except TemplateDoesNotExist:
            action_output = render_to_string(('activity/action.html'),{ 'hide_actor':True, 'action':action_instance },context)
        
        if self.varname is not None:
            context[self.varname] = action_output
            return ""
        else:
            return action_output        
        
class DisplayGroupedActions(Node):
    def __init__(self, actions, varname=None):
        self.actions = Variable(actions)
        self.varname = varname
        
    def render(self, context):
        actions_instance = self.action.resolve(context)
        try:
            action_output = render_to_string(('activity/%(verb)s/grouped.html' % { 'verb':actions_instance[0].verb }),{ 'actions':actions_instance })
        except TemplateDoesNotExist:
            action_output = render_to_string(('activity/grouped.html'),{ 'actions':actions_instance })
        if self.varname is not None:
            context[self.varname] = action_output
            return ""
        else:
            return action_output        
        
def do_print_action(parser, token):
    bits = token.contents.split()
    if len(bits) > 3:
        if len(bits) != 4:
            raise TemplateSyntaxError, "Accepted formats {% display_action [action] %} or {% display_action [action] as [var] %}"
        if bits[2] != 'as':
            raise TemplateSyntaxError, "Accepted formats {% display_action [action] %} or {% display_action [action] as [var] %}"
        return DisplayAction(bits[1],bits[3])
    else:
        return DisplayAction(bits[1])
        
def do_print_action_short(parser, token):
    bits = token.contents.split()
    if len(bits) > 3:
        if len(bits) != 4:
            raise TemplateSyntaxError, "Accepted formats {% display_action [action] %} or {% display_action [action] as [var] %}"
        if bits[2] != 'as':
            raise TemplateSyntaxError, "Accepted formats {% display_action [action] %} or {% display_action [action] as [var] %}"
        return DisplayActionShort(bits[1],bits[3])
    else:
        return DisplayActionShort(bits[1])
        
def do_print_grouped_actions(parser, token):
    bits = token.contents.split()
    if len(bits) > 3:
        if len(bits) != 4:
            raise TemplateSyntaxError, "Accepted formats {% display_grouped_actions [action] %} or {% display_action [action] as [var] %}"
        if bits[2] != 'as':
            raise TemplateSyntaxError, "Accepted formats {% display_grouped_actions [action] %} or {% display_action [action] as [var] %}"
        return DisplayAction(bits[1],bits[3])
    else:
        return DisplayAction(bits[1])
        
def do_print_action_label(parser, token):
    bits = token.contents.split()
    if len(bits) > 3:
        if len(bits) != 4:
            raise TemplateSyntaxError, "Accepted formats {% action_label [action] %} or {% action_label [action] as [var] %}"
        if bits[2] != 'as':
            raise TemplateSyntaxError, "Accepted formats {% action_label [action] %} or {% action_label [action] as [var] %}"
        return DisplayActionLabel(bits[1],bits[3])
    else:
        return DisplayActionLabel(bits[1])
    
def do_get_user_contenttype(parser, token):
    return UserContentTypeNode(*token.split_contents())

class UserContentTypeNode(Node):
    def __init__(self, *args):
        self.args = args
        
    def render(self, context):
        context[self.args[-1]] = ContentType.objects.get_for_model(User)
        return ''
    
register.tag('display_action', do_print_action)
register.tag('display_action_short', do_print_action_short)
register.tag('display_grouped_actions', do_print_grouped_actions)
register.tag('action_label', do_print_action_label)
register.tag('get_user_contenttype', do_get_user_contenttype)
