from datetime import datetime
from django.db import models
from django.core.urlresolvers import reverse
from django.utils.translation import ugettext_lazy as _
from django.utils.timesince import timesince as timesince_
from django.contrib.contenttypes import generic
from django.contrib.contenttypes.models import ContentType
from django.contrib.auth.models import User
from django.template import TemplateDoesNotExist, RequestContext
from django.template.loader import render_to_string

from actstream.signals import action
from actstream.specs import ActstreamSpecs
from django.conf import settings
from djangotoolbox.fields import SetField

from django.db.models import Q

class FollowManager( models.Manager ):
    def stream_for_user( self, user, include_self=False, only_self=False ):
        """
        Produces a QuerySet of most recent activities from actors the user follows
        
        If include_self, includes user's own actions in stream
        If only_self, only includes user's own actions in stream
        """

        if ContentType.objects.get_for_model( user ) == ContentType.objects.get( app_label="user_profiles", model="userprofile" ):
            user = user.user
        
        if not only_self:    
            follows = self.get_follower(user)
            if include_self:
                follows.add(user.pk)
    
            qs = Action.objects.filter( 
                Q(actor_object_id__in=follows) | Q(push_to=user.id)
            ).order_by( '-timestamp' )
            
        else:
            qs = Action.objects.filter( 
                Q(actor_object_id = user.pk) | Q(push_to=user.id)
            ).order_by( '-timestamp' )
        
        return qs

    def get_follower( self, content_object ):
        """
        input: the target content object
        output: followers of the target content object 
        """
        result_query_set = self.filter( object_id=content_object.id, content_type=ContentType.objects.get_for_model( content_object ) )
        followers = [o.user for o in result_query_set]
        
        return followers

    def get_follower_count( self, content_object ):
        return self.filter( object_id=content_object.id, content_type=ContentType.objects.get_for_model( content_object ) ).count()

    def get_following( self, user ):
        """
        input: the target content object and a user
        output: all content objects which the user are following 
        """
        result_query_set = self.filter( user=user )
        following = [o.actor for o in result_query_set]
        return following

    def get_following_count( self, user ):
        return self.filter( user=user ).count()


    def get_followed_users( self, user ):
        """
        Produces a List of users who are followed by "user"
        """
        followed_relation_list = self.filter( user=user, content_type=ContentType.objects.get_for_model( User ) )
        followed_users = [f.actor for f in followed_relation_list]
        return followed_users

    def get_follower_users( self, user ):
        """
        Produces a List of users who follows "user"
        """
        follower_relation_list = self.filter( object_id=user.id )
        follower_user_list = [f.user for f in follower_relation_list]
        
        return follower_user_list

    def get_followed_user_count( self, user ):
        """ return the number of following(user) who follows """
        count = self.filter( user=user, content_type=ContentType.objects.get_for_model( User ) ).count()
        return count

    def get_follower_user_count( self, user ):
        count = self.filter( object_id=user.id ).count()
        return count
        

class Follow( models.Model ):
    """
    Lets a user follow the activities of any specific actor
    """
    user = models.ForeignKey( User )

    content_type = models.ForeignKey( ContentType )
    object_id = models.TextField()
    actor = generic.GenericForeignKey()

    objects = FollowManager()

    def __unicode__( self ):
        return u'%s -> %s' % ( self.user, self.actor )

class ActionManager( models.Manager ):
    def stream_for_actor( self, actor ):
        """
        Produces a QuerySet of most recent activities for any actor
        """
        if ContentType.objects.get_for_model( actor ) == ContentType.objects.get( app_label="user_profiles", model="userprofile" ):
            actor = actor.user

        return self.filter( 
            actor_content_type=ContentType.objects.get_for_model( actor ),
            actor_object_id=actor.pk,
        ).order_by( '-timestamp' )

    def stream_for_model( self, model ):
        """
        Produces a QuerySet of most recent activities for any model
        """
        return self.filter( 
            target_content_type=ContentType.objects.get_for_model( model )
        ).order_by( '-timestamp' )
        

class Action( models.Model ):
    """
    Action model describing the actor acting out a verb (on an optional target). 
    Nomenclature based on http://martin.atkins.me.uk/specs/activitystreams/atomactivity
    
    Generalized Format::
    
        <actor> <verb> <time>
        <actor> <verb> <target> <time>
        <actor> <verb> <action_object> <target> <time>
    
    Examples::
    
        <justquick> <reached level 60> <1 minute ago>
        <brosner> <commented on> <pinax/pinax> <2 hours ago>
        <washingtontimes> <started follow> <justquick> <8 minutes ago>
        <mitsuhiko> <closed> <issue 70> on <mitsuhiko/flask> <about 3 hours ago>
        
    Unicode Representation::
    
        justquick reached level 60 1 minute ago
        mitsuhiko closed issue 70 on mitsuhiko/flask 3 hours ago
        
    HTML Representation::
    
        <a href="http://oebfare.com/">brosner</a> commented on <a href="http://github.com/pinax/pinax">pinax/pinax</a> 2 hours ago

    """
    actor_content_type          = models.ForeignKey( ContentType, related_name='actor' )
    actor_object_id             = models.TextField()
    actor                       = generic.GenericForeignKey( 'actor_content_type', 'actor_object_id' )

    verb                        = models.CharField( max_length=255 )
    description                 = models.TextField( blank=True, null=True )

    target_content_type         = models.ForeignKey( ContentType, related_name='target', blank=True, null=True )
    target_object_id            = models.TextField( blank=True, null=True )
    target                      = generic.GenericForeignKey( 'target_content_type', 'target_object_id' )

    action_object_content_type  = models.ForeignKey( ContentType, related_name='action_object', blank=True, null=True )
    action_object_object_id     = models.TextField( blank=True, null=True )
    action_object               = generic.GenericForeignKey( 'action_object_content_type', 'action_object_object_id' )

    timestamp                   = models.DateTimeField( auto_now_add=True )

    public                      = models.BooleanField( default=True )
    
    push_to                     = SetField(models.CharField(max_length=30))
    
    
    objects = ActionManager()

    def __unicode__( self ):
        if self.target:
            if self.action_object:
                return u'%s %s %s on %s %s ago' % ( self.actor, self.verb, self.action_object, self.target, self.timesince() )
            else:
                return u'%s %s %s %s ago' % ( self.actor, self.verb, self.target, self.timesince() )
        return u'%s %s %s ago' % ( self.actor, self.verb, self.timesince() )

    def actor_url( self ):
        """
        Returns the URL to the ``actstream_actor`` view for the current actor
        """
        return reverse( 'actstream_actor', None,
                       ( self.actor_content_type.pk, self.actor_object_id ) )

    def target_url( self ):
        """
        Returns the URL to the ``actstream_actor`` view for the current target
        """
        return reverse( 'actstream_actor', None,
                       ( self.target_content_type.pk, self.target_object_id ) )

    def timesince( self, now=None ):
        """
        Shortcut for the ``django.utils.timesince.timesince`` function of the current timestamp
        """
        return timesince_( self.timestamp, now )

    def render( self, request=None ):
        """
        Returns an HTML representation of the current action
        """
        try:
            if request:
                action_output = render_to_string( 'activity/%s/action.html' % self.verb.replace( ' ', '_' ), {'action': self}
                                                , context_instance=RequestContext( request ) )
            else:
                action_output = render_to_string( 'activity/%s/action.html' % self.verb.replace( ' ', '_' ), {'action': self} )

        except TemplateDoesNotExist:
            if request:
                action_output = render_to_string( 'activity/action.html', {'action': self}
                                                , context_instance=RequestContext( request ) )
            else:
                action_output = render_to_string( 'activity/action.html', {'action': self} )
        return action_output

    @models.permalink
    def get_absolute_url( self ):
        return ( 'actstream.views.detail', [self.pk] )


# TODO: test cases for is_following
def is_following( user, actor ):
    """
    Returns True if user is following actor, False otherwise
    """
    if Follow.objects.filter( user=user, object_id=actor.pk,
        content_type=ContentType.objects.get_for_model( actor ) ).count() > 0:
        return True
    return False


def follow( user, actor, send_notify=False, send_action=True ):
    """
    Creates a ``User`` -> ``Actor`` follow relationship such that the actor's activities appear in the user's stream.
    Also sends the ``<user> started following <actor>`` action signal.
    Returns the created ``Follow`` instance.
    If ``send_action`` is false, no "started following" signal will be created
    
    Syntax::
    
        follow(<user>, <actor>)
    
    Example::
    
        follow(request.user, group)
    
    """
    from notify.models import Notify
    follow, created = Follow.objects.get_or_create( user=user, object_id=actor.pk,
        content_type=ContentType.objects.get_for_model( actor ) )
    if send_action and created:

        action.send( user, verb=_( ActstreamSpecs.STARTED_FOLLOWING ), target=actor )
        
        """ notify comment owner """
        if send_notify:            
            if ContentType.objects.get_for_model( actor ) == ContentType.objects.get( app_label="auth", model="user" ):            
                
                email= render_to_string('email_templates/inc_email_follow.html', {'from_user':user, 'site_url':settings.SITE_DOMAIN})
                _notify = Notify.objects.send( from_user=user, type=Notify.FOLLOWS,  message = "", 
                    email_message = email, contact=None, to_user=actor)
            
    return follow


def unfollow( user, actor, send_action=False ):
    """
    Removes ``User`` -> ``Actor`` follow relationship. 
    Optionally sends the ``<user> stopped following <actor>`` action signal.
    
    Syntax::
    
        unfollow(<user>, <actor>)
    
    Example::
    
        unfollow(request.user, other_user)
    
    """
    Follow.objects.filter( user=user, object_id=actor.pk,
        content_type=ContentType.objects.get_for_model( actor ) ).delete()
    if send_action:
        action.send( user, verb=_( ActstreamSpecs.STARTED_FOLLOWING ), target=actor )

def actor_stream( actor ):
    return Action.objects.stream_for_actor( actor )
actor_stream.__doc__ = Action.objects.stream_for_actor.__doc__

def user_stream( user, include_self=False ):
    return Follow.objects.stream_for_user( user, include_self )
user_stream.__doc__ = Follow.objects.stream_for_user.__doc__

def model_stream( model ):
    return Action.objects.stream_for_model( model )
model_stream.__doc__ = Action.objects.stream_for_model.__doc__

def action_handler( verb, **kwargs ):
    kwargs.pop( 'signal', None )
    actor = kwargs.pop( 'sender' )
    newaction = Action( actor_content_type=ContentType.objects.get_for_model( actor ),
                    actor_object_id=actor.pk,
                    verb=unicode( verb ),
                    public=bool( kwargs.pop( 'public', True ) ),
                    description=kwargs.pop( 'description', None ),
                    timestamp=kwargs.pop( 'timestamp', datetime.now() ) )

    target = kwargs.pop( 'target', None )
    if target:
        newaction.target_object_id = target.pk
        newaction.target_content_type = ContentType.objects.get_for_model( target )
        
        from events.models import Event
        if newaction.target_content_type == ContentType.objects.get_for_model( Event ):
            newaction.push_to = target.gulu_user_set

    action_object = kwargs.pop( 'action_object', None )
    if action_object:
        newaction.action_object_object_id = action_object.pk
        newaction.action_object_content_type = ContentType.objects.get_for_model( action_object )

    newaction.save()
    return newaction

action.connect( action_handler, dispatch_uid="actstream.models" )
