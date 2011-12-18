from django.shortcuts import render_to_response
from django.template.context import RequestContext
from jobs.models import Job

def view_job(request, job_id):
    job = Job.objects.get(id=job_id)
    
    context = {
               'title': job.title, 
               'city': job.city,
               'place_addr': job.place_addr, 
               'company': job.company,
               'description': job.description,
               'benefits': job.benefits,
               'num_opening': job.num_opening,
               'photo': job.photo,
               'min_qualifications': job.min_qualifications, 
               'preferred_qualifications': job.preferred_qualifications, 
               'skill_set': job.skill_set,
               'part_time': job.part_time,
               'full_time': job.full_time, 
               'language': job.language,
               'education': job.education,
               'req_working_years': job.req_working_years, 
               'contact_info': job.contact_info,
               'updated': job.updated,
               'other_info': job.other_info,
               }
    
    
    return render_to_response('job_details.html', context, context_instance=RequestContext(request))
    