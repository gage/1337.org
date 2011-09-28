import email
import sys
import re

from lxml import html, etree
from lxml.cssselect import CSSSelector

SENDER = re.escape("gulumail.com")

PLAINTEXT_REGEXPS = [
    r"^Sent from my ", # BlackBerry, iPhone, etc
    r" wrote:$", # Mail.app
    r"^__", # Yahoo webmail
    r"^From:", # Hotmail webmail

    # Other stuff that seems plausible
    r"^--",
    r"^Date:",
    r"^To:",
    r"^Subject:",
]

def parse(rawMessage, writer):
    msg = email.message_from_string(rawMessage)

    parts = {}

    if msg.is_multipart():
        writer("Multipart")
        for part in msg.get_payload():
            mimetype = "%s/%s" % (part.get_content_maintype(), part.get_content_subtype())
            writer("Part: %s" % mimetype)
            parts[mimetype] = part.get_payload(decode=True)

    else:        
        writer("Single-part")
        mimetype = "%s/%s" % (msg.get_content_maintype(), msg.get_content_subtype())
        writer("Part: %s" % mimetype)
        parts[mimetype] = msg.get_payload(decode=True)

    if "text/html" in parts:
        writer("Decoding HTML...")
        writer(parts["text/html"])
        try:
            parts["lxml"] = html.document_fromstring(parts["text/html"])
        except Exception, e:
            writer("Oh noes: %s" % e)

    result = MailCleaner(parts, writer).clean()
    if result is not None:
        result = result.strip()
    return result


class MailCleaner(object):
    
    def __init__(self, parts, writer):
        self.parts = parts
        self.writer = writer

    def clean(self):
        for method in ('gmail_html', 'plaintext_regexps'):
            result = getattr(self, "try_%s" % method)()
            if result is not None:
                return result

        return self.return_everything()


    def try_gmail_html(self):
        if "lxml" not in self.parts:
            return

        tree = self.parts["lxml"]
        sel = CSSSelector('.gmail_quote')
        quote = sel(tree)
        if not quote:
            return

        self.writer("Gmail HTML message detected!")
        for e in quote:
            e.clear()

        return tree.text_content()
        

    def try_plaintext_regexps(self):
        if "text/plain" not in self.parts:
            return

        text = self.parts["text/plain"]

        self.writer("Text: " + text)

        for r in PLAINTEXT_REGEXPS:
            self.writer("Trying %s" % r)
            match = re.search(r, text, re.MULTILINE)
            if not match:
                continue

            return text[:match.start()]
        

    def return_everything(self):
        if "text/plain" in self.parts:
            return self.parts["text/plain"]

        if "lxml" in self.parts:
            return self.parts["lxml"].text_content()

        return None
