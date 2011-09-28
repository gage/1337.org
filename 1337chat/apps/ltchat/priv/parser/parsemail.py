#!/usr/bin/env python
# ~*~ coding: utf-8 ~*~

import sys
import struct
from cStringIO import StringIO
import textwrap
import re

import parser

def send(data):
    prefix = struct.pack(">I", len(data))
    sys.stdout.write(prefix)
    sys.stdout.write(data)

def getBytes(length):
    buffer = StringIO()
    byteCount = 0
        
    while byteCount < length:
        data = sys.stdin.read(length - byteCount)
        buffer.write(data)
        byteCount += len(data)
    return buffer.getvalue()

length = struct.unpack(">I", getBytes(4))[0]

message = getBytes(length)

result = parser.parse(message, lambda m: None)

if result is not None:
    result = result.replace("\r", "")
    paragraphs = re.split(r"\n{2,}", result)
    result = "\n\n".join("\n".join(textwrap.wrap(p, 60)) for p in paragraphs)
    send(result)



