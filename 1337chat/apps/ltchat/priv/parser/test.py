import os, os.path, sys
from StringIO import StringIO

import parser

trace = False

if len(sys.argv) > 1:
    tests = [sys.argv[1]]
    trace = True
else:
    tests = os.listdir("tests")

for testname in tests:
    testdir = os.path.join("tests", testname)
    if not os.path.isdir(testdir):
        continue
    
    orig = open(os.path.join(testdir, "in.txt"), "rb").read().strip()
    output = open(os.path.join(testdir, "out.txt"), "rb").read().strip()

    buffer = StringIO()
    result = parser.parse(orig, lambda s: buffer.write("%s\n" % s))

    if result is None:
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        print "PARSE FAIL: %s" % testname
        print buffer.getvalue()
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        continue

    if result != output:
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        print "MISMATCH: %s (%s) (%s)" % (testname, repr(result), repr(output))
        print buffer.getvalue(),
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        continue

    if trace:
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        print buffer.getvalue(),
        print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

    print "    %s [OK]: %s" % (testname, result)
    
