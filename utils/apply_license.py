#!/usr/bin/env python

import sys
import os, os.path

try:
    pathroot = sys.argv[1]
    base_ext = sys.argv[2]
    header_file = sys.argv[3]
except:
    print "usage ./apply_license.py <root> <extension> <header file>"
    exit()

with open(header_file, 'r') as f:
    license = f.readlines()

full_license = ''.join(license)
consistent = ''.join(license[1:])

for (dirpath, dirnames, filenames) in os.walk(pathroot):
    for filename in filenames:
        basename, extension = os.path.splitext(filename)
        if extension != base_ext:
            continue

        with open(dirpath + '/' + filename, 'r') as f:
            contents = f.read()

            #Yes, this only works for licenses that start with 'Copyright'
            existing = contents.find('Copyright')
            if existing != -1:
                license_start = contents.find('--', 0, existing)
                license_end = contents.find(consistent) + len(consistent)
                new_contents = contents[0:license_start] + full_license + contents[license_end:]
            else:
                new_contents = full_license + '\n' + contents
        print dirpath + '/' + filename

        with open(dirpath + '/' + filename, 'w') as f:
            assert new_contents
            f.write(new_contents)
