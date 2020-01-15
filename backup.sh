#!/bin/bash
tar cvzf emacs.d.tgz . \
    && restic backup emacs.d.tgz --exclude-file /Users/michal/.config/restic/excludes.txt \
    && rm emacs.d.tgz
