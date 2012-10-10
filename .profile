#!/usr/bash

for sh in ${HOME}/.profile.d/*.sh ; do
        [ -r "$sh" ] && . "$sh"
done
