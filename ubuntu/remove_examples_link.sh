#!/bin/bash

# Removes the dumb examples link from a newly created home
# directory. That's it.

if [ -f ~/examples.desktop ]; then
    rm ~/examples.desktop
fi
