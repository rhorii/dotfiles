#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Create New Window
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🦊
# @raycast.packageName Firefox

# Documentation:
# @raycast.author rhorii
# @raycast.authorURL https://github.com/rhorii

open -na 'Firefox' --args -new-window about:blank
