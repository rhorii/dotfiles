#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Create New Window
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🧭
# @raycast.packageName Safari

# Documentation:
# @raycast.author rhorii
# @raycast.authorURL https://github.com/rhorii

open -na 'Safari' --args -new-window about:blank
