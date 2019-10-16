#!/usr/bin/env bash
VOLUME=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print $2 }}' | head -n 1)
echo "$1: $VOLUME%"
exit 0
