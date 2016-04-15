#!/bin/bash
# Make this file executable by using chmod +x <file name>.sh and place it in bin

# Set your device variables here
device1="Logitech G400s Optical Gaming Mouse"

#xinput --set-prop "$device1" 270 1
resolution_percent="241" # Greater than 100, use constant deceleration otherwise with profile -1. 

xinput set-prop "$device1" "Device Accel Profile" -1 # Simple profile with threshold 0 allows constant scaling up
xinput set-ptr-feedback "$device1" 0 "$resolution_percent" 100 # Set threshold to 0 and acceleration to $resolution_percent/100

