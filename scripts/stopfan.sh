#!/bin/sh
sudo chown rune /sys/kernel/debug
sudo chown rune /sys/kernel/debug/vgaswitcheroo/switch
echo OFF > /sys/kernel/debug/vgaswitcheroo/switch
