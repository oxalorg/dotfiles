#!/bin/sh
wget -erobots=off --mirror --convert-links --adjust-extension --page-requisites --no-parent --remote-encoding=utf-8 $1
