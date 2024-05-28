#!/usr/bin/env bash

echo "display2d: false; ratprint: false; $1;" | maxima -q | grep "(%o3) " | sed "s/(%o3) //1"

