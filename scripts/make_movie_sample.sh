#!/bin/sh
#
# encoding for wmv
#mencoder "mf://image_*.jpg" -mf fps=10 -o movie.wmv -ovc lavc -lavcopts vcodec=msmpeg4v2 
#
# requires: mplayer on Linux

# set the extension (remove .)
# -ovc
ovcopt=lavc
#Available codecs:
#   copy     - frame copy, without re-encoding. Doesn't work with filters.
#               THIS DOES NOT WORK on my laptop
#   frameno  - special audio-only file for 3-pass encoding, see DOCS.
#   raw      - uncompressed video. Use fourcc option to set format explicitly.
#   nuv      - nuppel video
#   lavc     - libavcodec codecs - best quality!
#   libdv    - DV encoding with libdv v0.9.5
#   xvid     - XviD encoding
#   x264     - H.264 encoding (for YouTube, does not work??)

# set file extension (remove .)
ext=png
filebase=../run/pool/fields
# set filename
filename=$filebase
#var=p_stream
#filename=$filebase$var
mencoder  "mf://"$filename"_[0-9]*."$ext -mf fps=10 -o $filename".avi" -ovc $ovcopt -lavcopts vcodec=msmpeg4v2 

