#!/bin/bash
elm-make spaceInv.elm
echo "<!DOCTYPE HTML><html><head><meta charset='UTF-8'><title>Main</title><style>html,head,body { padding:0; margin:0; }body { font-family: calibri, helvetica, arial, sans-serif; }</style><script type='text/javascript'>" > spaceinv.html
cat elm.js >> spaceinv.html
echo "</script></head><body><script type='text/javascript'>Elm.fullscreen(Elm.Main)</script></body></html>" >> spaceinv.html