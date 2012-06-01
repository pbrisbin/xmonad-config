#!/bin/bash -ex

HsColour -css -anchor xmonad.hs > docs/src/xmonad.html

for file in lib/*.hs; do
  HsColour -css -anchor "$file" > docs/src/$(basename "$file" .hs).html
done

haddock --no-warnings --html --title="pbrisbin modules" --odir=docs lib/*.hs \
  --source-module="src/%{MODULE/.//}.html" \
  --source-entity="src/%{MODULE/.//}.html#%{NAME}"
