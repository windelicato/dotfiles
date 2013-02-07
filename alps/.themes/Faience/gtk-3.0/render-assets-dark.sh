#! /bin/bash

INKSCAPE="/usr/bin/inkscape"
OPTIPNG="/usr/bin/optipng"

SRC_FILE="assets-dark.svg"
ASSETS_DIR="assets"
INDEX="export.lst"

for i in `cat $INDEX`
do
if [ -f $ASSETS_DIR/dark-$i.png ]; then
    echo $ASSETS_DIR/dark-$i.png exists.
else
    echo Rendering $ASSETS_DIR/dark-$i.png
    $INKSCAPE --export-id=$i \
              --export-id-only \
              --export-png=$ASSETS_DIR/dark-$i.png $SRC_FILE >/dev/null \
    && $OPTIPNG -o7 --quiet $ASSETS_DIR/dark-$i.png
fi
done
exit 0
