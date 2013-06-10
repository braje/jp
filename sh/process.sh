#!/bin/bash

IFS="$(printf '\n\t')"   # Remove 'space', so filenames with spaces work well.

for file in "$@"
do
    if [ -e "$file" ] ; then # Make sure it isn't an empty match

        echo "Processing ${file}"

	filename=$(basename "$file")
	extension="${filename##*.}"
	filename="${filename%.*}"

        echo "extension: " ${extension}

#        convert "${file}" -crop \
#          `convert "${file}" -virtual-pixel edge -blur 0x15 -fuzz 15% -trim -format '%wx%h%O' info:` \
#          +repage "${croppedFile}"

        if [ ${extension} == "tif" ] ; then
            convert "${file}" -quality 75 "${filename}.jpg"
        fi

        convert -define tif:size=500x180 "${file}" -auto-orient -thumbnail 250x100 -unsharp 0x.5 "${filename}-thumb.jpg"

        echo "writing ${filename}.jpg"
    fi

done
