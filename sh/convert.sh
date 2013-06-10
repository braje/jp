#!/bin/bash

IFS="$(printf '\n\t')"   # Remove 'space', so filenames with spaces work well.

for file in `ls $1`
do
    if [ -e "$file" ] ; then # Make sure it isn't an empty match

        echo "Processing ${file}"

        extension=${file##*.}
        croppedFile="${file}-cropped.${extension}"

        echo "extension: " ${extension}
        echo ${croppedFile}

        convert "${file}" -crop \
          `convert "${file}" -virtual-pixel edge -blur 0x15 -fuzz 15% -trim -format '%wx%h%O' info:` \
          +repage "${croppedFile}"

        if [ ${extension} == "tif" ] ; then
            convert "${croppedFile}" -scale $2 "${file}-cropped.png"
        fi

        convert -define tif:size=500x180 "${croppedFile}" -auto-orient -thumbnail 250x100 -unsharp 0x.5 "${file}-thumb.png"
        convert -define tif:size=500x180 "${croppedFile}" -auto-orient -thumbnail 250x100 -unsharp 0x.5 "${file}-thumb.jpg"

        echo "writing ${file}.png"
    fi

done
