#!/bin/sh
if test -f ~/.sbtconfig; then
  . ~/.sbtconfig
fi
java -Xms512M -Xmx8g -Xss1M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launch-0.13.1.jar "$@"
