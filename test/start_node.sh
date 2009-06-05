#!/usr/bin/env sh
echo Starting node$1@localhost...
if [ "$1" != "1" ]; then
    echo "Not first Node"
    config="-config test/node";
fi
erl -pa ebin ../ebin -sname node$1@localhost -boot chordial $config