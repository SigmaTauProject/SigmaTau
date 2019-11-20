trash ./build/*
flatc --js -o ./build/scripts ../../../schemas/down.fbs
flatc --js -o ./build/scripts ../../../schemas/common.fbs
flatc --js -o ./build/scripts ../../../schemas/wire.fbs
flatc --js -o ./build/scripts ../../../schemas/radarArc.fbs

