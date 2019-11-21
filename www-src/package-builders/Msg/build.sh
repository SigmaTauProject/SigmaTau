trash ./build/*
flatc --js -o ./build/scripts ../../../schemas/common.fbs
flatc --js -o ./build/scripts ../../../schemas/bridge.fbs
flatc --js -o ./build/scripts ../../../schemas/wire.fbs
flatc --js -o ./build/scripts ../../../schemas/radarArc.fbs
flatc --js -o ./build/scripts ../../../schemas/hackEV.fbs

