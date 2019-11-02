
./prebuild.sh && cabal new-run --extra-lib-dirs="${PWD}/world-src/build/" --extra-lib-dirs="${PWD}/dlib"
##./prebuild.sh && hpack && cabal new-run --extra-lib-dirs="${PWD}/world-src/build/" 
