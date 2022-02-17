gen:
	protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --haskell_out=./gen-hs -I ./protos ./protos/helloworld.proto
