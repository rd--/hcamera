install:
	cabal v1-install --allow-newer

clean:
	rm -Rf dist
	(cd cmd ; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hcamera

push-all:
	make push-rd

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hcamera

indent:
	fourmolu -i Graphics

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Graphics

install-dep:
	sudo apt install libexif-dev
	cabal v1-install hsexif
