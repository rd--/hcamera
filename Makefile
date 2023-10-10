clean:
	rm -Rf dist
	(cd cmd ; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hcamera

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hcamera

install-dep:
	sudo apt install libexif-dev
	cabal v1-install hsexif
