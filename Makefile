install:
	cabal v1-install --allow-newer

clean:
	rm -Rf dist
	(cd cmd ; make clean)

push-all:
	r.gitlab-push.sh hcamera
	r.github-push.sh hcamera

indent:
	fourmolu -i Graphics

doctest:
	doctest -Wno-x-partial -Wno-incomplete-uni-patterns Graphics

install-dep:
	sudo apt install libexif-dev
	cabal v1-install hsexif
