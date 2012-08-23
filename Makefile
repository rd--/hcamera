prefix=$(HOME)/opt
B=hcamera

build-ghc:
	ghc -Wall -fwarn-tabs -O2 --make $(B).hs
	strip -s $(B)
	cp $(B) $(prefix)/bin

clean:
	find . -name '*.hi' | xargs rm -f
	find . -name '*.o' | xargs rm -f
	rm -f $(B)
	rm -f dist
