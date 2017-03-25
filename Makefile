clean:
	rm -Rf dist
	(cd cmd ; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/hcamera

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hcamera
