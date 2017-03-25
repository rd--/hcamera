clean:
	rm -Rf dist

push-sp:
	darcs push -a rd@slavepianos.org:sw/hcamera

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hcamera
