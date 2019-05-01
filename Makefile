all: data

clean:
	rm -f data/GLODAPv2MergedMaster.csv
	rm -f data/expocodes.csv
data:
	r 'getGLODAP.R'
