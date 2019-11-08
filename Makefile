# Makefile for GLODAP d13C project


RDIR = .
DATA_DIR = $(RDIR)/data

all: data

$(DATA_DIR)/glodapv2MMF.csv.zip: 
	curl https://www.nodc.noaa.gov/archive/arc0107/0162565/2.2/data/0-data/data_product/GLODAPv2%20Merged%20Master%20File.csv.zip -o $@


$(DATA_DIR)/glodapv2MMF.csv: $(DATA_DIR)/glodapv2MMF.csv.zip
	unzip -d $(DATA_DIR) $<
	mv $(DATA_DIR)/GLODAPv2\ Merged\ Master\ File.csv $@

$(DATA_DIR)/expocodes.txt: 
	curl https://www.nodc.noaa.gov/archive/arc0107/0162565/2.2/data/0-data/data_product/EXPOCODES.txt -o $@

$(DATA_DIR)/nosams_clivar.csv: 
	cd src; R CMD BATCH getNOSAMSdata.R 

$(DATA_DIR)/nosams_glodap.csv: 
	cd src; R CMD BATCH joinGLODAPNOSAMS.R 

clean:
	rm -f $(DATA_DIR)/glodapv2MMF.csv.zip
	rm -f $(DATA_DIR)/glodapv2MMF.csv
	rm -f $(DATA_DIR)/expocodes.txt
	rm -f $(DATA_DIR)/nosams_clivar.csv 
	rm -f $(DATA_DIR)/nosams_glodap.csv

data: $(DATA_DIR)/expocodes.txt $(DATA_DIR)/glodapv2MMF.csv $(DATA_DIR)/nosams_clivar.csv $(DATA_DIR)/nosams_glodap.csv
