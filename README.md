# SuessEffect
Files related to Ocean DI13C Project

## Getting data

This project uses make to handle downloading data, since the files are large.
Makefile contains targets for getting and processing stuff. 

To get glodap data and expocodes: `make data` from the command line.

`make all` should check that the data is downloaded and get it if neccessary,
as well as running all joining and analysis steps.
