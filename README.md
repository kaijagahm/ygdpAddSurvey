### README: ygdpAddSurvey package

This package was written by Kaija for the YGDP. It contains functions that help with the process of adding a new YGDP survey to the existing database. It should be used in conjunction with the contents of the ygdpDB repository.

There is a script in the [ygdpDB repository](https://github.com/kaijagahm/ygdpDB) called "READONLY_surveyProcessingTemplate.R". This provides a full example workflow for adding a new survey, in this case Survey 13, to the database. A script called "READONLY_checkCompletionScript.R" likewise provides an example workflow for *only* checking completion by comparing AMT and QT files, without simultaneously adding a whole survey to the database.

Documentation for individual functions can be found in the "man/" folder on this repo.

I haven't created examples/vignettes here because I don't intend this package for general use, although anyone is welcome to borrow the code if you think you'll find it useful. This packages is written specifically for the YGDP survey data, and the functions will probably not be suitable for most other applications. 

If you have questions, feel free to contact me at kaija.gahm@aya.yale.edu.
