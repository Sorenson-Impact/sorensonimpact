#sorensonimpact 0.0.1.9061

* Added `zscore()` as a convenience function wrapper around `scale()` that returns a vector instead of a matrix.

# sorensonimpact 0.0.1.9060

* Added `xview()` which opens a table in Excel.  

# sorensonimpact 0.0.1.9059

* Added convenience function `ipeds_add_info()` that will add details from HD to any ipeds table. Currently adds sector, institution_entity_name, and state_abbreviation_label. Contact JZ if you have suggestions for other vars to add that are frequently wanted.

# sorensonimpact 0.0.1.9058

* Added function `si_google_drive_path_fix()` to check for the correct GDFS path and correct it (only for OSX).
* Added check on package load to warn OSX users if the GDFS path is not set correctly.

# sorensonimpact 0.0.1.9056

* Minor changes to file_trace.r

# sorensonimpact 0.0.1.9054

* Are you tired of trying to figure out where an rds came from? Especially when you have to go through multiple files to trace back a variable? I've added experimental feature for tracing all the inputs to and outputs from a file.  See ?file_trace or try `file_trace("institution_base")` now!  This has currently only been tested for the maps project.

# sorensonimpact 0.0.1.9053

* Added experimental feature for ipeds usage that provides information about a survey and details about variables and terms using the standard help syntax, i.e. `?gr`.  Only available on `gr` and `efd`. Please offer feedback as I'll be waiting to roll this out for the other surveys until I gather some usage experience from you all.

# sorensonimpact 0.0.1.9048

* Added `idf()`. This function makes it easy to look up an individual based on a key column in multiple tables from the same dataset when working interactively (ie ipeds `unitid`). See ?idf for details.

# sorensonimpact 0.0.1.9047

* Added `common_vars()` which shows common variable names between two or more tibbles.
* Added `si_news()` which shows news updates for version bumps
* Renamed `update_si()` to `si_update()` to match conventions.
* Added call to `si_news()` in `update_si()` to display news in console after an update if there was a version bump.

# sorensonimpact 0.0.1.9035

* Added a `NEWS.md` file to track changes to the package.
