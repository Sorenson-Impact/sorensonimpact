# sorensonimpact 0.0.1.9052

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
