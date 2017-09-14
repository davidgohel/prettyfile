prettyfile R package
================

[![Travis-CI Build Status](https://travis-ci.org/davidgohel/prettyfile.svg?branch=master)](https://travis-ci.org/davidgohel/prettyfile) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/davidgohel/prettyfile?branch=master&svg=true)](https://ci.appveyor.com/project/davidgohel/prettyfile)

An helper package to help:

-   prettify minified XML, JSON, CSS and SQL Files.
-   minify XML, JSON, CSS and SQL Files.

This is a simple interface with [pretty-data](http://www.eslinstructor.net/pretty-data/).

Example
-------

``` r
library(prettyfile)
str <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?><note to=\"david\" from=\"world\">hello</note><note to=\"world\" from=\"david\">hello</note>"
cat( str )
#> <?xml version="1.0" encoding="UTF-8"?><note to="david" from="world">hello</note><note to="world" from="david">hello</note>
```

``` r
test <- beautify_str(str, format = "xml") 
cat( test )
#> <?xml version="1.0" encoding="UTF-8"?>
#> <note to="david" from="world">hello</note>
#> <note to="world" from="david">hello</note>
```

``` r
cat( minify_str(test, format = "xml") )
#> <?xml version="1.0" encoding="UTF-8"?><note to="david" from="world">hello</note><note to="world" from="david">hello</note>
```

Installation
------------

``` r
devtools::install_github("davidgohel/prettyfile")
```
