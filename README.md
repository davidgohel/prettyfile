prettyfile R package
================

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
