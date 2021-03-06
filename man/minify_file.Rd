% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minify.R
\name{minify_file}
\alias{minify_file}
\alias{minify_file_json}
\alias{minify_file_css}
\alias{minify_file_sql}
\alias{minify_file_xml}
\title{beautify a file}
\usage{
minify_file(file, outfile = file)

minify_file_json(file, outfile = file)

minify_file_css(file, outfile = file)

minify_file_sql(file, outfile = file)

minify_file_xml(file, outfile = file)
}
\arguments{
\item{file}{file to be beautified}

\item{outfile}{output file}
}
\description{
beautify a file containing either json, xml, css or sql content.
}
\examples{
f_json <- system.file(package = "prettyfile", "data_examples/simple.json")
beautify_file(f_json, outfile = "simple.json")
minify_file("simple.json", outfile = "simple.json")

f_xml <- system.file(package = "prettyfile", "data_examples/simple.xml")
beautify_file(f_xml, outfile = "simple.xml")
minify_file("simple.xml", outfile = "simple.xml")

f_css <- system.file(package = "prettyfile", "data_examples/simple.css")
beautify_file(f_css, outfile = "simple.css")
minify_file("simple.css", outfile = "simple.css")

f_sql <- system.file(package = "prettyfile", "data_examples/simple.sql")
beautify_file(f_sql, outfile = "simple.sql")
minify_file("simple.sql", outfile = "simple.sql")
}
