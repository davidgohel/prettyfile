#' @export
#' @title minify a string
#' @description minify a string containing either json, xml, css or sql content.
#' @param str json, xml, css or sql string
#' @param format format data contained in the string (one of json, xml, css or sql)
minify_str <- function(str, format = "json"){

  stopifnot(format %in% c("json", "xml", "css", "sql") )

  ct <- v8()
  js_file <- system.file(package = "prettyfile", "js/pretty-data.js")
  ct$source(js_file)
  switch(format,
         json = ct$call("pd.jsonmin", str),
         xml = ct$call("pd.xmlmin", str, TRUE),
         css = ct$call("pd.cssmin", str, TRUE),
         sql = ct$call("pd.sqlmin", str)
         )

}

minify_file_ <- function(file, format = "xml", outfile = file){
  stopifnot( file.exists(file) )
  str <- paste0(readLines(file, warn = FALSE), collapse = "")
  out <- minify_str(str, format = format)
  cat(out, file = outfile)
  invisible()
}


#' @export
#' @title beautify a file
#' @description beautify a file containing either json, xml, css or sql content.
#' @param file file to be beautified
#' @param outfile output file
#' @examples
#' f_json <- system.file(package = "prettyfile", "data_examples/simple.json")
#' beautify_file(f_json, outfile = "simple.json")
#' minify_file("simple.json", outfile = "simple.json")
#'
#' f_xml <- system.file(package = "prettyfile", "data_examples/simple.xml")
#' beautify_file(f_xml, outfile = "simple.xml")
#' minify_file("simple.xml", outfile = "simple.xml")
#'
#' f_css <- system.file(package = "prettyfile", "data_examples/simple.css")
#' beautify_file(f_css, outfile = "simple.css")
#' minify_file("simple.css", outfile = "simple.css")
#'
#' f_sql <- system.file(package = "prettyfile", "data_examples/simple.sql")
#' beautify_file(f_sql, outfile = "simple.sql")
#' minify_file("simple.sql", outfile = "simple.sql")
minify_file <- function(file, outfile = file){

  stopifnot( file.exists(file) )

  str <- paste0(readLines(file, warn = FALSE), collapse = "")

  if( grepl("\\.json$", file, ignore.case = TRUE) )
    out <- minify_str(str, format = "json")
  else if( grepl("\\.xml$", file, ignore.case = TRUE) )
    out <- minify_str(str, format = "xml")
  else if( grepl("\\.css$", file, ignore.case = TRUE) )
    out <- minify_str(str, format = "css")
  else if( grepl("\\.sql$", file, ignore.case = TRUE) )
    out <- minify_str(str, format = "sql")
  else stop("unsupported file type")

  cat(out, file = outfile)
  invisible()
}


#' @export
#' @rdname minify_file
minify_file_json <- function(file, outfile = file){
  minify_file_(file = file, format = "json", outfile = outfile)
}

#' @export
#' @rdname minify_file
minify_file_css <- function(file, outfile = file){
  minify_file_(file = file, format = "css", outfile = outfile)
}

#' @export
#' @rdname minify_file
minify_file_sql <- function(file, outfile = file){
  minify_file_(file = file, format = "sql", outfile = outfile)
}

#' @export
#' @rdname minify_file
minify_file_xml <- function(file, outfile = file){
  minify_file_(file = file, format = "xml", outfile = outfile)
}

