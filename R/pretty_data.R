
#' @importFrom V8 v8
#' @export
#' @title beautify a string
#' @description beautify a string containing either json, xml, css or sql content.
#' @param str json, xml, css or sql string
#' @param format format data contained in the string (one of json, xml, css or sql)
beautify_str <- function(str, format = "json"){

  stopifnot(format %in% c("json", "xml", "css", "sql") )

  ct <- v8()
  js_file <- system.file(package = "prettyfile", "js/pretty-data.js")
  ct$source(js_file)

  switch(format,
         json = ct$call("pd.json", str),
         xml = ct$call("pd.xml", str),
         css = ct$call("pd.css", str),
         sql = ct$call("pd.sql", str)
         )

}

beautify_file_ <- function(file, format = "xml", outfile = file){
  stopifnot( file.exists(file) )
  str <- paste0(readLines(file, warn = FALSE), collapse = "")
  out <- beautify_str(str, format = format)
  cat(out, file = outfile)
  invisible()
}


#' @export
#' @title beautify a file
#' @description beautify a file containing either json, xml, css or sql content.
#' @param file file to be beautified
#' @param outfile output file
#' @examples
#' # beautify_file ----
#'
#' f_json <- system.file(package = "prettyfile", "data_examples/simple.json")
#' beautify_file(f_json, outfile = "simple.json")
#'
#' f_xml <- system.file(package = "prettyfile", "data_examples/simple.xml")
#' beautify_file(f_xml, outfile = "simple.xml")
#'
#' f_css <- system.file(package = "prettyfile", "data_examples/simple.css")
#' beautify_file(f_css, outfile = "simple.css")
#'
#' f_sql <- system.file(package = "prettyfile", "data_examples/simple.sql")
#' beautify_file(f_sql, outfile = "simple.sql")
beautify_file <- function(file, outfile = file){

  stopifnot( file.exists(file) )

  str <- paste0(readLines(file, warn = FALSE), collapse = "")

  if( grepl("\\.json$", file, ignore.case = TRUE) )
    out <- beautify_str(str, format = "json")
  else if( grepl("\\.xml$", file, ignore.case = TRUE) )
    out <- beautify_str(str, format = "xml")
  else if( grepl("\\.css$", file, ignore.case = TRUE) )
    out <- beautify_str(str, format = "css")
  else if( grepl("\\.sql$", file, ignore.case = TRUE) )
    out <- beautify_str(str, format = "sql")
  else stop("unsupported file type")

  cat(out, file = outfile)
  invisible()
}


#' @export
#' @rdname beautify_file
beautify_file_json <- function(file, outfile = file){
  beautify_file_(file = file, format = "json", outfile = outfile)
}

#' @export
#' @rdname beautify_file
beautify_file_css <- function(file, outfile = file){
  beautify_file_(file = file, format = "css", outfile = outfile)
}

#' @export
#' @rdname beautify_file
beautify_file_sql <- function(file, outfile = file){
  beautify_file_(file = file, format = "sql", outfile = outfile)
}

#' @export
#' @rdname beautify_file
beautify_file_xml <- function(file, outfile = file){
  beautify_file_(file = file, format = "xml", outfile = outfile)
}



#' @export
#' @title beautify files in directory
#' @description beautify file in a directory.
#' @param path directory
#' @param pattern an optional regular expression. Only file
#' names which match the regular expression will be beautified.
#' @param beautifier function to use to format data
#' @examples
#' #' # beautify_dir ----
#'
#' # simulate a directory with many files ----
#' f_json <- system.file(package = "prettyfile", "data_examples/simple.json")
#' f_xml <- system.file(package = "prettyfile", "data_examples/simple.xml")
#' f_css <- system.file(package = "prettyfile", "data_examples/simple.css")
#' f_sql <- system.file(package = "prettyfile", "data_examples/simple.sql")
#' dir.create("data_files")
#' file.copy(c(f_json, f_xml, f_css, f_sql), to = "data_files" )
#'
#' beautify_dir("data_files")
beautify_dir <- function(path, pattern = NULL, beautifier = beautify_file){
  if( is.null(pattern) )
    pattern <- "(\\.xml(\\.rels){0,1}$|\\.json$|\\.css$|\\.sql$)"
  files_ <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  for(file_ in files_ ){
    beautifier(file = file_)
  }
  path
}


#' @export
#' @rdname beautify_str
#' @description \code{beautify_current_doc} is not to be used
#' by the user.
#' @importFrom R.utils filePath
#' @importFrom rstudioapi getActiveDocumentContext
beautify_current_doc <- function() {
  con <- getActiveDocumentContext()
  text <- con$path
  beautify_file(filePath(text))
  invisible()
}

