#' CBN
#'
#' This package contains tools and experimental items necessary to
#' replicate Caliskan, Bryson, and Narayanan (2017). 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'
#' @source Caliskan, Bryson, and Narayanan (2017). 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'         \emph{Science}. 356:6334 \url{http://doi.org/10.1126/science.aal4230}.
#'
#' @docType package
#' @name cbn
#' @useDynLib cbn
#' @importFrom Rcpp sourceCpp
NULL

#' CBN Items
#'
#' This data frame contains all the items used in all the studies.
#' It is the data source for \code{get_study_items} and \code{get_items}.
#' Most of the time you should probably use them.
#'
#' @source Caliskan, Bryson, and Narayanan (2017). 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'         \emph{Science}. 356:6334 \url{http://doi.org/10.1126/science.aal4230}.
"cbn_items"


#' Get the Items from a Study
#'
#' @param study_type WEAT (default) or WEFAT
#' @param number study number (default: 1)
#' @return a data frame of items in columns
#' @export
get_study_items <- function(study_type = c("WEAT", "WEFAT"), number = 1){
  study_type <- match.arg(study_type)
  sname <- paste0(study_type, number)
  df <- cbn::cbn_items[cbn::cbn_items$Study == sname, ]
  data.frame(Filter(function(x){ !all(is.na(x)) }, df))
}

#' Get All the Study Items
#'
#' @return a vector of all items from all studies
#' @export
get_items <- function(){
  sort(as.vector(na.omit(unique(unlist(cbn::cbn_items[,-1])))))
}


#' Set the Location of the Vectors File
#'
#' If you want prefer this setting to persist
#' across sessions, add \code{CBN_VECTORS_LOC=/Users/me/Documents/myvectors.txt}
#' or similar to your \code{~/.Renviron} file (creating the file if necessary).
#'
#' @param f path where you unzipped your vectors file
#' @return Nothing
#' @export
set_vectors_location <- function(f){
  f <- normalizePath(f)
  if (file.exists(f))
    Sys.setenv(VECTORS_LOC = f)
  else
    message(f, "does not exist!")
}

#' Get the Location of the Vectors File
#'
#' Returns the full path to the file of word vectors.  If there is no
#' environment variable \code{CBN_VECTORS_LOC}, prompts to set one.
#'
#' If you want prefer the location of your downloaded vectors to persist
#' across sessions, add \code{CBN_VECTORS_LOC=/Users/me/Documents/myvectors.txt}
#' or similar to your \code{~/.Renviron} file (creating the file if necessary).
#'
#' @return a full path to the vectors file
#' @export
vectors_location <- function(){
  cc_loc <- Sys.getenv("VECTORS_LOC")
  if (is.null(cc_loc))
    stop("Location unkown: use set_vectors_file to assign it")
  else
    cc_loc
}


.onUnload <- function(libpath) {
  library.dynam.unload("cbn", libpath)
}
