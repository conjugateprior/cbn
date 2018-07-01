#' CBN
#'
#' This package contains tools and experimental items necessary to
#' replicate Caliskan, Bryson, and Narayanan (2017). 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'
#' @source A. Caliskan, J. J. Bryson, and A. Narayanan (2017) 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'         \emph{Science}. 356:6334 \url{http://doi.org/10.1126/science.aal4230}.
#'
#' @docType package
#' @name cbn
#' @useDynLib cbn
#' @importFrom Rcpp sourceCpp
NULL

#' All Items Used in All Studies
#'
#' This data frame contains all the items used in all the studies.
#' It is the data source for \code{cbn_get_items}.
#' Most of the time you should probably use that.
#'
#' @source A. Caliskan, J. J. Bryson, and A. Narayanan (2017) 'Semantics derived
#'         automatically from language corpora contain human-like biases'
#'         \emph{Science}. 356:6334 \url{http://doi.org/10.1126/science.aal4230}.
"cbn_items"

#' Vectors for All Items Used in All Studies
#'
#' Extracted from the 840B word 300-dimensional vector Common Crawl
#' data (downloaded on Jun 30th, 2018).
#'
#' @source J. Pennington, R. Socher, and C. D. Manning (2014)
#'         'GloVe: Global vectors for word representation'
#'         \url{https://nlp.stanford.edu/projects/glove/}.
"cbn_items"

#' Get the Items from a Study
#'
#' Returns a data frame containing the items from any of the studies
#' (WEAT1 through WEAT10 or WEFAT1 or WEFAT2) or a vector containing
#' all items from all studies if \code{type} == "all".
#'
#' @param type "all" (the default), "WEAT", or "WEFAT"
#' @param number study number (default: 1) Ignored if \code{type} = "all"
#' @return a data frame of items in columns or a vector of all items
#' @export
cbn_get_items <- function(type = c("all", "WEAT", "WEFAT"), number = 1){
  study_type <- match.arg(type)
  if (study_type == "all") {
    its <- sort(as.vector(na.omit(unique(unlist(cbn::cbn_items[,-1])))))
    return(its)
  }
  sname <- paste0(study_type, number)
  df <- cbn::cbn_items[cbn::cbn_items$Study == sname, ]
  data.frame(Filter(function(x){ !all(is.na(x)) }, df))
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
cbn_set_vectorfile_location <- function(f){
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
cbn_get_vectorfile_location <- function(){
  cc_loc <- Sys.getenv("VECTORS_LOC")
  if (is.null(cc_loc))
    stop("Location unkown: use set_vectors_file to assign it")
  else
    cc_loc
}

#' Extract Word Vectors
#'
#' This function provides a more convenient wrapper for \code{extract_words}.
#'
#' @param words words to get vectors for
#' @param verbose whether to report on progress
#' @param report_every how often to check in to see if we should stop
#'
#' @return a matrix with word vectors as rows
#' @export
cbn_extract_word_vectors <- function(words, verbose = FALSE, report_every = 100000){
  loc <- cbn_get_vectorfile_location()
  mat <- cbn:::extract_words(words, vectors_file = loc, verbose = verbose,
                            report_every = report_every)
  mat
}

.onUnload <- function(libpath) {
  library.dynam.unload("cbn", libpath)
}
