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

#' Cosine Similarity for Every Pair of Study Items
#'
#' A matrix of cosine similarities between each item and every other one.
#' Uses \code{cbn_items}.
#'
"cbn_item_cosines"

#' Vectors for All Items Used in All Studies
#'
#' A 457 x 300 matrix of (row) vectors for all study items,
#' extracted from the 840B word Common Crawl data on Jun 30th, 2018.
#'
#' @source J. Pennington, R. Socher, and C. D. Manning (2014)
#'         'GloVe: Global vectors for word representation'
#'         \url{https://nlp.stanford.edu/projects/glove/}.
"cbn_item_vectors"

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
    unique(cbn::cbn_items$Word)
  } else {
    sname <- paste0(study_type, number)
    cbn::cbn_items[cbn::cbn_items$Study == sname, ]
  }
}

#' Set the Location of the Vectors File
#'
#' This function adds the location of the file of vectors to the
#' current environment (as the value of \code{CBN_VECTORS_LOCATION}).
#' If \code{persist} is TRUE it also adds this key to
#' \code{~/.Renviron} so that it is retained across R sessions.
#'
#' To recover the current location, use
#' \code{\link{cbn_get_vectorfile_location}}.
#'
#' @param f path where you unzipped your vectors file
#' @param persist Whether to add this to your R startup file
#' @return Nothing
#' @seealso \code{\link{cbn_get_vectorfile_location}}
#' @export
cbn_set_vectorfile_location <- function(f, persist = FALSE){
  f <- normalizePath(f)
  if (file.exists(f)) {
    Sys.setenv(CBN_VECTORS_LOCATION = f)
    add_to_Renviron("CBN_VECTORS_LOCATION", f)
  } else
    message(f, "does not exist!")
}

add_to_Renviron <- function(key, value) {
  renviron = "~/.Renviron"
  if (file.exists(renviron)) {
    lines <- readLines(renviron)
    new_lines <- Filter(function(x) { !startsWith(x, key) }, lines)
    new_lines[[length(new_lines) + 1]] <- paste0(key, "=", value)
    writeLines(paste0(new_lines, collapse = "\n"), renviron)
  } else {
    line <- paste0(key, "=", value, "\n")
    writeLines(line, renviron)
  }
}

#' Get the Location of the Vectors File
#'
#' Returns the full path to the file of word vectors.  If there is no
#' environment variable \code{CBN_VECTORS_LOCATION} in the current
#' environment it prompts to set a location with
#' \code{cbn_set_vectorfile_location}
#'
#' If you want prefer the location of your downloaded vectors to persist
#' across sessions, add
#' \code{CBN_VECTORS_LOCATION=/Users/me/Documents/myvectors.txt}
#' or similar to your \code{~/.Renviron} file (creating the file if necessary).
#'
#' @return a full path to the vectors file
#' @seealso \code{\link{cbn_set_vectorfile_location}}
#' @export
cbn_get_vectorfile_location <- function(){
  cc_loc <- Sys.getenv("CBN_VECTORS_LOCATION")
  if (is.null(cc_loc))
    stop("Location unknown: use cbn_set_vectorfile_location to assign it")
  else
    cc_loc
}

#' Extract Word Vectors From Current Vector File
#'
#' This function provides a more convenient wrapper for \code{extract_words}.
#' It uses the current vector file, whose location can be found using
#' \code{\link{cbn_get_vectorfile_location}} and assigned with
#' \code{\link{cbn_set_vectorfile_location}}.
#'
#' @param words words to get vectors for
#' @param verbose whether to report on progress
#' @param report_every how often to check in to see if we should stop
#'
#' @return a matrix with word vectors as rows
#' @export
cbn_extract_word_vectors <- function(words, verbose = FALSE, report_every = 100000){
  loc <- cbn_get_vectorfile_location() # stops if none is set
  mat <- cbn:::extract_words(words, vectors_file = loc, verbose = verbose,
                             report_every = report_every)
  mat
}


#' Calculates Cosine Similarity Between Matrix Rows
#'
#' This function calculates the cosine similarity matrix between all
#' rows of a matrix \code{x}. When \code{x} and \code{y} are vectors
#' it calculates the cosine similarity between them.  When \code{x}
#' is a vector and \code{y} is a matrix it calculates the cosine
#' between \code{x} and each row of \code{y}.
#'
#' This code is taken directly from the \code{lsa} package but adjusted to
#' operate rowwise.
#'
#' @param x A vector or a matrix (e.g., a document-term matrix).
#' @param y A vector with compatible dimensions to x. If NULL, use all columns of \code{x}.
#' @source The original code is from the \code{cosine} function by
#'         Fridolin Wild (f.wild@open.ac.uk) in the \code{lsa} package.
#' @return An \code{ncol(x)} by \code{ncol(x)} matrix of cosine similarities, a scalar
#'         cosine similarity, or a vector of cosine simialrities of length \code{nrow(y)}.
#' @export
cbn_cosine <- function(x, y = NULL){
  if (is.matrix(x) && is.null(y)) {
    co = array(0, c(nrow(x), nrow(x)))
    f = rownames(x)
    dimnames(co) = list(f, f)
    for (i in 2:nrow(x)) {
      for (j in 1:(i - 1)) {
        co[i, j] = cbn_cosine(x[i, ], x[j, ])
      }
    }
    co = co + t(co)
    diag(co) = 1
    return(as.matrix(co))
  } else if (is.vector(x) && is.vector(y)) {
    return(crossprod(x, y) / sqrt(crossprod(x) * crossprod(y)))
  } else if (is.vector(x) && is.matrix(y)) {
    co = vector(mode = "numeric", length = nrow(y))
    names(co) = rownames(y)
    for (i in 1:nrow(y)) {
      co[i] = cbn_cosine(x, y[i, ])
    }
    return(co)
  } else {
    stop("Either one matrix, a vector and a matrix, or two vectors needed as input")
  }
}


.onUnload <- function(libpath) {
  library.dynam.unload("cbn", libpath)
}
