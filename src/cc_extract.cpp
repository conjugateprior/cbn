#include<Rcpp.h>
#include<iostream>
#include<fstream>
#include<string>
#include<sstream>
#include <iterator>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// split a line on spaces and return the length - 1
// assuming that that was the word at the beginning
int get_vector_dim(std::string fname){
  std::ifstream vectors(fname);
  std::string line;
  getline(vectors, line);
  std::istringstream buf(line);
  std::istream_iterator<std::string> beg(buf), end;
  std::vector<std::string> tokens(beg, end);
  vectors.close();
  return tokens.size() - 1;
}

// Extract Word Vectors from a File
//
// This function reads a file of vectors in line by line and returns
// the vectors correspoinding to the words provided as the first argument
// as rows. If a word cannot be found, this row of the matrix is all NAs.
// The matrix returned has the words as rownames and no column names.
// It expects word vectors in the style of GLOVE common crawl.
//
// \code{report_every} is the interval (in lines of \code{vectors_file})
// at which the code checks whether the user is trying to stop the function.
// If \code{verbose} is also TRUE then a period is printed and the code
// also reports which words it has found vectors for.
//
// Note: In order to guess the dimensionality of the vectors in \code{vectors_file}
// we assume that each line of the file is a word and then K floats separated
// by a single space.
//
// @param words a vector of words to search for
// @param vectors_file a full path to the vectors file
// @param verbose whether to report on progress
// @param report_every if \code{verbose} is TRUE
// @return A matrix with word vectors from \code{vectors_file} as rows
// [[Rcpp::export]]
NumericMatrix extract_words(CharacterVector words, // words to look for
                            CharacterVector vectors_file,
                            bool verbose, // whether should we report
                            int report_every) {
  std::string ccfile = Rcpp::as<std::string>(vectors_file);
  if (verbose)
    Rcerr << "Searching " << vectors_file << " for words" << "\n";

  int width = get_vector_dim(ccfile); // try to guess the dimensionality
  if (verbose)
    Rcerr << "Assuming all these vectors have " << width << " elements\n";

  NumericMatrix nm(words.size(), width);
  nm.fill(NumericVector::get_na());
  rownames(nm) = words;
  // fill a map with: std:string words_to_look_for -> row_of_nm_to_put_vector_in
  std::map<std::string,int> vocab;
  CharacterVector::iterator it;
  int ii = 0;
  for(it = words.begin(); it != words.end(); ++it) {
    std::string val = Rcpp::as<std::string>(*it);
    vocab.insert(std::pair<std::string,int>(val, ii));
    ii++;
  }
  std::vector<bool> done(words.size(), false);

  std::string line;
  std::ifstream vectors(ccfile);
  if (vectors.is_open()){
    int counter = 0;
    while (getline(vectors, line)){
      std::size_t space = line.find_first_of(" ");
      std::string word = line.substr(0, space);
      try {
        int row = vocab.at(word);
        if (verbose)
          Rcerr << "\nFound: " << word << "\n";
        std::istringstream buf(line);
        std::istream_iterator<std::string> beg(buf), end;
        std::vector<std::string> tokens(beg, end);
        for (int jj = 1; jj < tokens.size(); jj++){
          nm(row, jj-1) = std::stof(tokens[jj]);
        }
        done[row] = true;

        // bail out if we've got all the words we wanted
        int how_much = 0;
        for (int jj = 0; jj < words.size(); jj++)
          if (done[jj])
            how_much++;
        if (how_much == words.size()){
          if (verbose)
            Rcerr << "Found all the vectors" << "\n";
          break;
        }

      } catch(...){ }
      if ((counter % report_every) == 0){
        if (verbose)
          Rcerr << ".";
        Rcpp::checkUserInterrupt(); // we check every anyway
      }
      counter++;
    }
  }
  vectors.close();

  return nm;
}


