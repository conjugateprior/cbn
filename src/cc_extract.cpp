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

// [[Rcpp::export]]
NumericMatrix extract_words(CharacterVector p, // words to look for
                            int width, // dimensionality of these vectors
                            CharacterVector cc_path,
                            bool verbose, // should we report
                            int report_every) // every how many lines
{
  std::string ccfile = Rcpp::as<std::string>(cc_path);
  if (verbose)
    Rcerr << "Searching " << cc_path << " for words" << "\n";

  NumericMatrix nm(p.size(), width);
  nm.fill(NumericVector::get_na());
  rownames(nm) = p;
  // fill a map with: std:string words_to_look_for -> row_of_nm_to_put_vector_in
  std::map<std::string,int> vocab;
  CharacterVector::iterator it;
  int ii = 0;
  for(it = p.begin(); it != p.end(); ++it) {
    std::string val = Rcpp::as<std::string>(*it);
    vocab.insert(std::pair<std::string,int>(val, ii));
    ii++;
  }
  std::vector<bool> done(p.size(), false);

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
        for (int jj = 0; jj < p.size(); jj++)
          if (done[jj])
            how_much++;
        if (how_much == p.size()){
          if (verbose)
            Rcerr << "Found all the words. Closing the file" << "\n";
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


