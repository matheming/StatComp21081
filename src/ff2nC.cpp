#include <Rcpp.h>
using namespace Rcpp;

//' @title A function generates an index matrix
//' @description A function generates an index matrix
//' @param p the dimension of the matrix, \code{p} shouldn't be large
//' @return a \code{2^s*s} index matrix filled with 0 and 1
//' @examples 
//' \dontrun{
//' Index <- ff2n(3)
//' print(Index)
//' }
//' @export
// [[Rcpp::export]]
IntegerMatrix ff2nC(int p) {
  IntegerMatrix mat(pow(2,p), p);
  IntegerMatrix mat0(1,p);
  IntegerMatrix mat1(1,p);
  for(int i=0; i < p; i++){
    if(i==0){
      mat(0,i) = 0;
      mat0(0,i) = 1;
      mat1(0,i) = 0;
    }
    else{
      mat(0,i) = 0;
      mat0(0,i) = 0;
      mat1(0,i) = 0;
    }
  }
  for(int i=1; i < pow(2,p); i++){
    for(int k=0; k < p; k++){
      mat1(0,k) = mat1(0,k) + mat0(0,k); 
    }
    for(int j=0; j < p; j++){
      if(mat1(0,j)==2){
        mat1(0,j+1) = mat1(0, j+1) + 1;
        mat1(0,j) = 0;
      }
      mat(i,j) = mat1(0,j);
    }
  }
  return(mat);
}