#include <Rcpp.h>
using namespace Rcpp;

// Source:
// https://dirk.eddelbuettel.com/papers/useR2019_rcpp_tutorial.pdf p. 54
// https://www.w3schools.com/cpp/cpp_classes.asp
// https://www.programiz.com/cpp-programming/object-class

class myDate {
private:
  unsigned int year;
  unsigned int month;
  unsigned int day;
  
public:
  // Constructor
  myDate(int y, int m, int d) : year(y), month(m), day(d) {}
  
  // Set date method
  void setDate(int y, int m, int d) {
    year = y;
    month = m;
    day = d;
  }
  
  // Getter methods
  int getDay() { return day; }
  int getMonth() { return month; }
  int getYear() { return year; }
  
  // Print formatted date
  void calculateDate() {
    Rcout << getDay() << "-" << getMonth() << "-" << getYear() << std::endl;
  }
};

// [[Rcpp::export]]
void test_myDate(int y, int m, int d) {
  myDate date(y, m, d);
  date.calculateDate();
}


