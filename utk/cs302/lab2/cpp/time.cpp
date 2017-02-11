/*****************************************************************************/
// time.cpp Implements all methods and operators for the time class.  The time 
/*****************************************************************************/
#include "lab2.h"
#include <iomanip>


/** Blank constructor for time.  Make a time with 0 mins and 0 secs.
******************************************************************************/
Time::Time() {
  mins = secs = 0;
}


/** Construct a time object from a string of the format "mins:secs".
******************************************************************************/
Time::Time(string input) {
  sscanf(input.c_str(), "%d:%d", &mins, &secs);
}


/** Construct a time object from two numbers, minutes and seconds.
******************************************************************************/
Time::Time(int mins_, int secs_) {
  mins = mins_;
  secs = secs_;
}


/** Adds two times together keeping the number of seconds below 60.
******************************************************************************/
Time Time::operator+ (const Time time) const {
  int result_secs = time.secs + secs;
  int result_mins = time.mins + mins;
  
  if (result_secs/60) {
    result_secs -= 60;
    result_mins++;
  }
  
  return Time(result_mins, result_secs);
}


/** Outputs a time object to a stream 'os.  Uses the 'mins:secs:'
    format.  The seconds are always two digits (eg. '03', '33')
******************************************************************************/
ostream & operator<< (ostream& os, const Time time) {
  os << time.mins << ":" << setw(2) << setfill('0') << time.secs;
  return os;
}
