/*****************************************************************************/
// track.cpp Implements all methods and operators for the track class.
/*****************************************************************************/

#include "lab2.h"

/** Constructor for a track. Simply store all the passed
    imformation.  
******************************************************************************/
Track::Track(string name_, Time time_, string genre_, int number_):
  time(time_) {
  name = name_;
  time = time_;
  genre = genre_;
  number = number_;
}

/** Write out the track imformation to the stream 'os.  
******************************************************************************/
ostream & operator<< (ostream& os, Track track) {
  os << "        " << "        ";
  os << track.number << ". ";
  os << track.name << ": ";
  os << track.time << endl;
  return os;
}
