/*****************************************************************************/
// album.cpp Implements all methods and operators for the album class.
/*****************************************************************************/
#include "lab2.h"


/** Constructor for the album;  Just save the name.  
******************************************************************************/
Album::Album(string name_) {
  name = name_;
}


/** Insert a new track into our number-track map.
    
    There should never be two of the same tracks, and we don't
    validate the input file, so We don't need to make sure the track
    isn't already present.
******************************************************************************/
void Album::insert(string track_name,
		   Time time,
		   string genre,
		   int track_number) {
  Track *track = new Track(track_name, time, genre, track_number);
  tracks.insert(make_pair(track_number, track));
}


/** Returns the number of tracks in the album.  
******************************************************************************/
inline int Album::numTracks() {
  return tracks.size();
}


/** Returns the total time of all the tracks in the album.  
******************************************************************************/
Time Album::totalTime() {
  map <int, Track*>::iterator track_iter = tracks.begin();
  Time result; // Initialized to zero time automatically

  /** Loop through the tracks, accumulating their lengths.  */  
  while(track_iter != tracks.end()) {
    result = result + track_iter->second->time;
    track_iter++;
  }
  
  return result;
}


/** Output the album and all tracks to the stream 'os.  
******************************************************************************/
ostream & operator<< (ostream& os, Album album) {
  os << "        " << album.name << ": ";
  os << album.numTracks() << ", ";
  os << album.totalTime() << endl;

  /** Output all tracks.  */  //fixme: write and use a templat for this.
  map <int, Track*>::iterator track_iter = album.tracks.begin();
  while(track_iter != album.tracks.end()) {
    Track* track = track_iter->second;
    os << *track;
    track_iter++;
  }
  
  return os;
}
