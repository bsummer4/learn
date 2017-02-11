/*****************************************************************************/
// lab2.h: The classes and function prototypes for lab2.
/*****************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <string.h>

using namespace std;


/******************************************************************************
Class definitions.
******************************************************************************/


/** Handles storing lenghts of time. It stored the time as two
    numbers, minutes and seconds. Instances of this class can be added
    or outputed to a stream
******************************************************************************/
class Time {
 public:
  Time();
  Time(string input);
  Time(int mins_, int secs_);
  Time operator+ (const Time time) const;
  int mins;
  int secs;
};
ostream & operator<< (ostream & os, const Time time);


/** Stores the name, duration, and track number for a track.
******************************************************************************/
class Track {
 public:
  Track(string name_, Time time_, string genre_, int number_);
  Track* create(string name, Time time, string genre, int number);
  
  string name;
  Time time;
  string genre;
  int number;
};
ostream & operator<< (ostream& os, Track track);


/** Stores the name of an album and a map of track numbers to Track
    instances.  Knows how to get number of tracks and the total
    duration of the album from the tracks.
******************************************************************************/
class Album {
 public:
  Album(string album_name);
  void insert(string track_name,
	      Time time,
	      string genre,
	      int track_number);
  int numTracks();
  Time totalTime();

  string name;
  map <int,Track*> tracks;
};
ostream& operator<< (ostream& os, Album album);


/** Stores the name of an artist in addition to a map of albums names
    to Album instances.
******************************************************************************/
class Artist {
 public:
  Artist(string artist_name);
  void insert(string track_name,
	      Time time,
	      string album_name,
	      string genre,
	      int track_number);
  void output(ostream *output_stream);
  int numTracks();
  Time totalTime();
  
  string name;
  map <string, Album*> albums;
};
ostream & operator<< (ostream& os, Artist artist);


/******************************************************************************
Function prototypes:
******************************************************************************/

// io.cpp
map <string, Artist*> *read_file(ifstream *input_file);
void write_albums(map <string, Artist*> *artists, ostream *output_stream);

// misc.cpp
string removeUnderscores(string input);
