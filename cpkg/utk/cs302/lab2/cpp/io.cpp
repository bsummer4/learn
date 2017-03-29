/*****************************************************************************/
// artist.cpp: Deals with reading a file containing a list of tracks
// and outputing them to stdout.  Formatting details at:
// "http://www.cs.utk.edu/~plank/plank/classes/cs302/Labs/Lab2"
/*****************************************************************************/
#include "lab2.h"

/** Read a file and add all it's imformation to a map of strings and
    artists.  Return a pointer to that map. 
******************************************************************************/
map <string, Artist*> *read_file(ifstream *input_file) {
  map <string, Artist*> *result = new map <string, Artist*>;
  map <string, Artist*>::iterator result_iter;
  Artist* artist;
  
  string track_name, timeStr, artist_name, album_name, genre;
  int track_number;
 
  while(1) {

    *input_file >> track_name >> timeStr >> artist_name;
    *input_file >> album_name >> genre >> track_number;
    Time time(timeStr); // fixme: time has >> operator;
    artist_name = removeUnderscores(artist_name);
    album_name = removeUnderscores(album_name);
    track_name = removeUnderscores(track_name);


    if(input_file->eof()) break;
    
    result_iter = result->find(artist_name);
    bool is_new = (result_iter == result->end());
    if (is_new) artist = new Artist(artist_name);
    else artist = result_iter->second;
    artist->insert(track_name, time, album_name, genre, track_number);
    if (is_new) result->insert(make_pair(artist_name, artist));
  }
  
  return result;
}


/** Simply iterate through a map of string to artists and output each
    artist.  */
//fixme: this code is duplicated in several places. write a template
//that handles all of the cases.
void write_albums(map <string, Artist*> *artists, ostream *os) {
  map <string, Artist*>::iterator artist_iter;
  
  artist_iter = artists->begin();
  while(artist_iter != artists->end()) {
    Artist *artist = (artist_iter->second);
    *os << *artist;
    artist_iter++;
  }
}
