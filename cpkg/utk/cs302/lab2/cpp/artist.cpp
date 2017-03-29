/*****************************************************************************/
// artist.cpp: Implements all methods and operators for the Album
// class.
/*****************************************************************************/
#include "lab2.h"


/** Constuctor for an artist.  Just saves the name.
******************************************************************************/
Artist::Artist(string artist_name) {
  name = artist_name;
}


/** Insert a new track into our name-album map.  Adds albums if neccesary
    and inserts the track into the album.
******************************************************************************/
void Artist::insert(string track_name,
		    Time time,
		    string album_name,
		    string genre,
		    int track_number) {

  Album *album; // The album to insert the track into.
  map <string, Album*>::iterator album_iter;
  
  /** Either find an album with name 'album_name or create a new
      one.  */  
  album_iter = albums.find(album_name);
  bool exists = (album_iter != albums.end());
  if (exists)
    album = album_iter->second;
  else
    album = new Album(album_name);
  
  /** Insert the track into the album.  */  
  album->insert(track_name,
		time,
		genre,
		track_number);
  
  /** Insert the album into our map if it's not already there.  */  
  if(!exists)
    albums.insert(make_pair(album_name, album));
}


/** Retutns the total number of tracks in all albums this artist has
    written.  
******************************************************************************/
int Artist::numTracks() {
  map <string, Album*>::iterator album_iter = albums.begin();
  int result=0;
  
  /** Add up the total number of tracks for all albums.  */
  while(album_iter != albums.end()) {
    result += album_iter->second->numTracks();
    album_iter++;
  }
  
  return result;
}


/** Returns the total time for all the albums this artist has
    written.  
******************************************************************************/
Time Artist::totalTime() {
  map <string, Album*>::iterator album_iter = albums.begin();
  Time result; // Initialized to zero time automatically.
  
  /** Loop through the albums, accumulating their lengths.  */
  while(album_iter != albums.end()) {
    Time albumTime = album_iter->second->totalTime();
    result = result + albumTime; //fixme: Maybe write += operator?
    album_iter++;
  }
  
  return result;
}


/** Output the Artist and all his/her albums to a stream.  
******************************************************************************/
ostream & operator<< (ostream& os, Artist artist) {
  os << artist.name << ": ";
  os << artist.numTracks() << ", ";
  os << artist.totalTime() << endl;
  
  /** Output all albums.  */ //fixme: write and use a templat for this.
  map <string, Album*>::iterator album_iter = artist.albums.begin();
  while(album_iter != artist.albums.end()) {
    Album* album = album_iter->second;
    os << *album;
    album_iter++;
  }

  return os;
}
