/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  events.cpp

  Implements methods for the Events class which are stored on an event
  queue in the Simulation class.  

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab5/
******************************************************************************/


#include "lab5.h"


/** Constructor.  Simply stores the arguments.  */
Event::Event(double time_, int type_, Disk* disk_) {
  time = time_;
  type = type_;
  disk = disk_;
}


/** Constructor.  overloading; No disk means that the disk pointer is
    null.  */
Event::Event(double time_, int type_) {
  time = time_;
  type = type_;
  disk = NULL;
}


/** Return a string for the type of event we represent.   */
string Event::typeStr() {
  switch(type) {
  case DONE_EVENT: return "Simulation_Over";
  case OFAIL_EVENT: return "Operational_Failure";
  case SFAIL_EVENT: return "Latent_Sector_Failure";
  case REPAIR_EVENT: return "Repair";
  case SCRUB_EVENT: return "Scrub";
  }

  /** This should never happen.  */
  throw string("Invalid Event");
}
