/******************************************************************************
  Benjamin Summers
  bsummer4
  cs302 Lab5
  Fall 2008
  simulation.cpp

  Implements methods for the Simulation class which does all the work
  in running the raid simulation.

  See:
  http://www.cs.utk.edu/~cs302/Labs/Lab5/
******************************************************************************/


#include "lab5.h"

/** Initializer for Simulation.  Store what arguments it needs, then
    set up the disks, and then add initial events to the event queue.
******************************************************************************/
Simulation::Simulation(int numDisks,
		       int seed,
		       double time_,
		       double beta_of,
		       double eta_of,
		       double lambda_lf,
		       double gamma_r,
		       double beta_r,
		       double eta_r,
		       double gamma_s_,
		       double beta_s_,
		       double eta_s_) {

  /** Copy needed arguments, initialize status, and seed the random
      number generator.  */
  time = 0;
  gamma_s = gamma_s_;
  beta_s = beta_s_;
  eta_s = eta_s_;
  status = GOOD_STATUS;
  srand48(seed);

  /** Build N+1 disks.  */
  disks.resize(numDisks+1);
  for(int ii=0; ii<numDisks+1; ii++)
    disks[ii] = new Disk(ii, 0, beta_of, eta_of, lambda_lf, gamma_r, beta_r, eta_r);

  
  /** Generate End of simulation event.  */
  addEvent(new Event(time_, DONE_EVENT));

  /** Generate latent failures events for each disk.  */
  for(int ii=0; ii<numDisks+1; ii++)
    addEvent(disks[ii]->genLatentFailure(time));

  /** Genrate the initial scrub event.  */
  addEvent(genScrub(time));

  /** Generate operational failures events for each disk.  */
  for(int ii=0; ii<numDisks+1; ii++)
    addEvent(disks[ii]->genOperationalFailure(time));
}


/** Add an event to the event queue.
******************************************************************************/
inline void Simulation::addEvent(Event* event) {
  double time = event->time;
  events.insert(make_pair(time, event));
}


/** Run the simulation until their is data loss or until we reach the
    end of the simulation time.
*****************************************************************************/
void Simulation::run() {
  multimap <double, Event*>::iterator eventIter;

  bool isDone = 0;
  while(!isDone) {

    /** Get the earlist event.  */
    eventIter = events.begin();
    
    /** Record the status of the system before handling the event.  */
    int previousStatus = status;

    /** Handle the event.  */
    isDone = handleEvent(eventIter->second);

    /** Print the event.  */
    printEvent(eventIter->second, previousStatus);

    /** Update simulation time.  */
    time = eventIter->second->time;

    /** Erase the old event from the event queue.  */
    events.erase(eventIter);
  }
}


/** Called when the simulation end event is encountered.  Return a
    bool repersenting whether the simulation is over.
*****************************************************************************/
bool Simulation::end(Event* current) {
  status = DONE_STATUS;
  return DONE;
}


/** Perform the required actions for a sector failure.  Return a bool
    repersenting whether the simulation is over.
*****************************************************************************/
bool Simulation::sfail(Event* e) {

  /** Make the disk have the sector failure.  */
  e->disk->sfail();

  /** Add a new latent failure event.  */
  addEvent(e->disk->genLatentFailure(e->time));

  /** Update the status, and handle status related stuff.  */
  return updateStatus(e); 
}


/** Perform the required actions for an operational failure.  Return a
    bool repersenting whether the simulation is over.
*****************************************************************************/
bool Simulation::ofail(Event* e) { 

  /** Make the disk have the sector failure.  */
  e->disk->ofail();

  /** Add a repair event.  */
  addEvent(e->disk->genRepair(e->time));

  /** Remove the latent failure event for the disk in question.  */

  // Rant: the way Dr Plank sudgests that we handle this is a clear
  // violation of the princibles of object-oriented design.  The code
  // in the disk class shouln't need know that the queue even exists.
  multimap <double, Event*>::iterator qit;
  for(qit = events.begin(); qit != events.end(); qit++) {
    Event* loopEvent = qit->second;
    if (loopEvent->disk == e->disk && loopEvent->type == SFAIL_EVENT)
      events.erase(qit); // This calls the destructor of the event.
  }

  /** Update the status, and handle status related stuff.  */
  return updateStatus(e);
}


/** Perform the required actions for a system scrub.  Return a bool
    repersenting whether the simulation is over.
*****************************************************************************/
bool Simulation::scrub(Event* e) {

  /** Scrub all latent failures.  */
  for(int ii=0; ii<(int)disks.size(); ii++)
      disks[ii]->scrub();
  
  /** Generate a new scrub event.  */
  addEvent(genScrub(e->time));

  return updateStatus(e);
}


/** Perform the required actions for repairing a disk.  Return a bool
    repersenting whether the simulation is over.
*****************************************************************************/
bool Simulation::repair(Event* e) { 
  /** Repair the disk.  */
  e->disk->repair();

  /** New sector Failure */
  addEvent(e->disk->genLatentFailure(e->time));
    
  /** New operational Failure */
  addEvent(e->disk->genOperationalFailure(e->time));
  


  return updateStatus(e);
}


/** generates a scrub event that happens at a random amount of time
    after the passed time.
*****************************************************************************/
Event* Simulation::genScrub(double currentTime) {
  double newTime = currentTime + myWeibull(gamma_s, beta_s, eta_s);
  return new Event(newTime, SCRUB_EVENT);
}


/** Update the status for an event.  Set the status and return whether
    the system simulation is over.
*****************************************************************************/
bool Simulation::updateStatus(Event* e) {
  switch(e->type) {

  case DONE_EVENT:
    return end(e);

  case OFAIL_EVENT: //fixme: anyway to make this cleaner?

    if (status == GOOD_STATUS) {
      status = OFAIL_STATUS;
      return NOT_DONE;
    }

    if (status == OFAIL_STATUS) {
      status = LOSS_STATUS;
      return DONE;
    }


    // If we make it here then status is SFAIL,

    /** Check all disks besides the current one for sector
	failures.  */
    for (int ii=0; ii<(int)disks.size(); ii++)
      if ((disks[ii] != e->disk) && (disks[ii]->latentFailures)) {
	status = LOSS_STATUS;
	return DONE; // simulation is over; we lost data.
      }
    
    /** All other disks are clean, so we go to op fail state (instead of dataLoss).  */
    status = OFAIL_STATUS;
    return NOT_DONE;
    
  case SFAIL_EVENT:
    if (status == OFAIL_STATUS) {
      status = LOSS_STATUS;
      return DONE;
    }
    else {
      status = SFAIL_STATUS;
      return NOT_DONE;
    }

  case REPAIR_EVENT:
    //if anything else was broken, we would already have had data loss.
    status = GOOD_STATUS;
    return NOT_DONE;
    
  case SCRUB_EVENT:
    if (status == SFAIL_STATUS)
      status = GOOD_STATUS;
    return NOT_DONE; //either way
  }
  
  throw "Invalid Event";
}


/** return a strring representing the simulation status.  
*****************************************************************************/
string Simulation::statusStr(int s) {
  switch(s) {
  case GOOD_STATUS: return "N+1-W&C";
  case OFAIL_STATUS: return "N-W&C";
  case SFAIL_STATUS: return ">=1-SF";
  case LOSS_STATUS: return "Data-Loss";
  case DONE_STATUS: return "DONE";
  }

  throw string("Invalid Type");
}


/** Call whatever method needs to be called for the type of the passed
    event.
******************************************************************************/
bool Simulation::handleEvent(Event* e) {
  switch(e->type) {
  case DONE_EVENT: return end(e);
  case OFAIL_EVENT: return ofail(e);
  case SFAIL_EVENT: return sfail(e);
  case REPAIR_EVENT: return repair(e);
  case SCRUB_EVENT: return scrub(e);
  }

  throw ("Invalid Event");
}


/** Print a line representing one event.  
******************************************************************************/
void Simulation::printEvent(Event* event, int prevStatus) {

  /**  Set up the diskNum String.  */
  char diskNum[4] = "   ";
  if(event->disk)
    sprintf(diskNum, "%3d", event->disk->id);

  /**  Set up the state String.  */
  char statStr[26] = "";
  if(status != DONE_STATUS)
    sprintf(statStr, "  %-9s ->   %-9s",
	    statusStr(prevStatus).c_str(),
	    statusStr(status).c_str());

  /** Actually print out the line.  */
  printf("%14.3lf %-21s %s%s\n",
	 event->time,
	 event->typeStr().c_str(),
	 diskNum,
	 statStr);
}

