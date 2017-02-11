#include <math.h>
#include <stdio.h>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <vector>

using namespace std;


class Simulation;
class Disk;
class Event;

double myWeibull(double gamma, double beta, double eta);
double myExponential(double lambda);




class Disk {

 public:
  int id; // fixme: private

  Disk(int id_,
       double gamma_of,
       double beta_of,
       double eta_of,
       double lambda_lf,
       double gamma_r,
       double beta_r,
       double eta_r);

  int operationalFailures;
  int latentFailures;

  void fail();// { operationalFailures++; }
  void sectorFail();// { latentFailures++; }
  void repair();
  void scrub();
  string statusStr();

  Event* genOperationalFailure(double currentTime);
  Event* genLatentFailure(double currentTime);
  Event* genRepair(double currentTime);

  void sfail();
  void ofail();

 private:
  double gamma_of;
  double beta_of;
  double eta_of;
  double lambda_lf;
  double gamma_r;
  double beta_r;
  double eta_r;
};



/** Event types.  */
enum {DONE_EVENT, OFAIL_EVENT, SFAIL_EVENT, REPAIR_EVENT, SCRUB_EVENT};

class Event {

 public:
  Event(double time_, int type_, Disk* disk_);
  Event(double time_, int type_);
  string typeStr();
  double time;
  int type;
  Disk* disk;
};


enum {GOOD_STATUS, LOSS_STATUS, OFAIL_STATUS, SFAIL_STATUS, DONE_STATUS};
enum {NOT_DONE, DONE}; // flag for ending loop

class Simulation {
  
 public:
  Simulation(int disks,
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
	     double eta_s_);
  void run();
  

  // private:
  multimap <double, Event*> events;
  vector <Disk*> disks;
  int status;
  double time;
  double gamma_s;
  double beta_s;
  double eta_s;

  Event *genScrub(double currentTime);

  bool handleEvent(Event* current);
  void printEvent(Event* event, int prevStatus);
  void addEvent(Event* event);
  string statusStr(int s);
  bool updateStatus(Event* e);

  bool dataLoss(Event *event);
  bool end(Event* current);
  bool sfail(Event* current);
  bool ofail(Event* current);
  bool scrub(Event* current);
  bool repair(Event* current);
};
