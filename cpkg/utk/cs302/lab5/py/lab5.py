#!/usr/bin/python


import sys
from random import random
from math import log
from pprint import pprint


def exponential(lmbda):
    return -1.0 * log(1.0 - random()) / lmbda;

def weibull(gamma, beta, eta):
    tmp1 = -log(1.0 - random());
    tmp2 = pow(tmp1, 1.0/beta);
    return eta * tmp2 + gamma;


class Disk:

    def __init__(self, ofRate, lfRate, repairRate):
        self.lfRate = lfRate
        self.ofRate = ofRate
        self.repairRate = repairRate
        self.latentFailures = 0
        self.operationalFailures = False

    def latentFailure(self):
        self.latentFailures += 1

    def operationalFailure(self):
        self.operationalFailures = True

    def genOperationalFailure(self, currentTime):
        time = currentTime + apply(weibull, self.ofRate)
        return Event(time, "operationalFailure", self)

    def genLatentFailure(self, currentTime):
        time = currentTime + apply(exponential, self.lfRate)
        return Event(time, "latentFailure", self)

    def genRepair(self, currentTime):
        time = currentTime + apply(weibull, self.repairRate)
        return Event(time, "repair", self)

    def repair(self):
        self.operationalFailure = False

    def scrub(self):
        self.latentFailures = 0


class Event:

    def __init__(self, time, type, disk=None):
        self.time = time 
        self.type = type
        self.disk = disk


    def cmp(x, y):
        if x.time == y.time: return 0
        if x.time < y.time: return 1
        return -1


    def __repr__(self):
        result = ""
        result += "event"
        result += str(self.type)
        result += " "
        result += str(self.time)
        return result


class Simulation():

    def __init__(self, disks, time, ofRate, lfRate, scrub, repair):

        self.time = 0
        self.scrubRate = scrub
        
        # Make 'disks disk objects
        self.disks = [Disk(ofRate, lfRate, repair) for ii in range(disks)]

        # Generate events
        self.events = [Event(time, "done")]
        self.addEvent(self.genScrub())
        for disk in self.disks:
            self.addEvent(disk.genLatentFailure(self.time))
            self.addEvent(disk.genOperationalFailure(self.time))


    def run(self):
        while(True): 
            event = self.events.pop()

            # Update the simulation time.
            self.time = event.time

            # Do whatever needs to be done for the event
            actions = {"done": self.done,
                       "scrub": self.scrub,
                       "dataLoss": self.dataLoss,
                       "operationalFailure": self.operationalFailure,
                       "latentFailure": self.latentFailure,
                       "repair": self.repair}
            actions[event.type](event)


    def dataLoss():
        raise Exception("You data is fucking gone motherfucker!")


    def latentFailure(self, event):
        disk = event.disk

        # Make the latent failure actually happen
        disk.latentFailure()

        # Make a new latent failure event
        self.addEvent(disk.genLatentFailure(self.time))

        
    def operationalFailure(self, event):
        disk = event.disk
        
        # Make the disk fail
        event.disk.operationalFailure()
        
        # Generate repair event
        self.addEvent(disk.genRepair(self.time))

        # Remove any latent failure events for the failed disk
        for loopEvent in self.events:
            if loopEvent.type == "latentFailure" and loopEvent.disk == event.disk:
                self.events.remove(loopEvent)
                
                
    def done(self, event):
        raise Exception("Done")
    
    
    def scrub(self, event):
        
        # Scrub any latent failures.
        for disk in self.disks:
            disk.scrub()

        # Generate a new scrub event.
        self.addEvent(self.genScrub())
        
    
    def repair(self, event):
        disk = event.disk

        # repair the disk
        disk.repair()

        # add new failure events to queue
        self.addEvent(disk.genLatentFailure(self.time))
        self.addEvent(disk.genOperationalFailure(self.time))
        

    def status(self):
        
        # Calculate the number of failures in all disks
        brokenDisks=0
        latentFailures=0
        for disk in self.disks:
            latentFailures += disk.latentFailures
            if disk.operationalFailure:
                brokenDisks += 1

        #fixme: Is there some way to do a truth table?
        if not brokenDisks and not latentFailures:
            return "N+1-W&C"
        if brokenDisks == 1 and not latentFailures:
            return "N-W&C"
        if not brokenDisks and latentFailures:
            return ">=1-SF"
        return "Data-Loss"

        
    def addEvent(self, event):
        self.events.append(event)
        self.events.sort(Event.cmp)


    def genRepair(self):
        time = self.time + apply(weibull, self.repairRate)
        return Event(time, "repair")
        

    def genScrub(self):
        time = self.time + apply(weibull, self.scrubRate)
        return Event(time, "scrub")
        

def main():

    def bad():
        usage = ""
        usage += "usage: raid N seed time beta_of eta_of lambda_lf gamma_r"
        usage += "beta_r eta_r gamma_s beta_s eta_s"
        raise Exception(usage)

    if len(sys.argv) != 13:
        bad()


    def positiveFloat(x):
        return type(x)==float and x>0

    def positiveInt(x):
        return type(x)==int and x>0

    def nonNegativeFloat(x):
        return type(x)==float and x>=0

    #fixme: better way?
    x = time, beta_of, beta_r, beta_s, lambda_lf, eta_of, eta_r, eta_s, gamma_r, gamma_s
    (time, beta_of, beta_r, beta_s, lambda_lf, eta_of, eta_r, eta_s, gamma_r, gamma_s) = map(float, x)

    assertions = (
        positiveInt(disks),
        type(seed) == int,
        positiveFloat(time),
        positiveFloat(beta_of),
        positiveFloat(beta_r),
        positiveFloat(beta_s),
        positiveFloat(lambda_lf),
        positiveFloat(eta_of),
        positiveFloat(eta_r),
        positiveFloat(eta_s),
        nonNegativeFloat(gamma_r),
        nonNegativeFloat(gamma_s))
    
    for assertion in assertions:
        if not assertion:
            bad()

    latentFailureRate = tuple([lambda_lf])
    operationalFailureRate = (0.0, beta_of, eta_of)
    scrub = (gamma_s, beta_s, eta_s)
    repair = (gamma_r, beta_r, eta_r)
    
    simulation = Simulation(disks, time,
                            operationalFailureRate, latentFailureRate,
                            scrub, repair)
    simulation.run()

if __name__ == "__main__":
    sys.argv = ["raid", 7, 0, 87600, 1.12, 461386, 0.000108003, 6, 2, 12, 36, 3, 168]
    main()
