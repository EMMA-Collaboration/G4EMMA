//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// $Id: EMMADriftChamber.hh,v 1.4 2006-06-29 16:30:53 gunter Exp $
// --------------------------------------------------------------
//
#ifndef EMMADriftChamber_h
#define EMMADriftChamber_h 1

#include "G4VSensitiveDetector.hh"
#include "EMMADriftChamberHit.hh"
class G4Step;
class G4HCofThisEvent;
class G4TouchableHistory;

class EMMADriftChamber : public G4VSensitiveDetector
{

  public:
  
	//! Constructor.
	
      EMMADriftChamber(G4String name);
	  
	//! Destructor
	
      virtual ~EMMADriftChamber();

	//! Method Initialize, which doesn't return anything, 
	//! takes in one input of type G4HCofThisEvent*HCE
	//! Method ProcessHits, which returns a G4bool,
	//! takes in two inputs of type G4Step*aStep and G4TouchableHistory*ROhist
	//! Method EndOfEvent, which doesn't return anything,
	//! takes in one input of type G4HCofThisEvent*HCE,
	//! and ends the event (it doesn't do anything)
	
      virtual void Initialize(G4HCofThisEvent*HCE);
      virtual G4bool ProcessHits(G4Step*aStep,G4TouchableHistory*ROhist);
      virtual void EndOfEvent(G4HCofThisEvent*HCE);

  private:
  
	//! hitsCollection is an instance of the
	//! EMMADriftChamberHitsCollection object
	//! also declares private variable HCID of type G4int
	
      EMMADriftChamberHitsCollection * hitsCollection;
      G4int HCID;
};




#endif

