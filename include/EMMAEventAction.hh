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
// $Id: EMMAEventAction.hh,v 1.6 2006-06-29 16:31:04 gunter Exp $
// --------------------------------------------------------------
//
#ifndef EMMAEventAction_h
#define EMMAEventAction_h 1


#include "G4UserEventAction.hh"
#include "G4ThreeVector.hh"
#include "globals.hh"
#include "EMMADriftChamberHit.hh"
#include "EMMAIonChamber.hh"

#ifdef G4ANALYSIS_USE

#include <TFile.h>
#include <TH2F.h>
#include <TH1F.h>
#include <TFolder.h>
#include <TTree.h>
#include <TROOT.h>
#include <TAxis.h>

#endif // G4ANALYSIS_USE

class EMMAEventActionMessenger;

class EMMAEventAction : public G4UserEventAction
{
  public:
  //! Constructor and Destructor.
    EMMAEventAction();
    virtual ~EMMAEventAction();

  public:
  //! Public virtual methods BeginOfEventAction and
  //! EndOfEventAction takes in a G4Event* and returns nothing
    virtual void BeginOfEventAction(const G4Event*);
    virtual void EndOfEventAction(const G4Event*);

  private:
  //! Private variables DHC2ID of type G4int, localPos of type
  //! G4ThreeVector, and theta, Edep, Ekin, fp_pos[2], fp_theta,
  //! fp_Ekin, fp_Edep, fp_posEdep[2], fp_posX all of type G4double
    G4int DHC2ID;
    G4ThreeVector localPos;
    G4double theta;
    G4double Edep;
    G4double Ekin;
    G4double fp_pos[2],fp_theta,fp_Ekin,fp_Edep,fp_posEdep[2],fp_posX;

  //! Private variables messenger of type EMMAEventActionMessenger*, and
  //! verboseLevel of type G4int
    EMMAEventActionMessenger* messenger;
    G4int verboseLevel;

  //! Private method GetHitsCollection takes in a const G4String&, a const
  //! G4Event*, and returns an EMMAIonChamberHitsCollection* (a count of
  //! the number of hits, while private method PrintEventStatistics takes in
  //! two g4double, returns nothing but prints event statistics
    EMMAIonChamberHitsCollection* GetHitsCollection(const G4String& hcName,
						const G4Event* event) const;
    void PrintEventStatistics(G4double IonChamberEdep, G4double IonChamberTrackLength) const; 

#ifdef G4ANALYSIS_USE
  //! Private variables rootfile of type TFile*, fp_tree of type TTree*,
  //! fp_hitpos, fp_hitposEdep both of type TH2F*, and
  //! fp_hitposX, fp_hitangle, fp_hitEkin, fp_hitEdep, all of type TH1F*, 
	TFile* rootfile;
	TTree* fp_tree;
	TH2F* fp_hitpos;
	TH1F* fp_hitposX;
	TH1F* fp_hitangle;
        TH1F* fp_hitEkin;
	TH1F* fp_hitEdep;
	TH2F* fp_hitposEdep;
#endif // G4ANALYSIS_USE

  public:
  //! Public inline methods to set and get verboseLevel
    inline void SetVerbose(G4int val) { verboseLevel = val; }
    inline G4int GetVerbose() const { return verboseLevel; }
};

#endif
