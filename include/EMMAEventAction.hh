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
#include "EMMASiliconDetector.hh"
<<<<<<< HEAD
=======
#include "EMMAPrimaryGeneratorAction.hh"
>>>>>>> 328d247d31d8d865b2def4e9637587fef9e7941b

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
    EMMAEventAction();
    virtual ~EMMAEventAction();


  public:
    virtual void BeginOfEventAction(const G4Event*);
    virtual void EndOfEventAction(const G4Event*);

  private:
    G4int DHC2ID;
    G4ThreeVector localPos;
    G4double theta;
    G4double Edep, Edep2;
    G4double EdepSilicon;
    G4double Ekin;
<<<<<<< HEAD
    G4double fp_pos[2],fp_theta,fp_Ekin,fp_Edep,fp_Edep2,fp_2DEdep[2],fp_posX,fp_Edep_Silicon;
=======
    G4double fp_posX, fp_posY, fp_theta,fp_Ekin,fp_Edep,fp_Edep2,fp_2DEdep[2],fp_Edep_Silicon;
    G4double target_posX, target_posY, target_Ekin_tree, target_angX, target_angY;
    G4double target_x;
    G4double target_y;
    G4double target_xang;
    G4double target_yang;
    G4double target_Ekint;
>>>>>>> 328d247d31d8d865b2def4e9637587fef9e7941b

    EMMAEventActionMessenger* messenger;
    G4int verboseLevel;

    EMMAIonChamberHitsCollection* GetHitsCollection(const G4String& hcName,
						const G4Event* event) const;

   // EMMASiliconDetectorHitsCollection* GetHitsCollection(const G4String& hcName,
//						const G4Event* event) const;

<<<<<<< HEAD
    void PrintEventStatistics(G4double IonChamberEdep, G4double IonChamberTrackLength, G4double SiliconDetectorEdep, G4double SiliconDetectorTrackLength) const; 
=======
    void PrintEventStatistics(G4double IonChamberEdep, G4double IonChamberTrackLength, G4double SiliconDetectorEdep, G4double SiliconDetectorTrackLength) const;
>>>>>>> 328d247d31d8d865b2def4e9637587fef9e7941b

#ifdef G4ANALYSIS_USE
	TFile* rootfile;
	TTree* fp_tree;
  TTree* target_tree;
  TH1F* target_Ekin;
  TH2F* target_pos;
  TH2F* target_dir;
	TH2F* fp_hitpos;
	TH1F* fp_hitposX;
	TH1F* fp_hitangle;
  TH1F* fp_hitEkin;
	TH1F* fp_hitEdep;
<<<<<<< HEAD
        TH1F* fp_hitEdep2;
=======
  TH1F* fp_hitEdep2;
>>>>>>> 328d247d31d8d865b2def4e9637587fef9e7941b
	TH1F* fp_hitEdep_Silicon;
	TH2F* fp_hit2DEdep;
#endif // G4ANALYSIS_USE

  public:
    inline void SetVerbose(G4int val) { verboseLevel = val; }
    inline G4int GetVerbose() const { return verboseLevel; }
};

#endif
