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
//
// $Id: ExN02SteppingAction.hh,v 1.8 2006-06-29 17:47:48 gunter Exp $
// GEANT4 tag $Name: geant4-09-04-patch-02 $
// 
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#ifndef EMMASteppingAction_h
#define EMMASteppingAction_h 1

#include "G4UserSteppingAction.hh"
#include "EMMADetectorConstruction.hh"
#include "EMMAGlobalField.hh"
#include "EMMAElementField.hh"

#ifdef G4ANALYSIS_USE

#include <TFile.h>
#include <TH2F.h>
#include <TH1F.h>
#include <TFolder.h>
#include <TTree.h>
#include <TROOT.h>

#endif // G4ANALYSIS_USE


// Global constants
extern G4double targetThickness;

// Global variables from EMMAPrimaryGeneratorAction.cc
extern G4bool prepareBeam;
extern G4String inTargetFileName;
extern G4String postTargetFileName;
extern G4String postDegrader1FileName;
extern G4double depth;

// Global variables from EMMAapp.cc
extern G4int NOHslits1;
extern G4int NOHslits2;
extern G4int NOHslits3;
extern G4int NOHslits4;

// Global variables from SpectrometerConstruction.cc
extern G4double zQ1ends;
extern G4double zQ2ends;
extern G4double zQ3ends;
extern G4double zQ4ends;

// Global variables from EMMADetectorConstruction.cc
extern G4double magneticScaling;
extern G4double electricScaling;


//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

class EMMASteppingAction : public G4UserSteppingAction
{
public:
  EMMASteppingAction();
  virtual ~EMMASteppingAction();
  
  virtual void UserSteppingAction(const G4Step* theStep);
  
  G4int getDeadInt() {return deadint;}
  G4bool getDead() {return dead;}
  
private:
  G4int deadint;
  G4bool dead;
  
  G4String element[8];
    
#ifdef G4ANALYSIS_USE
  TH1I* dead_hit;
#endif // G4ANALYSIS_USE
  
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
