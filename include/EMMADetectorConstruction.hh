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
// $Id: A01DetectorConstruction.hh,v 1.6 2009-11-21 00:22:55 perl Exp $
// --------------------------------------------------------------
//

#ifndef EMMADetectorConstruction_h
#define EMMADetectorConstruction_h 1

#include "globals.hh"
#include "G4VUserDetectorConstruction.hh"
#include "G4RotationMatrix.hh"



// global variable
extern G4double zQ1begins; // see SpectrometerConstruction.cc
extern G4double zQ4ends; // SpectrometerConstruction.cc
extern G4double zAnode; // SpectrometerConstruction.cc
extern G4double zFocalPlane; // SpectrometerConstruction.cc
extern G4String MotherDir; // EMMAapp.cc
extern G4String UserDir; // EMMAapp.cc


class G4VPhysicalVolume;
class G4Material;
class G4VSensitiveDetector;
class G4VisAttributes;
class G4UserLimits;
class EMMADetectorConstMessenger;
class SpectrometerConstruction;

class EMMADetectorConstruction : public G4VUserDetectorConstruction
{
public:
  EMMADetectorConstruction();
  virtual ~EMMADetectorConstruction();
  void CalculateScalingFactors();  
  void ReadUserInput();

public:
  virtual G4VPhysicalVolume* Construct();
  
private:
  void ConstructMaterials();
  void DestroyMaterials();
  void DumpGeometricalTree(G4VPhysicalVolume* aVolume,G4int depth=0);
  
private:	
  SpectrometerConstruction* Spectrometer;	
  EMMADetectorConstMessenger* messenger;
  G4Material* air;
  G4Material* Vacuum;
  G4Material* Aluminum;
  G4Material* lead;
  G4Material* graphite;  
  G4Material* CD2;  
  G4Material* silicon;  
  G4Material* mylar;
  G4Material* isobutane;
  G4Material* isobutaneIC;
  G4VisAttributes* worldVisAtt;  
  G4VisAttributes* cellVisAtt;
  G4VisAttributes* WallVisAtt;
  G4VisAttributes* wedgeVisAtt;
  G4VisAttributes* BeamLineVisAtt;
  G4VisAttributes* PoleVisAtt;
  G4VisAttributes* BendingVisAtt; 
  G4UserLimits* stepLimit;             // pointer to user step limits 
  //G4VPhysicalVolume* EMMAPhys;  
  //G4VPhysicalVolume* targetBoxPhys;
  //  G4VPhysicalVolume* spectrometerBoxPhys;
  G4VPhysicalVolume* detectorBoxPhys;

private:
  G4double centralZ;
  G4double centralA;
  G4double centralQ;
  G4double centralE;

private:
  G4bool insertTarget;
  G4Material* targetMaterial;  
  G4bool insertDegrader1;
  G4double degrader1Thickness;
  G4Material* degrader1Material;  
  G4bool insertDegrader2;
  G4double degrader2Thickness;
  G4Material* degrader2Material;  
  G4bool insertMWPC;
  G4bool insertIC;
  G4double MWPCwindowCathodeSeparation;
  G4double MWPCgasThickness;
  G4double MWPCwindowThickness;
  G4double ICwindowThick;
  G4double pTorr;
  G4double pTorrIC;
  G4double TCelsius;
  G4bool fCheckOverlaps;
  
public:
  inline void SetCentralZ(G4double val) { centralZ = val; }
  inline G4double GetCentralZ() const { return centralZ; }
  inline void SetCentralA(G4double val) { centralA = val; }
  inline G4double GetCentralA() const { return centralA; }  
  inline void SetCentralQ(G4double val) { centralQ = val; }
  inline G4double GetCentralQ() const { return centralQ; }  
  inline void SetCentralE(G4double val) { centralE = val; }
  inline G4double GetCentralE() const { return centralE; }  

};

#endif

