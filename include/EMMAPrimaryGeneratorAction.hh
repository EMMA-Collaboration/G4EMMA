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
// $Id: EMMAPrimaryGeneratorAction.hh,v 1.4 2006-06-29 16:31:35 gunter Exp $
// --------------------------------------------------------------
//

#ifndef EMMAPrimaryGeneratorAction_h
#define EMMAPrimaryGeneratorAction_h 1

#include "G4VUserPrimaryGeneratorAction.hh"
#include "globals.hh"
#include "Randomize.hh"
#include "G4Element.hh"
#include "G4ElementVector.hh"
#include "G4ElementTable.hh"
#include "G4PhysicsTable.hh"
#include "G4PhysicsVector.hh"
#include "G4LPhysicsFreeVector.hh"
#include "G4ThreeVector.hh"


// global variables 
extern G4double targetThickness; // EMMADetectorConstMessenger.cc
extern G4double targetZoffset; // EMMADetectorConstMessenger.cc
extern G4String MotherDir; // EMMAapp.cc
extern G4String UserDir; // EMMAapp.cc


class G4ParticleGun;
class G4Event;
class G4ParticleDefinition;
class EMMAPrimaryGeneratorMessenger;


class EMMAPrimaryGeneratorAction : public G4VUserPrimaryGeneratorAction
{

public:
  EMMAPrimaryGeneratorAction();
  virtual ~EMMAPrimaryGeneratorAction();
  virtual void GeneratePrimaries(G4Event*);
  	
private:
  G4ParticleGun* particleGun;
  G4ParticleGun* particleGun2;
  EMMAPrimaryGeneratorMessenger* gunMessenger;
  G4double beamZ, beamA, beamCharge;
  G4double energy;
  G4double sigmaEnergy;
  G4double Angle;
  G4double transEmittance;
  G4double beamSpotDiameter;
  G4double mass;
  G4double * energyBeam;
  G4double * posxBeam;
  G4double * posyBeam;
  G4double * poszBeam;
  G4double * dirxBeam;
  G4double * diryBeam;
  G4double * dirzBeam;
  G4int nEvents;
  G4double fZ1,fA1, fZ2,fA2;
  G4double fZ3,fA3, fZ4,fA4;
  G4double fqmin,fqmax;
  G4double fCharge3;
  G4double fExcitationEnergy3;
  G4bool simulateReaction;
  G4bool useAlphaSource;
  G4double energyAlphaSource;
  G4double maxAngleAlphaSource;

public:
  void initializeReactionSimulation();
  void initializeBeamSimulation();
  void initializeBeamPreparation();

  void simulateTwoBodyReaction( G4double &Ebeam, G4ThreeVector &dir);

  inline void SetBeamZ(G4double val) { beamZ = val; }
  inline G4double GetBeamZ() const { return beamZ; }
  inline void SetBeamA(G4double val) { beamA = val; }
  inline G4double GetBeamA() const { return beamA; }
  inline void SetBeamCharge(G4double val) { beamCharge = val; }
  inline G4double GetBeamCharge() const { return beamCharge; }

  inline void SetEnergy(G4double val) { energy = val; }
  inline G4double GetEnergy() const { return energy; }
  inline void SetSigmaEnergy(G4double val) { sigmaEnergy = val; }
  inline G4double GetSigmaEnergy() const { return sigmaEnergy; }
  inline void SetAngle(G4double val) { Angle = val; }
  inline G4double GetAngle() const { return Angle; }
  inline void SetTransEmittance(G4double val) { transEmittance = val; }
  inline G4double GetTransEmittance() const { return transEmittance; }
  inline void SetBeamSpotDiameter(G4double val) { beamSpotDiameter = val; }
  inline G4double GetBeamSpotDiameter() const { return beamSpotDiameter; }

  inline G4double GetMass() const { return mass; }

  inline void SetNEvents(G4int val) { nEvents = val; }
  inline G4int GetNEvents() const { return nEvents; }

  inline void SetZ1(G4double val) { fZ1 = val; }
  inline G4double GetZ1() const { return fZ1; }
  inline void SetA1(G4double val) { fA1 = val; }
  inline G4double GetA1() const { return fA1; }

  inline void SetZ2(G4double val) { fZ2 = val; }
  inline G4double GetZ2() const { return fZ2; }
  inline void SetA2(G4double val) { fA2 = val; }
  inline G4double GetA2() const { return fA2; }

  inline void SetZ3(G4double val) { fZ3 = val; }
  inline G4double GetZ3() const { return fZ3; }
  inline void SetA3(G4double val) { fA3 = val; }
  inline G4double GetA3() const { return fA3; }

  inline void SetZ4(G4double val) { fZ4 = val; }
  inline G4double GetZ4() const { return fZ4; }
  inline void SetA4(G4double val) { fA4 = val; }
  inline G4double GetA4() const { return fA4; }

  inline void Setqmin(G4double val) { fqmin = val; }
  inline G4double Getqmin() const { return fqmin; }
  inline void Setqmax(G4double val) { fqmax = val; }
  inline G4double Getqmax() const { return fqmax; }

  inline void SetCharge3(G4double val) { fCharge3 = val; }
  inline G4double GetCharge3() const { return fCharge3; }
  inline void SetExcitationEnergy3(G4double val) { fExcitationEnergy3 = val; }
  inline G4double GetExcitationEnergy3() const { return fExcitationEnergy3; }

};

#endif


