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
// $Id: EMMAIonPhysics.hh,v 1.8 2009-11-21 01:00:19 perl Exp $
// --------------------------------------------------------------
// 05-Jan-2004 Add G4ionIonisation T. Koi
//

#ifndef EMMAIonPhysics_h
#define EMMAIonPhysics_h 1

#include "globals.hh"
#include "G4ios.hh"
#include "G4VPhysicsConstructor.hh"
#include "G4HadronElasticProcess.hh"
#include "G4LElastic.hh"
#include "G4DeuteronInelasticProcess.hh"
#include "G4LEDeuteronInelastic.hh"
#include "G4TritonInelasticProcess.hh"
#include "G4LETritonInelastic.hh"
#include "G4AlphaInelasticProcess.hh"
#include "G4LEAlphaInelastic.hh"
#include "G4hIonisation.hh"
#include "G4ionIonisation.hh"
#include "G4hMultipleScattering.hh"

#include "EMMANuclearReactionProcess.hh"
#include "EMMANuclearReactionTwoBody.hh"


class EMMAIonPhysicsMessenger;


class EMMAIonPhysics : public G4VPhysicsConstructor
{
public:
  EMMAIonPhysics(const G4String& name="ion");
  virtual ~EMMAIonPhysics();


public:
  // This method will be invoked in the Construct() method.
  // each particle type will be instantiated
  virtual void ConstructParticle(){;};
  
  // This method will be invoked in the Construct() method.
  // each physics process will be instantiated and
  // registered to the process manager of each particle type
  virtual void ConstructProcess();
  
  void AddIonGasModels();
  void SetReactionParameters(); 


private:
  EMMAIonPhysicsMessenger* fMessenger;
  G4double fcs;
  G4double fZ1,fA1, fZ2,fA2;
  G4double fZ3,fA3, fZ4,fA4;
  G4double fqmax;
  EMMANuclearReactionProcess* theNuclearReactionProcess;
  EMMANuclearReactionTwoBody* theNuclearReactionTwoBody;


public:
  inline void SetCrossSection(G4double val) { fcs = val; }
  inline G4double GetCrossSection() const { return fcs; }

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

  inline void Setqmax(G4double val) { fqmax = val; }
  inline G4double Getqmax() const { return fqmax; }


protected:

};


#endif

