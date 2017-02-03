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
// $Id: EMMAPhysicsList.cc,v 1.8 2006-06-29 16:33:03 gunter Exp $
// --------------------------------------------------------------
//
// 28-Jan-04 Add QGSP_BERT and QGSP_BIC for hadronic lists. T. Koi
// 22-Nov-04 Comment out QGSP_BERT and QGSP_BIC
//           Output Notificaiton message             
//           All Particles are created in GeneralPhysics 

#include "EMMAPhysicsList.hh"

#include "globals.hh"
#include "G4ParticleDefinition.hh"
#include "G4ParticleWithCuts.hh"
#include "G4ProcessManager.hh"
#include "G4ProcessVector.hh"
#include "G4ParticleTypes.hh"
#include "G4ParticleTable.hh"

#include "G4Material.hh"
#include "G4MaterialTable.hh"
#include "G4ios.hh"
#include <iomanip>

#include "EMMAGeneralPhysics.hh"
#include "EMMAEMPhysics.hh"
#include "EMMAMuonPhysics.hh"
#include "EMMAHadronPhysics.hh"
#include "EMMAIonPhysics.hh"

//#include "F04StepMax.hh"


//#include "HadronPhysicsQGSP_BERT.hh"
//#include "HadronPhysicsQGSP_BIC.hh"

EMMAPhysicsList::EMMAPhysicsList():  G4VModularPhysicsList()
{

  // G4cout << "You are using the EMMAPhysicsList" << G4endl;
  // G4cout << "Full set of particles (barions bosons and mesons) will be created and" << G4endl;
  // G4cout << "Standard EM Physics and Low & High Energy parameterized models will be applied." << G4endl;
  // G4cout << "EMMAPhysicsList is optimized for robustness" << G4endl;
  // G4cout << "and not for any particular usage." << G4endl;
  // G4cout << "For the hadronic physics, educated guesses of physics list are prepared for various use cases." << G4endl;
  // G4cout << "When you will start REAL calculations for your own interest," << G4endl;
  // G4cout << "please consider the usage of hadronic_lists instead of EMMAPhysicsLists." << G4endl;
  // G4cout << "More information can also be found from the Geant4 HyperNews." << G4endl;
  // G4cout << "http://geant4-hn.slac.stanford.edu:5090/Geant4-HyperNews/index" << G4endl;
  // G4cout << "The StepLimit is set in EMMAGeneralPhysics.cc" << G4endl;

  // default cut value  (1.0mm)
  defaultCutValue = 1.0*mm; //declared in G4VUserPhysicsList via G4VModularPhysicsList
  SetVerboseLevel(1); //This does NOTHING

  // General Physics ( Create ALL Particle and apply Decay )
  RegisterPhysics( new EMMAGeneralPhysics("general") );

  // EM Physics ( Apply related Processes to gamma and e-/+)
  RegisterPhysics( new EMMAEMPhysics("standard EM"));

  //  // Muon Physics ( Apply related processes to mu and tau
  //  RegisterPhysics(  new EMMAMuonPhysics("muon"));

   // Hadron Physics ( Apply related processes to hadrons )
  RegisterPhysics(  new EMMAHadronPhysics("hadron"));

  // Ion Physics ( Apply related processes to ions )
  RegisterPhysics( new EMMAIonPhysics("ion"));

}

EMMAPhysicsList::~EMMAPhysicsList()
{
//	delete stepMaxProcess;
}




void EMMAPhysicsList::SetCuts()
{
  //Each particle has a suggested threshold below which secondary particles will not be produced. In
  //G4ParticleDefinition, this threshold is a distance, or range, which is converted to an energy for all
  //materials. The range threshold should be set for all particle types in the initialization phase using the
  //SetCuts() method of G4VUserPhysicsList.

  //  " G4VUserPhysicsList::SetCutsWithDefault" method sets
  //   the default cut value for all particle types
  SetCutsWithDefault();
}



