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
// $Id: EMMAIonPhysics.cc,v 1.10 2009-11-21 01:00:19 perl Exp $
// --------------------------------------------------------------
//
// 13-Oct-2003 Add Comment for Ionisation of Generic Ion by T. Koi 
// 05-Jan-2004 Change G. Ion Ionisation from G4hIonisation
//                                      to G4ionIonisation T. Koi

#include "EMMAIonPhysics.hh"

#include "globals.hh"
#include "G4ios.hh"
#include <iomanip>

#include "G4Region.hh"
#include "G4RegionStore.hh"
#include "G4ProductionCuts.hh"
#include "G4EmConfigurator.hh"
#include "G4LossTableManager.hh"

#include "G4BraggIonGasModel.hh"
#include "G4BetheBlochIonGasModel.hh"
#include "G4IonFluctuations.hh"
#include "G4UniversalFluctuation.hh"

#include "G4ParticleDefinition.hh"
#include "G4ParticleTable.hh"
#include "G4ProcessManager.hh"

#include "G4EmProcessOptions.hh"
#include "G4IonParametrisedLossModel.hh"
#include "G4NuclearStopping.hh"
#include "G4UrbanMscModel90.hh"
#include "G4UrbanMscModel95.hh"
#include "G4WentzelVIModel.hh"
#include "G4CoulombScattering.hh"
#include "G4IonCoulombScatteringModel.hh"
#include "G4ScreenedNuclearRecoil.hh"
#include "EMMAIonPhysicsMessenger.hh"



EMMAIonPhysics::EMMAIonPhysics(const G4String& name)
                 :  G4VPhysicsConstructor(name)
{
  //create a messenger for this class
  fMessenger = new EMMAIonPhysicsMessenger(this);
}


EMMAIonPhysics::~EMMAIonPhysics()
{
}


void EMMAIonPhysics::ConstructProcess()
{
   G4ProcessManager * pManager = 0;

   //G4Region* region = G4RegionStore::GetInstance()->GetRegion("energyLossRegion",false);



   // Generic Ion
   pManager = G4GenericIon::GenericIon()->GetProcessManager();


   G4hMultipleScattering* thegionMultipleScattering = new G4hMultipleScattering();
   thegionMultipleScattering->SetEmModel(new G4UrbanMscModel90());
   //   thegionMultipleScattering->SetEmModel(new G4WentzelVIModel());
   pManager->AddProcess(thegionMultipleScattering);
   pManager->SetProcessOrdering(thegionMultipleScattering, idxAlongStep,1);
   pManager->SetProcessOrdering(thegionMultipleScattering, idxPostStep,1);

   G4ionIonisation* thegionIonisation = new G4ionIonisation();
   //   thegionIonisation->SetEmModel(new G4IonParametrisedLossModel());
   thegionIonisation->SetStepFunction(0.01, 0.01*um);
   pManager->AddProcess(thegionIonisation);
   pManager->SetProcessOrdering(thegionIonisation, idxAlongStep,2);
   pManager->SetProcessOrdering(thegionIonisation, idxPostStep,2);


//    // Coulomb scattering
//    G4CoulombScattering* thegionCoulombScattering = new G4CoulombScattering();
//    thegionCoulombScattering->SetEmModel(new G4IonCoulombScatteringModel());
//    pManager->AddDiscreteProcess(thegionCoulombScattering);  



   // Screened nuclear recoil
   // G4ScreenedNuclearRecoil* thegionScreenedNuclearRecoil = new G4ScreenedNuclearRecoil();
   // pManager->AddDiscreteProcess(thegionScreenedNuclearRecoil);  

   // Nuclear stopping
   //   G4VProcess* thegionNuclearStopping = new G4NuclearStopping();
   //   pManager->AddDiscreteProcess(thegionNuclearStopping);  




   // Deuteron
   pManager = G4Deuteron::Deuteron()->GetProcessManager();

   // add processes
   G4HadronElasticProcess* thedueElasticProcess = new G4HadronElasticProcess();
   G4LElastic* thedueElasticModel = new G4LElastic();
   thedueElasticProcess->RegisterMe(thedueElasticModel);
   pManager->AddDiscreteProcess(thedueElasticProcess);

   G4DeuteronInelasticProcess* theDeuteronInelasticProcess = new G4DeuteronInelasticProcess();

   G4LEDeuteronInelastic* theDeuteronLEPModel = new G4LEDeuteronInelastic();
   theDeuteronInelasticProcess->RegisterMe(theDeuteronLEPModel);
   pManager->AddDiscreteProcess(theDeuteronInelasticProcess);

   G4VProcess* thedueMultipleScattering = new G4hMultipleScattering();
   G4VProcess* thedueIonisation        = new G4hIonisation();
   //
   pManager->AddProcess(thedueIonisation);
   pManager->AddProcess(thedueMultipleScattering);
   //
   // set ordering for AlongStepDoIt
   pManager->SetProcessOrdering(thedueMultipleScattering, idxAlongStep,1);
   pManager->SetProcessOrdering(thedueIonisation,        idxAlongStep,2);
   //
   // set ordering for PostStepDoIt
   pManager->SetProcessOrdering(thedueMultipleScattering, idxPostStep,1);
   pManager->SetProcessOrdering(thedueIonisation,        idxPostStep,2);



   // Triton
   pManager = G4Triton::Triton()->GetProcessManager();

   // add process
   G4HadronElasticProcess* thetriElasticProcess = new G4HadronElasticProcess();
   G4LElastic* thetriElasticModel = new G4LElastic();
   thetriElasticProcess->RegisterMe(thetriElasticModel);
   pManager->AddDiscreteProcess(thetriElasticProcess);

   G4TritonInelasticProcess* theTritonInelasticProcess = new G4TritonInelasticProcess();

   G4LETritonInelastic* theTritonLEPModel = new G4LETritonInelastic();
   theTritonInelasticProcess->RegisterMe(theTritonLEPModel);
   pManager->AddDiscreteProcess(theTritonInelasticProcess);

   G4VProcess* thetriMultipleScattering = new G4hMultipleScattering();
   G4VProcess* thetriIonisation        = new G4hIonisation();
   //
   pManager->AddProcess(thetriIonisation);
   pManager->AddProcess(thetriMultipleScattering);
   //
   // set ordering for AlongStepDoIt
   pManager->SetProcessOrdering(thetriMultipleScattering, idxAlongStep,1);
   pManager->SetProcessOrdering(thetriIonisation,        idxAlongStep,2);
   //
   // set ordering for PostStepDoIt
   pManager->SetProcessOrdering(thetriMultipleScattering, idxPostStep,1);
   pManager->SetProcessOrdering(thetriIonisation,        idxPostStep,2);



   // Alpha
   pManager = G4Alpha::Alpha()->GetProcessManager();

   // add processes
   // G4HadronElasticProcess* thealElasticProcess = new G4HadronElasticProcess();
   // G4LElastic* thealElasticModel = new G4LElastic();
   // thealElasticProcess->RegisterMe(thealElasticModel);
   // pManager->AddDiscreteProcess(thealElasticProcess);

   // G4AlphaInelasticProcess* theAlphaInelasticProcess = new G4AlphaInelasticProcess();

   // G4LEAlphaInelastic* theAlphaLEPModel = new G4LEAlphaInelastic();
   // theAlphaInelasticProcess->RegisterMe(theAlphaLEPModel);
   // pManager->AddDiscreteProcess(theAlphaInelasticProcess);

   G4VProcess* thealpMultipleScattering = new G4hMultipleScattering();
   G4VProcess* thealpIonisation        = new G4hIonisation();
   //
   pManager->AddProcess(thealpIonisation);
   pManager->AddProcess(thealpMultipleScattering);
   //
   // set ordering for AlongStepDoIt
   pManager->SetProcessOrdering(thealpMultipleScattering, idxAlongStep,1);
   pManager->SetProcessOrdering(thealpIonisation,        idxAlongStep,2);
   //
   // set ordering for PostStepDoIt
   pManager->SetProcessOrdering(thealpMultipleScattering, idxPostStep,1);
   pManager->SetProcessOrdering(thealpIonisation,        idxPostStep,2);



   // He3
   pManager = G4He3::He3()->GetProcessManager();

   // add processes
   G4HadronElasticProcess* thehe3ElasticProcess = new G4HadronElasticProcess();
   G4LElastic* thehe3ElasticModel = new G4LElastic();
   thehe3ElasticProcess->RegisterMe(thehe3ElasticModel);
   pManager->AddDiscreteProcess(thehe3ElasticProcess);

   G4VProcess* thehe3MultipleScattering = new G4hMultipleScattering();
   G4VProcess* thehe3Ionisation        = new G4hIonisation();
   //
   pManager->AddProcess(thehe3Ionisation);
   pManager->AddProcess(thehe3MultipleScattering);
   //
   // set ordering for AlongStepDoIt
   pManager->SetProcessOrdering(thehe3MultipleScattering, idxAlongStep,1);
   pManager->SetProcessOrdering(thehe3Ionisation,        idxAlongStep,2);
   //
   // set ordering for PostStepDoIt
   pManager->SetProcessOrdering(thehe3MultipleScattering, idxPostStep,1);
   pManager->SetProcessOrdering(thehe3Ionisation,        idxPostStep,2);



   // // EMMA E and B fields ...
   // G4EmConfigurator* em_config = G4LossTableManager::Instance()->EmConfigurator();
   // AddIonGasModels();
   // em_config->AddModels();
}




void EMMAIonPhysics::AddIonGasModels() //NOT USED
{
  G4EmConfigurator* em_config = G4LossTableManager::Instance()->EmConfigurator();
  //G4Region* vacuumRegion = G4RegionStore::GetInstance()->GetRegion("vacuumRegion",false);
  
  theParticleIterator->reset();
  while ((*theParticleIterator)())
    {
      G4ParticleDefinition* particle = theParticleIterator->value();
      G4String partname = particle->GetParticleName();
      if(partname == "alpha" || partname == "He3" || partname == "GenericIon") { // what about protons, deuterons and tritons?
	G4BraggIonGasModel* mod1 = new G4BraggIonGasModel();
	G4BetheBlochIonGasModel* mod2 = new G4BetheBlochIonGasModel();
	G4double eth = 2.*MeV*particle->GetPDGMass()/proton_mass_c2;
	em_config->SetExtraEmModel(partname,"ionIoni",mod1,"vacuumRegion",0.0,eth,new G4IonFluctuations());
	em_config->SetExtraEmModel(partname,"ionIoni",mod2,"vacuumRegion",eth,100*TeV,new G4UniversalFluctuation());
      }
    }
}



void EMMAIonPhysics::SetReactionParameters() {

  G4cout << G4endl;
  G4cout << "*** Two-body process activated ***" << G4endl;
  G4cout << "Reaction: "  
	 << "(" << fZ1 << "," << fA1 << ")+" 
	 << "(" << fZ2 << "," << fA2 << ") -> " 
	 << "(" << fZ3 << "," << fA3 << ")+" 
	 << "(" << fZ4 << "," << fA4 << ")" 
	 << G4endl;
  G4cout << "with cross section = " << fcs/millibarn << " mb" << G4endl;
  G4cout << G4endl;

  theNuclearReactionProcess->Setcs(fcs);
  theNuclearReactionProcess->SetZ1(fZ1);
  theNuclearReactionProcess->SetA1(fA1);
  theNuclearReactionProcess->SetZ2(fZ2);
  theNuclearReactionProcess->SetA2(fA2);
  theNuclearReactionTwoBody->SetZ3(fZ3);
  theNuclearReactionTwoBody->SetA3(fA3);
  theNuclearReactionTwoBody->SetZ4(fZ4);
  theNuclearReactionTwoBody->SetA4(fA4);
  theNuclearReactionTwoBody->Setqmax(fqmax);

}
