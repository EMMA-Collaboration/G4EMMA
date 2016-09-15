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
// $Id: TrackingAction.cc,v 1.1 2009-04-24 09:11:18 maire Exp $
// GEANT4 tag $Name: geant4-09-04-patch-02 $
//
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#include "TrackingAction.hh"


#include "G4Track.hh"
#include "G4Positron.hh"

#include "G4ios.hh"

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

TrackingAction::TrackingAction()
{ }
 
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void TrackingAction::PreUserTrackingAction(const G4Track* PreTrack)
{ 

/*	G4ParticleDefinition* particleType = PreTrack->GetDefinition();
	const G4DynamicParticle* theParticle = PreTrack->GetDynamicParticle();
	G4double theCharge = theParticle->GetCharge();
	G4cout << "PreTrack: Particle Name: " << particleType->GetParticleName()
	<< " has charge: " << theCharge << G4endl;	
*/

}

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

void TrackingAction::PostUserTrackingAction(const G4Track* PostTrack)
{ 
  //
 
/*	G4ParticleDefinition* particleType = PostTrack->GetDefinition();
	const G4DynamicParticle* theParticle = PostTrack->GetDynamicParticle();
	G4double theCharge = theParticle->GetCharge();
	G4cout << "PostTrack: Particle Name: " << particleType->GetParticleName()
	<< " has charge: " << theCharge << G4endl;     
*/	

	// const G4ParticleDefinition* part = PostTrack->GetParticleDefinition();
	// G4int Z = part->GetAtomicNumber();
	// G4int A = part->GetAtomicMass();
	// if (Z==50 && A==132) {
	//   G4cout << PostTrack->GetDynamicParticle()->GetCharge() << G4endl;
	// }

}	
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

