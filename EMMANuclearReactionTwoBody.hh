/* 
   Oliver Kirsebom, TRIUMF, February 2013
*/
  //! Class description:
  //! Nuclear-reaction model for two-body final-state (Z3,A3)+(Z4,A4)

  //! (G4HadronElastic.hh used as starting point)



#ifndef EMMANuclearReactionTwoBody_h
#define EMMANuclearReactionTwoBody_h 1

#include "globals.hh"
#include "Randomize.hh"
#include "G4Element.hh"
#include "G4ElementVector.hh"
#include "G4ElementTable.hh"
#include "G4PhysicsTable.hh"
#include "G4PhysicsVector.hh"
#include "G4LPhysicsFreeVector.hh"
#include "G4LightMedia.hh"
#include "G4Step.hh"
#include "G4TrackStatus.hh"
#include "G4HadronicInteraction.hh"


class EMMANuclearReactionTwoBody : public G4HadronicInteraction
{
public:

	//! Constructor and Destructor.

  EMMANuclearReactionTwoBody(const G4String& name = "EMMANuclearReactionTwoBody"
			     , G4double Z3=0, G4double A3=0
			     , G4double Z4=0, G4double A4=0
			     , G4double qmax=180. );
  ~EMMANuclearReactionTwoBody() {};
  
  //! ApplyYourself method, which takes in aTrack of type
  //! G4HadProjectile&, and targetNucleus of type G4Nucleus&,
  //! outputs a G4HadFinalState*
  
  G4HadFinalState* ApplyYourself(const G4HadProjectile& aTrack, G4Nucleus& targetNucleus);

  //! 4 methods to set Z3, Z4, A3, A4
  
  void SetZ3(G4double x) { Z3rea=x; };
  void SetA3(G4double x) { A3rea=x; };
  void SetZ4(G4double x) { Z4rea=x; };
  void SetA4(G4double x) { A4rea=x; };
  
  //! method to set qmax
  
  void Setqmax(G4double x) { thetaCMmax=x; };

private:

  //! defines private variables:
  //! theProton, theNeutron, theDeuteron,
  //! theTriton, theAlpha, theHe3, theGamma,
  //! all of type G4ParticleDefinition*, as well as
  //! private variables Z3rea, A3rea, Z4rea, A4rea, 
  //! and thetaCMmax, all of type G4double

  G4ParticleDefinition* theProton;
  G4ParticleDefinition* theNeutron;
  G4ParticleDefinition* theDeuteron;
  G4ParticleDefinition* theTriton;
  G4ParticleDefinition* theAlpha;
  G4ParticleDefinition* theHe3;
  G4ParticleDefinition* theGamma;
  G4double Z3rea,A3rea,Z4rea,A4rea;
  G4double thetaCMmax;

};
#endif
