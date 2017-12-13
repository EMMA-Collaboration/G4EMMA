/* 
   Oliver Kirsebom, TRIUMF, February 2013

   Class description:
   Nuclear-reaction process for projectile (Z1,A1) 
   striking target (Z2,A2) with cross section (cs)

   (G4HadronElasticProcess.hh used as starting point)
*/



#ifndef EMMANuclearReactionProcess_h
#define EMMANuclearReactionProcess_h 1
 
#include "globals.hh"
#include "G4HadronicProcess.hh"
#include "EMMANuclearReactionDataSet.hh"


class G4ParticleDefinition;
class G4CrossSectionDataStore;

class EMMANuclearReactionProcess : public G4HadronicProcess
{
public:
  EMMANuclearReactionProcess(const G4String& procName = "EMMANuclearReactionProcess"
		  , G4double Z1=0, G4double A1=0
		  , G4double Z2=0, G4double A2=0
		  , G4double cs=0 );
  virtual ~EMMANuclearReactionProcess();
  virtual G4VParticleChange* PostStepDoIt(const G4Track& aTrack, const G4Step& aStep);

  // initialise thresholds
  virtual void PreparePhysicsTable(const G4ParticleDefinition&);

  // set internal limit
  virtual void SetLowestEnergy(G4double);

  virtual void Description() const;

  void SetZ1(G4double x) { ods->SetZ1(x); };
  void SetA1(G4double x) { ods->SetA1(x); };
  void SetZ2(G4double x) { ods->SetZ2(x); };
  void SetA2(G4double x) { ods->SetA2(x); };
  void Setcs(G4double x) { ods->Setcs(x); };

private:
  // hide assignment operator as private 
  EMMANuclearReactionProcess& operator=(const EMMANuclearReactionProcess &right);
  EMMANuclearReactionProcess(const EMMANuclearReactionProcess& );

  EMMANuclearReactionDataSet * ods;

  G4double lowestEnergy;
  G4bool   isInitialised;
};

#endif
