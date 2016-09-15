/* 
   Oliver Kirsebom, TRIUMF, February 2013

   Class description:
   Nuclear-reaction process for projectile (Z1,A1) 
   striking target (Z2,A2) with cross section (cs)

   (G4HadronElasticProcess.cc used as starting point)
*/




#include <iostream>
#include <typeinfo>

#include "G4SystemOfUnits.hh"
#include "G4Nucleus.hh"
#include "G4ProcessManager.hh"
#include "G4CrossSectionDataStore.hh"
#include "EMMANuclearReactionDataSet.hh"
#include "G4ProductionCutsTable.hh"
#include "G4HadronicException.hh"
#include "G4HadronicDeprecate.hh"

#include "EMMANuclearReactionProcess.hh"
#include "EMMANuclearReactionTwoBody.hh"



// Constructor
EMMANuclearReactionProcess::EMMANuclearReactionProcess(const G4String& pName
				 , G4double Z1, G4double A1
				 , G4double Z2, G4double A2
				 , G4double cs )
  : G4HadronicProcess(pName, fHadronElastic), isInitialised(false)
{
  ods = new EMMANuclearReactionDataSet("ConstantCS",Z1,A1,Z2,A2,cs); // create data set for two-body reaction 1+2->X+Y
  AddDataSet(ods);
  lowestEnergy = 1.*keV;
}



// Destructor
EMMANuclearReactionProcess::~EMMANuclearReactionProcess()
{}




void EMMANuclearReactionProcess::Description() const
{}




G4VParticleChange* 
EMMANuclearReactionProcess::PostStepDoIt(const G4Track& track, 
				     const G4Step& /*step*/)
{
  theTotalResult->Clear();
  theTotalResult->Initialize(track);
  G4double weight = track.GetWeight();
  theTotalResult->ProposeWeight(weight);

  // For elastic scattering, _any_ result is considered an interaction
  ClearNumberOfInteractionLengthLeft();

  G4double kineticEnergy = track.GetKineticEnergy();
  const G4DynamicParticle* dynParticle = track.GetDynamicParticle();
  const G4ParticleDefinition* part = dynParticle->GetDefinition();

  // NOTE:  Very low energy scatters were causing numerical (FPE) errors
  //        in earlier releases; these limits have not been changed since.
  //  if (kineticEnergy <= 100.*GeV)   return theTotalResult;
  if (kineticEnergy <= lowestEnergy)   return theTotalResult;



  G4Material* material = track.GetMaterial();
  G4Nucleus* targNucleus = GetTargetNucleusPointer();


  // Select element
  G4Element* elm = 0;
  try {
    elm = GetCrossSectionDataStore()->SampleZandA(dynParticle, material, *targNucleus);
  }
  catch(G4HadronicException & aR) {
    G4ExceptionDescription ed;
    DumpState(track,"SampleZandA",ed); 
    ed << " PostStepDoIt failed on element selection" << G4endl;
    G4Exception("EMMANuclearReactionProcess::PostStepDoIt", "had003", FatalException, ed);
  }


  /* 
     choses the interaction model depending on kinetic energy,
     material and element (only has been defined for this process,
     so that is the one that will be selected)
  */
  G4HadronicInteraction* hadi = 0;
  try {
    hadi = ChooseHadronicInteraction( kineticEnergy, material, elm ); 
  }
  catch(G4HadronicException & aE) {
    G4ExceptionDescription ed;
    ed << "Target element "<< elm->GetName()<<"  Z= " 
       << targNucleus->GetZ_asInt() << "  A= " 
       << targNucleus->GetA_asInt() << G4endl;
    DumpState(track,"ChooseHadronicInteraction",ed);
    ed << " No HadronicInteraction found out" << G4endl;
    G4Exception("EMMANuclearReactionProcess::PostStepDoIt", "had005", FatalException, ed);
  }



  G4double tcut = 0.1*MeV; // low-energy cut for production of secondaries
  hadi->SetRecoilEnergyThreshold(tcut);


  // Initialize the hadronic projectile from the track
  G4HadProjectile theProj(track);
  G4HadFinalState* result = 0;
  try {
    /* 
       here we call the actual routine that simulates the interaction
    */
    result = hadi->ApplyYourself( theProj, *targNucleus );
  }
  catch(G4HadronicException aR) {
    G4ExceptionDescription ed;
    ed << "Call for " << hadi->GetModelName() << G4endl;
    ed << "Target element "<< elm->GetName()<<"  Z= " 
       << targNucleus->GetZ_asInt() 
       << "  A= " << targNucleus->GetA_asInt() << G4endl;
    DumpState(track,"ApplyYourself",ed);
    ed << " ApplyYourself failed" << G4endl;
    G4Exception("EMMANuclearReactionProcess::PostStepDoIt", "had006", 
		FatalException, ed);
  }
  


  // Directions
  G4ThreeVector indir = track.GetMomentumDirection();
  G4double phi = CLHEP::twopi*G4UniformRand();
  G4ThreeVector it(0., 0., 1.);
  G4ThreeVector outdir = result->GetMomentumChange();


  // Energies  
  G4double edep = result->GetLocalEnergyDeposit();
  G4double efinal = result->GetEnergyChange();
  if(efinal < 0.0) { efinal = 0.0; }
  if(edep < 0.0)   { edep = 0.0; }
  if(efinal <= lowestEnergy) {
    edep += efinal;
    efinal = 0.0;
  }


  // primary
  theTotalResult->ProposeEnergy(efinal);
  G4TrackStatus status = track.GetTrackStatus();
  if(efinal > 0.0) {
    /* 
       this serves to rotate things from the 'internal frame' of the
       interaction process to the 'world frame' of the laboratory
    */    
    outdir.rotate(phi, it);
    outdir.rotateUz(indir);
    theTotalResult->ProposeMomentumDirection(outdir);
  } else {
    if(part->GetProcessManager()->GetAtRestProcessVector()->size() > 0)
         { status = fStopButAlive; }
    else { status = fStopAndKill; }
    theTotalResult->ProposeTrackStatus(status);
  }



  // Secondaries
  G4int NoS = result->GetNumberOfSecondaries();
  theTotalResult->SetNumberOfSecondaries(NoS);

  for (G4int i=0; i<NoS; i++) {
    G4DynamicParticle* p = result->GetSecondary(i)->GetParticle();

    /* 
       this, again, serves to rotate things from the 'internal frame' of the
       interaction process to the 'world frame' of the laboratory
    */    
    G4ThreeVector pdir = p->GetMomentumDirection();
    pdir.rotate(phi, it);
    pdir.rotateUz(indir);
    p->SetMomentumDirection(pdir);
    
    //  time and weight are not changed
    G4Track* t = new G4Track(p, track.GetGlobalTime(), track.GetPosition());
    t->SetWeight(weight);
    t->SetTouchableHandle(track.GetTouchableHandle());
    theTotalResult->AddSecondary(t);
  }
  

  theTotalResult->ProposeLocalEnergyDeposit(edep);
  theTotalResult->ProposeNonIonizingEnergyDeposit(edep);
  result->Clear();

  return theTotalResult;
}



void 
EMMANuclearReactionProcess::PreparePhysicsTable(const G4ParticleDefinition& part)
{
  if(!isInitialised) {
    isInitialised = true;
    if(G4Neutron::Neutron() == &part) { lowestEnergy = 1.e-6*eV; }
  }
  G4HadronicProcess::PreparePhysicsTable(part);
}



void EMMANuclearReactionProcess::SetLowestEnergy(G4double val)
{
  lowestEnergy = val;
}
