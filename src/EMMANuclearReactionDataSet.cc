/* 
   Oliver Kirsebom, TRIUMF, February 2013

   Class description:
   Very simple cross-section data set for nuclear-reaction process

   (G4HadronElasticDataSet.cc used as starting point)
*/



#include "G4NistManager.hh"
#include "G4HadTmpUtil.hh"
#include <iostream>
#include "EMMANuclearReactionDataSet.hh"



EMMANuclearReactionDataSet::EMMANuclearReactionDataSet(const G4String& nam
				 , G4double Z1, G4double A1
				 , G4double Z2, G4double A2
				 , G4double cs )
 : G4VCrossSectionDataSet(nam)
{
  theHadronCrossSections = G4HadronCrossSections::Instance(); 

  Z1rea = Z1; A1rea = A1;
  Z2rea = Z2; A2rea = A2;
  CrossSection = cs;
}


EMMANuclearReactionDataSet::~EMMANuclearReactionDataSet() {}


void EMMANuclearReactionDataSet::CrossSectionDescription(std::ostream& outFile) const
{}



G4bool EMMANuclearReactionDataSet::IsIsoApplicable(const G4DynamicParticle* aParticle, G4int Z, G4int A,    
						   const G4Element* elm,
						   const G4Material* mat)
{
  return true;
}




G4double EMMANuclearReactionDataSet::GetIsoCrossSection(const G4DynamicParticle* aParticle, G4int Z, G4int A,  
							const G4Isotope* iso,
							const G4Element* elm,
							const G4Material* mat)
{
  G4int Zpart = aParticle->GetParticleDefinition()->GetAtomicNumber();
  G4int Apart = aParticle->GetParticleDefinition()->GetAtomicMass();
  if (Zpart==Z1rea && Apart==A1rea && Z==Z2rea && A==A2rea) { 
    // G4cout << G4endl;
    // G4cout << A << ", " << A2rea << G4endl;
    // G4cout << "actual cross section: " << CrossSection/millibarn << " mb" << G4endl;
    // G4cout << G4endl;
    return CrossSection;
  }
  else {
    return 0;
  }
}
