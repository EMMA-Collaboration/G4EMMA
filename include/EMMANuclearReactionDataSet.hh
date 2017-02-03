//
// ********************************************************************
// * License and Disclaimer                                           *
/* 
   Oliver Kirsebom, TRIUMF, February 2013

   Class description:
   Very simple cross-section data set for nuclear-reaction process

   (G4HadronElasticDataSet.hh used as starting point)
*/




#ifndef EMMANuclearReactionDataSet_h
#define EMMANuclearReactionDataSet_h 1

#include "G4VCrossSectionDataSet.hh"
#include "G4HadronCrossSections.hh"
#include "G4DynamicParticle.hh"
#include "G4Element.hh"


class EMMANuclearReactionDataSet : public G4VCrossSectionDataSet
{
public:
  EMMANuclearReactionDataSet(const G4String& name = "EMMANuclearReactionDataSet"
		  , G4double Z1=0, G4double A1=0
		  , G4double Z2=0, G4double A2=0
		  , G4double cs=0 );
  virtual ~EMMANuclearReactionDataSet();
  virtual void CrossSectionDescription(std::ostream&) const;

  virtual
  G4bool IsIsoApplicable(const G4DynamicParticle*, G4int Z, G4int A,    
			 const G4Element* elm = 0,
			 const G4Material* mat = 0);
  virtual
  G4double GetIsoCrossSection(const G4DynamicParticle*, G4int Z, G4int A,  
			      const G4Isotope* iso = 0,
			      const G4Element* elm = 0,
			      const G4Material* mat = 0);

  void SetZ1(G4double x) { Z1rea=x; };
  void SetA1(G4double x) { A1rea=x; };
  void SetZ2(G4double x) { Z2rea=x; };
  void SetA2(G4double x) { A2rea=x; };
  void Setcs(G4double x) { CrossSection=x; };

private:
  G4HadronCrossSections* theHadronCrossSections;
  G4double Z1rea,A1rea,Z2rea,A2rea,CrossSection;

};

#endif
