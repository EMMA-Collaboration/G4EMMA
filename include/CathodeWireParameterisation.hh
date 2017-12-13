// Created by Matthew Williams
// Adapted from example B2b


#ifndef CathodeWireParameterisation_h
#define CathodeWireParameterisation_h 1

#include "globals.hh"
#include "G4VPVParameterisation.hh"

class G4VPhysicalVolume;
class G4Box;

// Dummy declarations to get rid of warnings ...
class G4Trd;
class G4Trap;
class G4Cons;
class G4Orb;
class G4Sphere;
class G4Torus;
class G4Para;
class G4Hype;
class G4Tubs;
class G4Polycone;
class G4Polyhedra;

///  A parameterisation that describes a series of cylinders along X.
///
///  The cylinders have equal radius and length.
///  They are spaced an equal distance apart, starting from given location.

class CathodeWireParameterisation : public G4VPVParameterisation
{ 
  public:
  
    CathodeWireParameterisation(G4int    noWires, 
                              G4double startX, 
                              G4double spacing,
                              G4double wireRadius, 
                              G4double wireLength );

    virtual ~CathodeWireParameterisation();
   
    void ComputeTransformation (const G4int copyNo,
                                G4VPhysicalVolume* physVol) const;
    
    void ComputeDimensions (G4Tubs & CathodeWire, const G4int copyNo, //check this 
                            const G4VPhysicalVolume* physVol) const;

  private:  // Dummy declarations to get rid of warnings ...

    void ComputeDimensions (G4Box&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Trd&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Trap&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Cons&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Sphere&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Orb&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Torus&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Para&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Hype&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Polycone&,const G4int,const G4VPhysicalVolume*) const {}
    void ComputeDimensions (G4Polyhedra&,const G4int,const G4VPhysicalVolume*) const {}

  private:

    G4int    fNoWires;   
    G4double fStartX;
    G4double fHalfRadius;        //  The half-radius of each wire
    G4double fSpacing;          //  The distance between the wires' center
    G4double fHalfLength;        //  half-length 
             
};

//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

#endif
