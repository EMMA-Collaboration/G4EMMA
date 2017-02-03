//
// Created from BuildGeometry program
//
#ifndef BGField6_H
#define BGField6_H 1
#include "globals.hh"
#include "G4MagneticField.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"

#include "EMMAElementField.hh"



// global variables 
extern G4double currentCharge; // EMMASteppingAction.cc
extern G4double userCharge; // EMMAPrimaryGeneratorAction.cc


//class BGField6 : public G4MagneticField
class BGField6 : public EMMAElementField
{
public:
  //offset coordinates are of the x and z location of where the field begins in reference to 
  //the world logical volume
  //BGField6(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter);
   BGField6(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter, G4LogicalVolume*, G4ThreeVector);
  ~BGField6();

  // instead you can limit the size

  virtual G4double GetLength() { return DBL_MAX; }
  virtual G4double GetWidth() { return DBL_MAX; }
  virtual G4double GetHeight() { return DBL_MAX; }

  // AddFieldValue() adds the field for this element into the field[].
  // Point() is in global coordinates.
  
  virtual void AddFieldValue( const G4double Point[3], G4double field[6]) const;
  
 // virtual void GetFieldValue( const double Point[3], G4double *Bfield) const;
  
private:
  double data[75], offset[3];
  G4int i;
  G4double Pi;
  G4double FieldStrength_0;

public:
  G4double GetFieldStrength() { return data[13]; }; 
  void ScaleFieldStrength( G4double msf ) { data[13] = msf * FieldStrength_0 ; }; 

};
#endif
