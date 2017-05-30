//!
//! Created from BuildGeometry program
//!
#ifndef BGField4_H
#define BGField4_H 1

#include "globals.hh"
#include "G4MagneticField.hh"

#include "G4LogicalVolume.hh"
#include "G4ThreeVector.hh"

#include "EMMAElementField.hh"

#include <iomanip> 

//! global variables 
extern G4double currentCharge; //! EMMASteppingAction.cc
extern G4double userCharge; //! EMMAPrimaryGeneratorAction.cc

//class BGField4 : public G4MagneticField
class BGField4 : public EMMAElementField

{
public:

  //! Constructor
  //! offset coordinates are of the x and z location of where the field begins in reference to 
  //! the world logical volume
  //! BGField3(G4double xoffset,G4double zoffset,G4double zbefore,G4double zafter);

  BGField4(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter, G4LogicalVolume*, G4ThreeVector);

  ~BGField4();

  //! instead you can limit the size
  //! Retrieves length, width, and height (of type G4double) and returns the maximum representable finite double number


  virtual G4double GetLength() { return DBL_MAX; }
  virtual G4double GetWidth() { return DBL_MAX; }
  virtual G4double GetHeight() { return DBL_MAX; }

  //! AddFieldValue() adds the field for this element into the field[].
  //! Point() is in global coordinates.
  
  virtual void AddFieldValue( const G4double Point[3], G4double field[6]) const;

  //! declares a 75-element array of doubles named "data"
  //! and a 3-element array of doubles named "offset"
  //! declares variables i (of type G4int) and Pi (of type G4double)
  //! declares variable FieldStrength_0 (of type G4double)
 
private:

  double data[75], offset[3];

  G4int i;
  G4double Pi;
  G4double FieldStrength_0;

  //! GetFieldStrength() returns data[14]
  //! ScaleFieldStrength(G4double) sets data[14] to the
  //! product of the G4double parameter and FieldStrength_0
  //! ScaleFieldStrength(G4double) prints a line consisting of
  //! "msf: " and then the value of the G4double input

public:

  G4double GetFieldStrength() { return data[14]; }; 
  void ScaleFieldStrength( G4double msf ) { G4cout << "msf: " << msf << G4endl; data[14] = msf * FieldStrength_0 ; }; 
  
};
#endif
