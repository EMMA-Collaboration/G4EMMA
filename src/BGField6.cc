//
// Created from BuildGeometry program
//
#include "BGField6.hh"
#include "fortran_subs.inc"
#include "G4UnitsTable.hh"
#include <fstream>

#include "EMMAGlobalField.hh"

//BGField6::BGField6(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter)

BGField6::BGField6(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter, G4LogicalVolume* lv, G4ThreeVector c) : EMMAElementField(c,lv)

{

  FieldStrength_0 = -0.60155; //from specifications in T*m

  Pi = 3.1415926535898;
  for (i = 0; i < 75; i++) data[i] = 0;

  data[0] = 1;
  data[1] = 1;
  data[2] = 1;
  data[9] = zbefore/cm; //drift length before quad
  data[10] = zafter/cm; //drift length after quad
  data[11] = 29.882; //effective field length in cm
  data[12] = 7.5; //poll radius in cm
  data[13] = FieldStrength_0;
  data[18] = 20;
  data[19] = -10;
  data[20] = -10;
  data[21] = 20;
  data[22] = 0.1122;
  data[23] = 6.2671;
  data[24] = -1.4982;
  data[25] = 3.5882;
  data[26] = -2.1209;
  data[27] = 1.723;
  data[28] = 0.1122;
  data[29] = 6.2671;
  data[30] = -1.4982;
  data[31] = 3.5882;
  data[32] = -2.1209;
  data[33] = 1.723;
  data[34] = 1;
  data[35] = 1;
  data[36] = 1;
  data[37] = 1;
  offset[0] = xoffset/cm; // x-coord. of beginning of field wrt world logical volume
  offset[1] = 0;
  offset[2] = zoffset/cm; // z-coord. of beginning of field wrt world logical volume

}

BGField6::~BGField6()
{
}
void BGField6::AddFieldValue(const double Point[3],G4double field[6]) const
{
	double pos[3], pos2[3];
	
	pos2[0] = Point[0]/cm - offset[0];
	pos2[1] = Point[1]/cm - offset[1];
	pos2[2] = Point[2]/cm - offset[2];
	// Rotate the position to the reference frame of the element.  These are counter clockwise rotations
	pos[0] = cos(Pi/180*360)*pos2[0] + sin(Pi/180*360)*pos2[2];
    pos[1] = pos2[1];
	pos[2] = -sin(Pi/180*360)*pos2[0] + cos(Pi/180*360)*pos2[2];
	pos[0] += 0;
	pos[1] += 0;
	pos[2] += 0;
	G4double Bfield[3];
	Bfield[0] = 0;
	Bfield[1] = 0;
	Bfield[2] = 0;
	mitray_poles__(data,pos,Bfield);
	
	Bfield[0] = cos(Pi/180*360)*Bfield[0] - sin(Pi/180*360)*Bfield[2];
	Bfield[1] = Bfield[1];
	Bfield[2] = sin(Pi/180*360)*Bfield[0] + cos(Pi/180*360)*Bfield[2];
	

	Bfield[0] *= tesla;
	Bfield[1] *= tesla;
	Bfield[2] *= tesla;

	// scale Bfield (work-around solution to keep charge state fixed)
	if (currentCharge!=0.0) {
	  Bfield[0] *= userCharge/currentCharge;
	  Bfield[1] *= userCharge/currentCharge;
	  Bfield[2] *= userCharge/currentCharge;
	}
	field[0] += Bfield[0];
	field[1] += Bfield[1];
	field[2] += Bfield[2];
}
