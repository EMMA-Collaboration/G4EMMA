//
// Created from BuildGeometry program
//
#include "BGField3.hh"
#include "fortran_subs.inc"
#include "G4UnitsTable.hh"
#include <fstream>
#include "EMMAGlobalField.hh"

//BGField3::BGField3(G4double xoffset,G4double zoffset,G4double zbefore,G4double zafter)

BGField3::BGField3(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter, G4LogicalVolume* lv, G4ThreeVector c) : EMMAElementField(c,lv)

{

  FieldStrength_0 = -35.9652; //2*electricRigidity_0 (EMMADetectorConstruction.cc) in kV/cm

  Pi = 3.1415926535898;
  for (i = 0; i < 75; i++) data[i] = 0;

  data[0] = 1;
  data[1] = 1;
  data[2] = 1;
  data[3] = 1;
  data[10] = zbefore/cm; //drift length before quad
  data[11] = zafter/cm; //drift length after quad
  data[12] = 12.5; // gap
  data[13] = 500; //radius of ED in cm
  data[14] = FieldStrength_0;
  data[15] = 20;  //angle subtended by ED in deg (modified to get correct eff. field length)
  data[24] = 50;
  data[25] = -25;
  data[26] = -25;
  data[27] = 50;
  data[28] = 0.2401;
  data[29] = 1.8639;
  data[30] = -0.5572;
  data[31] = 0.3904;
  data[34] = 0.2401;
  data[35] = 1.8639;
  data[36] = -0.5572;
  data[37] = 0.3904;
  offset[0] = xoffset/cm; // x-coord. of beginning of field wrt world logical volume
  offset[1] = 0;
  offset[2] = zoffset/cm; // z-coord. of beginning of field wrt world logical volume
}

BGField3::~BGField3()
{
}
void BGField3::AddFieldValue(const double Point[3],G4double field[6]) const
{
	double pos[3], pos2[3];
	
	pos2[0] = Point[0]/cm - offset[0];
	pos2[1] = Point[1]/cm - offset[1];
	pos2[2] = Point[2]/cm - offset[2];
	// Rotate the position to the reference frame of the element.  These are counter clockwise rotations
	pos[0] = cos(Pi/180*0)*pos2[0] + sin(Pi/180*0)*pos2[2];

	//G4cout << "pos[0]: " << pos[0] << G4endl;

	pos[1] = pos2[1];
	pos[2] = -sin(Pi/180*0)*pos2[0] + cos(Pi/180*0)*pos2[2];
	pos[0] += -0.09375;
	pos[1] += 0;
	pos[2] += 0;
	G4double Efield[3];
	G4double Efield2[3];

        Efield[0] = 0;
	Efield[1] = 0;
	Efield[2] = 0;
	Efield2[0] = 0;
	Efield2[1] = 0;
	Efield2[2] = 0;

	mitray_edipol__(data,pos,Efield);
       

	Efield2[0] = cos(Pi/180*0)*Efield[0] - sin(Pi/180*0)*Efield[2];
	Efield2[1] = Efield[1];
	Efield2[2] = sin(Pi/180*0)*Efield[0] + cos(Pi/180*0)*Efield[2];
	

	//G4cout << "ED1 field: " << field[3] << G4endl;

	Efield2[0] *= kilovolt/cm;
        Efield2[1] *= kilovolt/cm;
	Efield2[2] *= kilovolt/cm; //0.99752

	//G4cout << "ED1 field: " << field[3] << G4endl;

	// scale Efield (work-around solution to keep charge state fixed)
	if (currentCharge!=0.0) {
	  Efield2[0] *= userCharge/currentCharge;
	  Efield2[1] *= userCharge/currentCharge;
	  Efield2[2] *= userCharge/currentCharge;
	}

	field[3] += Efield2[0];
	field[4] += Efield2[1];
	field[5] += Efield2[2];

}
