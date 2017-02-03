//
// Created from BuildGeometry program
//
#include "BGField4.hh"

#include "fortran_subs.inc"
//#include "G4SystemOfUnits.hh"
#include "G4UnitsTable.hh"

#include "EMMAGlobalField.hh"

#include <fstream>


//BGField4::BGField4(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter)

BGField4::BGField4(G4double xoffset, G4double zoffset,G4double zbefore,G4double zafter, G4LogicalVolume* lv, G4ThreeVector c) : EMMAElementField(c,lv)

{

  FieldStrength_0 = 0.966274; //magneticRigidity_0*1./(2.99792458e2) (EMMADetectorConstruction.cc) in T*m

  Pi = 3.1415926535898;
  for (i = 0; i < 75; i++) data[i] = 0;

  data[0] = 1;	// entrance fringe field integration step size (cm)
  data[1] = 1;	// uniform field integration step size (cm)
  data[2] = 1;	// exit fringe field integration step size (cm)
  data[3] = 1;	// differentil step size used for determining off-midplane B-field components (cm)
  data[4] = 2;	// uniform field dipole with fringe fields determined in a particular way.
  data[10] = zbefore/cm; //drift length before previous element EFB
  data[11] = zafter/cm; //drift length to subsequent element EFB
  data[12] = 12;	// gap length (cm)
  data[13] = 100;	//radius of MD in cm
  data[14] = FieldStrength_0;	// nominal value of field on central radius (T)
  data[15] = 40;	//40.165; //; //angle subtended by MD in deg (modified to get correct eff. field length) Original = 40 , Modified = 39.89865
  data[16] = 8.2993;	// angle between central trajectory & normal to entrance EFB (deg)
  data[17] = 8.2993;	// angle between central trajectory & normal to exit EFB (deg)
  data[24] = 50;	// integration limit defining start of entrance fringefield zone in co-ordinate system of entrance EFB
  data[25] = -25;	// integration limit defining end of entrance fringefield zone in co-ordinate system of entrance EFB
  data[26] = -25;	// integration limit defining start of exit fringefield zone in co-ordinate system of exit EFB
  data[27] = 50;	// integration limit defining end of exit fringefield zone in co-ordinate system of exit EFB
  data[28] = 0.2401;	//{
  data[29] = 1.8639;	//	coefficients used in expansion of fringing field at entrance	
  data[30] = -0.5572;	//
  data[31] = 0.3904;	//}
  data[34] = 0.2401;	//{
  data[35] = 1.8639;	//	coefficients used in expansion of fringing field at exit
  data[36] = -0.5572;	//
  data[37] = 0.3904;	//}
  data[46] = 0.00288;	// inverse of radius of curvature of entrance boundary (cm^-1)
  data[47] = 0.00288;	// inverse of radius of curvature of exit boundary (cm^-1)
  data[48] = 40;	// ?
  data[49] = 40;	// ?
  offset[0] = xoffset/cm; // x-coord. of beginning of field wrt world logical volume
  offset[1] = 0;
  offset[2] = zoffset/cm; // z-coord. of beginning of field wrt world logical volume
}

BGField4::~BGField4()
{
}
void BGField4::AddFieldValue(const double Point[3],G4double field[6]) const
{
	double pos[3], pos2[3];
	
	pos2[0] = Point[0]/cm - offset[0];
	pos2[1] = Point[1]/cm - offset[1];
	pos2[2] = Point[2]/cm - offset[2];
	// Rotate the position to the reference frame of the element.  These are counter clockwise rotations
	pos[0] = cos(Pi/180*20)*pos2[0] + sin(Pi/180*20)*pos2[2];
    pos[1] = pos2[1];
	pos[2] = -sin(Pi/180*20)*pos2[0] + cos(Pi/180*20)*pos2[2];
	pos[0] = -pos[0];
	pos[1] = -pos[1];
	pos[0] += 0.4087;
	pos[1] += 0;
	pos[2] += 0;
	G4double Bfield[3];
	G4double Bfield2[3];
	Bfield[0] = 0;
	Bfield[1] = 0;
	Bfield[2] = 0;
	Bfield2[0] = 0;
	Bfield2[1] = 0;
	Bfield2[2] = 0;

	mitray_dipole__(data,pos,Bfield);

	Bfield2[0] = cos(Pi/180*20)*Bfield[0] - sin(Pi/180*20)*Bfield[2];
	Bfield2[1] = Bfield[1];
	Bfield2[2] = sin(Pi/180*20)*Bfield[0] + cos(Pi/180*20)*Bfield[2];


	Bfield2[0] *= -tesla;
	Bfield2[1] *= -tesla;
	Bfield2[2] *= tesla;//*0.98114;

	// scale Bfield (work-around solution to keep charge state fixed)
	if (currentCharge!=0.0) {
	  Bfield2[0] *= userCharge/currentCharge;
	  Bfield2[1] *= userCharge/currentCharge;
	  Bfield2[2] *= userCharge/currentCharge;
	}
	G4cout<<std::setprecision(14);

	field[0] += Bfield2[0];
	field[1] += Bfield2[1];
	field[2] += Bfield2[2];
}
