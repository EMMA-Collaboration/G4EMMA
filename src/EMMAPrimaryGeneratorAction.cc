//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// $Id: EMMAPrimaryGeneratorAction.cc,v 1.5 2006-06-29 16:33:05 gunter Exp $
// --------------------------------------------------------------
//

#include "EMMAPrimaryGeneratorAction.hh"
#include "EMMAPrimaryGeneratorMessenger.hh"

#include "G4Event.hh"
#include "G4ParticleGun.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"
#include "Randomize.hh"
#include "G4ios.hh"
#include "G4UnitsTable.hh"

#include "G4Track.hh"
#include "G4Step.hh"
#include "G4ParticleDefinition.hh"
#include "G4ParticleTypes.hh"

#include "G4PhysicalConstants.hh"
#include "G4SystemOfUnits.hh"
#include "G4IonTable.hh"
#include "G4NucleiProperties.hh"

#include "G4RunManager.hh"
#include "G4LogicalVolumeStore.hh"
#include "G4LogicalVolume.hh"
#include "G4VSolid.hh"
#include "G4Box.hh"

#include <string> //words and sentences
#include <fstream> //Stream class to both read and write from/to files
#include <sstream>
using namespace std;



// global variables 
G4bool prepareBeam = true;
G4String inTargetFileName;
G4String postTargetFileName;
G4String focalPlaneFileName;
G4double userCharge = 54.; // default value
G4String postDegrader1FileName;
G4double depth;

EMMAPrimaryGeneratorAction::EMMAPrimaryGeneratorAction()  // constructor
{
	sigmaEnergy = 0.*MeV;
	transEmittance = 0.*mm*mrad;

	G4int n_particle = 1;
	//G4ParticleGun class generates primary particle(s) with a given momentum and position
	particleGun  = new G4ParticleGun(n_particle);

	//create a messenger for this class
	gunMessenger = new EMMAPrimaryGeneratorMessenger(this);

	// default particle kinematics
	particleGun->SetParticlePosition(G4ThreeVector(0.,0.,0.*m));
	particleGun->SetParticleMomentumDirection(G4ThreeVector(0.,0.,1.));

	G4Step aStep;
	
	energy = aStep.GetTotalEnergyDeposit(); // =0
	Angle = 0;

	// default values
	nEvents = 1e6;
	fZ1=0.,fA1=0., fZ2=0.,fA2=0.;
	fZ3=0.,fA3=0., fZ4=0.,fA4=0.;
	fqmin = 180.*deg;
	fqmax = 0.*deg;
	fCharge3 = 0.;

	inTargetFileName = UserDir + "/BeamSampling/beam.dat";  //Used in EMMASteppingAction

	// alpha-source input file
	useAlphaSource = false;
	G4String text, line;
	ifstream inputfil;
	G4String filename = UserDir + "/UserInput/alphaSource.dat";
	inputfil.open ( filename, ios::in );
	if ( inputfil.is_open() ) {
	  int n=0;
	  while ( inputfil.good() ) {
	    inputfil >> text;
	    if (text=="#") { // skip comments
	      getline (inputfil,line);
	    }
	    else {
	      n = n+1;
	      if (n==1) {if (text=="YES") useAlphaSource = true;}
	      if (n==2) {energyAlphaSource = atof(text.c_str());} 
	      if (n==3) {maxAngleAlphaSource = atof(text.c_str());} 
	    }
	  }
	  inputfil.close();
	}
	else G4cout << "Unable to open " << filename << G4endl; 

}

EMMAPrimaryGeneratorAction::~EMMAPrimaryGeneratorAction()
{
  delete particleGun;	//must delete G4ParticleGun
  delete gunMessenger;
}

void EMMAPrimaryGeneratorAction::GeneratePrimaries(G4Event* anEvent)
{ // This method is invoked at the beginning of each event

  G4double randNumb;
  if (prepareBeam) randNumb = G4UniformRand(); //used to chose a random reaction depth
  else randNumb = 1.0;
  
  
  // this is to update target thickness
  // <><><><><><><><><><><><><><><><><><><><> //
  depth = targetThickness * randNumb;
  G4VSolid* targetSolid = new G4Box("target",5.*cm,5.*cm,depth/2.0);
  G4LogicalVolumeStore* logVolStore = G4LogicalVolumeStore::GetInstance();	
  G4LogicalVolume* target = logVolStore->GetVolume("targetLogical",true); //"targetLogical" declared in DetectorConstruction
  target->SetSolid(targetSolid);
  G4RunManager::GetRunManager()->ReOptimize( target );
  // <><><><><><><><><><><><><><><><><><><><> //
		
  G4double Ekin;
  G4ParticleDefinition* particleDef;


  // to simulate just an isotropic alpha source
  if (useAlphaSource) {	//boolean determined from alphaSource.dat input file
    simulateReaction = false;
    
    // Ion (values read in from alphaSource.dat in constructor)
    /* 
       NB: The simulation terminates with a bus error if we pick an alpha particle, so instead
       we "cheat" and use a 6Li(3+) ion instead, while scaling the kinetic energy by a factor of 1.5
    */
    particleDef = G4ParticleTable::GetParticleTable()->GetIonTable()->GetIon(3,6,0.0); //FindParticle("alpha");
    particleGun->SetParticleDefinition(particleDef);
    // charge
    userCharge = 3; // userCharge is a global variable! (used e.g. in BGFields1-7.cc)
    particleGun->SetParticleCharge(userCharge);    
    // Energy
    Ekin = energyAlphaSource * 1.5;
    particleGun->SetParticleEnergy(Ekin *MeV);
    // Position
    G4double xBeam=0.*m, yBeam=0.*m;
    particleGun->SetParticlePosition(G4ThreeVector(xBeam,yBeam,0.0*m));
    // Sample angle
    G4double x, y, z;
    G4double costh = 1.0 - G4UniformRand() * (1-std::cos(maxAngleAlphaSource*CLHEP::pi/180));
    G4double theta = std::acos(costh);
    G4double phi = G4UniformRand()*CLHEP::twopi;
    x = std::sin(theta) * std::cos(phi);
    y = std::sin(theta) * std::sin(phi);
    z = std::cos(theta);
    particleGun->SetParticleMomentumDirection(G4ThreeVector(x,y,z));

  }


  // BEAM
  if (simulateReaction==false && !useAlphaSource) {
    // Ion
    particleDef = G4ParticleTable::GetParticleTable()->GetIonTable()->GetIon(beamZ,beamA,0.0);
    particleGun->SetParticleDefinition(particleDef);
    particleGun->SetParticleCharge(userCharge);    
    // Sample energy
    Ekin = energy;
    if (sigmaEnergy>0.) { 
      G4double mean = energy;
      G4double FWHM = sigmaEnergy/100.*energy;
      G4double std = FWHM/2.35; 
      Ekin = CLHEP::RandGauss::shoot(mean,std);
    }
//---------------------------------------------------------------------------------------//
    //energy including spread
    particleGun->SetParticleEnergy(Ekin *MeV);
    //fixed energy
    //particleGun->SetParticleEnergy(energy *MeV);
//---------------------------------------------------------------------------------------//
    // Sample position
    G4double xBeam=0.*m, yBeam=0.*m;
    G4double xBeam_offset=0.*mm, yBeam_offset=0.*mm;
    G4double r=0.*mm;
    G4double rmax = beamSpotDiameter / 2.0;
    if (beamSpotDiameter>0.) {
      r = G4UniformRand() * rmax;
      G4double phi = G4UniformRand()*CLHEP::twopi;
      xBeam = r*std::cos(phi) + xBeam_offset;
      yBeam = r*std::sin(phi) + yBeam_offset;
    }
    // beam particle z emission location is set to 10 Angstroms, i.e. immediately, in front of the target
    G4double zemit = targetZoffset - targetThickness/2 - 10*angstrom;
//---------------------------------------------------------------------------------------//
    //random emittance off optical axis
    particleGun->SetParticlePosition(G4ThreeVector(xBeam,yBeam,zemit));
    //fix emittance location to optical axis
    //particleGun->SetParticlePosition(G4ThreeVector(0,0,zemit));
//---------------------------------------------------------------------------------------//
    // Determine max angle from normalized transverse emittance
    G4double mass = particleDef->GetPDGMass();
    G4double Etot = Ekin + mass;
    G4double gamma = Etot/mass;
    G4double beta = sqrt(1.0-1.0/(gamma*gamma));
    G4double MaxAngle = 0.0*deg;
    if (rmax>0) {
      MaxAngle = transEmittance/(gamma*beta*rmax);
      MaxAngle = MaxAngle * 180./CLHEP::pi/1000. * deg; // mrad to deg conversion
    }
        
	G4cout << "RADIUS: " << rmax/mm << " mm" << G4endl;
        G4cout << "ANGLE: " << MaxAngle/deg << " deg" << G4endl;

    // Sample angle
    G4double x=0., y=0., z=1.;
    G4double theta = 0.0*deg; G4double THETA = 0.0*deg;
    G4double phi = 0.0*deg;
//---------------------------------------------------------------------------------------//
   
 //fixed angles  
    x = sin(theta) * cos(phi);
    y = sin(theta) * sin(phi);
    z = cos(theta);

 //random angles
    if (MaxAngle>0.) {
      theta = G4UniformRand() * MaxAngle * sqrt(1.0-(r/rmax)*(r/rmax));
      phi = G4UniformRand()*CLHEP::twopi;
      THETA = Angle + theta*cos(phi);
      x = sin(THETA); 
      y = sin(theta) * sin(phi);
      z = cos(THETA);
    }

    particleGun->SetParticleMomentumDirection(G4ThreeVector(x,y,z));
    
    G4cout<<"Prim.Gen.Action output "<<"Energy(MeV)= "<<energy <<" z emission location (mm) "
          <<zemit/mm<< "Angle Offset (deg): "<< Angle/deg << " theta (deg)= "<< theta/deg <<" phi(deg)= "<< phi/deg << " THETA(deg)= "<< THETA/deg <<G4endl;

    G4cout<<"Momentum Dir [x,y,z]: ["<< x <<","<< y << "," << z << "]" << G4endl;
   

//---------------------------------------------------------------------------------------//
  }


  // REACTION (values read in from beam.dat in EMMAapp)
  else if (simulateReaction) {
    G4int id=anEvent->GetEventID();
    Ekin = energyBeam[id]; //from initializeReactionSimulation()
    G4ThreeVector dir(dirxBeam[id],diryBeam[id],dirzBeam[id]);	  
    if (fZ1==0.) {
      G4cout << "ERROR: Two-body reaction not defined" << G4endl;
      exit (EXIT_FAILURE);
    }
    simulateTwoBodyReaction( Ekin, dir );
    G4int Z3=fZ3, A3=fA3;
    G4double Ex = fExcitationEnergy3;
    particleDef = G4ParticleTable::GetParticleTable()->GetIonTable()->GetIon(Z3,A3,Ex);  // Create new ion
    particleGun->SetParticleDefinition(particleDef);

    particleGun->SetParticleEnergy(Ekin*MeV);
    particleGun->SetParticleMomentumDirection(dir);
    particleGun->SetParticleCharge(userCharge);
    G4double x=posxBeam[id]*mm, y=posyBeam[id]*mm, z=poszBeam[id]*mm;
    G4double dz=depth/2.;
    z = z-dz; // correction needed because target placement refers to center of target ...
    particleGun->SetParticlePosition(G4ThreeVector( x, y, z ));
    
  }
  
  
  particleGun->GeneratePrimaryVertex(anEvent);
  
  
  // Print info:
  G4bool printInfo=true;
  G4double mass = particleDef->GetPDGMass();
  G4double pp = std::sqrt((Ekin + mass)*(Ekin + mass)-(mass*mass));
  G4double charge = particleGun->GetParticleCharge();	
  mass = mass / 931.4940954;  // convert to amu
  if (printInfo) {
    G4cout << "\n Momentum " << pp << ", Mass " << mass << " amu, Ekin " 
	   << Ekin << " MeV, Charge " << charge << G4endl;
  }

}





void EMMAPrimaryGeneratorAction::initializeReactionSimulation() // called using /mydet/doReaction
{
  prepareBeam = false;
  simulateReaction = true;
  userCharge = fCharge3; //read in from reaction.dat in EMMAapp
  std::ofstream outfile; 
  focalPlaneFileName = UserDir;
  focalPlaneFileName.append("/Results/fp_reaction.dat"); //Used in EMMADriftChamberHit
  outfile.open (focalPlaneFileName);
  outfile.close();
  postTargetFileName = UserDir;
  postTargetFileName.append("/Results/postTarget_reaction.dat"); //Used in EMMASteppingAction
  outfile.open (postTargetFileName);
  outfile.close();
  postDegrader1FileName = UserDir;
  postDegrader1FileName.append("/Results/postDegrader1_reaction.dat"); //Used in EMMASteppingAction
  outfile.open (postDegrader1FileName);
  outfile.close();

  // read in previously simulated data
  energyBeam = new G4double[nEvents];
  posxBeam = new G4double[nEvents];
  posyBeam = new G4double[nEvents];
  poszBeam = new G4double[nEvents];
  dirxBeam = new G4double[nEvents];
  diryBeam = new G4double[nEvents];
  dirzBeam = new G4double[nEvents];
  std::ifstream beamFile(inTargetFileName, std::ios::in); //read in from /BeamSampling/beam.dat
  for (int i=0; i<nEvents; i++) {
    beamFile >> energyBeam[i];
    beamFile >> posxBeam[i];
    beamFile >> posyBeam[i];
    beamFile >> poszBeam[i];
    beamFile >> dirxBeam[i];
    beamFile >> diryBeam[i];
    beamFile >> dirzBeam[i];
  }
  beamFile.close();	

  // simulated nEvents
  G4RunManager::GetRunManager()->BeamOn(nEvents);
}



void EMMAPrimaryGeneratorAction::initializeBeamSimulation() // called using /mydet/doBeam
{
  prepareBeam = false;
  simulateReaction = false;
  userCharge = beamCharge; //read in from beam.dat in EMMAapp
  std::ofstream outfile;
  focalPlaneFileName = UserDir;
  focalPlaneFileName.append("/Results/fp_beam.dat"); //Used in EMMADriftChamberHit
  outfile.open (focalPlaneFileName);
  outfile.close();	  
  postTargetFileName = UserDir;
  postTargetFileName.append("/Results/postTarget_beam.dat"); //Used in EMMASteppingAction
  outfile.open (postTargetFileName);
  outfile.close();
  postDegrader1FileName = UserDir;
  postDegrader1FileName.append("/Results/postDegrader1_beam.dat"); //Used in EMMASteppingAction
  outfile.open (postDegrader1FileName);
  outfile.close();

  // simulated nEvents
  G4RunManager::GetRunManager()->BeamOn(nEvents);
}



void EMMAPrimaryGeneratorAction::initializeBeamPreparation() // called using /mydet/doPrepare
{
  prepareBeam = true;
  simulateReaction = false;
  userCharge = beamCharge; //read in from beam.dat in EMMAapp
  std::ofstream outfile; 
  outfile.open (inTargetFileName); //declared in constructor
  outfile.close();

  // simulated nEvents
  G4RunManager::GetRunManager()->BeamOn(nEvents);
}



void EMMAPrimaryGeneratorAction::simulateTwoBodyReaction( G4double &Ebeam, G4ThreeVector &dir ) 
{

  // Z and A of projectile and target (1+2):
  G4int Z1 = fZ1;
  G4int A1 = fA1;
  G4int Z2 = fZ2;
  G4int A2 = fA2;

  // masses of 1+2
  G4double m1 = G4NucleiProperties::GetNuclearMass(A1, Z1);
  G4double m2 = G4NucleiProperties::GetNuclearMass(A2, Z2);

  // Z and A of reaction products (3+4):
  G4int Z3 = fZ3;
  G4int A3 = fA3;
  G4int Z4 = fZ4;
  G4int A4 = fA4;

  // masses of 3+4
  G4double m3 = G4NucleiProperties::GetNuclearMass(A3, Z3);
  G4double m4 = G4NucleiProperties::GetNuclearMass(A4, Z4);

  // take into account excitation energy of fragment 3
  m3 = m3 + fExcitationEnergy3;

  // 4-momentum of beam
  G4double p1n = sqrt( (m1+Ebeam)*(m1+Ebeam) - m1*m1 );
  G4double dirn = sqrt( dir[0]*dir[0] + dir[1]*dir[1] + dir[2]*dir[2] );
  G4ThreeVector p1 = dir/dirn*p1n;

  // determine velocity of CM frame relative to LAB frame
  G4LorentzVector lv1(p1,m1+Ebeam);
  G4LorentzVector lv2(0.0,0.0,0.0,m2);   
  G4LorentzVector lv = lv1 + lv2;
  G4ThreeVector bst = lv.boostVector(); // divides the spatial component (p) by the time component (E)

  // transform 4-momenta to CM frame
  lv1.boost(-bst);
  lv2.boost(-bst);
  G4double etot = lv1[3] + lv2[3]; // total energy in CM

  // if energy is insufficient, nothing happens
  if (etot<m3+m4) {
    G4cout << "NOT ENOUGH ENERGY FOR REACTION" << G4endl;
  }

  // Compute c.m. energies and momentum of reaction products (3+4)
  G4double e3 = ( etot*etot + m3*m3 - m4*m4 ) / (2*etot); 
  G4double e4 = etot - e3; 
  G4double pcm = sqrt( e3*e3 - m3*m3 ); 

  // Max and min angles
  G4double fqrmax = (180-fqmin/deg)*deg; //compute recoil c.m. angles from ejectile c.m. angles
  G4double fqrmin = (180-fqmax/deg)*deg;
  G4double thetaCMmin = fqrmin;
  G4double t1 = std::cos(thetaCMmin/rad);
  G4double thetaCMmax = fqrmax;
  G4double t2 = std::cos(thetaCMmax/rad);
  
  // Sampling of directions in CM system
  G4double t    = G4UniformRand();
  G4double phi  = G4UniformRand()*CLHEP::twopi;
  G4double cost = t1 - (t1-t2)*t;
  G4double sint = std::sqrt((1.0-cost)*(1.0+cost));
  
  // Lorentz vectors of reaction products (3+4)
  G4ThreeVector v3(sint*std::cos(phi),sint*std::sin(phi),cost);
  v3 = v3 * pcm;
  G4ThreeVector v4 = -v3;
  G4LorentzVector lv3(v3.x(),v3.y(),v3.z(),e3);
  G4LorentzVector lv4(v4.x(),v4.y(),v4.z(),e4);
  
  // Transform to LAB frame
  lv3.boost(bst);
  lv4.boost(bst);
    
  // Kinetic energy in lab of product #3
  Ebeam = lv3[3] - m3;

  // Momentum in lab of product #3
  dir[0] = lv3[0];
  dir[1] = lv3[1];
  dir[2] = lv3[2];

}
