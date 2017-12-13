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
// $Id: EMMADriftChamberHit.cc,v 1.11 2006-11-14 07:11:18 perl Exp $
// --------------------------------------------------------------
//
#include "EMMADriftChamberHit.hh"
#include "G4ios.hh"
#include "G4VVisManager.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4AttDefStore.hh"
#include "G4AttDef.hh"
#include "G4AttValue.hh"
#include "G4UIcommand.hh"
#include "G4UnitsTable.hh"
#include "G4VisAttributes.hh"
#include "G4LogicalVolume.hh"
#include "G4SystemOfUnits.hh"

#include "G4SDManager.hh"
#include "G4VPrimitiveScorer.hh"
#include <assert.h>

#include "G4ParticleGun.hh"
#include "G4ParticleTable.hh"
#include "G4ParticleDefinition.hh"

#include <fstream>
#include <iostream>

G4Allocator<EMMADriftChamberHit> EMMADriftChamberHitAllocator;

EMMADriftChamberHit::EMMADriftChamberHit()
 : G4VHit(), 
   Edep(0.)
{
  layerID = -1;
  time = 0.;
}

EMMADriftChamberHit::EMMADriftChamberHit(G4int z)
: G4VHit(),
   Edep(0.)
{
  layerID = z;
  time = 0.;
}

EMMADriftChamberHit::~EMMADriftChamberHit()
{;}

EMMADriftChamberHit::EMMADriftChamberHit(const EMMADriftChamberHit &right)
    : G4VHit() {
  layerID = right.layerID;
  worldPos = right.worldPos;
  localPos = right.localPos;
  time = right.time;
  Momentum = right.Momentum;
  Ekin = right.Ekin;
  Edep = right.Edep;  

  dirn = sqrt(Momentum.x()*Momentum.x()+Momentum.y()*Momentum.y()+Momentum.z()*Momentum.z());
  theta = std::acos( Momentum.z()/dirn );
}

const EMMADriftChamberHit& EMMADriftChamberHit::operator=(const EMMADriftChamberHit &right)
{
  layerID = right.layerID;
  worldPos = right.worldPos;
  localPos = right.localPos;
  time = right.time;	
  Momentum = right.Momentum;
  Ekin = right.Ekin;
  theta = right.theta;
  Edep = right.Edep;
  
  return *this;
}

int EMMADriftChamberHit::operator==(const EMMADriftChamberHit &/*right*/) const
{
  return 0;
}

void EMMADriftChamberHit::Draw()
{
  G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
  if(pVVisManager)
  {
    G4Circle circle(worldPos);
    circle.SetScreenSize(2);
    circle.SetFillStyle(G4Circle::filled);
    G4Colour colour(1.,1.,0.);
    G4VisAttributes attribs(colour);
    circle.SetVisAttributes(attribs);
    pVVisManager->Draw(circle);
  }
}

const std::map<G4String,G4AttDef>* EMMADriftChamberHit::GetAttDefs() const
{
  G4bool isNew;
  std::map<G4String,G4AttDef>* store
    = G4AttDefStore::GetInstance("EMMADriftChamberHit",isNew);
  if (isNew) {
    G4String HitType("HitType");
    (*store)[HitType] = G4AttDef(HitType,"Hit Type","Physics","","G4String");

    G4String ID("ID");
    (*store)[ID] = G4AttDef(ID,"ID","Physics","","G4int");

    G4String Time("Time");
    (*store)[Time] = G4AttDef(Time,"Time","Physics","G4BestUnit","G4double");
	  
    G4String Pos("Pos");
    (*store)[Pos] = G4AttDef(Pos, "Position",
		      "Physics","G4BestUnit","G4ThreeVector");

    G4String Mom("Mom");
    (*store)[Mom] = G4AttDef(Mom, "Momentum",
			     "Physics","G4BestUnit","G4ThreeVector");

    G4String Ek("Ekin");
    (*store)[Ekin] = G4AttDef(Ek, "Ekin",
			      "Physics","G4BestUnit","G4double");
    
    G4String EDep("Edep");
    (*store)[Edep] = G4AttDef(EDep, "Edep",
			      "Physics","G4BestUnit","G4double");
//    G4String TL("Track Length");
//    (*store)[TrackLength] = G4AttDef(TL, "Track Length",
//			      "Physics","G4BestUnit","G4double");
	  
  }
  return store;
}

std::vector<G4AttValue>* EMMADriftChamberHit::CreateAttValues() const
{
  std::vector<G4AttValue>* values = new std::vector<G4AttValue>;

  values->push_back(G4AttValue("HitType","DriftChamberHit",""));

  values->push_back
    (G4AttValue("ID",G4UIcommand::ConvertToString(layerID),""));

  values->push_back
    (G4AttValue("Time",G4BestUnit(time,"Time"),""));

  values->push_back
    (G4AttValue("Pos",G4BestUnit(worldPos,"Length"),""));

  values->push_back
    (G4AttValue("Mom",G4BestUnit(Momentum,"Momentum"),""));

  values->push_back
    (G4AttValue("Ekin",G4BestUnit(Ekin,"Ekin"),""));

  values->push_back
    (G4AttValue("Edep",G4BestUnit(Edep,"Edep"),""));

//  values->push_back
//    (G4AttValue("Track Length",G4BestUnit(TrackLength,"Length"),""));
	
  return values;
}

void EMMADriftChamberHit::Print()
{
	std::ofstream outFile(focalPlaneFileName, std::ios::app); //Declared in EMMAPrimaryGeneratorAction
	double buffer;
	
	if(/*worldPos.z() <= 15000 && */ worldPos.y() < 10 && worldPos.y() > -10) //8815 //8875
	{
		
	  // G4String filename;
	  // std::ifstream Diag;
	  
	  // filename = MotherDir;
	  // filename.append("/output1.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tQ1  Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) <<"global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
		
	  // filename = MotherDir;
	  // filename.append("/output2.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tQ2  Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  // filename = MotherDir;
	  // filename.append("/output3.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tED1 Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  // filename = MotherDir;
	  // filename.append("/output4.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tMD  Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  // filename = MotherDir;
	  // filename.append("/output5.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tED2 Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  // filename = MotherDir;
	  // filename.append("/output6.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tQ3  Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  // filename = MotherDir;
	  // filename.append("/output7.bgf");
	  // Diag.open(filename);
	  // Diag >> buffer;
	  // G4cout << "\tQ4  Output Diagnostics x: " << buffer*10 << " mm," << std::setw(30) << "global z = ";
	  // Diag >> buffer;
	  // G4cout << buffer*10 << " mm" << G4endl;
	  // Diag.close();
	  
	  G4double X1 = worldPos.x()/mm + 542.712;		//540.164;
	  G4double Z1 = worldPos.z()/mm - 5126.64;		//3458.6;

	  G4double X2 = sqrt((X1*X1)+(Z1*Z1)); 
		
	  G4cout << "\tFocal Plane (x,y,z): " << worldPos.x()/mm << ", " << worldPos.y()/mm << ", " << worldPos.z()/mm << " mm."
		 << G4endl;
	  

	  G4cout << "\tEkin: " << Ekin << " MeV" << G4endl;

	  //G4cout << "\tEdep: " << G4BestUnit(Edep,"Energy") << G4endl;

	  //G4cout << "\tTrack Length: " << TrackLength << " mm" << G4endl;

	  G4cout <<"\tAngle " << theta/deg << " deg" << G4endl;

	  G4cout << "\tTime " << (time/s)*1000000 << " us " << G4endl;
	  
	  
	  //	outFile << Momentum.x() << ", " << Momentum.y() << ", " << Momentum.z() << ", " 
	  //	<< Ekin << ", " ;
	  dirn = sqrt(Momentum.x()*Momentum.x()+Momentum.y()*Momentum.y()+Momentum.z()*Momentum.z());
	  theta = std::acos( Momentum.z()/dirn );
      	  outFile << Ekin << ", " << theta/deg << ", " << worldPos.x()/mm << ", " << worldPos.y()/mm << std::endl;
	}
	
	outFile.close();
	
}


