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
// $Id: EMMADetectorConstMessenger.cc,v 1.4 2006-06-29 16:32:01 gunter Exp $
// --------------------------------------------------------------
//
#include "EMMADetectorConstMessenger.hh"
#include "EMMADetectorConstruction.hh"
#include "G4UIdirectory.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithoutParameter.hh"
#include "G4ios.hh"

EMMADetectorConstMessenger::EMMADetectorConstMessenger(EMMADetectorConstruction* mpga)
:target(mpga)
{
  mydetDirectory = new G4UIdirectory("/mydet/");
  mydetDirectory->SetGuidance("EMMA detector setup control commands.");

  centralZCmd = new G4UIcmdWithADouble("/mydet/centralZ",this);
  centralZCmd->SetGuidance("Z of central trajectory");
  centralZCmd->SetParameterName("Z",true);
  centralZCmd->SetRange("Z>=0.");
  centralZCmd->SetDefaultValue(54.);

  centralACmd = new G4UIcmdWithADouble("/mydet/centralA",this);
  centralACmd->SetGuidance("A of central trajectory");
  centralACmd->SetParameterName("A",true);
  centralACmd->SetRange("A>=0.");
  centralACmd->SetDefaultValue(100.);

  centralQCmd = new G4UIcmdWithADouble("/mydet/centralQ",this);
  centralQCmd->SetGuidance("Q of central trajectory");
  centralQCmd->SetParameterName("Q",true);
  centralQCmd->SetRange("Q>=0.");
  centralQCmd->SetDefaultValue(20.);

  centralECmd = new G4UIcmdWithADoubleAndUnit("/mydet/centralE",this);
  centralECmd->SetGuidance("E of central trajectory");
  centralECmd->SetParameterName("E",true);
  centralECmd->SetRange("E>=0.");
  centralECmd->SetDefaultValue(180.);
  centralECmd->SetDefaultUnit("MeV");

  updategeoCmd = new G4UIcmdWithoutParameter("/mydet/updategeo",this);
  updategeoCmd->SetGuidance("Update geometry");

}

EMMADetectorConstMessenger::~EMMADetectorConstMessenger()
{
  delete mydetDirectory;
  delete centralZCmd;
  delete centralACmd;
  delete centralQCmd;
  delete centralECmd;
  delete updategeoCmd;
}


void EMMADetectorConstMessenger::SetNewValue(G4UIcommand * command,G4String newValue)
{
  if( command==centralZCmd )
    { target->SetCentralZ(centralZCmd->GetNewDoubleValue(newValue)); }
  if( command==centralACmd )
    { target->SetCentralA(centralACmd->GetNewDoubleValue(newValue)); }
  if( command==centralQCmd )
    { target->SetCentralQ(centralQCmd->GetNewDoubleValue(newValue)); }
  if( command==centralECmd )
    { target->SetCentralE(centralECmd->GetNewDoubleValue(newValue)); }
  if( command==updategeoCmd )
    { target->CalculateScalingFactors(); }
}

G4String EMMADetectorConstMessenger::GetCurrentValue(G4UIcommand * command)
{
  G4String cv;
  if( command==centralZCmd )
    { cv = centralZCmd->ConvertToString(target->GetCentralZ(),""); }
  if( command==centralACmd )
    { cv = centralACmd->ConvertToString(target->GetCentralA(),""); }
  if( command==centralQCmd )
    { cv = centralQCmd->ConvertToString(target->GetCentralQ(),""); }
  if( command==centralECmd )
    { cv = centralECmd->ConvertToString(target->GetCentralE(),"MeV"); }

  return cv;
}
