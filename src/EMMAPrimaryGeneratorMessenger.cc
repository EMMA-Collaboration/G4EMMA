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
// $Id: EMMAPrimaryGeneratorMessenger.cc,v 1.4 2006-06-29 16:33:07 gunter Exp $
// --------------------------------------------------------------
//
#include "EMMAPrimaryGeneratorMessenger.hh"
#include "EMMAPrimaryGeneratorAction.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithABool.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithAnInteger.hh"
#include "G4UIcmdWithAString.hh"
#include "G4ios.hh"

EMMAPrimaryGeneratorMessenger::EMMAPrimaryGeneratorMessenger(EMMAPrimaryGeneratorAction * mpga)
:target(mpga)
{
  // mydet folder declared in EMMADetectorConstMessenger
  // ok because EMMADetectorConstMessenger is initialized first in EMMAapp.cc
  beamZCmd = new G4UIcmdWithADouble("/mydet/beamZ",this);
  beamZCmd->SetGuidance("Proton number of beam");
  beamZCmd->SetParameterName("z",true);
  beamZCmd->SetRange("z>=0.");
  beamZCmd->SetDefaultValue(50.);

  beamACmd = new G4UIcmdWithADouble("/mydet/beamA",this);
  beamACmd->SetGuidance("Nucleon number of beam");
  beamACmd->SetParameterName("a",true);
  beamACmd->SetRange("a>=0.");
  beamACmd->SetDefaultValue(132.);

  beamChargeCmd = new G4UIcmdWithADouble("/mydet/beamCharge",this);
  beamChargeCmd->SetGuidance("Charge state of beam of beam (after passage through target)");
  beamChargeCmd->SetParameterName("q",true);
  beamChargeCmd->SetRange("q>=0.");
  beamChargeCmd->SetDefaultValue(20.);

  energyCmd = new G4UIcmdWithADoubleAndUnit("/mydet/energy",this);
  energyCmd->SetGuidance("Mean energy of primaries");
  energyCmd->SetParameterName("e",true);
  energyCmd->SetRange("e>=0.");
  energyCmd->SetDefaultValue(180.);
  energyCmd->SetDefaultUnit("MeV");

  sigmaEngCmd = new G4UIcmdWithADouble("/mydet/sigmaEnergy",this);
  sigmaEngCmd->SetGuidance("Sigma energy of primaries (in % of beam energy)");
  sigmaEngCmd->SetParameterName("e",true);
  sigmaEngCmd->SetRange("e>=0.");
  sigmaEngCmd->SetDefaultValue(0.0);

  AngCmd = new G4UIcmdWithADoubleAndUnit("/mydet/Angle",this);
  AngCmd->SetGuidance("angle of primaries.  Not implemeted. To change angle need to change it in PrimaryGeneratorAction file");
  AngCmd->SetParameterName("t",true);
  AngCmd->SetRange("t<360.");
  AngCmd->SetDefaultValue(0.0);
  AngCmd->SetDefaultUnit("deg");

  beamSpotDiameterCmd = new G4UIcmdWithADoubleAndUnit("/mydet/beamSpotDiameter",this);
  beamSpotDiameterCmd->SetGuidance("Beam spot diamater in mm.");
  beamSpotDiameterCmd->SetParameterName("t",true);
  beamSpotDiameterCmd->SetRange("t>=0.");
  beamSpotDiameterCmd->SetDefaultValue(0.0);
  beamSpotDiameterCmd->SetDefaultUnit("mm");

  transEmittanceCmd = new G4UIcmdWithADoubleAndUnit("/mydet/transEmittance",this);
  transEmittanceCmd->SetGuidance("Normalized transverse emittance");
  transEmittanceCmd->SetParameterName("t",true);
  transEmittanceCmd->SetRange("t>=0.");
  transEmittanceCmd->SetDefaultValue(0.0);
  transEmittanceCmd->SetDefaultUnit("mm");

  energyDataCmd = new G4UIcmdWithAString("/mydet/energyData",this);
  energyDataCmd->SetGuidance("How the energies (spectra) of the beam is determined");
  energyDataCmd->SetParameterName("t",true);
  energyDataCmd->SetDefaultValue("SPEC");

  angularDataCmd = new G4UIcmdWithAString("/mydet/angularData",this);
  angularDataCmd->SetGuidance("How the angular distribution (spectra) of the beam is determined");
  angularDataCmd->SetParameterName("ang",true);
  angularDataCmd->SetDefaultValue("UNIF");



  doPrepareCmd = new G4UIcmdWithABool("/mydet/doPrepare",this);
  doPrepareCmd->SetGuidance("If true, file with beam data will be prepared (input for simulation).");
  doPrepareCmd->SetParameterName("flg",true);
  doPrepareCmd->SetDefaultValue(false);

  doBeamCmd = new G4UIcmdWithABool("/mydet/doBeam",this);
  doBeamCmd->SetGuidance("If true, beam will be simulated.");
  doBeamCmd->SetParameterName("flg",true);
  doBeamCmd->SetDefaultValue(false);

  doReactionCmd = new G4UIcmdWithABool("/mydet/doReaction",this);
  doReactionCmd->SetGuidance("If true, reaction will be simulated.");
  doReactionCmd->SetParameterName("flg",true);
  doReactionCmd->SetDefaultValue(false);

  nEventsCmd = new G4UIcmdWithAnInteger("/mydet/nEvents",this);
  nEventsCmd->SetGuidance("number of events in beam file");
  nEventsCmd->SetParameterName("t",true);
  nEventsCmd->SetDefaultValue(0);


  reactionDirectory = new G4UIdirectory("/twoBodyReaction/");
  reactionDirectory->SetGuidance("Nuclear reaction control commands.");

  fZ1Cmd = new G4UIcmdWithADouble("/twoBodyReaction/Z1",this);
  fZ1Cmd->SetGuidance("Z of projectile");
  fZ1Cmd->SetParameterName("Z1",true);
  fZ1Cmd->SetRange("Z1>=0.");
  fZ1Cmd->SetDefaultValue(0.);

  fA1Cmd = new G4UIcmdWithADouble("/twoBodyReaction/A1",this);
  fA1Cmd->SetGuidance("Z of projectile");
  fA1Cmd->SetParameterName("A1",true);
  fA1Cmd->SetRange("A1>=0.");
  fA1Cmd->SetDefaultValue(0.);

  fZ2Cmd = new G4UIcmdWithADouble("/twoBodyReaction/Z2",this);
  fZ2Cmd->SetGuidance("Z of projectile");
  fZ2Cmd->SetParameterName("Z2",true);
  fZ2Cmd->SetRange("Z2>=0.");
  fZ2Cmd->SetDefaultValue(0.);

  fA2Cmd = new G4UIcmdWithADouble("/twoBodyReaction/A2",this);
  fA2Cmd->SetGuidance("Z of projectile");
  fA2Cmd->SetParameterName("A2",true);
  fA2Cmd->SetRange("A2>=0.");
  fA2Cmd->SetDefaultValue(0.);

  fZ3Cmd = new G4UIcmdWithADouble("/twoBodyReaction/Z3",this);
  fZ3Cmd->SetGuidance("Z of projectile");
  fZ3Cmd->SetParameterName("Z3",true);
  fZ3Cmd->SetRange("Z3>=0.");
  fZ3Cmd->SetDefaultValue(0.);

  fA3Cmd = new G4UIcmdWithADouble("/twoBodyReaction/A3",this);
  fA3Cmd->SetGuidance("Z of projectile");
  fA3Cmd->SetParameterName("A3",true);
  fA3Cmd->SetRange("A3>=0.");
  fA3Cmd->SetDefaultValue(0.);

  fZ4Cmd = new G4UIcmdWithADouble("/twoBodyReaction/Z4",this);
  fZ4Cmd->SetGuidance("Z of projectile");
  fZ4Cmd->SetParameterName("Z4",true);
  fZ4Cmd->SetRange("Z4>=0.");
  fZ4Cmd->SetDefaultValue(0.);

  fA4Cmd = new G4UIcmdWithADouble("/twoBodyReaction/A4",this);
  fA4Cmd->SetGuidance("Z of projectile");
  fA4Cmd->SetParameterName("A4",true);
  fA4Cmd->SetRange("A4>=0.");
  fA4Cmd->SetDefaultValue(0.);

  fqminCmd = new G4UIcmdWithADoubleAndUnit("/twoBodyReaction/qmin",this);
  fqminCmd->SetGuidance("Minimum scattering angle in C.M.");
  fqminCmd->SetParameterName("qmin",true);
  fqminCmd->SetRange("qmin>=0.");
  fqminCmd->SetDefaultValue(0.);
  fqminCmd->SetDefaultUnit("deg");

  fqmaxCmd = new G4UIcmdWithADoubleAndUnit("/twoBodyReaction/qmax",this);
  fqmaxCmd->SetGuidance("Maximum scattering angle in C.M.");
  fqmaxCmd->SetParameterName("qmax",true);
  fqmaxCmd->SetRange("qmax>=0.");
  fqmaxCmd->SetDefaultValue(180.);
  fqmaxCmd->SetDefaultUnit("deg");

  fCharge3Cmd = new G4UIcmdWithADouble("/twoBodyReaction/Charge3",this);
  fCharge3Cmd->SetGuidance("Charge state of reaction product 3");
  fCharge3Cmd->SetParameterName("Charge3",true);
  fCharge3Cmd->SetRange("Charge3>=0.");
  fCharge3Cmd->SetDefaultValue(0.);

  fExcitationEnergy3Cmd = new G4UIcmdWithADoubleAndUnit("/twoBodyReaction/ExcitationEnergy3",this);
  fExcitationEnergy3Cmd->SetGuidance("Excitation energy of reaction product 3");
  fExcitationEnergy3Cmd->SetParameterName("ExcitationEnergy3",true);
  fExcitationEnergy3Cmd->SetRange("ExcitationEnergy3>=0.");
  fExcitationEnergy3Cmd->SetDefaultValue(0.);
  fExcitationEnergy3Cmd->SetDefaultUnit("MeV");

}

EMMAPrimaryGeneratorMessenger::~EMMAPrimaryGeneratorMessenger()
{
  delete beamZCmd;
  delete beamACmd;
  delete beamChargeCmd;
  delete energyCmd;
  delete sigmaEngCmd;
  delete AngCmd;
  delete beamSpotDiameterCmd;
  delete transEmittanceCmd;
  delete energyDataCmd;
  delete angularDataCmd;
  delete doPrepareCmd;
  delete doBeamCmd;
  delete doReactionCmd;
  delete nEventsCmd;

  delete fZ1Cmd;
  delete fA1Cmd;
  delete fZ2Cmd;
  delete fA2Cmd;
  delete fZ3Cmd;
  delete fA3Cmd;
  delete fZ4Cmd;
  delete fA4Cmd;
  delete fqminCmd;
  delete fqmaxCmd;
  delete fCharge3Cmd;
  delete fExcitationEnergy3Cmd;
}

void EMMAPrimaryGeneratorMessenger::SetNewValue(G4UIcommand * command,G4String newValue)
{
  // passes command line input values to EMMAPrimaryGenerator
  if( command==beamZCmd )
    { target->SetBeamZ(beamZCmd->GetNewDoubleValue(newValue)); }
  if( command==beamACmd )
    { target->SetBeamA(beamACmd->GetNewDoubleValue(newValue)); }
  if( command==beamChargeCmd )
    { target->SetBeamCharge(beamChargeCmd->GetNewDoubleValue(newValue)); }
  if( command==energyCmd )
    { target->SetEnergy(energyCmd->GetNewDoubleValue(newValue)); }
  if( command==sigmaEngCmd )
    { target->SetSigmaEnergy(sigmaEngCmd->GetNewDoubleValue(newValue)); }
  if( command==AngCmd )
    { target->SetAngle(AngCmd->GetNewDoubleValue(newValue)); }	//NOT USED
  if( command==transEmittanceCmd )
    { target->SetTransEmittance(transEmittanceCmd->GetNewDoubleValue(newValue)); }
  if( command==energyDataCmd )
    { target->SetEnergyData(newValue); }
  if( command==angularDataCmd )
    { target->SetAngularData(newValue); }
  if( command==beamSpotDiameterCmd )
    { target->SetBeamSpotDiameter(beamSpotDiameterCmd->GetNewDoubleValue(newValue)); }

  if( command==doPrepareCmd )
    { target->initializeBeamPreparation(); }
  if( command==doBeamCmd )
    { target->initializeBeamSimulation(); }
  if( command==doReactionCmd )
    { target->initializeReactionSimulation(); }

  if( command==nEventsCmd )
    { target->SetNEvents(nEventsCmd->GetNewIntValue(newValue)); }

  if( command==fZ1Cmd ) {
    target->SetZ1(fZ1Cmd->GetNewDoubleValue(newValue));
  }
  if( command==fA1Cmd ) {
    target->SetA1(fA1Cmd->GetNewDoubleValue(newValue));
  }

  if( command==fZ2Cmd ) {
    target->SetZ2(fZ2Cmd->GetNewDoubleValue(newValue));
  }
  if( command==fA2Cmd ) {
    target->SetA2(fA2Cmd->GetNewDoubleValue(newValue));
  }

  if( command==fZ3Cmd ) {
    target->SetZ3(fZ3Cmd->GetNewDoubleValue(newValue));
  }
  if( command==fA3Cmd ) {
    target->SetA3(fA3Cmd->GetNewDoubleValue(newValue));
  }

  if( command==fZ4Cmd ) {
    target->SetZ4(fZ4Cmd->GetNewDoubleValue(newValue));
  }
  if( command==fA4Cmd ) {
    target->SetA4(fA4Cmd->GetNewDoubleValue(newValue));
  }

  if( command==fqminCmd ) {
    target->Setqmin(fqminCmd->GetNewDoubleValue(newValue));
  }
  if( command==fqmaxCmd ) {
    target->Setqmax(fqmaxCmd->GetNewDoubleValue(newValue));
  }

  if( command==fCharge3Cmd ) {
    target->SetCharge3(fCharge3Cmd->GetNewDoubleValue(newValue));
  }
  if( command==fExcitationEnergy3Cmd ) {
    target->SetExcitationEnergy3(fExcitationEnergy3Cmd->GetNewDoubleValue(newValue));
  }

}

G4String EMMAPrimaryGeneratorMessenger::GetCurrentValue(G4UIcommand * command)
{
  G4String cv;
  if( command==beamZCmd )
    { cv = beamZCmd->ConvertToString(target->GetBeamZ(),""); }
  if( command==beamACmd )
    { cv = beamACmd->ConvertToString(target->GetBeamA(),""); }
  if( command==beamChargeCmd )
    { cv = beamChargeCmd->ConvertToString(target->GetBeamCharge(),""); }
  if( command==energyCmd )
    { cv = energyCmd->ConvertToString(target->GetEnergy(),"MeV"); }
  if( command==sigmaEngCmd )
    { cv = sigmaEngCmd->ConvertToString(target->GetSigmaEnergy(),""); }
  if( command==AngCmd )
    { cv = AngCmd->ConvertToString(target->GetAngle(),"deg"); }
  if( command==transEmittanceCmd )
    { cv = transEmittanceCmd->ConvertToString(target->GetTransEmittance(),"mm"); }
  if( command==energyDataCmd )
    { cv = target->GetEnergyData(); }
  if( command==angularDataCmd )
    { cv = target->GetAngularData(); }
  if( command==beamSpotDiameterCmd )
    { cv = beamSpotDiameterCmd->ConvertToString(target->GetBeamSpotDiameter(),"mm"); }

  if( command==nEventsCmd )
    { cv = nEventsCmd->ConvertToString(target->GetNEvents(),""); }

  if( command==fZ1Cmd )
  { cv = fZ1Cmd->ConvertToString(target->GetZ1(),""); }
  if( command==fA1Cmd )
  { cv = fA1Cmd->ConvertToString(target->GetA1(),""); }

  if( command==fZ2Cmd )
  { cv = fZ2Cmd->ConvertToString(target->GetZ2(),""); }
  if( command==fA2Cmd )
  { cv = fA2Cmd->ConvertToString(target->GetA2(),""); }

  if( command==fZ3Cmd )
  { cv = fZ3Cmd->ConvertToString(target->GetZ3(),""); }
  if( command==fA3Cmd )
  { cv = fA3Cmd->ConvertToString(target->GetA3(),""); }

  if( command==fZ4Cmd )
  { cv = fZ4Cmd->ConvertToString(target->GetZ4(),""); }
  if( command==fA4Cmd )
  { cv = fA4Cmd->ConvertToString(target->GetA4(),""); }

  if( command==fqminCmd )
  { cv = fqminCmd->ConvertToString(target->Getqmin(),"deg"); }
  if( command==fqmaxCmd )
  { cv = fqmaxCmd->ConvertToString(target->Getqmax(),"deg"); }

  if( command==fCharge3Cmd )
  { cv = fCharge3Cmd->ConvertToString(target->GetCharge3(),""); }
  if( command==fExcitationEnergy3Cmd )
  { cv = fExcitationEnergy3Cmd->ConvertToString(target->GetExcitationEnergy3(),"MeV"); }

  return cv;
}
