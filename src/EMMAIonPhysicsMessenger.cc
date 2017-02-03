#include "EMMAIonPhysicsMessenger.hh"
#include "EMMAIonPhysics.hh"
#include "G4UIcmdWithADoubleAndUnit.hh"
#include "G4UIcmdWithADouble.hh"
#include "G4UIcmdWithABool.hh"
#include "G4ios.hh"


EMMAIonPhysicsMessenger::EMMAIonPhysicsMessenger(EMMAIonPhysics * ionphys)
:fTarget(ionphys)
{
  reactionDirectory = new G4UIdirectory("/reaction/");
  reactionDirectory->SetGuidance("Nuclear reaction control commands.");

  fcsCmd = new G4UIcmdWithADoubleAndUnit("/reaction/cs",this);
  fcsCmd->SetGuidance("Cross section in mb");
  fcsCmd->SetParameterName("cs",true);
  fcsCmd->SetRange("cs>=0.");
  fcsCmd->SetDefaultValue(0.);
  fcsCmd->SetDefaultUnit("millibarn");

  fZ1Cmd = new G4UIcmdWithADouble("/reaction/Z1",this);
  fZ1Cmd->SetGuidance("Z of projectile");
  fZ1Cmd->SetParameterName("Z1",true);
  fZ1Cmd->SetRange("Z1>=0.");
  fZ1Cmd->SetDefaultValue(0.);

  fA1Cmd = new G4UIcmdWithADouble("/reaction/A1",this);
  fA1Cmd->SetGuidance("A of projectile");
  fA1Cmd->SetParameterName("A1",true);
  fA1Cmd->SetRange("A1>=0.");
  fA1Cmd->SetDefaultValue(0.);

  fZ2Cmd = new G4UIcmdWithADouble("/reaction/Z2",this);
  fZ2Cmd->SetGuidance("Z of target");
  fZ2Cmd->SetParameterName("Z2",true);
  fZ2Cmd->SetRange("Z2>=0.");
  fZ2Cmd->SetDefaultValue(0.);

  fA2Cmd = new G4UIcmdWithADouble("/reaction/A2",this);
  fA2Cmd->SetGuidance("A of target");
  fA2Cmd->SetParameterName("A2",true);
  fA2Cmd->SetRange("A2>=0.");
  fA2Cmd->SetDefaultValue(0.);

  fZ3Cmd = new G4UIcmdWithADouble("/reaction/Z3",this);
  fZ3Cmd->SetGuidance("Z of recoil");
  fZ3Cmd->SetParameterName("Z3",true);
  fZ3Cmd->SetRange("Z3>=0.");
  fZ3Cmd->SetDefaultValue(0.);

  fA3Cmd = new G4UIcmdWithADouble("/reaction/A3",this);
  fA3Cmd->SetGuidance("A of recoil");
  fA3Cmd->SetParameterName("A3",true);
  fA3Cmd->SetRange("A3>=0.");
  fA3Cmd->SetDefaultValue(0.);

  fZ4Cmd = new G4UIcmdWithADouble("/reaction/Z4",this);
  fZ4Cmd->SetGuidance("Z of ejectile");
  fZ4Cmd->SetParameterName("Z4",true);
  fZ4Cmd->SetRange("Z4>=0.");
  fZ4Cmd->SetDefaultValue(0.);

  fA4Cmd = new G4UIcmdWithADouble("/reaction/A4",this);
  fA4Cmd->SetGuidance("A of ejectile");
  fA4Cmd->SetParameterName("A4",true);
  fA4Cmd->SetRange("A4>=0.");
  fA4Cmd->SetDefaultValue(0.);

  fqmaxCmd = new G4UIcmdWithADoubleAndUnit("/reaction/qmax",this);
  fqmaxCmd->SetGuidance("Maximum scattering angle in C.M.");
  fqmaxCmd->SetParameterName("qmax",true);
  fqmaxCmd->SetRange("qmax>=0.");
  fqmaxCmd->SetDefaultValue(0.);
  fqmaxCmd->SetDefaultUnit("deg");

  fAddProcessCmd = new G4UIcmdWithABool("/reaction/addProcess",this);
  fAddProcessCmd->SetGuidance("Boolean flag for adding nuclear-reaction process.");
  fAddProcessCmd->SetParameterName("flg",true);
  fAddProcessCmd->SetDefaultValue(false);
}

EMMAIonPhysicsMessenger::~EMMAIonPhysicsMessenger()
{
  delete fcsCmd;
  delete fZ1Cmd;
  delete fA1Cmd;
  delete fZ2Cmd;
  delete fA2Cmd;
  delete fZ3Cmd;
  delete fA3Cmd;
  delete fZ4Cmd;
  delete fA4Cmd;
  delete fqmaxCmd;
  delete fAddProcessCmd;
}

void EMMAIonPhysicsMessenger::SetNewValue(G4UIcommand * command,G4String newValue)
{
  if( command==fcsCmd ) { 
    fTarget->SetCrossSection(fcsCmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fZ1Cmd ) { 
    fTarget->SetZ1(fZ1Cmd->GetNewDoubleValue(newValue)); 
  }
  if( command==fA1Cmd ) { 
    fTarget->SetA1(fA1Cmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fZ2Cmd ) { 
    fTarget->SetZ2(fZ2Cmd->GetNewDoubleValue(newValue)); 
  }
  if( command==fA2Cmd ) { 
    fTarget->SetA2(fA2Cmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fZ3Cmd ) { 
    fTarget->SetZ3(fZ3Cmd->GetNewDoubleValue(newValue)); 
  }
  if( command==fA3Cmd ) { 
    fTarget->SetA3(fA3Cmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fZ4Cmd ) { 
    fTarget->SetZ4(fZ4Cmd->GetNewDoubleValue(newValue)); 
  }
  if( command==fA4Cmd ) { 
    fTarget->SetA4(fA4Cmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fqmaxCmd ) { 
    fTarget->Setqmax(fqmaxCmd->GetNewDoubleValue(newValue)); 
  }

  if( command==fAddProcessCmd ) { 
    fTarget->SetReactionParameters(); 
  }
}

G4String EMMAIonPhysicsMessenger::GetCurrentValue(G4UIcommand * command)
{
  G4String cv;
  if( command==fcsCmd )
  { cv = fcsCmd->ConvertToString(fTarget->GetCrossSection(),"millibarn"); }

  if( command==fZ1Cmd )
  { cv = fZ1Cmd->ConvertToString(fTarget->GetZ1(),""); }
  if( command==fA1Cmd )
  { cv = fA1Cmd->ConvertToString(fTarget->GetA1(),""); }

  if( command==fZ2Cmd )
  { cv = fZ2Cmd->ConvertToString(fTarget->GetZ2(),""); }
  if( command==fA2Cmd )
  { cv = fA2Cmd->ConvertToString(fTarget->GetA2(),""); }

  if( command==fZ3Cmd )
  { cv = fZ3Cmd->ConvertToString(fTarget->GetZ3(),""); }
  if( command==fA3Cmd )
  { cv = fA3Cmd->ConvertToString(fTarget->GetA3(),""); }

  if( command==fZ4Cmd )
  { cv = fZ4Cmd->ConvertToString(fTarget->GetZ4(),""); }
  if( command==fA4Cmd )
  { cv = fA4Cmd->ConvertToString(fTarget->GetA4(),""); }

  if( command==fqmaxCmd )
  { cv = fqmaxCmd->ConvertToString(fTarget->Getqmax(),"deg"); }

  return cv;
}

