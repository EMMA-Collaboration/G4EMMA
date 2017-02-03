#ifndef EMMAIonPhysicsMessenger_h
#define EMMAIonPhysicsMessenger_h 1

class EMMAIonPhysics;
class G4UIcmdWithADoubleAndUnit;
class G4UIcmdWithADouble;
class G4UIcmdWithABool;

#include "G4UImessenger.hh"
#include "globals.hh"

class EMMAIonPhysicsMessenger: public G4UImessenger
{
public:
  EMMAIonPhysicsMessenger(EMMAIonPhysics* ionphys);
  ~EMMAIonPhysicsMessenger();
  void SetNewValue(G4UIcommand * command,G4String newValues);
  G4String GetCurrentValue(G4UIcommand * command);

private:
  EMMAIonPhysics * fTarget;
  
private: //commands
  G4UIdirectory * reactionDirectory;

private: //commands
  G4UIcmdWithADoubleAndUnit* fcsCmd;
  G4UIcmdWithADouble *fZ1Cmd,*fA1Cmd, *fZ2Cmd,*fA2Cmd, *fZ3Cmd,*fA3Cmd, *fZ4Cmd,*fA4Cmd;
  G4UIcmdWithADoubleAndUnit* fqmaxCmd;
  G4UIcmdWithABool* fAddProcessCmd;

};

#endif
