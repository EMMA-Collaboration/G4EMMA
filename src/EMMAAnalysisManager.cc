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
//
// **********************************************************************
// *                                                                    *
// *                    GEANT 4 tutorial 1                              *
// *                                                                    *
// * MODULE:            EMMAAnalysisManager.cc                           *
// * -------                                                            *
// *                                                                    *
// * Version:           0.1                                             *
// * Date:              January 28 2002                                 *
// * Author:            T.Johnson                                       *
// * Organisation:      SLAC                                            *
// *                                                                    *
// **********************************************************************
//
// CHANGE HISTORY
// --------------
//
// Nov 4 2002 -- Upgrade to AIDA 3.0
// **********************************************************************
#ifdef G4ANALYSIS_USE

#include <fstream>

#include "G4ios.hh"
#include "G4Run.hh"
#include "G4Event.hh"
#include "G4Track.hh"
#include "G4VVisManager.hh"
#include "G4TrajectoryContainer.hh"
#include "G4Trajectory.hh"

#include "EMMAAnalysisManager.hh"

//global variable
extern G4String UserDir;

EMMAAnalysisManager* EMMAAnalysisManager::instance = 0;

EMMAAnalysisManager::EMMAAnalysisManager()
:Hlist(0)
{

  //create root file
  G4String filename = UserDir + "/Results/GEMMAoutput.root";
  rootfile = new TFile(filename,"recreate");

  //create root tree
  if(rootfile){
    fproottree = new TTree("fphits","Focal plane hits");
    targetroottree = new TTree("targetplane","Target Plane Data");
  }

}

EMMAAnalysisManager::~EMMAAnalysisManager()
{
}

TFile* EMMAAnalysisManager::getRootfile()
{
  return rootfile;
}

TObjArray* EMMAAnalysisManager::getRootarray()
{
  return Hlist;
}

TTree* EMMAAnalysisManager::getfpRoottree()
{
  return fproottree;
}

TTree* EMMAAnalysisManager::gettargetRoottree()
{
  return targetroottree;
}

EMMAAnalysisManager* EMMAAnalysisManager::getInstance()
{
  if (instance == 0) instance = new EMMAAnalysisManager();
  return instance;
}

void EMMAAnalysisManager::dispose()
{
  if (instance != 0)
  {
    delete instance;
    instance = 0;
  }
}

#endif // G4ANALYSIS_USE
