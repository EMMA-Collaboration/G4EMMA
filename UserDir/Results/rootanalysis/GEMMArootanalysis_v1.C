//Use this routine to plot the histograms created in EMMAEventAction.cc and EMMASteppingAction.cc
//There is no event by event data here. Use GEMMArootanalysis_v2.C to analyse the data
//open root and run macro using '.x GEMMArootanalysis_v1.C+'

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TMath.h"
#include "TCutG.h"

//c++
#define _USE_MATH_DEFINES //defines constants like M_PI
#include "globals.hh"

//Run macro using command '.x GEMMArootanalysis_v1.C+'
void GEMMArootanalysis_v1(){
  //delete all histograms/trees from memory from current directory
  gDirectory->GetList()->Delete();
  gROOT->Reset();
  
  //variable declaration
  Double_t fp_pos[2];
  Double_t fp_theta;
  Double_t fp_Ekin;
  Double_t fp_Edep;
  Double_t fp_Edep2;
  Double_t fp_Edep_Silicon;
  Int_t Nentries;
  string comp[14];

//read in file
  std::stringstream fname;
  fname<<"../GEMMAoutput.root";	//root file name
  TFile f(fname.str().c_str());
  if (f.IsZombie()){
    cout<<fname<<" file not found"<<endl;
    exit(-1);
  }
  f.ls(); //lists contents of root file

//get histograms from root file
  //you can get the type and name of object in the file from the list that f.ls() produces
  TH2F* fp_hitpos = (TH2F*)f.Get("hitpos");
  //next line necessary so that histogram doesn't disappear when macro ends and root file is closed.
  //(only necessary for histograms called from root files)
  fp_hitpos->SetDirectory(0);
  TH1F* fp_hitposX = (TH1F*)f.Get("hitposX");
  fp_hitposX->SetDirectory(0);
  TH1F* fp_hitang = (TH1F*)f.Get("hitangle");
  fp_hitang->SetDirectory(0);
  TH1I* dead_hit = (TH1I*)f.Get("dead_hit");
  dead_hit->SetDirectory(0);
  TH1F* fp_hitEkin = (TH1F*)f.Get("hitEkin");
  fp_hitEkin->SetDirectory(0);
  TH1F* fp_hitEdep = (TH1F*)f.Get("hitEdep");
  fp_hitEdep->SetDirectory(0);
  TH1F* fp_hitEdep2 = (TH1F*)f.Get("hitEdep2");
  fp_hitEdep2->SetDirectory(0);

  TH1F* fp_hitEdep_Silicon = (TH1F*)f.Get("hitEdepSilicon");
  fp_hitEdep_Silicon->SetDirectory(0);

  TH2F* fp_hit2DEdep = (TH2F*)f.Get("hit2DEdep");
  fp_hit2DEdep->SetDirectory(0);

  // Preferences:
  fp_hitpos->SetMarkerStyle(20);
  fp_hitpos->SetMarkerSize(0.5);
  fp_hit2DEdep->SetMarkerStyle(20);
  fp_hit2DEdep->SetMarkerSize(0.5);
  
//plotting histograms (two methods)

//method 1: plot all four histograms on one canvas
  TCanvas* c1 = new TCanvas("canvas","All GEMMA historgrams",800,500);
  c1->Divide(2,2);	//split canvas into four pads
  c1->cd(1);	//go to 1st pad
  fp_hitpos->Draw();
  c1->cd(2);	//go to 2nd pad
  fp_hitposX->Draw();
  c1->cd(3);	//go to 3rd pad
  dead_hit->Draw();
  c1->cd(4);
  fp_hitEkin->Draw();

//method 2: create a new canvas for each histogram
  TCanvas* c2 = new TCanvas("canvas2","GEMMA output",800,500);
  fp_hitpos->Draw("colz");
  TCanvas* c3 = new TCanvas("canvas3","GEMMA output",800,500);
  fp_hitang->Draw();
  TCanvas* c4 = new TCanvas("canvas4","GEMMA output",800,500);
  dead_hit->Draw();
  TCanvas* c5 = new TCanvas("canvas5","GEMMA output",800,500);
  fp_hitEkin->Draw(); 
  TCanvas* c6 = new TCanvas("canvas6","GEMMA output",800,500);
  fp_hitEdep->Draw();
  TCanvas* c7 = new TCanvas("canvas7","GEMMA output",800,500);
  fp_hitEdep2->Draw();
  TCanvas* c8 = new TCanvas("canvas8","GEMMA output",800,500);
  fp_hit2DEdep->Draw("colz");
  TCanvas* c9 = new TCanvas("canvas9","GEMMA output",800,500);
  fp_hitEdep_Silicon->Draw();

}
