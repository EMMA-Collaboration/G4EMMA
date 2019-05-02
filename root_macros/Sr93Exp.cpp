#include "TROOT.h"
#include <TCanvas.h>
#include <TF1.h>
#include <TF2.h>
#include <TObject.h>
#include <TFile.h>
#include <TTree.h>

#define M_PI 3.14159265358979323846L


void Sr93Exp() {

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\

  // call the data files
  //TFile *file = new TFile("/home/awen/G4EMMA_data/Sr93Exp/Results/GEMMAoutput.root");
  TFile *file = new TFile("/home/awen/G4EMMA_data/Sr93Exp_Sr93_20k/Results/GEMMAoutput.root");

  // create a new file in case I want to write anything to save
  TFile *file_out = new TFile("Sr93Exp_Sr93.root","RECREATE");

  // call the trees in the data files
  TTree *tree = (TTree*)file->Get("fphits");

  // data variables
  Double_t ic_Edepfront, ic_Edepback; // raw values
  Double_t total_Edep; // derived values

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\

  // get the values of the data from the tree and populate our variables
  tree->SetBranchAddress("fp_Edep",&ic_Edepfront);
  tree->SetBranchAddress("fp_Edep2",&ic_Edepback);

  Int_t n = (Int_t)tree->GetEntries();

  // make the histograms to fill

  TH2F* deltaE_E = new TH2F("deltaEvsE","Edep in front of IC vs. Total Edep in IC",200,0,300,200,0,300);


  // fill histograms
  for (Int_t i=0; i<n; i++) {

    tree->GetEntry(i);

    total_Edep = ic_Edepfront + ic_Edepback;

    deltaE_E->Fill(total_Edep,ic_Edepfront);

  }


// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^


// make a bunch of canvases and display the histograms

/*
file_out->cd();
tree->Write();
//deltaE_E->Write();
file_out->Close();
*/




TCanvas * c1 = new TCanvas("c1");

deltaE_E->GetXaxis()->SetTitle("Total Energy Deposited in IC (MeV)");
deltaE_E->GetYaxis()->SetTitle("Energy Deposited in IC Front (MeV)");
deltaE_E->Draw("colz");

tree->Write(); 
file_out->Write();



}
