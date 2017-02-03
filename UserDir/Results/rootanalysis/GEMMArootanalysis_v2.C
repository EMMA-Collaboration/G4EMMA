#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1.h"
//Use this routine to read in event by event data of the focal plane hit position and angle and
//create histograms from this data here
//Open root and run macro using '.x GEMMArootanalysis_v2.C+'

#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TMath.h"
#include "TCutG.h"
#include "TChain.h"

//c++
#define _USE_MATH_DEFINES //defines constants like M_PI
#include "globals.hh"

//Run macro using command '.x GEMMArootanalysis_v2.C+()'
void GEMMArootanalysis_v2(){
  //delete all histograms/trees from memory from current directory
  gDirectory->GetList()->Delete();
  gROOT->Reset();
  
  //variable declaration
  Double_t fp_pos[2];
  Double_t fp_theta;
  Int_t Nentries;

//read in file
  std::stringstream fname;
  fname<<"../GEMMAoutput.root";	//root file name
  //fname << Estring;
  TChain *chain = new TChain("fphits","fphits");
  chain->Add(fname.str().c_str());
  Nentries=(Int_t)chain->GetEntries();

//get tree and branches from root file
  chain->SetBranchAddress("fp_pos",&fp_pos);
  chain->SetBranchAddress("fp_theta",&fp_theta);
//get number of entries in the tree
  Nentries=(Int_t)chain->GetEntries();
  cout<<"Number of hits at focal plane: "<<Nentries<<endl;

//Create histogram to fill with tree entries
  TH2F* fp_hitpos2 = new TH2F("hitpos2","Focal plane hit position",6000,-30,30,6000,-30,30);
  fp_hitpos2->GetXaxis()->SetTitle("X position (mm)");	//axis labels
  fp_hitpos2->GetYaxis()->SetTitle("Y position (mm)");
  TH1F* fp_hitposx = new TH1F("hitposx","x focal plane hit position",600,-30,30);
  fp_hitposx->GetXaxis()->SetTitle("X position (mm)");	//axis labels
  fp_hitposx->GetYaxis()->SetTitle("Counts");
  
//loop over number of entries that hit focal plane
  for(Int_t i=0;i<Nentries;i++){
    fp_pos[0]=fp_pos[1]=fp_theta=0;
    
    chain->GetEntry(i);	//get entry
    
    fp_hitpos2->Fill(fp_pos[0],fp_pos[1]); //add event to histogram
    fp_hitposx->Fill(fp_pos[0]);

    //do whatever you want with the entries
    //cout<<fp_theta<<" "<<fp_pos[0]<<" "<<fp_pos[1]<<endl;
  }

  TCanvas* c1 = new TCanvas("canvas1","GEMMA output1",800,500);
  fp_hitpos2->Draw();
  TCanvas* c2 = new TCanvas("canvas2","GEMMA output2",800,500);
  fp_hitposx->Draw();

}
