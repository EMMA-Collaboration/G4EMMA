#include "TROOT.h"
#include <TCanvas.h>
#include <TF1.h>
#include <TF2.h>
#include <TObject.h>
#include <TFile.h>
#include <TTree.h>

#define M_PI 3.14159265358979323846L

/* ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°)

Analysis code for analyzing angular and energy acceptances of G4EMMA for some 

This script does some basic analysis regarding energy and angular acceptances of the simulation spectrometer.
Run it by loading root in this directory, loading with

[] .L acceptance_plots.cpp

and running with

[] acceptance_plots()


It reads in two files: Ang_acceptance_test_10k/Results/GEMMAoutput.root and Energy_acceptance_1k/Results/GEMMAoutput.root
The first one was testing angular acceptance; beam was shot with a large normalized transverse emmittance
with the goal of finding the maximum angle a particle could be shot and still reach the focal plane.
The second one tests energy acceptance; beam was shot with a wide spectrum of energies to see the highest and
lowest energy particles that could still make it to the focal plane.

The energy and angular acceptances can be compared to the nominal values
E: +25%, -17%
a: +/-3.6 deg by +/-3.6 deg (square apeture)

This scipt outputs the max/min x/y angles based on the data read it (it is simply the largest/most extreme
angle in the given direction).

Also, it outputs four graphs:

Target energy distribution graphs the energy spectra of the total emitted beam (blue) and the energy spectra of the
accepted beam (red) for easy comparison. It is also easy to see where the nominal energy limits should be.

Target angle dist from focal plane events is simply the angular distribution at the target plane from every
accepted beam particle. This was found by getting every event that hit the focal plane and then plotting
target plane data from that event. This is slightly DIFFERENT from all the beam particles within the angular
acceptance range defined by the max/min angles.

The large canvas with 6 pads features the previous graph in the upper row along with its x and y projections, but in the
bottom row also features the histogram with ALL PARTICLES WITHIN the angular accecptance range and respective x and y
projections. The two are very similar but have subtle differences (as you can see from the # of events.)

The final canvas with the two pads are the x and y projections from the previous canvas simply overlaid to allow for
easier comparison.


( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) ( ͡° ͜ʖ ͡°) */





void acceptance_plots() {

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

  // call the data files
  //TFile *ang_file = new TFile("/home/awen/G4EMMA_data/Ang_acceptance_test_histat_uni/Results/GEMMAoutput_70k.root");
  TFile *ang_file = new TFile("/home/awen/G4EMMA/UserDir/Results/GEMMAoutput.root");
  TFile *energy_file = new TFile("/home/awen/G4EMMA/UserDir/Results/GEMMAoutput.root");

  // create a new file in case I want to write anything to save
  //TFile *file = new TFile("acceptance_plots.root","RECREATE");

  // call the trees in the data files
  TTree *ang_tree = (TTree*)ang_file->Get("fphits");
  TTree *energy_tree = (TTree*)energy_file->Get("fphits");
  TTree *ang_tree_target = (TTree*)ang_file->Get("targetplane");

  // data variables
  Double_t fp_posX_a, fp_posY_a, fp_Ekin_a, tar_angX_a, tar_angY_a; //angular data from events that made it to the focal plane
  Double_t fp_posX_e, fp_posY_e, tar_energy; // energy data from events taht made it to the focal plane
  Double_t target_angX_total, target_angY_total;  //angular data from the region of interest that were TOTAL emitted

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\

  // get the values of the data from the tree and populate our variables when we fill our histos
  ang_tree->SetBranchAddress("fp_posX",&fp_posX_a);
  ang_tree->SetBranchAddress("fp_posY",&fp_posY_a);
  ang_tree->SetBranchAddress("fp_Ekin",&fp_Ekin_a);
  ang_tree->SetBranchAddress("target_angX",&tar_angX_a);
  ang_tree->SetBranchAddress("target_angY",&tar_angY_a);
  Int_t nentries = (Int_t)ang_tree->GetEntries();
  energy_tree->SetBranchAddress("fp_posX",&fp_posX_e);
  energy_tree->SetBranchAddress("fp_posY",&fp_posY_e);
  energy_tree->SetBranchAddress("target_Ekin",&tar_energy);
  Int_t pentries = (Int_t)energy_tree->GetEntries();

  ang_tree_target->SetBranchAddress("target_xang", &target_angX_total);
  ang_tree_target->SetBranchAddress("target_yang", &target_angY_total);
  Int_t qentries = (Int_t)ang_tree_target->GetEntries();

  // make the histograms to fill

  TH2F* ang_XY = new TH2F("angle dist - fp","Target angle dist from focal plane events",100,-0.1,0.1,100,-0.1,0.1);

  TH1F* energy = new TH1F("energy","Target energy distribution",100,50,150);

  TH2F* ang_XY_total = new TH2F("angle dist - total","Target angle dist in acceptance region",100,-0.1,0.1,100,-0.1,0.1);


  Double_t fp_posX_llim = -40;
  Double_t fp_posX_hlim = 40;
  Double_t fp_posY_llim = -10;
  Double_t fp_posY_hlim = 10;
  Double_t fp_Ekin_llim = 60;
  Double_t fp_Ekin_hlim = 80;

  Double_t tar_angX_llim = -0.1;
  Double_t tar_angX_hlim = 0.1;
  Double_t tar_angY_llim = -0.1;
  Double_t tar_angY_hlim = 0.1;

  TH2F* fp_posX_tar_angX = new TH2F("fp_posX-tar_angX","fp xpos vs target xang",1000,tar_angX_llim,tar_angX_hlim,1000,fp_posX_llim,fp_posX_hlim);
  TH2F* fp_posX_tar_angY = new TH2F("fp_posX_tar_angY","fp xpos vs target yang",1000,tar_angY_llim,tar_angY_hlim,1000,fp_posX_llim,fp_posX_hlim);
  TH2F* fp_posY_tar_angX = new TH2F("fp_posY_tar_angX","fp ypos vs target xang",1000,tar_angX_llim,tar_angX_hlim,1000,fp_posY_llim,fp_posY_hlim);
  TH2F* fp_posY_tar_angY = new TH2F("fp_posY_tar_angY","fp ypos vs target yang",1000,tar_angY_llim,tar_angY_hlim,1000,fp_posY_llim,fp_posY_hlim);
  TH2F* fp_Ekin_tar_angX = new TH2F("fp_Ekin_tar_angX","fp ekin vs target xang",1000,tar_angX_llim,tar_angX_hlim,1000,fp_Ekin_llim,fp_Ekin_hlim);
  TH2F* fp_Ekin_tar_angY = new TH2F("fp_Ekin_tar_angY","fp ekin vs target yang",1000,tar_angY_llim,tar_angY_hlim,1000,fp_Ekin_llim,fp_Ekin_hlim);


  // get the max and min values of energy and angles that made it to the focal plane
  Double_t max_e = 100;
  Double_t min_e = 100;
  Double_t max_ang_x = 0;
  Double_t min_ang_x = 0;
  Double_t max_ang_y = 0;
  Double_t min_ang_y = 0;


// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^



  // fill the angular acceptance histograms
  for (Int_t i=0; i<nentries; i++) {

    ang_tree->GetEntry(i);

    //ang_X->Fill(tar_angX_a);
    //ang_Y->Fill(tar_angY_a);
    ang_XY->Fill(tar_angX_a,tar_angY_a);

    fp_posX_tar_angX->Fill(tar_angX_a,fp_posX_a);
    fp_posX_tar_angY->Fill(tar_angY_a,fp_posX_a);
    fp_posY_tar_angX->Fill(tar_angX_a,fp_posY_a);
    fp_posY_tar_angY->Fill(tar_angY_a,fp_posY_a);
    fp_Ekin_tar_angX->Fill(tar_angX_a,fp_Ekin_a);
    fp_Ekin_tar_angY->Fill(tar_angY_a,fp_Ekin_a);

    //-----------------------------------------------------------------------------------//
    // find min and max angles in the x and y directions
    if (tar_angX_a >= max_ang_x)
      max_ang_x = tar_angX_a;

    if (tar_angX_a <= min_ang_x)
      min_ang_x = tar_angX_a;

    if (tar_angY_a >= max_ang_y)
      max_ang_y = tar_angY_a;

    if (tar_angY_a <= min_ang_y)
      min_ang_y = tar_angY_a;



    //-----------------------------------------------------------------------------------//

  }

  // fill the histogram that is independent of the focal plane, for comparison
  // essentially "cut" by the max and min angles of the events that did make it through to the focal plane

  for (Int_t i=0; i<qentries; i++) {

    ang_tree_target->GetEntry(i);

    if (target_angX_total<max_ang_x
        && target_angX_total>min_ang_x
        && target_angY_total<max_ang_y
        && target_angY_total>min_ang_y)
      ang_XY_total->Fill(target_angX_total,target_angY_total);

  }



// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

  // fill the energy acceptance histograms
  for (int i=0; i<pentries; i++) {
    energy_tree->GetEntry(i);
    energy->Fill(tar_energy);

    //-----------------------------------------------------------------------------------//
    // find the max and min energy
    if (tar_energy >= max_e) {
      max_e = tar_energy;
    }
    if (tar_energy <= min_e) {
      min_e = tar_energy;
    }
    //-----------------------------------------------------------------------------------//
  }

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\


// make a bunch of canvases and display the histograms
TCanvas * c1 = new TCanvas("c1");
c1->Divide(3,2);
c1->cd(1);
TH1F* ang_X = (TH1F*)ang_XY->ProjectionX();
ang_X->GetXaxis()->SetTitle("X (rad)");
ang_X->SetLineColor(2);
ang_X->Draw();

c1->cd(2);
TH1F* ang_Y = (TH1F*)ang_XY->ProjectionY();
ang_Y->GetXaxis()->SetTitle("Y (rad)");
ang_Y->SetLineColor(2);
ang_Y->Draw();

c1->cd(3);
ang_XY->GetXaxis()->SetTitle("X (rad)");
ang_XY->GetYaxis()->SetTitle("Y (rad)");
ang_XY->Draw("colz");

c1->cd(4);
TH1F * ang_X_total = (TH1F*)ang_XY_total->ProjectionX();
ang_X_total->GetXaxis()->SetTitle("X (rad)");
ang_X_total->Draw();

c1->cd(5);
TH1F* ang_Y_total = (TH1F*)ang_XY_total->ProjectionY();
ang_Y_total->GetXaxis()->SetTitle("Y (rad)");
ang_Y_total->Draw();

c1->cd(6);
ang_XY_total->GetXaxis()->SetTitle("X (rad)");
ang_XY_total->GetYaxis()->SetTitle("Y (rad)");
ang_XY_total->Draw("colz");

TCanvas * c2 = new TCanvas("c2");
c2->Divide(2,1);

c2->cd(1);
ang_X->Draw();
ang_X_total->Draw("same");
c2->cd(2);
ang_Y->Draw();
ang_Y_total->Draw("same");

TCanvas * misc = new TCanvas("misc");
ang_XY->Draw("colz");


TCanvas * c3 = new TCanvas("c3");
energy->GetXaxis()->SetTitle("MeV");
TH1F* energy_all = (TH1F*)energy_file->Get("targetEkin");
energy->Draw();
energy_all->SetLineColor(2);
energy_all->Draw("same");



TCanvas * c4 = new TCanvas("c4");
c4->Divide(2,3);
c4->cd(1);
fp_posX_tar_angX->Draw("SCAT");
c4->cd(2);
fp_posX_tar_angY->Draw("SCAT");
c4->cd(3);
fp_posY_tar_angX->Draw("SCAT");
c4->cd(4);
fp_posY_tar_angY->Draw("SCAT");
c4->cd(5);
fp_Ekin_tar_angX->Draw("SCAT");
c4->cd(6);
fp_Ekin_tar_angY->Draw("SCAT");





// Display some important max/min values
std::cout << "Max energy accepted: " << max_e << " Min energy accepted: " << min_e << std::endl;
std::cout << "Max X angle: " << max_ang_x << " (" << (360/(2*M_PI))*max_ang_x << " deg)" << std::endl;
std::cout << "Min X angle: " << min_ang_x << " (" << (360/(2*M_PI))*min_ang_x << " deg)" << std::endl;
std::cout << "Max Y angle: " << max_ang_y << " (" << (360/(2*M_PI))*max_ang_y << " deg)" << std::endl;
std::cout << "Min Y angle: " << min_ang_y << " (" << (360/(2*M_PI))*min_ang_y << " deg)" << std::endl;


}
