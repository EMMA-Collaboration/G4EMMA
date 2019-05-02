#include "TROOT.h"
#include <TCanvas.h>
#include <TF1.h>
#include <TF2.h>
#include <TObject.h>
#include <TFile.h>
#include <TTree.h>
#include <TMath.h>
#include <TGraphErrors.h>
#include <TGraph.h>

#define M_PI 3.14159265358979323846L

void dispersion_plots() {

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

  // call the data files from the data folder
  TFile *z_file = new TFile("/home/awen/G4EMMA_data/Disp_test_0/Results/GEMMAoutput.root");

  TFile *p1_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+1/Results/GEMMAoutput.root");
  TFile *p2_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+2/Results/GEMMAoutput.root");
  TFile *p3_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+3/Results/GEMMAoutput.root");
  TFile *p0p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+0.5/Results/GEMMAoutput.root");
  TFile *p1p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+1.5/Results/GEMMAoutput.root");
  TFile *p2p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_+2.5/Results/GEMMAoutput.root");

  TFile *m1_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-1/Results/GEMMAoutput.root");
  TFile *m2_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-2/Results/GEMMAoutput.root");
  TFile *m3_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-3/Results/GEMMAoutput.root");
  TFile *m0p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-0.5/Results/GEMMAoutput.root");
  TFile *m1p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-1.5/Results/GEMMAoutput.root");
  TFile *m2p5_file = new TFile("/home/awen/G4EMMA_data/Disp_test_-2.5/Results/GEMMAoutput.root");

  // create a new file in case I want to write anything to save
  //TFile *file = new TFile("acceptance_plots.root","RECREATE");

  // call the trees in the data files
  TTree *z_tree = (TTree*)z_file->Get("fphits");

  TTree *p1_tree = (TTree*)p1_file->Get("fphits");
  TTree *p2_tree = (TTree*)p2_file->Get("fphits");
  TTree *p3_tree = (TTree*)p3_file->Get("fphits");
  TTree *p0p5_tree = (TTree*)p0p5_file->Get("fphits");
  TTree *p1p5_tree = (TTree*)p1p5_file->Get("fphits");
  TTree *p2p5_tree = (TTree*)p2p5_file->Get("fphits");

  TTree *m1_tree = (TTree*)m1_file->Get("fphits");
  TTree *m2_tree = (TTree*)m2_file->Get("fphits");
  TTree *m3_tree = (TTree*)m3_file->Get("fphits");
  TTree *m0p5_tree = (TTree*)m0p5_file->Get("fphits");
  TTree *m1p5_tree = (TTree*)m1p5_file->Get("fphits");
  TTree *m2p5_tree = (TTree*)m2p5_file->Get("fphits");

  // data variables
  Double_t z_x, p1, p2, p3, p0p5, p1p5, p2p5, m1, m2, m3, m0p5, m1p5, m2p5;

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

  // get the values of the data from the tree and populate our variables
  z_tree->SetBranchAddress("fp_posX",&z_x);

  p1_tree->SetBranchAddress("fp_posX",&p1);
  p2_tree->SetBranchAddress("fp_posX",&p2);
  p3_tree->SetBranchAddress("fp_posX",&p3);
  p0p5_tree->SetBranchAddress("fp_posX",&p0p5);
  p1p5_tree->SetBranchAddress("fp_posX",&p1p5);
  p2p5_tree->SetBranchAddress("fp_posX",&p2p5);

  m1_tree->SetBranchAddress("fp_posX",&m1);
  m2_tree->SetBranchAddress("fp_posX",&m2);
  m3_tree->SetBranchAddress("fp_posX",&m3);
  m0p5_tree->SetBranchAddress("fp_posX",&m0p5);
  m1p5_tree->SetBranchAddress("fp_posX",&m1p5);
  m2p5_tree->SetBranchAddress("fp_posX",&m2p5);

  Int_t nentries = (Int_t)z_tree->GetEntries();


  // make the histograms to fill
  TH1F* z_hist = new TH1F("z_x","z_x",100000,-50,50);

  TH1F* p1_hist = new TH1F("p1","p1",100000,-50,50);
  TH1F* p2_hist = new TH1F("p2","p2",100000,-50,50);
  TH1F* p3_hist = new TH1F("p3","p3",100000,-50,50);
  TH1F* p0p5_hist = new TH1F("p0p5","p0p5",100000,-50,50);
  TH1F* p1p5_hist = new TH1F("p1p5","p1p5",100000,-50,50);
  TH1F* p2p5_hist = new TH1F("p2p5","p2p5",100000,-50,50);

  TH1F* m1_hist = new TH1F("m1","m1",100000,-50,50);
  TH1F* m2_hist = new TH1F("m2","m2",100000,-50,50);
  TH1F* m3_hist = new TH1F("m3","m3",100000,-50,50);
  TH1F* m0p5_hist = new TH1F("m0p5","m0p5",100000,-50,50);
  TH1F* m1p5_hist = new TH1F("m1p5","m1p5",100000,-50,50);
  TH1F* m2p5_hist = new TH1F("m2p5","m2p5",100000,-50,50);



// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

  // fill the histograms with the data from the trees which came from the data files
  for (Int_t i=0; i<nentries; i++) {

    z_tree->GetEntry(i);
    p1_tree->GetEntry(i);
    p2_tree->GetEntry(i);
    p3_tree->GetEntry(i);
    p0p5_tree->GetEntry(i);
    p1p5_tree->GetEntry(i);
    p2p5_tree->GetEntry(i);
    m1_tree->GetEntry(i);
    m2_tree->GetEntry(i);
    m3_tree->GetEntry(i);
    m0p5_tree->GetEntry(i);
    m1p5_tree->GetEntry(i);
    m2p5_tree->GetEntry(i);

    z_hist->Fill(z_x);
    p1_hist->Fill(p1);
    p2_hist->Fill(p2);
    p3_hist->Fill(p3);
    p0p5_hist->Fill(p0p5);
    p1p5_hist->Fill(p1p5);
    p2p5_hist->Fill(p2p5);
    m1_hist->Fill(m1);
    m2_hist->Fill(m2);
    m3_hist->Fill(m3);
    m0p5_hist->Fill(m0p5);
    m1p5_hist->Fill(m1p5);
    m2p5_hist->Fill(m2p5);

  }

// define the variables that we are interested in finding
Double_t z_mean, p1_mean, p2_mean, p3_mean, p0p5_mean, p1p5_mean, p2p5_mean, m1_mean, m2_mean, m3_mean, m0p5_mean, m1p5_mean, m2p5_mean;
Double_t z_stdev, p1_stdev, p2_stdev, p3_stdev, p0p5_stdev,p1p5_stdev,p2p5_stdev, m1_stdev, m2_stdev, m3_stdev, m0p5_stdev,m1p5_stdev,m2p5_stdev;

// get the mean and stdev from the histogram values, so they can be plotted
z_mean = z_hist->GetMean();   z_stdev = z_hist->GetStdDev();
p1_mean = p1_hist->GetMean(); p1_stdev = p1_hist->GetStdDev();
p2_mean = p2_hist->GetMean(); p2_stdev = p2_hist->GetStdDev();
p3_mean = p3_hist->GetMean(); p3_stdev = p3_hist->GetStdDev();
p0p5_mean = p0p5_hist->GetMean(); p0p5_stdev = p0p5_hist->GetStdDev();
p1p5_mean = p1p5_hist->GetMean(); p1p5_stdev = p1p5_hist->GetStdDev();
p2p5_mean = p2p5_hist->GetMean(); p2p5_stdev = p2p5_hist->GetStdDev();
m1_mean = m1_hist->GetMean(); m1_stdev = m1_hist->GetStdDev();
m2_mean = m2_hist->GetMean(); m2_stdev = m2_hist->GetStdDev();
m3_mean = m3_hist->GetMean(); m3_stdev = m3_hist->GetStdDev();
m0p5_mean = m0p5_hist->GetMean(); m0p5_stdev = m0p5_hist->GetStdDev();
m1p5_mean = m1p5_hist->GetMean(); m1p5_stdev = m1p5_hist->GetStdDev();
m2p5_mean = m2p5_hist->GetMean(); m2p5_stdev = m2p5_hist->GetStdDev();

// /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^\ /^(o.o)^

// make a TGraphErrors to plot the points
const Int_t n = 13;
// array of x values (% dispersion in m/q)
Float_t x[n] = {-3.0,-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0,2.5,3.0};
// corresponding array of means and stdevs
Float_t y[n] = {m3_mean, m2p5_mean, m2_mean, m1p5_mean, m1_mean, m0p5_mean, z_mean, p0p5_mean, p1_mean, p1p5_mean, p2_mean, p2p5_mean, p3_mean};
Float_t ey[n] = {m3_stdev, m2p5_stdev, m2_stdev, m1p5_stdev, m1_stdev, m0p5_stdev, z_stdev, p0p5_stdev, p1_stdev, p1p5_stdev, p2_stdev, p2p5_stdev, p3_stdev};
Float_t ex[n] = {0,0,0,0,0,0,0,0,0,0,0,0,0};
// draw the graph
TGraphErrors *graph = new TGraphErrors(n,x,y,ex,ey);
// aesthetics
graph->SetTitle("Average x Dispersion vs. Percent m/q Dispersion");
graph->GetXaxis()->SetTitle("Percent m/q dispersion");
graph->GetYaxis()->SetTitle("Horizontal dispersion in focal plane (mm)");
// fit a linear line to get a slope
graph->Fit("pol1");
graph->SetMarkerStyle(21);

// draw a line for the nominal slope to compare with the best fit line
const Int_t n2 = 2;
Float_t x2[n] = {-3.5,3.5};
Float_t y2[n] = {35.0,-35.0};
TGraph *graph2 = new TGraph(n2,x2,y2);
graph2->SetLineColor(3);
graph2->SetLineWidth(3);

// make a canvas a draw the graphs
TCanvas *canvas = new TCanvas("canvas","graph");
canvas->cd();
canvas->SetGrid();
graph->Draw("AP");
graph2->Draw("L");



}
