/*-------------------Emacs PostScript "pretty-print" page width (97 columns)-------------------*/
/* Program: fit.cc
 *       Created  by Jon Lighthall
 *       Adapted from "linefit.cc" by Jack Winkelbauer (c. 12.02.2008)
 *       Adapted from "peakfit.cc" by Alan Wuosmaa     (c. 08.12.2009)
 *       Adapted from "util.cc" by Alan Wuosmaa        (c. 07.09.2009)
 *       Adapted from "bkffit.cc" by Alan Wuosmaa
 * Purpose: 
 *       A package of utilities developed for use with HELIOS data analysis.  The utilities are
 *       divided into five groups: 1). Display Utilities
 *                                     a). Style
 *                                     b). Canvas
 *                                     c). Creating Histograms
 *                                     d). Histogram Information
 *                                     e). Plotting
 *                                     f). Projecting
 *                                 2). Manipulation Utilities
 *                                     a). Acting on 1 histogram
 *                                     b). Acting on 2 histograms
 *                                 3). Fitting Utilities
 *                                     a). 1D Fits 
 *                                     b). 2D Fits
 *                                     c). Profile Fits
 *                                     d). Line Fits
 *                                 4). TSpectrum Utilities
 *                                     a). Peak
 *                                     b). Background
 *                                     c). Smoothing
 *                                 5). File Utilities
 *                                     a). Directory
 *                                     b). General
 *                                     c). Calibration
 *                                     d). Fill/dump 
 *                                     e). Cuts
 * Requires:
 *       none
 */
#include <iostream.h>
TH1F *hProj=0;
TH1F *hProf=0;
TH1F *hFit=0;
TH1F *hBkg=0;
TH1F *hResult=0;

TH2F *hInput=0;
TH2F *hOutput=0;
TH2F *h;

Float_t array[24][11];
Int_t det=0;
TString hname;

TCutG *cWindow;

/* 1). Display Utilities-------------------------------------------------------------------------
 *
 */
//-------------------------------------------------------------------------------------
// 1a). Style Utilities----------------------------------------------------------------
void setplain(void)
{//adapted from util.cc
  //gROOT->SetStyle("Plain");
  //gStyle->SetPalette(1,0);
  printf("Default style being used.\n");
}

void printVer()
{//print the version and date of the current ROOT build
  printf("Root Version %s - %s\n",gROOT->GetVersion(),gROOT->GetSvnDate());
}

void settime(Bool_t bdisplay=kTRUE)
{//turns time stamp on/off.  copied from /net/helios/H008/oneline/rootlogon.C
  if(bdisplay)
    {
      gStyle->SetOptDate(4);
      gStyle->GetAttDate()->SetTextSize(0.015);
      gStyle->SetDateX(0);
      gStyle->SetDateY(0);
    }
  else
    gStyle->SetOptDate(0);
}

void setdisplay()
{
  setplain();
  gStyle->SetOptStat("neMiou"); //sets what info is displayed in histograms
  gStyle->SetOptFit(0111); //sets info is displayed in fits
  settime();
  printVer();
}

//-------------------------------------------------------------------------------------
// 1b). Canvas Utilities---------------------------------------------------------------
void newcanvas(Char_t *name)
{//copied form util.cc
  TCanvas *canvasname=new TCanvas(name,name,1);
  canvasname->cd();
}

void savecanvas(Char_t *cnvname, Char_t * filename)
{
  TCanvas *thecanvas=(TCanvas *) gROOT->FindObject(cnvname);
  thecanvas->SaveAs(filename);
}

void mkCanvas2(Char_t* cvname="cFit",Char_t *cvtitle="cFit",Int_t ww=675,Int_t wh=615)
{//make a TCanvas, adapted from plot_tools.cc
  TCanvas * cFit=new TCanvas(cvname,cvtitle,0,0,ww,wh);
  if(!(cFit->GetShowEventStatus()))cFit->ToggleEventStatus();
  if(!(cFit->GetShowToolBar()))cFit->ToggleToolBar();
}

void prop(Float_t x_prop=1.61803398875,Float_t y_prop=1,Float_t x_size=1000,
	  Char_t* cvname="cFit")
{//set the proportion and size of the canvas for printing
  if(!((TCanvas *) gROOT->FindObject(cvname))) mkCanvas2(cvname,cvname);
  TCanvas *thecanvas=(TCanvas *)gROOT->FindObject(cvname);
  Float_t ref=thecanvas->GetWindowWidth();
  ref=x_size;
  thecanvas->SetWindowSize(ref,(ref/x_prop)*y_prop);
}

void getpave()
{//shows the position of a TPave
  Float_t a,b,c,d,e,f,g,h;
  a=TPave->GetX1();
  b=TPave->GetY1();
  c=TPave->GetX2();
  d=TPave->GetY2();
  e=TPave->GetX1NDC();
  f=TPave->GetY1NDC();
  g=TPave->GetX2NDC();
  h=TPave->GetY2NDC();
  printf("%.1f,%.1f,%.1f,%.1f,\"br\"\n",a,b,c,d);
  printf("%.3f,%.3f,%.3f,%.3f,\"NDC\"\n",e,f,g,h);
  FILE * savefile;
  savefile=fopen("pave.lst","w");
  fprintf(savefile,"%.1f,%.1f,%.1f,%.1f,\"br\"\n",a,b,c,d);
  fprintf(savefile,"%.3f,%.3f,%.3f,%.3f,\"NDC\"\n",e,f,g,h);
  fclose (savefile);
}

//-------------------------------------------------------------------------------------
// 1c). Creating Histograms------------------------------------------------------------
void h1(Char_t *histname, Char_t *title, Int_t nchan=100,Float_t low=0,Float_t high=100)
{//copied form util.cc
  TH1F *newhist=new TH1F(histname,title,nchan,low,high);
  newhist->SetName(histname);
}

void h2(Char_t *histname, Char_t *title, Int_t nxchan=100,Float_t lowx=0,Float_t highx=100,
	Int_t nychan=100,Float_t lowy=0,Float_t highy=100)
{//copied form util.cc
  TH2F *newhist=new TH2F(histname,title,nxchan,lowx,highx,nychan,lowy,highy);
  newhist->SetName(histname);
}

void h3(Char_t *histname, Char_t *title, Int_t nxchan=100,Float_t lowx=0,Float_t highx=100,
	Int_t nychan,Float_t lowy=0,Float_t highy=100, Int_t nzchan=100, 
	Float_t lowz=0, Float_t highz=100)
{//copied form util.cc
  TH3F *newhist=new TH3F(histname,title,nxchan,lowx,highx,nychan,lowy,highy,
			 nzchan,lowz,highz);
  newhist->SetName(histname);
}

void mkhist1(Char_t *histin="h", Int_t bins=3, Float_t size=10)
{//creates a small 1D histogram
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  hname=histin;
  if ((TH1F *) gROOT->FindObject(hname.Data())) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  } 
  TH1F * hProj=new  TH1F(hname.Data(),"Small Histogram",bins,0.,size);
  hProj->Draw("");
}

void mkhist2(Char_t *histin="h", Int_t bins=3, Float_t size=10)
{//creates a small 2D histogram to test copy2()
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  hname=histin;
  if ((TH2F *) gROOT->FindObject(hname.Data())) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  } 

  h = new TH2F(hname.Data(),"Small Histogram",bins,0.,size,bins,0.,size);
  h->Fill(3,3,1);
  h->Fill(8,8,2);
  h->Fill(-bins,0.5*size,1);//underfill
  h->Fill(1.2*size,0.8*size,1);//overfill
  h->Fill(8,8,1);//second entry in same bin
  h->Fill((size/bins)/2+(size/bins)*(bins-1),(size/bins)/2,10);
  h->Fill((size/bins)/2,(size/bins)/2+(size/bins)*(bins-1),5);  
  h->Draw("colz");
}

//-------------------------------------------------------------------------------------
// 1d). Histogram Information----------------------------------------------------------
void listall(Int_t option=0)
{//copied form util.cc
  switch (option) {
      
  case 0:
    cout << endl<<"All histograms in memory:"<<endl<<endl;
    gDirectory->GetList()->ls();
    cout << endl<<"All Special objects in memory:"<<endl<<endl;;
    gROOT->GetListOfSpecials()->ls();
    cout << endl<<"All Canvases in memory:"<<endl<<endl;
    gROOT->GetListOfCanvases()->ls();
    break;
      
  case 1:
    cout << endl<<"All histograms in memory:"<<endl<<endl;
    gDirectory->GetList()->ls();
    break;
      
  case 2:
    cout << endl<<"All Special objects in memory:"<<endl<<endl;;
    gROOT->GetListOfSpecials()->ls();
    break;
      
  case 3:
    cout << endl<<"All Canvases in memory:"<<endl<<endl;
    gROOT->GetListOfCanvases()->ls();
    break;
  }
}

Int_t whatis(Char_t *hname,Int_t verbose=1)
{
  Int_t returnvalue=0;
   
  //   gDirectory->pwd();
  //   if (gROOT->FindObject(hname)) {
  //      cout << "found it somewhere."<<endl;
  //   }

  if (gROOT->FindObject(hname)->InheritsFrom("TH1F")) {
    if (verbose==1) cout << "histogram " <<hname<<" is a TH1F"<<endl;
    returnvalue=1;
  }
  if (gROOT->FindObject(hname)->InheritsFrom("TH1D")) {
    if (verbose==1) cout << "histogram " <<hname<<" is a TH1D"<<endl;
    returnvalue=2;
  }
  if (gROOT->FindObject(hname)->InheritsFrom("TH2F")) {
    if (verbose==1) cout << "histogram "<< hname <<" is a TH2F"<<endl;
    returnvalue=3;
  }
  if (gROOT->FindObject(hname)->InheritsFrom("TH2D")) {
    if (verbose==1) cout << "histogram "<<hname<<"is a TH2D"<<endl;
    returnvalue=4;
  }
  if (gROOT->FindObject(hname)->InheritsFrom("TH3F")) {
    if (verbose==1) cout << "histogram "<<hname<<"is a TH3F"<<endl;
    returnvalue=5;
  }
  if (gROOT->FindObject(hname)->InheritsFrom("TProfile")) {
    if (verbose==1) cout << "histogram "<<hname<<" is a TProfile"<<endl;
    returnvalue=6;
  }
  return returnvalue;
}

void show(Char_t *histname)
{//copied form util.cc
  Int_t whatsit=whatis(histname);
  switch(whatsit) {
  case 1:
    TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
    break;
  case 2:
    TH1D *hist1=(TH1D *) gROOT->FindObject(histname);
    break;
  case 3:
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    break;
  case 4:
    TH2D *hist2=(TH2D *) gROOT->FindObject(histname);
    break;
  case 5:
    TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
    break;
  case 6:
    TProfile *hist1=(TProfile *) gROOT->FindObject(histname);
    break;
  }
   
  if (whatsit==1||whatsit==2) {
    cout << "1D histogram "<<histname<<" : "<<endl;
    cout << hist1->GetNbinsX()<<" bins from " <<hist1->GetXaxis()->GetXmin()
	 << " to " << hist1->GetXaxis()->GetXmax()<<endl;
  }
  if (whatsit==3 || whatsit==4) {
    cout <<"2D histogram "<<histname<<" : "<<endl;
    cout << "X axis: "<<hist2->GetNbinsX()<<" bins from " <<
      hist2->GetXaxis()->GetXmin()<<" to "<<hist2->GetXaxis()->GetXmax()
	 <<endl;
    cout << "Y axis: "<<hist2->GetNbinsY()<<" bins from " <<
      hist2->GetYaxis()->GetXmin()<<" to "<<hist2->GetYaxis()->GetXmax()
	 <<endl;
  }
  dr(histname);
}

//-------------------------------------------------------------------------------------
// 1e). Drawing Histograms-------------------------------------------------------------
void draw(Char_t *histname,Char_t *dopt="",Float_t xmin=-999999.,Float_t xmax=999999.)
{// Draws a 1D histogram with a given option and over a specified (user) range
  //copied form util.cc
  Int_t minbin,maxbin;
  Int_t lowbin,highbin;
  Float_t xlow,xhigh;
   
  TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
  lowbin=hist1->GetXaxis()->GetFirst();
  highbin=hist1->GetXaxis()->GetLast();
  if (xmin==-999999. && xmax==999999.) {
    hist1->GetXaxis()->UnZoom();//added
    // hist1->GetXaxis()->SetRange(lowbin,highbin);
  } else if (xmax==999999. && xmin !=-999999.) {
    minbin=hist1->FindBin(xmin);
    hist1->GetXaxis()->SetRange(minbin,highbin);
  } else {
    minbin=hist1->FindBin(xmin);
    maxbin=hist1->FindBin(xmax);
    hist1->GetXaxis()->SetRange(minbin,maxbin);
  }
  hist1->Draw(dopt);
}

void draw2(Char_t *histname,Float_t xmin=-999999.,Float_t xmax=999999.,
	   Float_t ymin=-999999.,Float_t ymax=999999.)
{// Draws a 2D histogram with a given option and over a specified (user) range
  //copied from util.cc
  Int_t minxbin,maxxbin;
  Int_t lowxbin,highxbin;
  Float_t xlow,xhigh;
  Int_t minybin,maxybin;
  Int_t lowybin,highybin;
  Float_t ylow,yhigh;
   
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
   
  lowxbin=hist2->GetXaxis()->GetFirst();
  highxbin=hist2->GetXaxis()->GetLast();
  lowybin=hist2->GetYaxis()->GetFirst();
  highybin=hist2->GetYaxis()->GetLast();
   
  if (xmin==-999999. && xmax==999999.) {
    hist2->GetXaxis()->UnZoom();//added
    //hist2->GetXaxis()->SetRange(lowxbin,highxbin);
  } else if (xmax==999999. && xmin !=-999999.) {
    minxbin=hist2->GetXaxis()->FindBin(xmin);
    hist2->GetXaxis()->SetRange(minxbin,highxbin);
  } else {
    minxbin=hist2->GetXaxis()->FindBin(xmin);
    maxxbin=hist2->GetXaxis()->FindBin(xmax);
    hist2->GetXaxis()->SetRange(minxbin,maxxbin);
  }
  if (ymin==-999999. && ymax==999999.) {
    hist2->GetYaxis()->UnZoom();//added    
    //hist2->GetYaxis()->SetRange(lowybin,highybin);
  } else if (ymax==999999. && ymin !=-999999.) {
    minybin=hist2->GetYaxis()->FindBin(ymin);
    hist2->GetYaxis()->SetRange(minybin,highybin);
  } else {
    minybin=hist2->GetYaxis()->FindBin(ymin);
    maxybin=hist2->GetYaxis()->FindBin(ymax);
    hist2->GetYaxis()->SetRange(minybin,maxybin);
  }
  hist2->Draw("col2");
}

void dr(Char_t *histname,Float_t xmin=-999999.,Float_t xmax=999999.,Float_t ymin=-999999.,
	Float_t ymax=999999., Bool_t clear=1)
{//Extension to draw() and draw2().  
 //Accepts either 1-, 2- or 3-D histograms as input, then via the whatis() command, draws the
 //histogram.  Makes the cFit canvas, if it is not present, and clears it if it is.
  if(gROOT->FindObject(histname)) {//take no action if histogram not found.
    if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
    if(clear) cFit->Clear();  
    if(whatis(histname,0)==1||whatis(histname,0)==2){
      draw(histname,"",xmin,xmax);//draw() takes the draw option as the second argument.
    }
    if(whatis(histname,0)==3||whatis(histname,0)==4)
      draw2(histname,xmin,xmax,ymin,ymax);
    if(whatis(histname,0)==5)
      gROOT->FindObject(histname)->Draw();
  }
  else
    printf("Histogram \"%s\" not recognized.\n",histname);
}

void odr(Char_t *histname)
{//Extension to draw() and draw2().  
 //Accepts either 1-, 2- or 3-D histograms as input, then via the whatis() command, draws the
 //histogram.  Makes the cFit canvas, if it is not present, and clears it if it is.
  if(gROOT->FindObject(histname)) {//take no action if histogram not found.
    hname=histname;
    if(whatis(histname,0)==1||whatis(histname,0)==2){
      hProj=(TH1F*)gROOT->FindObject(hname.Data());
      hProj->SetLineColor(2);
      hProj->Draw("same");
    }
    if(whatis(histname,0)==3||whatis(histname,0)==4){
      hInput=(TH2F*)gROOT->FindObject(hname.Data());
      hInput->SetMarkerStyle(21);     
      hInput->SetMarkerSize(0.125);
      hInput->Draw("same");
    }
    if(whatis(histname,0)==5)
      gROOT->FindObject(histname)->Draw("same");
  }
  else
    printf("Histogram \"%s\" not recognized.\n",histname);
}

void lowstat(Char_t *histname, Int_t style=7, Int_t size=1, Int_t color=1)
{//Drawing options for low statistics
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit");  
  hInput=(TH2F *) gROOT->FindObject(histname);
  cFit->cd();
  if(style==0){//Draw color histogram on black background
    cFit->SetFrameFillColor(1);
    draw2(histname);  
  }
  else{//Manually set marker
    cFit->SetFrameFillColor(0);
    hInput->SetMarkerStyle(style);
    hInput->SetMarkerSize(size);
    hInput->SetMarkerColor(color);
    hInput->Draw();
  }
}

//-------------------------------------------------------------------------------------
// 1f). Projecting Histograms----------------------------------------------------------
// 1fi). Project 2D Histograms-----------------------------------------------
void pjx(Char_t *histname, Float_t ymin=-999999., Float_t ymax=999999.)
{//copied form util.cc
  Int_t ybin1,ybin2;
  if ((TH1D *) gROOT->FindObject("xproj")) xproj->Delete();
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
   
  if (ymin==-999999. && ymax==999999.) {
    ybin2=hist2->GetNbinsY();
    ybin1=0;
  } else if (ymax==999999. && ymin !=-999999.) {
    ybin1=hist2->GetYaxis()->FindBin(ymin);
    ybin2=ybin1;
  } else {
    ybin1=hist2->GetYaxis()->FindBin(ymin);
    ybin2=hist2->GetYaxis()->FindBin(ymax);
  }
   
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  hist2->ProjectionX("xproj",ybin1,ybin2)->Draw();
}

void pjy(Char_t *histname, Float_t xmin=-999999., Float_t xmax=999999.)
{//copied form util.cc
  Int_t xbin1,xbin2;
  if ((TH1D *) gROOT->FindObject("yproj")) yproj->Delete();

  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  if (xmin==-999999. && xmax==999999.) {
    xbin2=hist2->GetNbinsX();
    xbin1=0;
  } else if (xmax==999999. && xmin !=-999999.) {
    xbin1=hist2->GetXaxis()->FindBin(xmin);
    xbin2=xbin1;
  } else {
    xbin1=hist2->GetXaxis()->FindBin(xmin);
    xbin2=hist2->GetXaxis()->FindBin(xmax);
  }
  hist2->ProjectionY("yproj",xbin1,xbin2)->Draw();
}

void pjxy(Char_t *histin)
{//plots the x- and y-projections of a 2D histogram
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  cFit->Clear();
  cFit->Divide(1,2);
  cFit->cd(1);
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  //  hInput->Draw("COL2");

  hname=histin;
  hname+="_px"; 
  hInput->ProjectionX(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->Draw();
  
  cFit->cd(2);
  hname=histin;
  hname+="_py"; 
  hInput->ProjectionY(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->Draw();
}

void drpjxy(Char_t *histin)
{//plots the x- and y-projections of a 2D histogram
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  cFit->Clear();
  cFit->Divide(2,2);
  cFit->cd(2);
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  hInput->Draw("COL2");
  
  cFit->cd(4);
  hname=histin;
  hname+="_px"; 
  hInput->ProjectionX(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->Draw();
  int color=hProj->GetLineColor();

  cFit->cd(1);
  hname=histin;
  hname+="_py"; 
  hInput->ProjectionY(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->SetLineColor(color);
  hProj->Draw();//hbar2
}

void opjx(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Int_t col=2)
{//"overlay ProjectionX" - extension to pjx()
  hInput=(TH2F *) gROOT->FindObject(histin);
  hname=histin;
  hname+="_px";
  if(maxpf==minpf){
    minpf=hInput->GetYaxis()->GetXmin();
    maxpf=hInput->GetYaxis()->GetXmax();
  }
  minpf=hInput->GetYaxis()->FindBin(minpf);
  maxpf=hInput->GetYaxis()->FindBin(maxpf);
  
  hInput->ProjectionX(hname,minpf,maxpf);

  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->SetLineColor(col);
  hProj->Draw("same");
}

void opjy(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Int_t col=2,Int_t linesty=1)
{//
  hInput=(TH2F *) gROOT->FindObject(histin);
  hname=histin;
  hname+="_py";
  if(maxpf==minpf){
    minpf=hInput->GetXaxis()->GetXmin();
    maxpf=hInput->GetXaxis()->GetXmax();
  }
  minpf=hInput->GetXaxis()->FindBin(minpf);
  maxpf=hInput->GetXaxis()->FindBin(maxpf);
  hInput->ProjectionY(hname,minpf,maxpf);

  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->SetLineColor(col);
  hProj->SetLineStyle(linesty);
  hProj->Draw("same");
}

// 1fii). Profile 2D Histograms----------------------------------------------
void pfx(Char_t *histname,Float_t ymin=-999999.,Float_t ymax=999999.)
{//copied form util.cc
  if ((TProfile *) gROOT->FindObject("xprof")) xprof->Delete();
  Int_t yminbin,ymaxbin;
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  if (ymin==-999999.) {
    yminbin=0;
  } else {
    yminbin=hist2->GetYaxis()->FindBin(ymin);
  }
  if (ymax==999999.) {
    ymaxbin=hist2->GetNbinsY();
  } else {
    ymaxbin=hist2->GetYaxis()->FindBin(ymax);
  }
  hist2->ProfileX("xprof",yminbin,ymaxbin)->Draw();
}

void pfy(Char_t *histname,Float_t xmin=0, Float_t xmax=0)
{//copied form util.cc
  if ((TProfile *) gROOT->FindObject("yprof")) yprof->Delete();
  Int_t xminbin,xmaxbin;
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  if (xmin==0) {
    xminbin=0;
  } else {
    xminbin=hist2->GetXaxis()->FindBin(xmin);
  }
  if (xmax==0) {
    xmaxbin=hist2->GetNbinsX();
  } else {
    xmaxbin=hist2->GetXaxis()->FindBin(xmax);
  }
  hist2->ProfileY("yprof",xminbin,xmaxbin)->Draw();

}

// 1fiii). Project 3D Histograms---------------------------------------------
void pj3x(Char_t *histname, Float_t ylow=-999999., Float_t yhi=999999., Float_t zlow=-999999., Float_t zhi=999999.)
{//copied form util.cc
  Int_t ylobin,yhibin,zlobin,zhibin;
  TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
  Int_t nch=strlen(histname+3);
  char * projname=new char[nch];
  sprintf(projname,"%s_%s",histname,"x"); 

  if (ylow==-999999. && yhi=999999) {
    ylow=hist3->GetYaxis()->GetXmin();
    yhi=hist3->GetYaxis()->GetXmax();
  }
  if (zlow==-999999. && zhi=999999) {
    zlow=hist3->GetZaxis()->GetXmin();
    zhi=hist3->GetZaxis()->GetXmax();
  }

  ylobin=hist3->GetYaxis()->FindBin(ylow);
  yhibin=hist3->GetYaxis()->FindBin(yhi);
  zlobin=hist3->GetZaxis()->FindBin(zlow);
  zhibin=hist3->GetZaxis()->FindBin(zhi);

  hist3->GetYaxis()->SetRange(ylobin,yhibin);
  hist3->GetZaxis()->SetRange(zlobin,zhibin);
  hist3->GetXaxis()->SetRange(0,hist3->GetNbinsX());
  hist3->Project3D("x")->Draw();
}

void pj3y(Char_t *histname, Float_t xlow, Float_t xhi, Float_t zlow, Float_t zhi)
{//copied form util.cc
  TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
  hist3->GetXaxis()->SetRange(xlow,xhi);
  hist3->GetZaxis()->SetRange(zlow,zhi);
  hist3->Project3D("y");
}

void pj3z(Char_t *histname, Float_t xlow, Float_t xhi, Float_t ylow, Float_t yhi)
{//copied form util.cc
  TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
  hist3->GetXaxis()->SetRange(xlow,xhi);
  hist3->GetYaxis()->SetRange(ylow,yhi);
  hist3->Project3D("z");
}

void pj3xy(Char_t *histname, Float_t zmin=-999999., Float_t zmax=999999.)
{//copied form util.cc
  Int_t zbin1,zbin2;
  TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
  // search for an existing projection and delete it if it exists:
  Int_t nch=strlen(histname+4);
  char * projname=new char[nch];
  sprintf(projname,"%s_%s",histname,"xy"); 

  if (zmin==-999999. && zmax==999999.) {
    zmin=hist3->GetZaxis()->GetXmin();
    zmax=hist3->GetZaxis()->GetXmax();
  } else if (zmax==999999. && zmin !=-999999.) {
    zmax=zmin;
  } 
     
  zbin1=hist3->GetZaxis()->FindBin(zmin);
  zbin2=hist3->GetZaxis()->FindBin(zmax);
   
  hist3->GetZaxis()->SetRange(zbin1,zbin2);
     
  hist3->GetXaxis()->SetRange(0,hist3->GetNbinsX());
  hist3->GetYaxis()->SetRange(0,hist3->GetNbinsY());
   
  hist3->Project3D("xy")->Draw("col2");
}

void pj3yx(Char_t *histname, Float_t zmin=-999999., Float_t zmax=999999.)
{//copied form util.cc
  Int_t zbin1,zbin2;
  TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
  // search for an existing projection and delete it if it exists:
  Int_t nch=strlen(histname+4);
  char * projname=new char[nch];
  sprintf(projname,"%s_%s",histname,"xy"); 
  //   if ((TH2D *) gROOT->FindObject(projname)) 
  //     (TH2D *) gROOT->FindObject(projname)->Delete();     
   
  if (zmin==-999999. && zmax==999999.) {
    zmin=hist3->GetZaxis()->GetXmin();
    zmax=hist3->GetZaxis()->GetXmax();
  } else if (zmax==999999. && zmin !=-999999.) {
    zmax=zmin;
  } 
     
  zbin1=hist3->GetZaxis()->FindBin(zmin);
  zbin2=hist3->GetZaxis()->FindBin(zmax);
   
  hist3->GetZaxis()->SetRange(zbin1,zbin2);
   
  //   hist3->GetYaxis()->SetRange(hist3->GetYaxis()->GetXmin(),
  //			       hist3->GetYaxis()->GetXmax());
 
  //   hist3->GetXaxis()->SetRange(hist3->GetXaxis()->GetXmin(),
  //			       hist3->GetXaxis()->GetXmax());
  
  hist3->GetXaxis()->SetRange(0,hist3->GetNbinsX());
  hist3->GetYaxis()->SetRange(0,hist3->GetNbinsY());
   
  hist3->Project3D("yx")->Draw("col2");
}

//---------------------------------------------------------------------------
// 1g). Drawing Several Histograms-------------------------------------------
void plotall(Char_t *histin,Char_t *suffix="",Bool_t log=0,Float_t minX=0,Float_t maxX=0,
	     Float_t minY=0,Float_t maxY=0,Int_t scale=1,bool show_blank=false)
{//script to replace all of the macros in helios_plottools.cc
  Int_t col=0,row=0;
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit",1350,616);
  Int_t no=0,no1=0, no2=0; //number of histograms with given name
  Int_t no0=0;
  printf("Searching for histograms named %s%s...\n",histin,suffix);
  for(Int_t i=0;i<65;++i){
    hname=histin;
    hname+=i;//had to change from "=hname+i" to "+=i" to work on ROOT 5.26
    hname+=suffix;//updated to be compatible with poorly named histograms
    if(gROOT->FindObject(hname.Data())) {
      no++;
      if((gROOT->FindObject(hname.Data())->InheritsFrom("TH1F"))||(gROOT->FindObject(hname.Data())->InheritsFrom("TH1D"))) {
	no1++;
	hInput=(TH2F*)gROOT->FindObject(hname.Data());
	if(hInput->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
      if((gROOT->FindObject(hname.Data())->InheritsFrom("TH2F"))||(gROOT->FindObject(hname.Data())->InheritsFrom("TH2D"))) {
	no2++;
	hProj=(TH1F*)gROOT->FindObject(hname.Data());
	if(hProj->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
    }
  }
  printf(" Histograms in memory with name %s%s: %d.  1D: %d.  2D: %d.\n",histin,suffix,no,no1,no2);
  printf(" Histograms with zero entries: %d.\n",no0);
  if(show_blank)
    printf("Note: Blank spaces will be left for histograms with zero entries.\n");
  else
    no-=no0;
  
  if(no!=0){
    cFit->Clear();
    printf("Plotting %d histograms with name %s%s...\n",no,histin,suffix);  
    if(no>6){
      col=6;
      col=(Int_t)TMath::Nint(TMath::Sqrt(no));
    }
    else
      col=ceil(no/2.);
    row=ceil(no/(Float_t)col);
    
    cFit->Divide(col,row);
    printf(" Dividing canvas as %d,%d\n",col,row);

    TPad *pOutput=0;
    Int_t pno=0;
    for(int i=0;i<((no1+no2)+1);++i){
      TString pname="cFit_";
      pname+=pno+1;
      pOutput=(TPad*)gROOT->FindObject(pname.Data());
      cFit->cd(pno+1);
       
      hname=histin;
      hname+=i;
      hname+=suffix;     
 
      if(gROOT->FindObject(hname.Data())) {//only try to plot if histogram exists
	if(gROOT->FindObject(hname.Data())->InheritsFrom("TH2F")) {//if histograms are 2D
	  hInput=(TH2F*)gROOT->FindObject(hname.Data());

	  if(hInput->GetEntries()>0) {
	    pno++;
	    //printf(" Plotting %s\n",hname.Data()); 
	    if(maxX==minX){//show full range 
	      minX=hInput->GetXaxis()->GetXmin();
	      maxX=hInput->GetXaxis()->GetXmax();
	      scale=0;//added, otherwise assumes all histograms same size
	    }
	   
	    if(maxY==minY){
	      minY=hInput->GetYaxis()->GetXmin();
	      maxY=hInput->GetYaxis()->GetXmax();
	    } 
	    if((minY==0)&&(log))
	      minY=0.1;
	    if(scale==1){
	      hInput->SetAxisRange(minX,maxX,"X");
	      hInput->SetAxisRange(minY,maxY,"Y");
	    }
	    else{
	      hInput->SetAxisRange(-1,-1,"X");
	      hInput->SetAxisRange(-1,-1,"Y");
	      hInput->GetXaxis()->UnZoom();
	      hInput->GetYaxis()->UnZoom();
	    }
	    if(log)
	      pOutput->SetLogz();
	    hInput->Draw("COL2");
	  }else if(show_blank)pno++;
	}
	else{//if histograms are 1-D
	  hProj=(TH1F*)gROOT->FindObject(hname.Data());
	  if(hProj->GetEntries()>0) {
	    pno++;
	    //printf(" Plotting %s\n",hname.Data());
	    if(maxX==minX){
	      minX=hProj->GetXaxis()->GetXmin();
	      maxX=hProj->GetXaxis()->GetXmax();
	      if(maxY==minY)
		scale=0;
	    }
	   
	    if(scale==1){
	      hProj->SetAxisRange(minX,maxX,"X");
	      hProj->SetAxisRange(minY,maxY,"Y");
	    }
	    else{
	      hProj->SetAxisRange(-1,-1,"X");
	      hProj->GetXaxis()->UnZoom();
	    }
	    if(log)
	      pOutput->SetLogy();  
	    hProj->Draw("");
	  }else if(show_blank)pno++;
	}
      }
    }
  }
  else{//if only one histogram
    hname=histin;
    dr(hname);   
    //hInput=(TH2F*)gROOT->FindObject(hname.Data());
    //hInput->Draw("COL2");
  }
}

void plotallpjx(Char_t *histin,Float_t minY=0,Float_t maxY=0,Int_t scale=1)
{//note, mins and maxs not used.  consider matching input to pjx()
  Int_t col=0,row=0;  
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit",1272,695);
  Int_t no=0;
  Int_t no0=0;
  printf("Searching for histograms named %s...\n",histin);
  for(int i=0;i<65;++i){
    hname=histin;
    hname+=i;
    if((TH2F*)gROOT->FindObject(hname.Data())){
      no++;
      hInput=(TH2F*)gROOT->FindObject(hname.Data());  
      if(hInput->GetEntries()==0){
	no0++;
	printf(" Histogram %s has no entries.\n",hname.Data());
      }
    }
  }
  printf(" Histograms in memory with name %s: %d.\n",histin,no);
  printf(" Histograms with zero entries: %d.\n",no0);
  no-=no0;
  if(no!=0){
    cFit->Clear();
    printf("Plotting projections of %d histograms with name %s...\n",no,histin);  
    if(no>6){
      col=6;
      col=(Int_t)TMath::Nint(TMath::Sqrt(no));
    }
    else
      col=ceil(no/2.);
    row=ceil(no/(Float_t)col);

    cFit->Divide(col,row);
    printf(" Dividing canvas as %d,%d\n",col,row);

    TPad *pOutput=0;
    Int_t pno=0;
    for(int i=0;i<(no+no0);++i) {
      TString pname="cFit_";
      pname+=pno+1;
      pOutput=(TPad*)gROOT->FindObject(pname.Data());
      cFit->cd(pno+1);

      hname=histin;
      hname+=i;
      
      if(gROOT->FindObject(hname.Data())) {//only try to plot if histogram exists
	hInput=(TH2F*)gROOT->FindObject(hname.Data());  
	if(hInput->GetEntries()>0) {
	  pno++;
	  
	  if(maxY==minY){
	    minY=hInput->GetYaxis()->GetXmin();
	    maxY=hInput->GetYaxis()->GetXmax();
	    scale=0;
	  }
	  // minY=hInput->GetYaxis()->FindBin(minY);
	  //maxY=hInput->GetYaxis()->FindBin(maxY);
	  
	  hname=hname+"_px";
	  printf(" Plotting projection %s\n",hname.Data());	  
	  hInput->ProjectionX();//hname,minY,maxY);
	  
	  hProj=(TH1F *) gROOT->FindObject(hname.Data());
	  hProj->Draw();
	}
      }
    }
  }
}

void plotallpjy(Char_t *histin,Float_t minX=0,Float_t maxX=0,Int_t scale=1)
{
  Int_t col=0,row=0;
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit",1272,695);
  Int_t no=0;
  Int_t no0=0;
  printf("Searching for histograms named %s...\n",histin);
  for(int i=0;i<65;++i){
    hname=histin;
    hname+=i;
    if((TH2F*)gROOT->FindObject(hname.Data())){
      no++;
      hInput=(TH2F*)gROOT->FindObject(hname.Data());  
      if(hInput->GetEntries()==0){
	no0++;
	printf(" Histogram %s has no entries.\n",hname.Data());
      }
    }
  }
  printf(" Histograms in memory with name %s: %d.\n",histin,no);
  printf(" Histograms with zero entries: %d.\n",no0);
  no-=no0;
  if(no!=0){
    cFit->Clear();
    printf("Plotting projections of %d histograms with name %s...\n",no,histin);  
    if(no>6){
      col=6;
      col=(Int_t)TMath::Nint(TMath::Sqrt(no));
    }
    else
      col=ceil(no/2.);
    row=ceil(no/(Float_t)col);

    cFit->Divide(col,row);
    printf(" Dividing canvas as %d,%d\n",col,row);

    TPad *pOutput=0;
    Int_t pno=0;
    for(int i=0;i<(no+no0);++i) {
      TString pname="cFit_";
      pname+=pno+1;
      pOutput=(TPad*)gROOT->FindObject(pname.Data());
      cFit->cd(pno+1);

      hname=histin;
      hname+=i;
      
      if(gROOT->FindObject(hname.Data())) {//only try to plot if histogram exists
	hInput=(TH2F*)gROOT->FindObject(hname.Data());  
	if(hInput->GetEntries()>0) {
	  pno++;
	    
	  if(maxX==minX){
	    minX=hInput->GetXaxis()->GetXmin();
	    maxX=hInput->GetXaxis()->GetXmax();
	    scale=0;
	  }
	  //minX=hInput->GetXaxis()->FindBin(minX);
	  //maxX=hInput->GetXaxis()->FindBin(maxX);
	  
	  hname=hname+"_py";
	  printf(" Plotting projection %s\n",hname.Data());	  
	  hInput->ProjectionY();//hname,minX,maxX);
	  hProj=(TH1F *) gROOT->FindObject(hname);
	  hProj->Draw();
	}
      }
    }
  }
}

void plotalllow(Char_t *histin, Char_t *suffix="", Int_t style=7, Int_t size=1, Int_t color=1)
{//extention to plotall for low-statistics histograms
  Int_t col=0,row=0;
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit",1358,616);
  Int_t no=0,no1=0, no2=0; //number of histograms with given name
  Int_t no0=0;
  for(Int_t i=0;i<25;++i){
    hname=histin;
    hname+=i;//had to change from "=hname+i" to "+=i" to work on ROOT 5.26
    hname+=suffix;
    if(gROOT->FindObject(hname.Data())) {
      no++;
      if(gROOT->FindObject(hname.Data())->InheritsFrom("TH1F")) {
	no1++;
	hInput=(TH2F*)gROOT->FindObject(hname.Data());
	if(hInput->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
      if(gROOT->FindObject(hname.Data())->InheritsFrom("TH2F")) {
	no2++;
	hProj=(TH1F*)gROOT->FindObject(hname.Data());
	if(hProj->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
    }
  }
  printf(" Histograms with name %s%s: %d.  1D: %d.  2D: %d.\n",histin,suffix,no,no1,no2);
  printf(" Histograms with zero entries: %d.\n",no0);

  no-=no0;
  if(no!=0){
    cFit->Clear();
    printf("Plotting %2d histograms with name %s%s...\n",no,histin,suffix);  
    if(no>6){
      col=6;
      col=(Int_t)TMath::Nint(TMath::Sqrt(no));
    }
    else
      col=ceil(no/2.);
    row=ceil(no/(Float_t)col);
    
    cFit->Divide(col,row);
    printf("Dividing canvas as %d,%d\n",col,row);
    
    TPad *pOutput=0;
    Int_t pno=0;
    for(int i=0;i<((no1+no2)+1);++i){
      TString pname="cFit_";
      pname+=pno+1;
      pOutput=(TPad*)gROOT->FindObject(pname.Data());
      cFit->cd(pno+1);
      
      hname=histin;
      hname+=i;
      hname+=suffix;
      
      if(gROOT->FindObject(hname.Data())) {
	hInput=(TH2F*)gROOT->FindObject(hname.Data());
	if(hInput->GetEntries()>0) {
	  pno++;
	  if(style==0){//Draw color histogram on black background
	    pOutput->SetFrameFillColor(1);
	    hInput->Draw("col2");
	  }
	  else{
	    hInput->SetMarkerStyle(style);
	    hInput->SetMarkerSize(size);
	    hInput->SetMarkerColor(color);
	    hInput->Draw();
	  }
	}
      }
    }
  }
}

void setrange(Char_t *histin,Float_t minX=0,Float_t maxX=0,Float_t minY=0,Float_t maxY=0,Int_t scale=1)
{//doesn't work!
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  if(maxY==minY){
    minY=hInput->GetYaxis()->GetXmin();
    maxY=hInput->GetYaxis()->GetXmax();
    hInput->SetAxisRange(minY ,maxY ,"Y");
  }
  
  if(maxX==minX){
    minX=hInput->GetXaxis()->GetXmin();
    maxX=hInput->GetXaxis()->GetXmax();
    hInput->SetAxisRange(minX,maxX,"X");
  }
  
  if(scale==1){
    hInput->SetAxisRange(minX,maxX,"X");
    hInput->SetAxisRange(minY ,maxY ,"Y");
  }
  else{
    hInput->GetYaxis()->UnZoom();
    hInput->GetYaxis()->UnZoom();
  }
}

void setscale(Char_t *histin,Float_t minX=0,Float_t maxX=0,Float_t minY=0,Float_t maxY=0,
	      Int_t scale=1){
  //this program doesn't work because the variable go out of scope!
  Float_t xmin,xmax; 
  Float_t ymin,ymax; 

  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
 
  xmax=hInput->GetXaxis()->GetXmax();
  ymax=hInput->GetYaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin();
  ymin=hInput->GetYaxis()->GetXmin();

  if(scale==1){
    hInput->SetAxisRange(minX,maxX,"X");
    hInput->SetAxisRange(minY,maxY,"Y");
  }
  else{
    hInput->SetAxisRange(xmin,xmax,"X");
    hInput->SetAxisRange(ymin,ymax,"Y");
  }
 
  hInput->Draw("COL2");
}

void oplotall(Char_t *histin,Char_t *suffix="",Bool_t log=0,Float_t minX=0,Float_t maxX=0,
	     Float_t minY=0,Float_t maxY=0,Int_t scale=1,bool show_blank=false)
{//script to replace all of the macros in helios_plottools.cc
  Int_t col=0,row=0;
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2("cFit","cFit",1350,616);
  Int_t no=0,no1=0, no2=0; //number of histograms with given name
  Int_t no0=0;
  printf("Searching for histograms named %s%s...\n",histin,suffix);
  for(Int_t i=0;i<65;++i){
    hname=histin;
    hname+=i;//had to change from "=hname+i" to "+=i" to work on ROOT 5.26
    hname+=suffix;//updated to be compatible with poorly named histograms
    if(gROOT->FindObject(hname.Data())) {
      no++;
      if((gROOT->FindObject(hname.Data())->InheritsFrom("TH1F"))||(gROOT->FindObject(hname.Data())->InheritsFrom("TH1D"))) {
	no1++;
	hInput=(TH2F*)gROOT->FindObject(hname.Data());
	if(hInput->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
      if((gROOT->FindObject(hname.Data())->InheritsFrom("TH2F"))||(gROOT->FindObject(hname.Data())->InheritsFrom("TH2D"))) {
	no2++;
	hProj=(TH1F*)gROOT->FindObject(hname.Data());
	if(hProj->GetEntries()==0){
	  no0++;
	  printf(" Histogram %s has no entries.\n",hname.Data());
	}
      }
    }
  }
  printf(" Histograms in memory with name %s%s: %d.  1D: %d.  2D: %d.\n",histin,suffix,no,no1,no2);
  printf(" Histograms with zero entries: %d.\n",no0);
  if(show_blank)
    printf("Note: Blank spaces will be left for histograms with zero entries.\n");
  else
    no-=no0;
  
  if(no!=0){
    cFit->Clear();
    printf("Plotting %d histograms with name %s%s...\n",no,histin,suffix);  
  
    TPad *pOutput=0;
    Int_t pno=0;
    for(int i=0;i<((no1+no2)+1);++i){
      TString pname="cFit";
      //      pname+=pno+1;
      pOutput=(TPad*)gROOT->FindObject(pname.Data());
         
      hname=histin;
      hname+=i;
      hname+=suffix;     
 
      if(gROOT->FindObject(hname.Data())) {//only try to plot if histogram exists
	if(gROOT->FindObject(hname.Data())->InheritsFrom("TH2F")) {//if histograms are 2D
	  hInput=(TH2F*)gROOT->FindObject(hname.Data());

	  if(hInput->GetEntries()>0) {
	    pno++;
	    //printf(" Plotting %s\n",hname.Data()); 
	    if(maxX==minX){//show full range 
	      minX=hInput->GetXaxis()->GetXmin();
	      maxX=hInput->GetXaxis()->GetXmax();
	      scale=0;//added, otherwise assumes all histograms same size
	    }
	   
	    if(maxY==minY){
	      minY=hInput->GetYaxis()->GetXmin();
	      maxY=hInput->GetYaxis()->GetXmax();
	    } 
	    if(scale==1){
	      hInput->SetAxisRange(minX,maxX,"X");
	      hInput->SetAxisRange(minY,maxY,"Y");
	    }
	    else{
	      hInput->SetAxisRange(-1,-1,"X");
	      hInput->SetAxisRange(-1,-1,"Y");
	      hInput->GetXaxis()->UnZoom();
	      hInput->GetYaxis()->UnZoom();
	    }
	    if(log)
	      pOutput->SetLogz();
	    hInput->Draw("COL2");
	  }else if(show_blank)pno++;
	}
	else{//if histograms are 1-D
	  hProj=(TH1F*)gROOT->FindObject(hname.Data());
	  if(hProj->GetEntries()>0) {
	    pno++;
	    if(maxX==minX){
	      minX=hProj->GetXaxis()->GetXmin();
	      maxX=hProj->GetXaxis()->GetXmax();
	      if(maxY==minY)
		scale=0;
	    }
	    if((minY==0)&&(log))
	      minY=0.1;
	    if(scale==1){
	      hProj->SetAxisRange(minX,maxX,"X");
	      hProj->SetAxisRange(minY,maxY,"Y");
	    }
	    else{
	      hProj->SetAxisRange(-1,-1,"X");
	      hProj->GetXaxis()->UnZoom();
	    }
	    if(log)
	      pOutput->SetLogy();  
	    hProj->SetLineColor(i+1);
	    hProj->Draw("same");
	  }else if(show_blank)pno++;
	}
      }
    }
  }
  else{//if only one histogram
    hname=histin;
    dr(hname);   
    //hInput=(TH2F*)gROOT->FindObject(hname.Data());
    //hInput->Draw("COL2");
  }
}

/* 2). Manipulation Utilities----------------------------------------------------------
 *     Utilities for copying, shifting, and scaling histograms.
 */
//---------------------------------------------------------------------------
// 2a). Manipulate 1 histogram-----------------------------------------------
void copy1(Char_t *histin, Float_t miny=0, Float_t maxy=-1, Int_t plot=2)
{ //copies a 1D histogram with zero suppression
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Int_t xbin;
  Float_t x,y;
  
  TH1F * hFit=(TH1F *) gROOT->FindObject(histin);

  xbin=hFit->GetXaxis()->GetNbins();
  xmax=hFit->GetXaxis()->GetXmax();
  xmin=hFit->GetXaxis()->GetXmin();
 
  hname=histin;
  hname+="_copy"; 
  Char_t *htitle = hFit->GetTitle();
  printf("Output histogram is \"%s\"",hname.Data());
  printf(" with min=%f and max=%f\n",miny,maxy);
  if ((TH1F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf(" Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }

  TH1F * hResult=new  TH1F(hname,htitle,xbin,xmin,xmax);

  if(miny==-1) miny=(hFit->GetMaximum())/4;
  for(int i=0;i<(xbin+2);i++){
    //Note: The 0 bin contains the underflow, so the loop starts at 0;
    //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
    y=hFit->GetBinContent(i);
    if((maxy!=-1)&&(y>maxy))
      y=maxy;
    if(y>miny)
      hResult->SetBinContent(i,y);
  }

  switch (plot){
  case 0: 
    break;
  case 1:
    cFit->Clear();
    hResult->Draw("colz");
    break;
  case 2:
    cFit->Clear();
    cFit->Divide(1,2);
    cFit->cd(1);
    hFit->Draw();
    cFit->cd(2);
    hResult->Draw();
    break;
  default:
    break;
  }
}

void copy2(Char_t *histin, Float_t minz=0, Float_t maxz=-1, Int_t plot=2)
{ //copies a 2D histogram with zero suppression, developed from trim_xfxn()
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Float_t ymin,ymax; 
  Int_t xbin;
  Int_t ybin,entry=0;
  Float_t x,y,z;
  
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);

  xbin=hInput->GetXaxis()->GetNbins();
  ybin=hInput->GetYaxis()->GetNbins();

  xmax=hInput->GetXaxis()->GetXmax();
  ymax=hInput->GetYaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin();
  ymin=hInput->GetYaxis()->GetXmin();

  hname=histin;
  hname+="_copy"; 
  Char_t *htitle = hInput->GetTitle();
  printf(" Output histogram is \"%s\"\n",hname.Data());

  if ((TH2F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }

  // printf("Output histogram is constructed as:\n TH2F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,xmin,xmax,ybin,ymin,ymax);
  TH2F * hOutput=new  TH2F(hname,htitle,xbin,xmin,xmax,ybin,ymin,ymax);

  for(int i=0;i<(xbin+2);i++){
    for(int j=0;j<(ybin+2);j++){
      //Note: The 0 bin contains the underflow, so the loop starts at 0;
      //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
      x=hInput->GetXaxis()->GetBinCenter(i);
      y=hInput->GetYaxis()->GetBinCenter(j);
      z=hInput->GetBinContent(i,j);
      //printf("i=%2d, j=%2d, z=%2.0f \n",i,j,z);
      if(z!=0){
	if((Int_t)z-z)printf("Warning!  The content of bin (%d,%d) is not an integer! (%f)\n",i,j,z);
	for(int k=0;k<(z-minz);k++){//Each bin is filled with a for loop so the number of entries is the same in the copied histogram (for minz=0).  Otherwise, the number of entries is equal to the number of non-zero bins.
	  if(maxz==-1)
	    hOutput->Fill(x,y,1);
	  else
	    if(k<maxz)
	      hOutput->Fill(x,y,1);
	  // entry++;
	  //	 printf("Entry %2d is %2f,%2f,%2.0f\n",entry,x,y,1);
	}
      }
    }
  }

  switch (plot){
  case 0: 
    break;
  case 1:
    cFit->Clear();
    hOutput->Draw("colz");
    break;
  case 2:
    cFit->Clear();
    cFit->Divide(1,2);
    cFit->cd(1);
    hInput->Draw("colz");
    cFit->cd(2);
    hOutput->Draw("colz");
    break;
  default:
    break;
  }
}

void trimbin(Char_t *histin, Int_t left=0, Int_t right=0, Int_t top=0, Int_t bottom=0, Int_t plot=2)
{// copies a 2D histogram with zero suppression 
  //confined to a box from (left,bottom) to (right,top)
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Float_t ymin,ymax; 
  Int_t xbin;
  Int_t ybin,entry=0;
  Float_t x,y,z;
  
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);

  xbin=hInput->GetXaxis()->GetNbins();
  ybin=hInput->GetYaxis()->GetNbins();

  xmax=hInput->GetXaxis()->GetXmax();
  ymax=hInput->GetYaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin();
  ymin=hInput->GetYaxis()->GetXmin();

  hname=histin;
  hname+="_copy"; 
  Char_t *htitle = hInput->GetTitle();
  printf("Output histogram is \"%s\"\n",hname.Data());

  if ((TH2F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }

  // printf("Output histogram is constructed as:\n TH2F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,xmin,xmax,ybin,ymin,ymax);
  TH2F * hOutput=new  TH2F(hname,htitle,xbin,xmin,xmax,ybin,ymin,ymax);

  for(int i=left;i<(xbin+2-right);i++){
    for(int j=bottom;j<(ybin+2-top);j++){
      //Note: The 0 bin contains the underflow, so the loop starts at 0;
      //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
      x=hInput->GetXaxis()->GetBinCenter(i);
      y=hInput->GetYaxis()->GetBinCenter(j);
      z=hInput->GetBinContent(i,j);
      //printf("i=%2d, j=%2d, z=%2.0f \n",i,j,z);
      if(z!=0){
	if((Int_t)z-z)printf("Warning!  The content of bin (%d,%d) is not an integer! (%f)\n",i,j,z);
	for(int k=0;k<z;k++){//Each bin is filled with a for loop so the number of entries is the same in the copied histogram (for minz=0).  Otherwise, the number of entries is equal to the number of non-zero bins.
	 
	  hOutput->Fill(x,y,1);
	 
	  // entry++;
	  //	 printf("Entry %2d is %2f,%2f,%2.0f\n",entry,x,y,1);
	}
      }
    }
  }

  switch (plot){
  case 0: 
    break;
  case 1:
    cFit->Clear();
    hOutput->Draw("colz");
    break;
  case 2:
    cFit->Clear();
    cFit->Divide(1,2);
    cFit->cd(1);
    hInput->Draw("colz");
    cFit->cd(2);
    hOutput->Draw("colz");
    break;
  default:
    break;
  }
}

void shiftx1(Char_t *histin, Float_t shift=0, Bool_t move_axis=kFALSE, Int_t plot=2)
{ //shifts a 1D histogram along the x-axis
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Int_t xbin;
  Float_t x,y;
  TH1F * hFit=(TH1F *) gROOT->FindObject(histin);

  xbin=hFit->GetXaxis()->GetNbins();
  xmax=hFit->GetXaxis()->GetXmax();
  xmin=hFit->GetXaxis()->GetXmin();
 
  hname=histin;
  hname+="_shift"; 
  Char_t *htitle = hFit->GetTitle();
  printf("Output histogram is \"%s\"\n",hname.Data());
  
  if ((TH1F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf(" Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
  // printf("Output histogram is constructed as:\n TH2F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,xmin,xmax,ybin,ymin,ymax);
  
  if(move_axis)
    TH1F * hResult=new  TH1F(hname,htitle,xbin,xmin+shift,xmax+shift);
  else
    TH1F * hResult=new  TH1F(hname,htitle,xbin,xmin,xmax);

  for(int i=0;i<(xbin+2);i++){
    //Note: The 0 bin contains the underflow, so the loop starts at 0;
    //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
    x=hFit->GetBinCenter(i);   
    y=hFit->GetBinContent(i);
    
    hResult->Fill(x+shift,y);
  }

  switch (plot){
  case 0://no plots 
    break;
  case 1://output only
    cFit->Clear();
    hResult->Draw("colz");
    break;
  case 2:
    cFit->Clear();
    cFit->Divide(1,2);
    cFit->cd(1);
    hFit->Draw();
    cFit->cd(2);
    hResult->Draw();
    break;
  default:
    break;
  }
}

void shiftx2(Char_t *histin, Float_t shift_x=0, Bool_t move_x_axis=kFALSE, Float_t shift_y=0,
	     Bool_t move_y_axis=kFALSE, Int_t plot=2)
{//copies a 2D histogram with a given y-offset.
  if(plot>0)if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    

  Float_t xmin,xmax; 
  Float_t ymin,ymax; 
  Int_t xbin;
  Int_t ybin,entry=0;
  Float_t x,y,z;
  if(gROOT->FindObject(histin))  
    TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  else
    printf(" Histogram \"%s\" not recognized\n",histin);

  printf(" Notice: Overflow and underflow are neglected, so the\n         total number of entries may not match!\n");

  xbin=hInput->GetXaxis()->GetNbins();
  ybin=hInput->GetYaxis()->GetNbins();

  xmax=hInput->GetXaxis()->GetXmax();
  ymax=hInput->GetYaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin();
  ymin=hInput->GetYaxis()->GetXmin();

  hname=histin;
  hname+="_shift"; 
  Char_t *htitle = hInput->GetTitle();
  printf(" Output histogram is \"%s\"\n",hname.Data());

  if ((TH2F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf(" Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
 
  Float_t set_xmin=xmin, set_xmax=xmax, set_ymin=ymin, set_ymax=ymax;
   if(move_x_axis) {
    set_xmin+=shift_x;
    set_xmax+=shift_x;
  }
  if(move_y_axis) {
    set_ymin+=shift_y;
    set_ymax+=shift_y;
  }
  TH2F * hOutput=new  TH2F(hname,htitle,xbin,set_xmin,set_xmax,ybin,set_ymin,set_ymax);
  printf("Output histogram is constructed as:\n TH2F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,set_xmin,set_xmax,ybin,set_ymin,set_ymax); 

  Float_t xbinw=hInput->GetXaxis()->GetBinWidth(0);
  Float_t ybinw=hInput->GetYaxis()->GetBinWidth(0);
  printf(" Note: X bin width is %f.  X shift is %f bins.\n",xbinw,shift_x/xbinw);
  printf(" Note: Y bin width is %f.  Y shift is %f bins.\n",ybinw,shift_y/ybinw);
  if(shift_x!=0&&fabs(shift_x)<(hInput->GetXaxis()->GetBinWidth(0)))
    printf(" Note: the x-offset is less than the x-bin width.\n");
  if(shift_y!=0&&fabs(shift_y)<(hInput->GetYaxis()->GetBinWidth(0)))
    printf(" Note: the y-offset is less than the y-bin width.\n");
  
  for(int i=1;i<(xbin+1);i++){
    for(int j=1;j<(ybin+1);j++){
      //Note: The 0 bin contains the underflow, so the loop starts at 1;
      //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+1
      x=hInput->GetXaxis()->GetBinCenter(i);
      y=hInput->GetYaxis()->GetBinCenter(j);
      z=hInput->GetBinContent(i,j);
      if(z!=0){
	for(int k=0;k<(z);k++){//Each bin is filled with a for loop so the number of entries 
	  // is the same in the copied histogram (for minz=0).  Otherwise, the number of entries 
	  //is equal to the number of non-zero bins.
	  hOutput->Fill(x+shift_x,y+shift_y,1);
	}
      }
    }
  }
  if(plot==2){
    cFit->Clear();
    cFit->Divide(1,2);
    cFit->cd(1);
    hInput->Draw("colz");
    cFit->cd(2);
  }
  if(plot==1)cFit->Clear();
  if(plot>0) hOutput->Draw("colz");
}

void slopex(Char_t *histin, Float_t slope=1, Float_t offset=-99,
	    Bool_t scale=1, Float_t min=0, Float_t max=0)
{ //Copies and scales a 1D histogram with given slope and offset.
  //"scale" sets whether the bin size is scaled.
  //If no slope and offset are given, the values in temp.lst are used.
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    

  Float_t xmin,xmax; 
  Int_t xbin_in,xbin_out;
  Float_t x,y,z;
  TH1F * hProj=(TH1F *) gROOT->FindObject(histin);
  Float_t xwidth_in;

  xbin_in=hProj->GetXaxis()->GetNbins();
  xbin_out=xbin_in;
  xmax=hProj->GetXaxis()->GetXmax();
  xmin=hProj->GetXaxis()->GetXmin(); 
  xwidth_in=hProj->GetBinWidth(0);

  FILE * infile;
  Float_t fslope=1,foffset=0;
  infile = fopen ("temp.lst","r");
  fscanf(infile,"%f, ",&fslope);
  fscanf(infile,"%f",&foffset);
  fclose(infile);
  printf("Contents of temp.lst: %f, %f\n",fslope,foffset);

  if((slope==1)&&(offset==-99)){
    slope=fslope;
    offset=foffset;
  }

  if(min>=max){//i.e. no/bad range given
    if(slope<0){
      Float_t min=xmin;
      //  if(scale){
      xmin=(xmax-offset)/slope;
      xmax=(min-offset)/slope;
      // }
      // else{
      // xmin=xmax*-1;
      // xmax=min*-1;
      // }
    }
    else{
      // if(scale){
      xmax=(xmax-offset)/slope;
      xmin=(xmin-offset)/slope;
      // }
    }
    if(!scale)
      xbin_out=abs(xbin_in*slope);
  }
  else{
    if(scale)
      xbin_out=abs(xbin_in*slope*(max-min)/(xmax-xmin));
    xmin=min;
    xmax=max;
  }

  hname=histin;
  hname+="_slope"; 
  Char_t *htitle = hProj->GetTitle();
  if ((TH1F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
  printf("Output histogram is constructed as:\n TH1F(\"%s\",\"%s\",%d,%1.0f,%1.0f)\n",         hname.Data(),htitle,xbin_out,xmin,xmax);
  TH1F * hResult=new  TH1F(hname,htitle,xbin_out,xmin,xmax);

  for(int i=1;i<(xbin_in+1);i++){
    x=hProj->GetXaxis()->GetBinCenter(i);
    z=hProj->GetBinContent(i);
    if(z!=0){
      for(int k=0;k<(z);k++){
	hResult->Fill((x-offset)/slope,1);
      }
    }
  }
  cFit->Clear();
  cFit->Divide(1,2);
  cFit->cd(1);
  hProj->Draw();
  cFit->cd(2);
  hResult->Draw();
  Float_t xwidth_out=hResult->GetBinWidth(0);
  if(offset<xwidth_out&&offset!=0)
    printf("Notice: offset (%f) is smaller than bin size (%f).\n",offset,xwidth_out);

  if((fabs(xwidth_in-fabs(xwidth_out*slope))/xwidth_in)>1E-5){
    printf("Warning: Bin width mismatch! (%f)\n",(fabs(xwidth_in-fabs(xwidth_out*slope))/xwidth_in));
    printf("Input bin width is  %f\n",xwidth_in);
    printf("Output bin width is %f",xwidth_out);
    printf(" (Scaled bin width is %f)\n",xwidth_out*slope);
  }
}

void slopexy(Char_t *histin, Float_t slopex=1, Float_t offsetx=0,
	     Float_t slopey=1, Float_t offsety=0)
{ //Copies and scales a 2D histogram with given slope and offset.
  //"scale" sets whether the bin size is scaled. (disabled)
  //The axis range may be set manually with min and max. (disabled)
  Bool_t scale=1; Float_t min=0; Float_t max=0;
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  
  Float_t xmin,xmax; 
  Float_t ymin,ymax; 
  Int_t xbin_in,xbin_out;
  Int_t ybin_in,ybin_out;
  Int_t entry=0;
  Float_t x,y,z;
  Float_t xwidth_in;
  Float_t ywidth_in;
  
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  
  xbin_in=hInput->GetXaxis()->GetNbins();
  xbin_out=xbin_in;
  xmax=hInput->GetXaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin(); 
  xwidth_in=hInput->GetXaxis()->GetBinWidth(0);
  
  ybin_in=hInput->GetYaxis()->GetNbins();
  ybin_out=ybin_in;
  ymax=hInput->GetYaxis()->GetXmax();
  ymin=hInput->GetYaxis()->GetXmin(); 
  ywidth_in=hInput->GetYaxis()->GetBinWidth(0);
  
  if(slopex<0){
    Float_t min=xmin;
    xmin=(xmax-offsetx)/slopex;
    xmax=(min-offsetx)/slopex;
  }
  else{
    xmax=(xmax-offsetx)/slopex;
    xmin=(xmin-offsetx)/slopex;
  }

  if(slopey<0){
    Float_t min=xmin;
    ymin=(ymax-offsety)/slopey;
    ymax=(min-offsety)/slopey;
  }
  else{
    ymax=(ymax-offsety)/slopey;
    ymin=(ymin-offsety)/slopey;
  }
    
  hname=histin;
  hname+="_slope"; 
  Char_t *htitle = hInput->GetTitle();
  if ((TH2F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
  printf("Output histogram is constructed as:\n TH1F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%f,%f,%f)\n",         hname.Data(),htitle,xbin_out,xmin,xmax,ybin_out,ymin,ymax);
  TH2F * hOutput=new  TH2F(hname,htitle,xbin_out,xmin,xmax,ybin_out,ymin,ymax);
  
  for(int i=1;i<(xbin_in+1);i++){
    for(int j=0;j<(ybin_in+1);j++){
      x=hInput->GetXaxis()->GetBinCenter(i);
      y=hInput->GetYaxis()->GetBinCenter(j);
      z=hInput->GetBinContent(i,j);
      if(z!=0){
	for(int k=0;k<(z);k++){
	  hOutput->Fill((x-offsetx)/slopex,(y-offsety)/slopey,1);
	}
      }
    }
  }
  cFit->Clear();
  cFit->Divide(1,2);
  cFit->cd(1);
  hInput->Draw();
  cFit->cd(2);
  hOutput->Draw();
 
  //output warnings
  Float_t xwidth_out=hOutput->GetXaxis()->GetBinWidth(0);
  if(offsetx<xwidth_out&&offsetx!=0)
    printf("offset smaller than bin size\n");
  
  if((fabs(xwidth_in-fabs(xwidth_out*slopex))/xwidth_in)>0){
    printf("Warning: Bin width mismatch!\n");
    printf("Input bin width is  %f\n",xwidth_in);
    printf("Output bin width is %f",xwidth_out);
    printf(" (Scaled bin width is %f)\n",xwidth_out*slopex);
  }
}

void reflect1(Char_t *histin, Float_t offset=0)
{//reflects a 1d histogram about histogram center and overlays it onto itself with an offset
  if(!gROOT->FindObject(histin)) {
    printf(" Histogram \"%s\" not found.\n",histin);
    return;
  }
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Int_t xbin;
  Float_t x,y;
  
  TH1F * hFit=(TH1F *) gROOT->FindObject(histin);

  xbin=hFit->GetXaxis()->GetNbins();
  xmax=hFit->GetXaxis()->GetXmax();
  xmin=hFit->GetXaxis()->GetXmin();
  
  hname=histin;
  hname+="_reflect"; 
  Char_t *htitle = hFit->GetTitle();
  printf(" Output histogram is \"%s\"\n",hname.Data());
  if ((TH1F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf(" Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
  Float_t xbinw=hFit->GetXaxis()->GetBinWidth(0);
  printf(" Note: bin width is %f.  Shift is %f bins.\n",xbinw,offset/xbinw);
  if(offset!=0&&fabs(offset)<(hFit->GetXaxis()->GetBinWidth(0)))
    printf(" Note: the offset is less than the bin width.\n");
  
  TH1F * hResult=new  TH1F(hname,htitle,xbin,xmin,xmax);
  for(int i=0;i<(xbin+2);i++){
    //Note: The 0 bin contains the underflow, so the loop starts at 0;
    //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
    x=hFit->GetBinCenter(i);      
    y=hFit->GetBinContent(i);
    hResult->Fill((xmax-x)+xmin+offset,y);
  }
  cFit->Clear();
  hFit->SetLineColor(1);
  hFit->Draw("");
  hResult->SetLineColor(2);
  hResult->Draw("same");
}

void reflect2(Char_t *histin, Float_t size=1)
{//reflects a 2d histogram about y=x and overlays it onto itself
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    
  Float_t xmin,xmax; 
  Float_t ymin,ymax; 
  Int_t xbin;
  Int_t ybin;
  Float_t x,y,z;
  
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);

  xbin=hInput->GetXaxis()->GetNbins();
  ybin=hInput->GetYaxis()->GetNbins();
  if(xbin!=ybin)
    printf("Warning: histogram not symmetric about y=x\n");
  xmax=hInput->GetXaxis()->GetXmax();
  ymax=hInput->GetYaxis()->GetXmax();
  xmin=hInput->GetXaxis()->GetXmin();
  ymin=hInput->GetYaxis()->GetXmin();

  hname=histin;
  hname+="_reflect"; 
  Char_t *htitle = hInput->GetTitle();
  printf("Output histogram is \"%s\"\n",hname.Data());

  if ((TH2F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }

  // printf("Output histogram is constructed as:\n TH2F(\"%s\",\"%s\",%d,%1.0f,%1.0f,%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,xmin,xmax,ybin,ymin,ymax);
  TH2F * hOutput=new  TH2F(hname,htitle,ybin,ymin,ymax,xbin,xmin,xmax);

  for(int i=0;i<(xbin+2);i++){
    for(int j=0;j<(ybin+2);j++){
      //Note: The 0 bin contains the underflow, so the loop starts at 0;
      //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+2
      x=hInput->GetXaxis()->GetBinCenter(i);
      y=hInput->GetYaxis()->GetBinCenter(j);
      z=hInput->GetBinContent(i,j);
      //printf("i=%2d, j=%2d, z=%2.0f \n",i,j,z);
      if(z!=0){
	if((Int_t)z-z)printf("Warning!  The content of bin (%d,%d) is not an integer! (%f)\n",i,j,z);
	for(int k=0;k<z;k++){//Each bin is filled with a for loop so the number of entries is the same in the copied histogram (for minz=0).  Otherwise, the number of entries is equal to the number of non-zero bins.
	  hOutput->Fill(y,x,1);
	}
      }
    }
  }
  cFit->Clear();
  if(size!=1){
    hInput ->SetMarkerStyle(21);
    hOutput->SetMarkerStyle(21);
  }
  else{
    hInput ->SetMarkerStyle(1);
    hOutput->SetMarkerStyle(1);
  }
 
  hInput->SetMarkerColor(1);
  hInput->SetMarkerSize(size);
  hInput->Draw("");

  hOutput->SetMarkerColor(2);
  hOutput->SetMarkerSize(size);
  hOutput->Draw("same");
}

void scale_slope(Char_t *histin,Float_t slope=1)
{
  if(slope>1){
    printf("Scale XN (x) by %f with slopexy(\"%s\",%f)\n",slope,histin,(1/slope));
    slopexy(histin,(1/slope));
  }
  else{
    printf("Scale XF (y) by %f with slopexy(\"%s\",1,0,%f)\n",1/slope,histin,slope);
    slopexy(histin,1,0,slope);
  }
  hname=histin;
  hname+="_slope";
  reflect2(hname.Data());

  FILE * outfile;
  outfile=fopen("temp.lst","w");
  printf("The contents of \"temp.lst\" is: ");
  //in order to conform to a polynomial fit output, an arbitrary scalar offset is given.
  fprintf(outfile,"%d, %g\n",2048,-slope); 
  printf("2048, %g\n",slope);
  fclose(outfile);
}



//---------------------------------------------------------------------------
// 2b). Manipulate 2 histograms----------------------------------------------
void add2(Char_t *histin1, Char_t *histin2, Char_t *histout=0, 
	  Float_t scale1=1.0, Float_t scale2=1.0)
{//* Extension to add() from util.cc.  Adds two 2D histograms.  If no output is given, a new 
  // histogram is made with copy2().
  TH2F *hist1=(TH2F *) gROOT->FindObject(histin1);
  TH2F *hist2=(TH2F *) gROOT->FindObject(histin2);
  if(!gROOT->FindObject(histin1)) {
    printf(" Histogram \"%s\" not found.\n",histin1);
    return;
  }
  if(!gROOT->FindObject(histin2)) {
    printf(" Histogram \"%s\" not found.\n",histin2);
    return;
  }
  if(!histout){
    printf(" No output histogram given.\n");
    hname=histin1; 
    hname+="_copy";     
    if ((TH2F *) gROOT->FindObject(hname)) {
      printf(" Default output, %s, already exists",hname.Data());
      if(hname==histin2){
	printf(".  Creating new histogram...\n",hname.Data(),histin2);
	copy2(histin2,0);
      }
      else{
	printf("; it will be overwritten.\n",hname.Data());
      }
    }
    else{
      copy2(histin1,0);
    } 
    TH2F *hist3=(TH2F *) gROOT->FindObject(hname.Data());    
  }
  else{
    printf("Output histogram %s",histout);  
    if(!((TH2F *) gROOT->FindObject(histout))){
      printf(" does not exist.  Cloning %s...\n",histin1);
      hist1->Clone(histout);
    }
    else
      printf(" already exists; it will be overwritten.\n");
    TH2F *hist3=(TH2F *) gROOT->FindObject(histout);
  }  
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2(); 
  cFit->Clear();
  cFit->Divide(1,3);
  cFit->cd(1);
  hist1->Draw("COL");
  cFit->cd(2);
  hist2->Draw("COL");
  hist3->Add(hist1,hist2,scale1,scale2); 
  cFit->cd(3);
  hist3->Draw("COL");
}

void shiftadd2(Char_t *histin1, Float_t shift1=0, Char_t *histin2, Float_t shift2=0)
{//adds two 2D histograms with given x-offsets
  TString name1=histin1;
  TString name2=histin2;
  shiftx2(histin1,shift1);
  shiftx2(histin2,shift2);
  name1+="_shift";
  name2+="_shift";
  add2(name1,name2);
  name1+="_copy";
  dr(name1);
}

void subtract(Char_t *histin1, Char_t *histin2, Char_t *histout, Float_t scale1=1.0, Float_t scale2=-1.0)
{//updated to use scale, omits ierr
  //copied from util.cc
  TH1F *hist1=(TH1F *) gROOT->FindObject(histin1);
  TH1F *hist2=(TH1F *) gROOT->FindObject(histin2);
  TH1F *hist3=(TH1F *) gROOT->FindObject(histout);
  hist3->Add(hist1,hist2,scale1,scale2);
  hist3->Draw();
}

void divide(Char_t *histin1, Char_t *histin2, Char_t *histout, Int_t ierr=0)
{//copied from util.cc
  // ierr=-1: set all resultant errors to 0
  // ierr=0: no recalculation of errors
  // ierr=1: use statistical errors on histogram 1
  // ierr=2: use statistical errors on histogram 2
  // ierr=3: combine statistical errors from histograms 1 and 2
  // ierr=4: use existing errors from histogram 1
  // ierr=5: use existing errors from histogram 2
  // ierr=6: combine existing errors from histograms 1 and 2

  Float_t dy1,dy2,dy3,y1,y2,y3;
  TH1F *hist1=(TH1F *) gROOT->FindObject(histin1);
  TH1F *hist2=(TH1F *) gROOT->FindObject(histin2);
  TH1F *hist3=(TH1F *) gROOT->FindObject(histout);
  hist3->Divide(hist1,hist2);
  if (ierr!=0) {
    // recalculate errors assuming statistical or existing errors
    for (Int_t i=0; i<hist1->GetNbinsX(); i++) {
      y1 = hist1->GetBinContent(i);
      y2 = hist2->GetBinContent(i);
      y3 = hist3->GetBinContent(i);
      dy1= hist1->GetBinError(i);
      dy2= hist2->GetBinError(i);
      switch (ierr) {
      case -1:
	dy3 = 0;
	break;
      case 1:
	if (y1!=0) {
	  dy3=TMath::Sqrt(1/y1)*y3;
	} else {
	  dy3=0;
	}
	break;
      case 2:
	if (y2 !=0) {
	  dy3=TMath::Sqrt(1/y2)*y3;
	} else {
	  dy3=0.;
	}
	break;
      case 3:
	if (y1!=0 && y2 !=0) {
	  dy3=TMath::Sqrt(1/y1 + 1/y2)*y3;
	} else if (y1!=0 && y2==0) {
	  dy3=TMath::Sqrt(1/y1) * y3;
	} else if (y1==0 && y2 !=0) {
	  dy3=TMath::Sqrt(1/y2) * y3;
	}
	break;
      case 4:
	if (y1!=0) {
	  dy3=(dy1/y1) * y3;
	} else {
	  dy3=0.;
	}
	break;
      case 5:
	if (y2 !=0) {
	  dy3=(dy2/y2) * y3;
	} else {
	  dy3=0.;
	}
	break;
      case 6:
	//	cout << i << " "<<y1<<" "<<dy1<<" "<<y2<<" "<<dy2<<" "<<y3<<endl;
	if (y1!=0 && y2 !=0) {
	  dy3=TMath::Sqrt((dy1*dy1)/(y1*y1) + (dy2*dy2)/(y2*y2))*y3;
	} else if (y1!=0 && y2==0) {
	  dy3=(dy1/y1) * y3;
	} else if (y1==0 && y2 !=0) {
	  dy3=(dy2/y2) * y3;
	}
	break;
      }
      hist3->SetBinError(i,dy3);
    }
    hist3->Draw("E");
  } else {		     
    hist3->Draw("HIST");
  }
}

void comb1(Char_t *histin1,Char_t *histin2 )
{//combines two 1D histograms
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();    

  Float_t xmin,xmax; 
  Int_t xbin;
  Float_t xmin1,xmax1,xmin2,xmax2; 
  Int_t xbin1,xbin2;
  Float_t x,y,z;
  TH1F * hProj=(TH1F *) gROOT->FindObject(histin1);
  TH1F * hInput=(TH1F *) gROOT->FindObject(histin2);

  xbin1=hProj->GetXaxis()->GetNbins();
  xmax1=hProj->GetXaxis()->GetXmax();
  xmin1=hProj->GetXaxis()->GetXmin();
 
  xbin2=hInput->GetXaxis()->GetNbins();
  xmax2=hInput->GetXaxis()->GetXmax();
  xmin2=hInput->GetXaxis()->GetXmin();
 
  xmin=xmin1;
  if(xmin2<xmin)
    xmin=xmin2;
  xmax=xmax1;
  if(xmax2>xmax)
    xmax=xmax2;
  xbin=xbin1;
  if(xbin2<xbin)
    xbin=xbin2;
 
  hname=histin1;
  hname+="_sum"; 
  Char_t *htitle = hProj->GetTitle();
  printf("Output histogram is \"%s\"\n",hname.Data());

  if ((TH1F *) gROOT->FindObject(hname)) {
    gROOT->FindObject(hname)->Delete();  
    printf("Histogram \"%s\" already exists. Deleting old histogram.\n",hname.Data());
  }
  printf("Output histogram is constructed as:\n TH1F(\"%s\",\"%s\",%d,%1.0f,%1.0f)\n",hname.Data(),htitle,xbin,xmin,xmax);
  TH1F * hResult=new  TH1F(hname,htitle,xbin,xmin,xmax);

  for(int i=1;i<(xbin1+1);i++){
    //Note: The 0 bin contains the underflow, so the loop starts at 1;
    //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+1
    x=hProj->GetXaxis()->GetBinCenter(i);
  
    z=hProj->GetBinContent(i);
    if(z!=0){
      for(int k=0;k<(z);k++){//Each bin is filled with a for loop so the number of entries is the same in the copied histogram (for minz=0).  Otherwise, the number of entries is equal to the number of non-zero bins.
	hResult->Fill(x,1);
      }
    }
  }
 
  for(int i=1;i<(xbin2+1);i++){
    //Note: The 0 bin contains the underflow, so the loop starts at 1;
    //      and the max_bin+1 contains overflow, so the loop terminates at max_bin+1
    x=hInput->GetXaxis()->GetBinCenter(i);
    z=hInput->GetBinContent(i);
    if(z!=0){
      for(int k=0;k<(z);k++){//Each bin is filled with a for loop so the number of entries is the same in the copied histogram (for minz=0).  Otherwise, the number of entries is equal to the number of non-zero bins.
	hResult->Fill(x,1);
      }
    }
  }
 
  cFit->Clear();
  cFit->Divide(1,2);
  cFit->cd(1);
  hProj->Draw();
  cFit->cd(2);
  hResult->Draw();
}



/* 3). Fitting Utilities-------------------------------------------------------------------------
 *
 */
//-------------------------------------------------------------------------------------
// 3a). 1D Fits------------------------------------------------------------------------
void sum(Char_t *histname,Float_t xmin=-999999.,Float_t xmax=999999.,Float_t ymin=-999999.,Float_t ymax=999999.)
{//copied form util.cc
  Int_t minbin,maxbin;
  Int_t lowbin,highbin;
  Float_t sum,centroid,RMS,background=0.;
  Float_t xlow,xhigh;
  Bool_t usebkg=kTRUE;
  if(ymin==-999999. || ymax==999999.) usebkg=kFALSE;
   
  TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
  lowbin=hist1->GetXaxis()->GetFirst();
  highbin=hist1->GetXaxis()->GetLast();
  if (xmin==-999999. && xmax==999999.) {
    //      maxbin=hist1->GetNbinsX();
    //      minbin=0;
    minbin=lowbin;
    maxbin=highbin;
  } else if (xmax==999999. && xmin !=-999999.) {
    minbin=hist1->FindBin(xmin);
    maxbin=minbin;
  } else {
    minbin=hist1->FindBin(xmin);
    maxbin=hist1->FindBin(xmax);
  }
  xlow=hist1->GetXaxis()->GetBinCenter(minbin);
  xhigh=hist1->GetXaxis()->GetBinCenter(maxbin);
  hist1->GetXaxis()->SetRange(minbin,maxbin);
  centroid=hist1->GetMean();
  RMS=hist1->GetRMS();
  if (usebkg) background=(maxbin-minbin+1)*0.5*(ymax+ymin);
  sum=hist1->Integral(minbin,maxbin)-background;
  printf("%.2f - %.2f : Sum=%.1f Bkg= %.1f Centroid=%.3f RMS=%.3f\n",xlow,xhigh,sum,background,centroid,RMS); 
  hist1->GetXaxis()->SetRange(lowbin,highbin);
}

void gfit(Char_t *histname, Float_t xmin=-999999., Float_t xmax=999999, Char_t *option="W")
{//adapted from util.cc
  if(!(gROOT->FindObject(histname))) {
    printf("Histogram %s not found!\n",histname);
    return;
  }
  TH1F *hist1=(TH1F*) gROOT->FindObject(histname);
  if (xmin==-999999. && xmax==999999.) {
    xmin=hist1->GetXaxis()->GetXmin();
    xmax=hist1->GetXaxis()->GetXmax();
  } else if (xmax==999999. && xmin !=-999999.) {
    xmax=xmin;
  } 
  hist1->Fit("gaus",option,"",xmin,xmax);
  if(!((bool)(strchr(option,'q'))||(bool)(strchr(option,'Q'))))
    ginfo();
}

/*void test(Char_t *option="W")
  {
  if((bool)(strchr(option,'q')))
  printf("q is present\n");
  if((bool) (strchr(option,'Q')))
  printf("Q is present\n");
  if(!((bool)(strchr(option,'q'))||(bool)(strchr(option,'Q')))){
  printf("neither!\n");
  ginfo();
  }
  else
  printf("either!\n");
  return; 
  }*/

void ginfo (void)
{
  Float_t sigma=0, width=0, mean=0;
  Float_t gxmin=0, gxmax=0, grange=0; 
  sigma=gaus->GetParameter(2);
  mean=gaus->GetParameter(1);
  width=sigma*2.35482;
  printf("Width of peak is %f or %f FWHM\n",sigma,width);
  printf("Width of peak is %f ns or %f FWHM ns, mean %f ns indiv %f FWHM\n",sigma/5.,width/5.,mean/5.,width/5./sqrt(2));
  printf("Width of peak is %f mm or %f FWHM mm, mean %f mm\n",sigma/5./2.5,width/5./2.5,mean/5./2.5);

  gxmin=gaus->GetCurrent()->GetXmin();
  gxmax=gaus->GetCurrent()->GetXmax();
  grange=gxmax-gxmin;
  printf("The fit covers %f to %f, %f wide (%f sigma)\n",gxmin,gxmax,grange,grange/sigma);
  
  FILE * outfile;
  outfile=fopen("temp.lst","w");
  fprintf(outfile,"%g, %g, %g\n",mean,sigma,grange/sigma);
  fclose(outfile);
}

void gfitc(Char_t *histname, Float_t center=0, Float_t wide=1, Char_t *option="W")
{//fit a quadratic given a center and a width
  gfit(histname,center-wide,center+wide,option);
  hname=histname;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString tempst="";
    tempst=hname(i,hname.Length()-i);
    if(tempst.IsFloat())
      {
	det=tempst.Atoi();
	break;
      }
  }
}

//Float_t gfit_wide=0;
void gfitcm(Char_t *histname, Float_t center=-1, Float_t wide=1, Char_t *option="W")
{//fit a quadratic given a center and a width, automatically calculated about the mean
  if(!(gROOT->FindObject(histname))) {
    printf("Histogram %s not found!\n",histname);
    return;
  }
  //gfit_wide=wide;
  TH1F *hist1=(TH1F*) gROOT->FindObject(histname);
  Bool_t set_center=kFALSE;
  if(center==-1) {
    set_center=kTRUE;
    center=hist1->GetMean();
  }
  gfit(histname,center-wide,center+wide,option);
  if(set_center) {
    center=gaus->GetParameter(1);
    gfit(histname,center-wide,center+wide,option);
    center=gaus->GetParameter(1);
    gfit(histname,center-wide,center+wide,option);
  }
    
  hname=histname;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString tempst="";
    tempst=hname(i,hname.Length()-i);
    if(tempst.IsFloat())
      {
	det=tempst.Atoi();
	break;
      }
  }
}
Float_t gratio=0;
void gfitcp(Char_t *histname, Float_t center=-1, Float_t wide=1, Char_t *option="W")
{//fit a quadratic given a center and a width, automatically calculated about the mean
  if(!(gROOT->FindObject(histname))) {
    printf("Histogram %s not found!\n",histname);
    return;
  }
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();   
  TH1F *hist1=(TH1F*) gROOT->FindObject(histname);
 
  if(center==-1) {
    TSpectrum *spectrum=new TSpectrum();
    Float_t *positions;//moved * before variable name
    spectrum->Search(hist1);//,sigma,option,threshold);
    positions=spectrum->GetPositionX();//in ROOT 5.26+ this array is ordered by peak height!
    center=positions[0];
  }
  gfit(histname,center-wide,center+wide,option);
  center=gaus->GetParameter(1);
  wide=gaus->GetParameter(2)*2;
  //hist1->GetXaxis()->SetRangeUser(center-wide,center+wide);
  gfit(histname,center-wide,center+wide,option);
  
  hname=histname;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString tempst="";
    tempst=hname(i,hname.Length()-i);
    if(tempst.IsFloat())
      {
	det=tempst.Atoi();
	break;
      }
  }
  Float_t gxmin=0, gxmax=0;
  gxmin=gaus->GetCurrent()->GetXmin();
  gxmax=gaus->GetCurrent()->GetXmax();
  hist1->GetXaxis()->SetRangeUser(gxmin,gxmax);
  printf("The peak spans %f to %f\n",gxmin,gxmax);
 
  Float_t in_peak=0, in_full=0;
  in_peak=hist1->GetEffectiveEntries();
  in_full=hist1->GetEntries();
  gratio=in_peak/in_full;
  printf("Under peak %f, full %f, ratio %f\n",in_peak,in_full,gratio);
  hist1->GetXaxis()->SetRangeUser(gxmin-wide,gxmax+wide);

}

void pfit(Char_t *histname, Float_t xmin=-999999., Float_t xmax=999999,Int_t order=1)
{//copied form util.cc
  TH1F *hist1=(TH1F*) gROOT->FindObject(histname);
  if (xmin==-999999. && xmax==999999.) {
    xmin=hist1->GetXaxis()->GetXmin();
    xmax=hist1->GetXaxis()->GetXmax();
  } else if (xmax==999999. && xmin !=-999999.) {
    xmax=xmin;
  } 
  if (order<10) {
    switch(order) {
    case 0:
      cout <<" Not meaningful!!"<<endl;
      break;
    case 1:
      hist1->Fit("pol1","W","",xmin,xmax);
      break;
    case 2:
      hist1->Fit("pol2","W","",xmin,xmax);
      break;
    case 3:
      hist1->Fit("pol3","W","",xmin,xmax);
      break;
    case 4:
      hist1->Fit("pol4","W","",xmin,xmax);
      break;
    case 5:
      hist1->Fit("pol5","W","",xmin,xmax);
      break;
    case 6:
      hist1->Fit("pol6","W","",xmin,xmax);
      break;
    case 7:
      hist1->Fit("pol7","W","",xmin,xmax);
      break;
    case 8:
      hist1->Fit("pol8","W","",xmin,xmax);
      break;
    case 9:
      hist1->Fit("pol9","W","",xmin,xmax);
      break;
    }
  } else {
    cout << "Only works to 9th order."<<endl;
  }
}

//-------------------------------------------------------------------------------------
// 3b). 2D Fits------------------------------------------------------------------------
void sum2(Char_t *histname,Float_t xmin=-999999.,Float_t xmax=999999.,Float_t ymin=-999999.,Float_t ymax=999999.)
{//copied form util.cc
  Int_t minxbin,maxxbin;
  Int_t minybin,maxybin;
  Int_t lowxbin,highxbin;
  Int_t lowybin,highybin;
   
  Float_t sum,centroidx,RMSx,centroidy,RMSy;
  Float_t xlow,xhigh;
  Float_t ylow,yhigh;
   
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  lowxbin=hist2->GetXaxis()->GetFirst();
  highxbin=hist2->GetXaxis()->GetLast();
  lowybin=hist2->GetYaxis()->GetFirst();
  highybin=hist2->GetYaxis()->GetLast();
   
  if (xmin==-999999. && xmax==999999.) {
    //      maxxbin=hist2->GetNbinsX();
    //      minxbin=0;
    minxbin=lowxbin;
    maxxbin=highxbin;
  } else if (xmax==999999. && xmin !=-999999.) {
    minxbin=hist2->GetXaxis()->FindBin(xmin);
    maxxbin=minxbin;
  } else {
    minxbin=hist2->GetXaxis()->FindBin(xmin);
    maxxbin=hist2->GetXaxis()->FindBin(xmax);
  }

  if (ymin==-999999. && ymax==999999.) {
    //      maxybin=hist2->GetNbinsY();
    //      minybin=0;
    minybin=lowybin;
    maxybin=highybin;
  } else if (ymax==999999. && ymin !=-999999.) {
    minybin=hist2->GetYaxis()->FindBin(ymin);
    maxybin=minybin;
  } else {
    minybin=hist2->GetYaxis()->FindBin(ymin);
    maxybin=hist2->GetYaxis()->FindBin(ymax);
  }

   
  xlow=hist2->GetXaxis()->GetBinCenter(minxbin);
  xhigh=hist2->GetXaxis()->GetBinCenter(maxxbin);
  ylow=hist2->GetYaxis()->GetBinCenter(minybin);
  yhigh=hist2->GetYaxis()->GetBinCenter(maxybin);
   
  hist2->GetXaxis()->SetRange(minxbin,maxxbin);
  hist2->GetYaxis()->SetRange(minybin,maxybin);
   
  //   cout << "X axis bins: " << minxbin << " to " << maxxbin<<endl;
  //   cout << "Y axis bins: " << minybin << " to " << maxybin<<endl;

  sum=hist2->Integral(minxbin,maxxbin,minybin,maxybin);
  centroidx=hist2->GetMean(1);
  centroidy=hist2->GetMean(2);
  RMSx=hist2->GetRMS(1);
  RMSy=hist2->GetRMS(2);
  printf("%7.2f - %7.2f : Sum=%9.1f XCentroid=%.3f RMS=%.3f\n",xlow,xhigh,sum,centroidx,RMSx); 
  printf("%7.2f - %7.2f : Sum=%9.1f YCentroid=%.3f RMS=%.3f\n",ylow,yhigh,sum,centroidy,RMSy); 
  hist2->GetXaxis()->SetRange(lowxbin,highxbin);
  hist2->GetYaxis()->SetRange(lowybin,highybin);
}

//-------------------------------------------------------------------------------------
// 3c). Profile Fits-------------------------------------------------------------------
//---------------------------------------------------------------------------
// 3ci). Single-function Fits------------------------------------------------
void fitpfx(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Float_t minfit=0,Float_t maxfit=0,Int_t ord=1,Int_t scale=1,Float_t minz=0, Float_t maxz=-1)
{//adapted from linefit.cc
 //same first three parameters as pfx(), next three same as pfit() (developed independently)
  Float_t cp=0 ;  
  Float_t a=0,b=0,c=0,d=0;  
  Float_t p0=0,p1=0,p2=0,p3=0,p4=0;
  Float_t fits[10];
  hname=histin;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString temp="";
    temp=hname(i,hname.Length()-i);
    if(temp.IsFloat())
      {
	det=temp.Atoi();
	break;
      }
  }

  printf("Detector number %d is being read.\n",det);
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  cFit->Clear();
  cFit->SetWindowPosition(0,0);
  cFit->Divide(1,2);
  cFit->cd(1);

  if(!((minz==0)&&(maxz==-1))){
    copy2(histin,minz,maxz,0);
    hname=histin;
    hname+="_copy";
    histin=hname.Data();
  }

  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  printf("Input histogram is %s\n",histin);    
  if(maxpf==minpf){
    minpf=hInput->GetYaxis()->GetXmin();
    maxpf=hInput->GetYaxis()->GetXmax();
    hInput->SetAxisRange(minpf ,maxpf ,"Y");
  }
  
  if(maxfit==minfit){
    minfit=hInput->GetXaxis()->GetXmin();
    maxfit=hInput->GetXaxis()->GetXmax();
    hInput->SetAxisRange(minfit,maxfit,"X");
  }
    
  if(scale==1){
    hInput->SetAxisRange(minfit,maxfit,"X");
    hInput->SetAxisRange(minpf ,maxpf ,"Y");
  }
  else{
    hInput->GetXaxis()->UnZoom();
    hInput->GetYaxis()->UnZoom();
  }

  hInput->Draw("COL2");
  cFit->cd(2); 
  a=minpf;
  b=maxpf; 
  printf("Projection Limits are %3.2f to %3.2f\n",minpf,maxpf);  
  minpf=hInput->GetYaxis()->FindBin(minpf);
  maxpf=hInput->GetYaxis()->FindBin(maxpf);
  printf("       Fit Limits are %3.2f to %3.2f\n",minfit,maxfit);

  hname=histin;
  hname+="_pfx"; 
  hInput->ProfileX(hname,minpf,maxpf);
 
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->GetXaxis()->UnZoom();
  hProf->GetYaxis()->UnZoom();
  hProf->SetAxisRange(a,b,"Y");
  hProf->SetLineColor(2);
  hProf->Draw();
  
  if(ord>9||ord<1){
    printf("Polynomial fits only valid for orders 1-9.\n");
    ord=1;
  }
  hname="pol";
  hname+=ord;
  printf("Fit function is is \"%s\"\n",hname.Data()); 
  hProf->Fit(hname,"V","",minfit,maxfit);
  hProf->SetStats(kFALSE);
  switch(ord){
  case 1://adapted from fitpfx() in linefit.cc
    p0=hProf->GetFunction("pol1")->GetParameter(0);
    p1=hProf->GetFunction("pol1")->GetParameter(1);
    Float_t slope=p1;
    Float_t offset=p0;
    printf("p1 = %7.3f\n",p1);  
    if(p1==0)
      printf("Fit has no roots. Offset is %g.\n",offset);
    else {
      printf("Fit has root %f\n",-p0/p1);
      printf("Fit parameters are: Slope = %3.3f, Offset = %3.3f\n",slope,offset);
      printf("Inverse fit parameters are slope %f, offset %f\n",1/slope,-offset/slope); 
    }
    if(fabs(p0)<hInput->GetYaxis()->GetBinWidth(1))
      printf("p0 = %7.3f is less than bin width.\n",p0);  
    if(slope<-1)
      printf("Scale XN (x) by %f with slopexy(\"%s\",%f)\n",-slope,histin,(-1/slope));
    else
      printf("Scale XF (y) by %f with slopexy(\"%s\",1,0,%f)\n",-1/slope,histin,-slope);
    FILE * outfile;    
    outfile=fopen("temp_inv.lst","w");
    fprintf(outfile,"%g, %g\n",1/slope,-offset/slope);
    fclose(outfile);
    printf("y-mean is %f offset is %f\n",hInput->GetMean(2),20750-hInput->GetMean(2));
    break;
  case 2:// adapted from linefit.cc
    //      copied from minfit(), used to find minimum (center) of hEX plots
    //      then generalize to fit2pfx()
    p0=hProf->GetFunction("pol2")->GetParameter(0);//"c"
    p1=hProf->GetFunction("pol2")->GetParameter(1);//"b"
    p2=hProf->GetFunction("pol2")->GetParameter(2);//"a"
    cp=(-p1/(2*p2)); //critical point
    Float_t zero1=(-p1-sqrt(p1*p1-(4*p2*p0)))/(2*p2);
    Float_t zero2=(-p1+sqrt(p1*p1-(4*p2*p0)))/(2*p2);
    printf("p1 = %7.3f p2=%7.3f\n",p1,p2);
    printf("Fit has critical point = %7.3f\n",p1,p2,cp);
    Float_t slope=0;
    if((p1*p1-(4*p2*p0))>0){
      printf("Fit has roots = %.1f, %.1f\n",zero1,zero2);
      
      TLine *line = new TLine(0,p0,zero1,0);
      line->SetLineStyle(2);
      line->SetLineWidth(2);
      line->Draw();
      slope=(0-p0)/(zero1-0);
      printf("Linear fit has slope %f\n",slope);

    }
    else{
      printf("Fit has imaginary root(s).\n");
      TLine *line = new TLine(minfit,minfit*minfit*p2+minfit*p1+p0,maxfit,maxfit*maxfit*p2+maxfit*p1+p0);
      line->SetLineStyle(2);
      line->SetLineWidth(2);
      line->Draw();
      slope=((maxfit*maxfit*p2+maxfit*p1+p0)-(minfit*minfit*p2+minfit*p1+p0))/(maxfit-minfit);
      printf("Linear fit has slope %f\n",slope);
    }
    if(slope<-1)
      printf("Scale XN (x) by %f with slopexy(\"%s\",%f)\n",-slope,histin,(-1/slope));
    else
      printf("Scale XF (y) by %f with slopexy(\"%s\",1,0,%f)\n",-1/slope,histin,-slope);
   
    FILE * outfile;
    outfile=fopen("slope.lst","w");
    printf("The contents of \"slope.lst\" is: ");
    fprintf(outfile,"%g\n",slope);
    printf("%g\n",slope);
    fclose(outfile);
   
    break;
  case 3:
    p0=hProf->GetFunction("pol3")->GetParameter(0);
    p1=hProf->GetFunction("pol3")->GetParameter(1);
    p2=hProf->GetFunction("pol3")->GetParameter(2);
    p3=hProf->GetFunction("pol3")->GetParameter(3);
    a=p3;
    b=p2;
    c=p1;
    d=p0;
    Float_t disc=18*a*b*c*d-4*b*b*b*d+b*b*c*c-27*a*a*d*d;
    if(disc<0){
      printf("Fit has one real root\n");
      Float_t zero=0;
      //     TF1 *fit = hProf->GetFuction("pol3");
      //     printf("Fit at zero is %f\n",fit(zero));


    }
    else
      printf("Fit discriminant = %f, yielding all real roots.\n",disc);

    Float_t tol=1000;
    Float_t x0=0; 
    Bool_t bPos=kTRUE;
    Int_t i=0;
    Int_t j=0;
    while(tol>.0001&&i<100){
      if((x0*x0*x0*p3+x0*x0*p2+x0*p1+p0)>0){
	bPos=kTRUE;
	x0+=tol;
	i++;
      }
      else{
	x0-=tol;
	tol=tol/10;
	bPos=kFALSE;
	i=0;
      }
      // printf("f(%f) = %f\n",x0,x0*x0*x0*p3+x0*x0*p2+x0*p1+p0);
      j++;
    }
    printf("Loop exited in %d steps.  First zero located at %.3f\n",j,x0);  
    Float_t slope=(p0)/(x0);
    printf("Linear slope is %f.\n",slope);
    if(slope<1)
      printf("Intercept is %.1f(%.1f), slope is %.1f(%.1f)\n",p0,p0/slope,x0,x0);
    else
      printf("Intercept is %.1f(%.1f), slope is %.1f(%.1f)\n",p0,p0,x0,x0*slope);
    TLine *line = new TLine(0,p0,x0,0);
    line->SetLineStyle(2);
    line->SetLineWidth(2);
    line->Draw();

    break;
  case 4://copied from fit4pfx() in linefit.cc
    p0=hProf->GetFunction("pol4")->GetParameter(0);
    p1=hProf->GetFunction("pol4")->GetParameter(1);
    p2=hProf->GetFunction("pol4")->GetParameter(2);
    p3=hProf->GetFunction("pol4")->GetParameter(3);
    p4=hProf->GetFunction("pol4")->GetParameter(4);
    printf("p1 = %5.0f, p2 = %5.0f, p3 = %5.0f, p4 = %5.0f\ncp = %7.3f\n",p1,p2,p3,p4);
    break;
  default:
    break; 
  }
 
  FILE * outfile;
  outfile=fopen("temp.lst","w");
  printf("The contents of \"temp.lst\" are: ");
  for(Int_t i=0;i<=ord;i++){
    fprintf(outfile,"%g, ",hProf->GetFunction(hname)->GetParameter(i));
    printf("%g ",hProf->GetFunction(hname)->GetParameter(i));
  }
  fprintf(outfile,"\n");
  printf("\n");
  fclose(outfile);
}

void fitpfy(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Float_t minfit=0,Float_t maxfit=0,Int_t scale=1)
{//adapted from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  Float_t a=0,b=0,c=0,d=0; 
  Float_t p0=0,p1=0,p2=0;
  
  hname=histin;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString temp="";
    temp=hname(i,hname.Length()-i);
    if(temp.IsFloat())
      {
	det=temp.Atoi();
	break;
      }
  }

  cFit->Clear();
  cFit->Divide(1,2);
  cFit->cd(1);
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
    
  if(maxpf==minpf){
    minpf=hInput->GetXaxis()->GetXmin();
    maxpf=hInput->GetXaxis()->GetXmax();
  }
  
  if(maxfit==minfit){
    minfit=hInput->GetYaxis()->GetXmin();
    maxfit=hInput->GetYaxis()->GetXmax();
  }
    
  if(scale==1){
    hInput->SetAxisRange(minfit,maxfit,"Y");
    hInput->SetAxisRange(minpf ,maxpf ,"X");
  }
  else{
    hInput->SetAxisRange(-1,-1,"X");
    hInput->SetAxisRange(-1,-1,"Y");
  }

  hInput->Draw("COL2");
  cFit->cd(2); 
  a=minpf;
  b=maxpf;   
  printf("Projection Limits are %d to %d\n",minpf,maxpf);  
  minpf=hInput->GetXaxis()->FindBin(minpf);
  maxpf=hInput->GetXaxis()->FindBin(maxpf);
  printf("Fit Limits are %d to %d\n",minfit,maxfit);
 
  hname=histin;
  hname+="_pfy"; 
  
  hInput->ProfileY(hname,minpf,maxpf);
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->GetXaxis()->UnZoom(); //added
  hProf->GetYaxis()->UnZoom();
  hProf->SetAxisRange(a,b,"Y");
  hProf->SetLineColor(2);
  hProf->Draw();
 
  hProf->Fit("pol1","V","",minfit,maxfit);
  p0=hProf->GetFunction("pol1")->GetParameter(0);
  p1=hProf->GetFunction("pol1")->GetParameter(1);
  //  p2=hProf->GetFunction("pol2")->GetParameter(2);
  // cp=(-p1/(2*p2));
  printf("p1 = %7.3f\n",p1);
}

void timefit(Char_t *histin,Float_t minE=1,Float_t maxE=12,Int_t minpf=0,Int_t maxpf=1200)
{//prototype of fitpqpfy, from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  Float_t cp=0 ;
  Int_t i=0;
  Float_t p0=0,p1=0,p2=0;
  
  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);
  hInput->SetAxisRange(minpf,maxpf,"X");
  hInput->SetAxisRange(minE,maxE,"Y");
  hInput->Draw("COL2");
  cFit->cd(2); 
    
  hname=histin;
  hname+="_pfy"; 
  minpf=hInput->GetXaxis()->FindBin(minpf);
  maxpf=hInput->GetXaxis()->FindBin(maxpf);
  printf("Projection Limits are t = %d to %d\n",minpf,maxpf);
  //minpf=(Int_t)((256/1200.0)*minpf);
  //maxpf=(Int_t)((256/1200.0)*maxpf);
  hInput->ProfileY(hname,minpf,maxpf);
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->SetLineColor(2);
  hProf->Draw();
 
  printf("Fit range is %6.3f to %6.3f\n",minE,maxE);
  hProf->Fit("pol2","V","",minE,maxE);
  p0=hProf->GetFunction("pol2")->GetParameter(0);
  p1=hProf->GetFunction("pol2")->GetParameter(1);
  p2=hProf->GetFunction("pol2")->GetParameter(2);
  cp=(-p1/(2*p2));
  printf("p0 = %7.3f, p1 = %7.3f, p2 = %7.3f, critical point at z = %5.3f\n",p0,p1,p2,cp);
  
  while((fabs((maxE-cp)/cp)>.03)&&i<200){

    if(cp<maxE){
      maxE-=(maxE-cp)/10;
      printf("Fit range is %6.3f MeV to %6.3f MeV, ",minE,maxE);
         
      hProf->Fit("pol2","Q","",minE,maxE);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      
      printf("Critical Point at E = %5.3f\n          p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    else{
      maxE+=(cp-maxE)/10;
      printf("Fit range is %6.3f MeV to %6.3f MeV, \n",minE,maxE);
     
      hProf->Fit("pol2","Q","",minE,maxE);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      
      printf("Critical Point at E = %5.3f\n           p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    i++;
    printf("Iter. %2d: ",i);
    if(i==100)
      printf("Loop terminated.  Could not converge.\n");
  }
}
void timefit2(Char_t *histin,Float_t minE=1,Float_t maxE=24)
{//from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  Float_t cp=0 ;
  Int_t i=0;
  Float_t p0=0,p1=0,p2=0;
  
  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);
  hInput->Draw("COL2");
  cFit->cd(2); 
   
  hInput->ProfileY();
  hname=histin;
  hname+="_pfy";
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->Draw();
 
  printf("Fit range is %6.3f to %6.3f\n",minE,maxE);
  hProf->Fit("pol2","V","",minE,maxE);
  p0=hProf->GetFunction("pol2")->GetParameter(0);
  p1=hProf->GetFunction("pol2")->GetParameter(1);
  p2=hProf->GetFunction("pol2")->GetParameter(2);
  cp=(-p1/(2*p2));
  printf("p0 = %7.3f, p1 = %7.3f, p2 = %7.3f, critical point at z = %5.3f\n",p0,p1,p2,cp);
  
  while((fabs((((maxE+minE)/2)-cp)/cp)>.03)&&i<200){

    if(cp<((maxE+minE)/2)){
      maxE-=(((maxE+minE)/2)-cp)/10;
      printf("Fit range is %6.3f MeV to %6.3f MeV, ",minE,maxE);
         
      hProf->Fit("pol2","Q","",minE,maxE);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      
      printf("Critical Point at E = %5.3f\n          p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    else{
      maxE+=(((maxE+minE)/2)-maxE)/10;
      printf("Fit range is %6.3f MeV to %6.3f MeV, \n",minE,maxE);
     
      hProf->Fit("pol2","Q","",minE,maxE);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      
      printf("Critical Point at E = %5.3f\n           p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    i++;
    printf("Iter. %2d: ",i);
    if(i==100)
      printf("Loop terminated.  Could not converge.\n");
  }
}
void timefitg(Char_t *histin,Float_t minT=100,Float_t maxT=1200)
{//fit the x-projection of a histogram with a Gaussian; redundant, from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);
  hInput->Draw("COL2");
  cFit->cd(2); 
   
  hInput->ProjectionX();
  hname=histin;
  hname+="_px";
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProj->Draw();
 
  hProj->Fit("gaus","V","",minT,maxT);
}

//---------------------------------------------------------------------------
// 3cii). Group Fits---------------------------------------------------------
void fitallpjx(Char_t *histin,Float_t minpj=0,Float_t maxpj=0,Float_t minfit=0,Float_t maxfit=0,Int_t ord=6,Int_t writetofile=0)
{
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  Float_t a=0,b=0;
  for(int i=0;i<24;i++)
    for(int j=0;j<11;j++)
      array[i][j]=0;
  
  cFit->Clear();
  cFit->Divide(1,2);
  a=minpj;
  b=maxpj;

  for(int i=0;i<24;i++){
    array[i][0]=i+1;
    //      printf("input histogram is %s\n",histin); 
    hname=histin;
    hname=hname+(i+1);
    //      printf("input histogram is %s\n",hname.Data()); 
      
    hInput=(TH2F*)gROOT->FindObject(hname.Data());
    cFit->cd(1);
  
    if(maxpj==minpj){
      minpj=hInput->GetYaxis()->GetXmin();
      maxpj=hInput->GetYaxis()->GetXmax();
    }
    minpj=hInput->GetYaxis()->FindBin(minpj);
    maxpj=hInput->GetYaxis()->FindBin(maxpj);
  
    if(maxfit==minfit){
      minfit=hInput->GetXaxis()->GetXmin();
      maxfit=hInput->GetXaxis()->GetXmax();
    }

    hInput->Draw("COL2");
      
    cFit->cd(2); 
    hname+="_px"; 
    // printf("output histogram is %s\n",hname.Data()); 
    hInput->ProjectionX(hname,minpj,maxpj);
    hProj=(TH1F *) gROOT->FindObject(hname.Data());
    hProj->Draw();
     
    hname="pol";
    hname+=ord;
    //      printf("order is %s\n",hname.Data()); 
    hProj->Fit(hname,"Q","",minfit,maxfit);
       
    for (int j=0;j<ord+1;j++){
      array[i][j+1]=hProj->GetFunction(hname)->GetParameter(j);
      //	printf("function is %s, p%d = %f\n",hname.Data(),j,array[i][j+1]);
    }
    minpj=a;
    maxpj=b;
  }
  printcaldata();
  if(writetofile){
    createfile();
    printf("Fit parameters written to file \"calibration.cal\"\n");
  }
}

void pfitallpjx(Char_t *histin,Float_t minpj=0,Float_t maxpj=0,Float_t minfit=0,Float_t maxfit=0,Int_t ord=6,Int_t writetofile=0)
//to be used with addpositions.C
{
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  TString fname;
  for(int i=0;i<24;i++)
    for(int j=0;j<11;j++)
      array[i][j]=0;//initializes array to zero
  
  cFit->Clear();
  cFit->Divide(1,2);

  hname=histin;
  hname=hname+1;
            
  hInput=(TH2F*)gROOT->FindObject(hname.Data());
  if(maxpj==minpj){
    minpj=hInput->GetYaxis()->GetXmin();
    maxpj=hInput->GetYaxis()->GetXmax();
  }
  minpj=hInput->GetYaxis()->FindBin(minpj);
  maxpj=hInput->GetYaxis()->FindBin(maxpj);
      
  if(maxfit==minfit){
    minfit=hInput->GetXaxis()->GetXmin();
    maxfit=hInput->GetXaxis()->GetXmax();
  }
      
  if(i==0){
    printf("Projection range is %f to %f\n",minpj,maxpj);
    printf("Fit range is %f %f\n",minfit,maxfit);
  }


  for(int i=0;i<6;i++){
    array[i][0]=i+1;
    hname=histin;
    hname=hname+(i+1);
            
    hInput=(TH2F*)gROOT->FindObject(hname.Data());
    cFit->cd(1);
    hInput->Draw("COL2");
      
    cFit->cd(2); 
    hname+="_px"; 
    hInput->ProjectionX(hname,minpj,maxpj);
    hProj=(TH1F *) gROOT->FindObject(hname.Data());
    hProj->Draw();
    fname="pol";
    fname+=ord;
    if(i==0)printf("Fit function is \"%s\"\n",fname.Data()); 
    hProj->Fit(fname,"Q","",minfit,maxfit);
       
    for (int j=0;j<ord+1;j++){
      array[i][j+1]=hProj->GetFunction(fname)->GetParameter(j);
      //	printf("function is %s, p%d = %f\n",fname.Data(),j,array[i][j+1]);
    }
  }
  printcaldata();
  if(writetofile){
    createfile();
    printf("Fit parameters written to file \"calibration.cal\"\n");
  }
  else
    printf("Fit parameters NOT written to file.\n");
}

//---------------------------------------------------------------------------
// 3ciii). Piece-wise Fits---------------------------------------------------
void fitpqpfy(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Float_t minfit=0,Float_t maxfit=0,Int_t scale=1)
{//"Fit piece-wise quadratic, ProfileY" - generalized from timefit(), from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  Float_t cp=0 ;
  Int_t i=0;
  Float_t p0=0,p1=0,p2=0;
  Float_t a=0,b=0,c=0;
  Float_t tol=.01;

  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);

  if(maxpf==minpf){
    minpf=hInput->GetXaxis()->GetXmin();
    maxpf=hInput->GetXaxis()->GetXmax();
  }
  
  if(maxfit==minfit){
    minfit=hInput->GetYaxis()->GetXmin();
    maxfit=hInput->GetYaxis()->GetXmax();
  }
    
  if(scale==1){
    hInput->SetAxisRange(minfit,maxfit,"Y");
    hInput->SetAxisRange(minpf ,maxpf ,"X");
  }
  else{
    hInput->SetAxisRange(-1,-1,"X");
    hInput->SetAxisRange(-1,-1,"Y");
  }

  hInput->Draw("COL2");
  cFit->cd(2); 
    
  hname=histin;
  hname+="_pfy"; 
  a=minpf;
  b=maxpf;
  c=maxfit;

  minpf=hInput->GetXaxis()->FindBin(minpf);
  maxpf=hInput->GetXaxis()->FindBin(maxpf);
  printf("Projection Limits are t = %d to %d\n",a,b);
  hInput->ProfileY(hname,minpf,maxpf);
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->GetXaxis()->UnZoom();//added
  hProf->GetYaxis()->UnZoom();
  hProf->SetAxisRange(a,b,"Y");
  hProf->Draw();
 
  printf("Fit range is %6.3f to %6.3f\n",minfit,maxfit);
  hProf->Fit("pol2","V","",minfit,maxfit);
  p0=hProf->GetFunction("pol2")->GetParameter(0);
  p1=hProf->GetFunction("pol2")->GetParameter(1);
  p2=hProf->GetFunction("pol2")->GetParameter(2);
  cp=(-p1/(2*p2));
  printf("p0 = %7.3f, p1 = %7.3f, p2 = %7.3f, critical point at z = %5.3f\n",p0,p1,p2,cp);
  
  while((fabs((maxfit-cp)/cp)>tol)&&i<200){

    if(cp<maxfit){
      maxfit-=(maxfit-cp)/10;
      //      printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      //      printf("Critical Point at E = %5.3f\n          p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
      printf("-");
    }
    else{
      maxfit+=(cp-maxfit)/10;
      //       printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      // printf("Critical Point at E = %5.3f\n           p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
      printf("+");
    }
    i++;
    // printf("Iter. %3d: ",i);
    if(i==200){
      printf("\nLoop terminated.  Could not converge.\n");
      tol+=.01;
      maxfit=c;
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));

      printf("Re-initiating loop with tolerance set to %2.0f%%\n",tol*100);

      i=0;
    }
  }
  printf("Loop Exited at Iteration %3d.\nEndpoint tolerance is %2.0f%%\nFit range is %6.3f to %6.3f\nCritical Point is %5.3f\np0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",i,tol*100,minfit,maxfit,cp,p0,p1,p2);
  printf("Emax = %5.2f, p1 = %4.0f, p2 = %7.2f\n",cp,p1,p2);//added

}

void fitpqpfx(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Float_t minfit=0,Float_t maxfit=0,Int_t scale=1)
{//"Fit piece-wise quadratic, ProfileX", from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2(); 
  Float_t cp=0 ;
  Int_t i=0;
  Float_t p0=0,p1=0,p2=0;
  Float_t a=0,b=0,c=0;
  Float_t tol=.001;

  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);
  //if no limits are given, the entire range is projected and fitted
  if(maxpf==minpf){
    minpf=hInput->GetYaxis()->GetXmin();
    maxpf=hInput->GetYaxis()->GetXmax();
  }
  if(maxfit==minfit){
    minfit=hInput->GetXaxis()->GetXmin();
    maxfit=hInput->GetXaxis()->GetXmax();
  }
  
  if(scale==1){
    hInput->SetAxisRange(minfit,maxfit,"X");
    hInput->SetAxisRange(minpf ,maxpf ,"Y");
  }
  else{
    hInput->SetAxisRange(-1,-1,"X");
    hInput->SetAxisRange(-1,-1,"Y");
  }
 
  hInput->Draw("COL2");
  cFit->cd(2); 
    
  hname=histin;
  hname+="_pfx"; 
  a=minpf;
  b=maxpf;
  c=maxfit;

  minpf=hInput->GetYaxis()->FindBin(minpf);
  maxpf=hInput->GetYaxis()->FindBin(maxpf);
  printf("Projection Limits are t = %f to %f\n",a,b);
  hInput->ProfileX(hname,minpf,maxpf);
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->GetXaxis()->UnZoom();\\added
				hProf->GetYaxis()->UnZoom();
  hProf->SetAxisRange(a,b,"Y");
  hProf->SetLineColor(2);
  hProf->Draw();
 
  printf("Fit range is %6.3f to %6.3f\n",minfit,maxfit);
  hProf->Fit("pol2","V","",minfit,maxfit);
  p0=hProf->GetFunction("pol2")->GetParameter(0);
  p1=hProf->GetFunction("pol2")->GetParameter(1);
  p2=hProf->GetFunction("pol2")->GetParameter(2);
  cp=(-p1/(2*p2));
  printf("p0 = %7.3f, p1 = %7.3f, p2 = %7.3f, critical point at z = %5.3f\n",p0,p1,p2,cp);
  
  while((fabs((maxfit-cp)/cp)>tol)&&i<200){

    if(cp<maxfit){
      maxfit-=(maxfit-cp)/10;
      //      printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
         
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      printf("-");
      //      printf("Critical Point at E = %5.3f\n          p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    else{
      maxfit+=(cp-maxfit)/10;
      //       printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      // printf("Critical Point at E = %5.3f\n           p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
      printf("+");
    }
    i++;
    //    printf("Iter. %3d: ",i);
    if(i==200){
      printf("Loop terminated.  Could not converge.\n");
      tol*=1.01;\\modified from add 0.01
		  maxfit=c;
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      
      printf("Re-initiating loop with tolerance set to %2.0f%%\n",tol*100); cp=-1;

      i=0;
    }
  }
  printf("
Loop Exited at Iteration %3d.\nEndpoint tolerance is %2.0f%%\nFit range is %6.3f to %6.3f\nCritical Point is %5.3f\np0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",i,tol*100,minfit,maxfit,cp,p0,p1,p2);
}

void fitpqRpfx(Char_t *histin,Float_t minpf=0,Float_t maxpf=0,Float_t minfit=0,Float_t maxfit=0,Int_t scale=1)
{//"Fit piece-wise quadratic, ProfileX, from right", from linefit.cc
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();  
  Float_t cp=0 ;
  Int_t i=0;
  Float_t p0=0,p1=0,p2=0;
  Float_t a=0,b=0,c=0;
  Float_t tol=.001;

  cFit->Clear();
  cFit->Divide(1,2);
 
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin);
  cFit->cd(1);
  //if no limits are given, the entire range is projected and fitted
  if(maxpf==minpf){
    minpf=hInput->GetYaxis()->GetXmin();
    maxpf=hInput->GetYaxis()->GetXmax();
  }
  if(maxfit==minfit){
    minfit=hInput->GetXaxis()->GetXmin();
    maxfit=hInput->GetXaxis()->GetXmax();
  }
 
  if(scale==1){
    hInput->SetAxisRange(minfit,maxfit,"X");
    hInput->SetAxisRange(minpf ,maxpf ,"Y");
  }
  else{
    hInput->SetAxisRange(-1,-1,"X");
    hInput->SetAxisRange(-1,-1,"Y");
  }
 
  hInput->Draw("COL2");
  cFit->cd(2); 
    
  hname=histin;
  hname+="_pfx"; 
  a=minpf;
  b=maxpf;
  c=minfit;

  minpf=hInput->GetYaxis()->FindBin(minpf);
  maxpf=hInput->GetYaxis()->FindBin(maxpf);
  printf("Projection Limits are t = %d to %d\n",a,b);
  hInput->ProfileX(hname,minpf,maxpf);
  hProf=(TH1F *) gROOT->FindObject(hname.Data());
  cFit->cd(2);
  hProf->GetXaxis()->UnZoom();//added
  hProf->GetYaxis()->UnZoom();
  hProf->SetAxisRange(a,b,"Y");
  hProf->SetLineColor(2);  
  hProf->Draw();
 
  printf("Fit range is %6.3f to %6.3f\n",minfit,maxfit);
  hProf->Fit("pol2","V","",minfit,maxfit);
  p0=hProf->GetFunction("pol2")->GetParameter(0);
  p1=hProf->GetFunction("pol2")->GetParameter(1);
  p2=hProf->GetFunction("pol2")->GetParameter(2);
  cp=(-p1/(2*p2));
  printf("p0 = %7.3f, p1 = %7.3f, p2 = %7.3f, critical point at z = %5.3f\n",p0,p1,p2,cp);
  
  while((fabs((minfit-cp)/cp)>tol)&&i<200){

    if(cp<minfit){
      minfit-=(minfit-cp)/10;
      //      printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
         
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      printf("-"); 
      //      printf("Critical Point at X = %5.3f\n          p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    else{
      minfit+=(cp-minfit)/10;
      //       printf("Fit range is %6.3f to %6.3f, ",minfit,maxfit);
     
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p0=hProf->GetFunction("pol2")->GetParameter(0);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));
      printf("+");
      // printf("Critical Point at X = %5.3f\n           p0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",cp,p0,p1,p2);
    }
    i++;
    //    printf("Iter. %3d: ",i);
    if(i==200){
      printf("\nLoop terminated.  Could not converge.\n");
      tol*=1.01;//modified from add 0.01
      minfit=c;
      hProf->Fit("pol2","Q","",minfit,maxfit);
      p1=hProf->GetFunction("pol2")->GetParameter(1);
      p2=hProf->GetFunction("pol2")->GetParameter(2);
      cp=(-p1/(2*p2));

      printf("Re-initiating loop with tolerance set to %2.0f%%\n",tol*100);
      i=0;
    }
  }
  printf("\nLoop Exited at Iteration %3d.\nEndpoint tolerance is %2.0f%%\nFit range is %6.3f to %6.3f\nCritical Point is %5.3f\np0 = %7.3f, p1 = %7.3f, p2 = %7.3f\n",i,tol*100,minfit,maxfit,cp,p0,p1,p2);
}

//-------------------------------------------------------------------------------------
// 3c). Line Fits----------------------------------------------------------------------
void drawline(Char_t *filename, Char_t *linename="gline",Bool_t showpoints=1,Int_t linestyle=1,Int_t linewidth=1)
{//new
  //Int_t maxpoints=100;//not used
  Int_t point=0;
  Float_t x,y;
  //Int_t npts=0;//not used

  TGraph *graph = new TGraph(2);
  graph->SetName(linename);
  graph->SetTitle(linename);
  graph->SetFillColor(1);
  graph->SetLineStyle(linestyle);
  graph->SetLineWidth(linewidth);

  ifstream infile(filename);
  while (infile >> x) {
    infile>>y;
    graph->SetPoint(point,x,y);
    if(showpoints)
      cout<<"reading point "<<point<<endl;
    point++;
  }
  //   point=point/2;
  cout<<"points = "<<point<<endl;
  
  /*   for (Int_t i=point; i<100; i++) {
       graph->RemovePoint(i);
       npts=graph->GetN();
       cout<<"removed point "<<i<<" npoints = "<<npts<<endl;
       }
  */
  if(showpoints)  
    graph->Print();
  //  cout<<"number of points= "<<npts<<endl;
  graph->Draw("l");
}

void getline(void)
{//shows information about a TLine, adapted from linefit.cc
  Float_t x1,x2,y1,y2;
  Float_t slope=0,b,theta;
  x1=TLine->GetX1();
  y1=TLine->GetY1();
  x2=TLine->GetX2();
  y2=TLine->GetY2();
  slope=(y2-y1)/(x2-x1);
  b=y1-slope*x1;
  theta=atan(slope)/(4.0*atan(1.0))*180;
  printf("Angle of TLine is %6.3f\n",theta);
  if(x1==x2){
    printf("TLine is vertical at %f\n",x1);
  }
  else
    if(y1==y2){
      printf("TLine is horizontal at %f\n",y1);
    }
    else
      {
	if(fabs(slope)>.01) 
	  printf("Slope of TLine is %6.6f\n",slope);
	else
	  printf("Slope of TLine is %6.3e\n",slope);
	printf("Intercept of TLine is %6.3f\n",b);
      }
}

/* 4). TSpectrum Utilities-----------------------------------------------------------------------
 * 1-D peak search          : peakfit(), peakfitx(), peakfity() 
 * 1-D background estimation: bkgfit2()
 * 1-D smoothing            : smooth()
 */
//-------------------------------------------------------------------------------------
// 4a). 1D Peak Search-----------------------------------------------------------------
void peakfit(Char_t *histin, Char_t *filename="", Float_t resolution=2, Double_t sigma=3, 
	     Double_t threshold=0.05, Char_t *option="")
{//Program by AHW.  Modified to run in fit.cc and in "modern" version of ROOT.
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();//added
  hname=histin;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString temp="";
    temp=hname(i,hname.Length()-i);
    if(temp.IsFloat())
      {
	det=temp.Atoi();
	break;
      }
  }

  cFit->Clear();
  
  Float_t *positions;//moved * before variable name
  Float_t energies[100];
  Float_t min_space=25; //minimum space between adjacent peaks (note: no deconvolution is used)
  Float_t slope,offset,width;
  Float_t ein;
  Float_t max=0,min=0;//added
  Float_t a=0,b=0;//added
  Int_t npeaks,nlist=0;
  TSpectrum *spectrum=new TSpectrum();
  Bool_t filefail=false;
  ifstream listfile(filename);
  if(!(listfile))  {//just show the peaks!
    printf("No such file! Proceeding with peak find (without fit).\n");
    filefail=true;
  }
  else {
    printf("Reading file %s\n",filename);
    cFit->Divide(1,2);
  
  while(listfile>>ein) {
    energies[nlist]=ein;
    if(nlist==0)min=energies[nlist];//added
    if(energies[nlist]>max)max=energies[nlist];//added
    if(energies[nlist]<min)min=energies[nlist];//added
    cout << " Energy "<<nlist<< "= "<<energies[nlist]<<endl;
    nlist++;
  }
  printf(" Read in %d peaks from file.\n",nlist);
  if(nlist<=1){
    printf("WARNING: Only %d energy found in file \"%s\"\n         Check file to ensure there is a carriage return on the last line!\n",nlist,filename);
  }
  cFit->cd(1);
  }
  TH1F *hProj=(TH1F *) gROOT->FindObject(histin);//hInput changed to hProj from here on.  
  spectrum->SetResolution(resolution);
  spectrum->Search(hProj,sigma,option,threshold);
  positions=spectrum->GetPositionX();//in ROOT 5.26+ this array is ordered by peak height!
  npeaks=spectrum->GetNPeaks();
  cout << "Found "<<npeaks<<" peaks in spectrum from "<<hname.Data()<<"."<<endl;
  if(gROOT->GetVersionInt()<52400)
    printf("Deprecated (online) ROOT version.\n");
  else{
    printf("Modern ROOT version.\n");
    Float_t sorted[100];
    sorted[0]=positions[0];//initializes sorted array with a valid position
    for(Int_t i=0;i<npeaks;i++){
      if((positions[i])<(sorted[0])){
	sorted[0]=positions[i];//locates minimum
      }
    }
    for(Int_t i=1;i<npeaks;i++){
      sorted[i]=hProj->GetXaxis()->GetXmax();//initializes array element with non-minimum
      for(Int_t j=0;j<npeaks;j++){
	if(((positions[j])<(sorted[i]))&&((positions[j])>(sorted[i-1])))
	  sorted[i]=positions[j];//locates next-smallest position
      }
    }
    positions=sorted;  
  }//end version IF 
  for (Int_t i=0; i<npeaks; i++){//find min. peak spacing
    if(((positions[i+1]-positions[i])<min_space)&&((i+1)<npeaks))
      min_space=positions[i+1]-positions[i];
  }
  printf("The minimum spacing between adjacent peaks is %f\n",min_space);
  Float_t sig_av=0;
  for (Int_t i=0; i<npeaks; i++){
    //cout<<" Peak " <<i<<" found at channel "<<positions[i]<<endl;
    printf(" Peak %d found at channel %.2f",i,positions[i]);
    gfitc(hname.Data(),positions[i],min_space/2,"+q");
    printf(" (%.2g wide)\n",gaus->GetParameter(2));
    sig_av+=gaus->GetParameter(2);
  }
  printf("Average peak width is %f\n",sig_av/npeaks);
  
  hProj->Draw();
    
  if(!(filefail))  {
    if(gROOT->FindObject("hPeakFit"))hPeakFit->Delete();//added, moved
    hFit=new TH1F("hPeakFit","hPeakFit",1024,a,b);//added
    if((min-(max-min)/4)<0)printf("Notice: \"%s\" contains negatives value(s).\n        All zero-content bins are shown.\n",hFit->GetTitle());//added
    //  TH1F *hFit =(TH1F *) gROOT->FindObject("hPeakFit");//needed?
    hFit->Reset();

    a=hProj->GetXaxis()->GetXmin();
    b=hProj->GetXaxis()->GetXmax();

    for (Int_t i=0; i<npeaks; i++) {
      hFit->Fill(energies[i],positions[i]);
    }
    hFit->SetAxisRange(min,max);//added to set fit range to peak range
    //hFit->SetAxisRange(min-(max-min)/2,max+(max-min)/2);
    hFit->Fit("pol1","Q");
    hFit->GetXaxis()->UnZoom();
    hFit->SetStats(kFALSE);//
    slope=hFit->GetFunction("pol1")->GetParameter(1);
    offset=hFit->GetFunction("pol1")->GetParameter(0);
    //hProj->Fit("gaus","QW","",positions[npeaks-1]-min_space,positions[npeaks-1]+min_space);
    //gfitc(hname.Data(),positions[npeaks-1],min_space/2,"+q");
    width=hProj->GetFunction("gaus")->GetParameter(2);
    //cout<<"Fit parameters are:  Slope= "<<slope<<" offset= "<<offset<<" sigma(peak "<<npeaks-1<<")="<<width<<endl;
    printf("Fit parameters are: Slope = %3.3f, Offset = %3.3f\n",slope,offset);
    printf("Inverse fit parameters are slope %f, offset %f\n",1/slope,-offset/slope); 
    printf("Resolution of peak %.0f is = %3.3f MeV or %3.3f MeV FWHM \n",npeaks-1,(width)/slope,(width)/slope*2.35482);
    hFit->SetMarkerStyle(2);
    hFit->SetMarkerColor(1);
    hFit->SetMarkerSize(3);
    cFit->cd(2);
    hFit->Draw("P");
    printf("Testing fit:\n");
    for (Int_t i=0; i<npeaks; i++){
      printf(" Peak %d at %f is %f (%f)\n",i,positions[i],(positions[i]-offset)/slope,((positions[i]-offset)/slope)-energies[i]);
    }  
  }
  else {
printf("                         by height | by pos   | diff\n");
  for (Int_t i=0; i<npeaks; i++){
    printf("Peak %2d found at position %f | %f | %f\n",i,positions[i],sorted[i],sorted[i]-sorted[0]);
  }
  }

  delete spectrum;
  FILE * outfile;
  outfile=fopen("temp.lst","w");
  fprintf(outfile,"%g, %g\n",slope,offset);
  fclose(outfile);
  outfile=fopen("temp_inv.lst","w");
  fprintf(outfile,"%g, %g\n",1/slope,-offset/slope);
  fclose(outfile);
}

void peakfitx(Char_t *histin, Char_t *filename="", Float_t resolution=2, Double_t sigma=3, Double_t threshold=0.05, Char_t *option="")
{//extension of peakfit() - takes a 2D histogram as input
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  hname=histin;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString temp="";
    temp=hname(i,hname.Length()-i);
    if(temp.IsFloat())
      {
	det=temp.Atoi();
	break;
      }
  }

  cFit->Clear();
  Float_t * positions;
  Float_t energies[100];
  Float_t min_space=25; //minimum space between adjacent peaks (note: no deconvolution is used)
  Float_t slope,offset,width;
  Float_t ein; 
  Float_t max=0,min=0;
  Float_t a=0,b=0;
  Int_t npeaks,nlist=0;
  TSpectrum *spectrum=new TSpectrum();
  Bool_t filefail=false;
  ifstream listfile(filename);
  if(!(listfile)) {
    printf("No such file! Proceeding with peak find (without fit).\n");
    filefail=true;
    cFit->Divide(1,2);
  }
  else {
    cFit->Divide(1,3);
    printf("Reading file %s\n",filename);    
    while(listfile>>ein) {
      energies[nlist]=ein;
      if(nlist==0)min=energies[nlist];
      if(energies[nlist]>max)max=energies[nlist];
      if(energies[nlist]<min)min=energies[nlist];
      cout << " Energy "<<nlist<< "= "<<energies[nlist]<<endl;
      nlist++;
    }
    printf(" Read in %d peaks from file.\n",nlist);
    if(nlist<=1){
      printf("WARNING: Only %d energy found in file \"%s\"\n         Check file to ensure there is a carriage return on the last line!\n",nlist,filename);
    }
  }

if(gROOT->FindObject("hPeakFit"))hPeakFit->Delete();//added
 hFit=new TH1F("hPeakFit","hPeakFit",1024,a,b);//added
  if((min-(max-min)/4)<0)printf("Notice: \"%s\" contains negatives value(s).\n        All zero-content bins are shown.\n",hFit->GetTitle());//added
  //  TH1F *hFit =(TH1F *) gROOT->FindObject("hPeakFit");  //needed?
  hFit->Reset();

  cFit->cd(1);
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin); 
  hInput->Draw("COL");
 
  cFit->cd(2);
  hname=histin;
  hname+="_px";
  hInput->ProjectionX(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->Draw(); 
  a=hProj->GetXaxis()->GetXmin();
  b=hProj->GetXaxis()->GetXmax();

  spectrum->SetResolution(resolution);
  spectrum->Search(hProj,sigma,option,threshold);
  positions=spectrum->GetPositionX();
  //for(Int_t i=0;i<15;i++) //wtf!?
    npeaks=spectrum->GetNPeaks();
  cout << "Found "<<npeaks<<" peaks in spectrum from "<<hname.Data()<<"."<<endl;
  if(gROOT->GetVersionInt()<52400)
    printf("Deprecated (online) ROOT version.\n");
  else{
    printf("Modern ROOT version.\n");
    Float_t sorted[100];
    sorted[0]=positions[0];//initializes sorted array with a valid position
    for(Int_t i=0;i<npeaks;i++){
      if((positions[i])<(sorted[0])){
	sorted[0]=positions[i];//locates minimum
      }
    }
    for(Int_t i=1;i<npeaks;i++){
      sorted[i]=hProj->GetXaxis()->GetXmax();//initializes array element with non-minimum
      for(Int_t j=0;j<npeaks;j++){
	if(((positions[j])<(sorted[i]))&&((positions[j])>(sorted[i-1])))
	  sorted[i]=positions[j];//locates next-smallest position
      }
    }
    positions=sorted;  
  }//end version IF 
  for (Int_t i=0; i<npeaks; i++){//find min. peak spacing
    if(((positions[i+1]-positions[i])<min_space)&&((i+1)<npeaks))
      min_space=positions[i+1]-positions[i];
  }
  printf("The minimum spacing between adjacent peaks is %f\n",min_space);
  Float_t sig_av=0;
  for (Int_t i=0; i<npeaks; i++) {
    //cout<<" Peak " <<i<<" found at channel "<<positions[i]<<endl;
    printf(" Peak %2d found at channel %.3f",i,positions[i]);
    gfitc(hname.Data(),positions[i],min_space/2,"+q");
    printf(" (%.3f gaus center %.5f off )",gaus->GetParameter(1),positions[i]-gaus->GetParameter(1));
    printf(" (%.5f wide)\n",gaus->GetParameter(2));
    sig_av+=gaus->GetParameter(2);
    positions[i]=gaus->GetParameter(1);//is this better!?
  }
  printf(" Average peak width is %f\n",sig_av/npeaks);
 
  if(!filefail) {
    for (Int_t i=0; i<npeaks; i++) {
      hFit->Fill(energies[i],positions[i]);
    }
    
    cFit->cd(3);
    hFit->SetAxisRange(min-(max-min)/2,max+(max-min)/2);//set x-axis range
    printf(" Axis range is %f, %f; data (peak center) range is %f, %f\n",min-(max-min)/2,max+(max-min)/2,min,max); 
    hFit->SetAxisRange(a,b);//set x-axis range
    hFit->SetStats(kFALSE);//
    hFit->SetMarkerStyle(2);
    //hFit->SetMarkerColor(2);
    hFit->SetMarkerSize(3);
    hFit->Fit("pol1","Q");
    slope=hFit->GetFunction("pol1")->GetParameter(1);
    offset=hFit->GetFunction("pol1")->GetParameter(0);
    //    hProj->Fit("gaus","Q","",positions[npeaks-1]-(b-a)/15,positions[npeaks-1]+(b-a)/15);
    //width=hProj->GetFunction("gaus")->GetParameter(2);
    cout<<"Fit parameters are:  Slope= "<<slope<<" offset= "<<offset<<" sigma(peak "<<npeaks-1<<")="<<width<<endl;
    printf("Fit parameters are: Slope = %3.3f, Offset = %3.3f\n",slope,offset);
 printf("Inverse fit parameters are slope %f, offset %f\n",1/slope,-offset/slope); 
       
    hFit->Draw("P");
 printf("Testing fit:\n");
  for (Int_t i=0; i<npeaks; i++){
    printf(" Peak %2d at %f is %f (%f)\n",i,positions[i],(positions[i]-offset)/slope,((positions[i]-offset)/slope)-energies[i]);
  }  
  }
  delete spectrum;
  FILE * outfile;
  outfile=fopen("temp.lst","w");
  fprintf(outfile,"%g, %g\n",slope,offset);
  fclose(outfile);
 outfile=fopen("temp_inv.lst","w");
  fprintf(outfile,"%g, %g\n",1/slope,-offset/slope);
  fclose(outfile);
}

void peakfity(Char_t *histin, Char_t *filename="", Float_t resolution=2, Double_t sigma=3, Double_t threshold=0.05, Char_t *option="")
{//extension of peakfit() - takes a 2D histogram as input
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  hname=histin;
  for(Int_t i=0;i<hname.Length();i++){//loop added by Jack
    TString temp="";
    temp=hname(i,hname.Length()-i);
    if(temp.IsFloat())
      {
	det=temp.Atoi();
	break;
      }
  }

  cFit->Clear();
  Float_t * positions;
  Float_t energies[100];
  Float_t min_space=25; //minimum space between adjacent peaks (note: no deconvolution is used)
  Float_t slope,offset,width;
  Float_t ein; 
  Float_t max=0,min=0;
  Float_t a=0,b=0;
  Int_t npeaks,nlist=0;
  TSpectrum *spectrum=new TSpectrum();
  Bool_t filefail=false;
  ifstream listfile(filename);
  if(!(listfile)) {
    printf("No such file! Proceeding with peak find (without fit).\n");
    filefail=true;
    cFit->Divide(1,2);
  }
  else {
    cFit->Divide(1,3);
    printf("Reading file %s\n",filename);    
    while(listfile>>ein) {
      energies[nlist]=ein;
      if(nlist==0)min=energies[nlist];
      if(energies[nlist]>max)max=energies[nlist];
      if(energies[nlist]<min)min=energies[nlist];
      cout << " Energy "<<nlist<< "= "<<energies[nlist]<<endl;
      nlist++;
      printf(" Read in %d peaks from file.\n",nlist);
    }
    if(nlist<=1){
      printf("WARNING: Only %d energy found in file \"%s\"\n         Check file to ensure there is a carriage return on the last line!\n",nlist,filename);
    }
  }
  
if(gROOT->FindObject("hPeakFit"))hPeakFit->Delete();//added
  hFit=new TH1F("hPeakFit","hPeakFit",1024,a,b);//added
  if((min-(max-min)/4)<0)printf("Notice: \"%s\" contains negatives value(s).\n        All zero-content bins are shown.\n",hFit->GetTitle());//added
  //  TH1F *hFit =(TH1F *) gROOT->FindObject("hPeakFit");  //needed?
  //hFit =(TH1F *) gROOT->FindObject("hPeakFit");
  hFit->Reset();  


  cFit->cd(1);
  TH2F * hInput=(TH2F *) gROOT->FindObject(histin); 
  hInput->Draw("COL");
 
  cFit->cd(2);
  hname=histin;
  hname+="_py"; 
  hInput->ProjectionY(hname);
  hProj=(TH1F *) gROOT->FindObject(hname.Data());
  hProj->Draw(); 
  a=hProj->GetXaxis()->GetXmin();
  b=hProj->GetXaxis()->GetXmax();



spectrum->SetResolution(resolution);
  spectrum->Search(hProj,sigma,option,threshold);
  positions=spectrum->GetPositionX();
  npeaks=spectrum->GetNPeaks();
  cout << "Found "<<npeaks<<" peaks in spectrum."<<endl;
  if(gROOT->GetVersionInt()<52400)
    printf("Deprecated (online) ROOT version.\n");
  else{
    printf("Modern ROOT version.\n");
    Float_t sorted[100];
    sorted[0]=positions[0];//initializes sorted array with a valid position
    for(Int_t i=0;i<npeaks;i++){
      if((positions[i])<(sorted[0])){
	sorted[0]=positions[i];//locates minimum
      }
    }
    for(Int_t i=1;i<npeaks;i++){
      sorted[i]=hProj->GetXaxis()->GetXmax();//initializes array element with non-minimum
      for(Int_t j=0;j<npeaks;j++){
	if(((positions[j])<(sorted[i]))&&((positions[j])>(sorted[i-1])))
	  sorted[i]=positions[j];//locates next-smallest position
      }
    }
    positions=sorted;  
  }//end version IF 
  for (Int_t i=0; i<npeaks; i++){//find min. peak spacing
    if(((positions[i+1]-positions[i])<min_space)&&((i+1)<npeaks))
      min_space=positions[i+1]-positions[i];
  }
  printf("The minimum spacing between adjacent peaks is %f\n",min_space);
  Float_t sig_av=0;
  for (Int_t i=0; i<npeaks; i++) {
    //cout<<" Peak " <<i<<" found at channel "<<positions[i]<<endl;
    printf(" Peak %d found at channel %.2f",i,positions[i]);
    gfitc(hname.Data(),positions[i],min_space/2,"+q");
    printf(" (%.2f wide)\n",gaus->GetParameter(2));
    sig_av+=gaus->GetParameter(2);
  }
  printf(" Average peak width is %f\n",sig_av/npeaks);
 
  if(!filefail) {
    for (Int_t i=0; i<npeaks; i++) {
      hFit->Fill(energies[i],positions[i]);
    }
    
    cFit->cd(3);
    hFit->SetAxisRange(a,b);
    hFit->SetMarkerStyle(2);
    //hFit->SetMarkerColor(2);
    hFit->SetMarkerSize(3);
    hFit->Fit("pol1","Q","P");//why quiet?
    slope=hFit->GetFunction("pol1")->GetParameter(1);
    offset=hFit->GetFunction("pol1")->GetParameter(0);
    //     hProj->Fit("gaus","QW","",positions[npeaks-1]-min_space,positions[npeaks-1]+min_space);  
    //width=hProj->GetFunction("gaus")->GetParameter(2);

cout<<"Fit parameters are:  Slope= "<<slope<<" offset= "<<offset<<" sigma(peak "<<npeaks-1<<")="<<width<<endl;
    printf("Fit parameters are: Slope = %3.3f, Offset = %3.3f\n",slope,offset);
 printf("Inverse fit parameters are slope %f, offset %f\n",1/slope,-offset/slope); 
 //printf("Resolution of peak %.0f is = %3.3f MeV or %3.3f MeV FWHM \n",npeaks-1,(width)/slope,(width)/slope*2.35482);
printf("Testing fit:\n");
  for (Int_t i=0; i<npeaks; i++){
    printf(" Peak %d at %f is %f (%f)\n",i,positions[i],(positions[i]-offset)/slope,((positions[i]-offset)/slope)-energies[i]);
  }  
  }
  delete spectrum;
  FILE * outfile;
  outfile=fopen("temp.lst","w");
  fprintf(outfile,"%g, %g\n",slope,offset);
  fclose(outfile);
 outfile=fopen("temp_inv.lst","w");
  fprintf(outfile,"%g, %g\n",1/slope,-offset/slope);
  fclose(outfile);
}

//-------------------------------------------------------------------------------------
// 4b). 1D Background Estimation-------------------------------------------------------
void bkgfit2(Char_t *histin, Float_t resolution=2, Double_t sigma=3, Double_t threshold=.05, Int_t niter=20, Char_t *option="")
{//modified to run in fit.cc and automatically copy histin.
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();//added
  if(!((TH1F *) gROOT->FindObject("hPeakFit"))) hFit=new TH1F("hPeakFit","hPeakFit",1000,0,100);  //added
  hFit=(TH1F *) gROOT->FindObject("hPeakFit");//added
 
  cFit->Clear();//cFitCanvas renamed to CFit
  cFit->Divide(1,2);
  
  Float_t *positions;//* moved to start to avoid automatic variable warning
  Float_t energies[10];
  Float_t slope,offset,width;
  Float_t ein;
  Int_t npeaks,nlist=0;
  float result[2048];
  Int_t nbins;
  TSpectrum *spectrum=new TSpectrum();
 
  //The non-histogram-declaration lines in the following block were added to copy the input histogram automatically.
  TH1F * hProj=(TH1F *) gROOT->FindObject(histin);
  hname=histin;
  hname+="_bkg"; 
  hProj->Clone(hname);
  TString histbkg=hname.Data();
  TH1F *hBkg=(TH1F *) gROOT->FindObject(hname.Data());
  hname=histin;
  hname+="_out"; 
  hProj->Clone(hname);
  TString histout=hname.Data();
  TH1F *hRresult=(TH1F *) gROOT->FindObject(hname.Data());

  hFit->Reset();
  cFit->cd(1);
  spectrum->SetResolution(resolution);
  spectrum->Search(hProj,sigma,option,threshold);
  positions=spectrum->GetPositionX();
  npeaks=spectrum->GetNPeaks();
  cout << "Found "<<npeaks<<" peaks in spectrum."<<endl;
  for (Int_t i=0; i<npeaks; i++){
    cout<<"Peak " <<i<<" found at channel "<<positions[i]<<endl;
  }
  cFit->cd(1);
  hProj->Draw();
  nbins=hProj->GetNbinsX();
  for (Int_t i=0; i<nbins; i++){
    result[i]=hProj->GetBinContent(i);
  }
  spectrum->Background(result,nbins,niter,1,1,kFALSE,1,kFALSE);
  for (Int_t i=0; i<nbins; i++){
    hBkg->SetBinContent(i,result[i]);
  }
  hBkg->SetLineColor(2);
  hBkg->Draw("same");
  cFit->cd(2);
  subtract(histin,histbkg,histout);
}

//-------------------------------------------------------------------------------------
// 4c). Smoothing----------------------------------------------------------------------
void smooth(Char_t *histin,Int_t averWindow=3)
{
  if(!((TCanvas *) gROOT->FindObject("cFit"))) mkCanvas2();
  TH1F * hProj=(TH1F *) gROOT->FindObject(histin);
  hname=histin;
  hname+="_smooth";
  hProj->Clone(hname);
  TH1F *hResult=(TH1F *) gROOT->FindObject(hname.Data());

  Float_t nbins;
  nbins=hProj->GetXaxis()->GetNbins();
 
  Float_t * source = new float[nbins];
  for(int i=0;i<nbins;i++){
    source[i]=hProj->GetBinContent(i+1);
  }
      
  TSpectrum *s = new TSpectrum();
  s->SmoothMarkov(source,nbins,averWindow);  //3, 7, 10
  for (int i = 0; i < nbins; i++) hResult->SetBinContent(i + 1,source[i]);
  cFit->Clear();//cFitCanvas renamed to CFit
  cFit->Divide(1,2);
  cFit->cd(1);
  hProj->Draw();
  cFit->cd(2);
  hResult->Draw("L");
}

/* 5). File Utilities----------------------------------------------------------------------------
 *
 */
//-------------------------------------------------------------------------------------
// 5a). Directory Utilities------------------------------------------------------------
void browse()
{//copied form util.cc
  TBrowser *b=new TBrowser();
}

void setdef(Char_t *dirname)
{//copied form util.cc
  gDirectory->cd(dirname);
  cout << "Current directory is " <<gDirectory->pwd()<<endl;
}

void setname(Char_t *oldname, Char_t *newname)
{//copied form util.cc
  TNamed *obj=(TNamed *) gROOT->FindObject(oldname);
  obj->SetName(newname);
}

void where(void)
{//copied form util.cc
  cout << "Current directory is "<<gDirectory->pwd()<<endl;
}

void sethome()
{ //may not work?
  gROOT->ProcessLine("TDirectory *home=gDirectory"); 
}

//-------------------------------------------------------------------------------------
// 5b). General File Utilities---------------------------------------------------------
void dir(void)
{
  gROOT->GetListOfFiles()->Print();
}

void getfile(Char_t *filename)
{//copied form util.cc
  TFile *f=new TFile(filename);
}

void saveall(Char_t *filename)
{//copied form util.cc
  TList *hlist=gDirectory->GetList();
  TList *clist=gROOT->GetListOfCanvases();
  TList *speclist=gROOT->GetListOfSpecials();
  TFile *fout=new TFile(filename,"recreate");
  fout->cd();
  hlist->Write();
  clist->Write();
  speclist->Write();
  fout->Close();
  cout << "Histograms, canvases and specials written to file "<<filename<<endl;
}

//-------------------------------------------------------------------------------------
// 5c). Calibration File Utilities-----------------------------------------------------
void printcaldata(void)
{//adapted from linefit.cc
  Float_t p0av=0;
  Int_t entries=0;
  printf("Contents of array are: \n");
  for(int i=0;i<24;i++){
    for(int j=0;j<11;j++){
      //cout<<array[i][j]<<" ";
      if(j==0)
	printf("%2d ",array[i][j]);
      else
	printf("%7.0f ",array[i][j]);
    }
    cout<<endl;
    if(array[i][0]){
      // printf("Det %2d, p0 is %f, p0av is %f\n",i+1,array[i][1],p0av);
      p0av+=array[i][1];
      entries++;
    }
  }
  p0av=p0av/entries;
  printf("p0 average is %5.1f for %d entries\n",p0av,entries);
}

void createfile(Int_t numbered=0)
{//writes array[i][j] to file or creates blank file, adapted from linefit.cc
  ofstream outfile("calibration.cal");
  for(int i=0;i<24;i++){
    outfile<<i+1<<" ";
    for(int j=0;j<11;j++){
      switch(numbered){
      case 0:
	outfile<<array[i][j]<<" ";
	break;
      case 1:
	outfile<<0<<" ";
	break;
      case 2:
	outfile<<j<<" ";
	break;
      default:break;  
      }
    }
    outfile<<endl;
  }
}

void readfile(Char_t *filename="test.cal", Bool_t showtest=1, const int numdet=24, int start_no=1)
{
  Float_t param[numdet][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  printf("param array size is: [%d][%d].\n",(int)(sizeof(param)/sizeof(param[0])),size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<numdet;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
 
  FILE * infile;
  Int_t k=1;//array length
  while(!fit&&k<=(size)) {
    infile = fopen (filename,"r");
    for(Int_t i=0;i<numdet;i++) {
      for(Int_t j=0;j<k;j++) {
	fscanf(infile,"%f",&param[i][j]);
      }
    }
    fclose(infile);
    if(showtest) printf("Testing array length %d:\n",k);

    if (param[0][0]==start_no) {
      fit=kTRUE;
      for(Int_t i=0;i<numdet;i++) {
	fit=(fit&&(param[i][0]==(i+start_no)));	
	if(fit){
	  if(showtest) printf("  %2.0f ",param[i][0]);
	  for(Int_t j=1;j<k;j++){
	    if(showtest) printf("  %7.2f ",param[i][j]);
	  }
	  if(showtest) printf("\n");
	  if((i+1)>errorline)errorline=i+1;
	}
      }
    }
    else {
      fit=kFALSE;
      k=size;
      printf("  Each line of \"%s\" is expected to start with a detector number.\n",filename);
      printf("  File \"%s\" is expected to start with %d.\n",filename,start_no);
    }
    k++;
  }//end while
  if(fit) {
    printf("  File \"%s\" has %d elements per line.\n",filename,k-1);
    FILE * outfile;
    outfile=fopen("output.txt","w");
    printf("The contents of \"%s\" are:\n",filename);
    for(Int_t i=0;i<numdet;i++){
      fprintf(outfile,"%2.0f ",param[i][0]);
      printf("  %2.0f ",param[i][0]);
      for(Int_t j=1;j<k-1;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%7.2f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
    }
    fclose(outfile);
  }
  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n",filename,size,errorline);
}

void truncfile(Char_t *filename="source.cal",Char_t *output="shortsource.cal",Int_t trunc=6)
{
  Float_t param[24][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  printf("param array size is: [24][%d].\n",size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<24;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
 
  FILE * infile;
  Int_t k=2;
  while(!fit&&k<=(size)){
    infile = fopen (filename,"r");
    for(Int_t i=0;i<24;i++){
      for(Int_t j=0;j<k;j++){
	fscanf(infile,"%f",&param[i][j]);
      }
    }
    fclose(infile);
    printf("Testing array length %2d:\n",k);
    if (param[0][0]==1)fit=kTRUE;
    else fit=kFALSE;
    for(Int_t i=0;i<24;i++){
      fit=(fit&&(param[i][0]==(i+1)));	
      if(fit){
	printf("%2.0f ",param[i][0]);
	for(Int_t j=1;j<k;j++){
	  printf("%7.3f ",param[i][j]);
	}
	printf("\n");
	if((i+1)>errorline)errorline=i+1;
      }
    }
    if(fit)printf("File \"%s\" has %d elements per line.\n",filename,k);
    k++;
  }
  if(fit){   
    FILE * outfile;
    outfile=fopen(output,"w");
    printf("The new contents of \"%s\" are:\n",output);
    for(Int_t i=0;i<24;i++){
      fprintf(outfile,"%2.0f ",param[i][0]);
      printf("%2.0f ",param[i][0]);
      for(Int_t j=1;j<trunc;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%7.3f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
    }
    fclose(outfile);
  }
  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n
File \"%s\" not written.\n",filename,size,errorline,output);
}

void writetemp(Char_t *calfile="calibration.cal", Int_t detno=-1, Char_t *tempfile="temp.lst",Int_t index=0,Bool_t overwrite=1,Bool_t showtest=0,const int dets=24,Int_t start=0)
{
  if(detno==-1)
    detno=det;
  printf("Input detector number is %d\n",det);
  printf("Number of detectors = %d, ",dets);
  Float_t param[dets][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  if(showtest)  printf("param array size is: [%d][%d].\n",sizeof(param)/sizeof(param[0]),size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<dets;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
  
  FILE * infile;
  Int_t k=1;
  
  while(!fit&&k<=(size)){
    infile = fopen (calfile,"r");
    if(infile!=NULL){
      for(Int_t i=0;i<dets;i++){
	for(Int_t j=0;j<k;j++){
	  fscanf(infile,"%f",&param[i][j]);
	}
      }
      fclose(infile);
    }
    else{
      printf("File is NULL!\n");
      //for(Int_t l=0;l<size;l++){
      //	Fit[l]=0;
    }
      
    if(showtest) printf("Testing array length %d:\n",k);
      
    if(param[0][0]==start)
      fit=kTRUE;
    else
      fit=kFALSE;
      
    for(Int_t i=0;i<dets;i++){
      fit=(fit&&(param[i][0]==(i+start)));	
      if(fit){
	if(showtest) printf("%2.0f ",param[i][0]);
	for(Int_t j=1;j<k;j++){
	  if(showtest) printf("%7.2f ",param[i][j]);
	}
	if(showtest) printf("\n");
	if((i+1)>errorline)errorline=i+1;
      }
    }
           
    if(fit)printf("File \"%s\" has %d elements per line.\n",calfile,k);
     
    k++;
  }
    
  if(fit){   
    Float_t Fit[10];
    Int_t size=sizeof(Fit)/sizeof(Fit[0]); 
    infile = fopen (tempfile,"r");
    if(infile!=NULL){
      for(Int_t l=0;l<size;l++){
	if(fscanf(infile,"%f, ",&Fit[l])==-1)
	  Fit[l]=0;
	if(Fit[l])//added
	  param[detno-start][l+1+index]=Fit[l];//note use of index
      }
      fclose(infile);
    }
    else
      printf("File is NULL!\n");
     
    FILE * outfile;
    if(overwrite)
      outfile=fopen(calfile,"w");
    else{
      outfile=fopen("output.txt","w");
      printf("Output file is \"output.txt\"\n");
    }
    printf("The contents of \"%s\" are approximately:\n",calfile);
    for(Int_t i=0;i<dets;i++){
      fprintf(outfile,"%2.0f ",param[i][0]);
      printf("%2.0f ",param[i][0]);
      for(Int_t j=1;j<k-1;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%5.0f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
    }
    fclose(outfile);
  }
  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n",calfile,size,errorline);
  
}

void expandcal(Char_t *calfile="calibration.cal",Int_t index=0,Bool_t overwrite=1,Bool_t showtest=0,const int dets=4,Int_t start=0)
{
  Float_t param[dets][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  if(showtest)  printf("param array size is: [%d][%d].\n",sizeof(param)/sizeof(param[0]),size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<dets;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
  
  FILE * infile;
  Int_t k=1;
  
  while(!fit&&k<=(size)){
    infile = fopen (calfile,"r");
    if(infile!=NULL){
      for(Int_t i=0;i<dets;i++){
	for(Int_t j=0;j<k;j++){
	  fscanf(infile,"%f",&param[i][j]);
	}
      }
      fclose(infile);
    }
    else{
      printf("File is NULL!\n");
    }
      
    if(showtest) printf("Testing array length %d:\n",k);
      
    if(param[0][0]==start)
      fit=kTRUE;
    else
      fit=kFALSE;
    
    for(Int_t i=0;i<dets;i++){
      fit=(fit&&(param[i][0]==(i+start)));	
      if(fit){
	if(showtest) printf("%2.0f ",param[i][0]);
	for(Int_t j=1;j<k;j++){
	  if(showtest) printf("%7.2f ",param[i][j]);
	}
	if(showtest) printf("\n");
	if((i+1)>errorline)errorline=i+1;
      }
    }
           
    if(fit)printf("File \"%s\" has %d elements per line.\n",calfile,k);
     
    k++;
  }
    
  if(fit) {   
    FILE * outfile;
    if(overwrite)
      outfile=fopen(calfile,"w");
    else{
      outfile=fopen("output.txt","w");
      printf("Output file is \"output.txt\"\n");
    }
    printf("The contents of \"%s\" are approximately:\n",calfile);
    for(Int_t i=0;i<dets;i++) {
        for(Int_t l=0;l<3;l++) {
      
      fprintf(outfile,"%2.0f ",param[i][0]*3+l);
      printf("%2.0f ",param[i][0]*3+l);
      for(Int_t j=1;j<k-1;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%5.0f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
	}
    }
    fclose(outfile);
  }

  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n",calfile,size,errorline);
  
}

void rewritetemp(Char_t *calfile="calibration.cal", Int_t detno=-1, Char_t *tempfile="temp.lst",Int_t index=0,Bool_t overwrite=1,Bool_t showtest=0,const int dets=24,Int_t start=0)
{//re-writes calibration constants after testing fit.
  if(detno==-1)
    detno=det;
  printf("Input detector number is %d\n",det);
  printf("Number of detectors = %d, ",dets);
  Float_t param[dets][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  if(showtest)  printf("param array size is: [%d][%d].\n",sizeof(param)/sizeof(param[0]),size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<dets;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
  
  FILE * infile;
  Int_t k=1;
  
  while(!fit&&k<=(size)){
    infile = fopen (calfile,"r");
    if(infile!=NULL){
      for(Int_t i=0;i<dets;i++){
	for(Int_t j=0;j<k;j++){
	  fscanf(infile,"%f",&param[i][j]);
	}
      }
      fclose(infile);
    }
    else{
      printf("File is NULL!\n");
      //for(Int_t l=0;l<size;l++){
      //	Fit[l]=0;
    }
      
    if(showtest) printf("Testing array length %d:\n",k);
      
    if(param[0][0]==start)
      fit=kTRUE;
    else
      fit=kFALSE;
      
    for(Int_t i=0;i<dets;i++){
      fit=(fit&&(param[i][0]==(i+start)));	
      if(fit){
	if(showtest) printf("%2.0f ",param[i][0]);
	for(Int_t j=1;j<k;j++){
	  if(showtest) printf("%7.2f ",param[i][j]);
	}
	if(showtest) printf("\n");
	if((i+1)>errorline)errorline=i+1;
      }
    }
           
    if(fit)printf("File \"%s\" has %d elements per line.\n",calfile,k);
     
    k++;
  }
  double old[2]={param[detno-start][1+index],param[detno-start][1+1+index]}; 
  printf("The old calibration constants are %9.4f %9.4f\n",old[0],old[1]);
  double change[2]={0,0};
  
  if(fit) {//readin successful   
    Float_t Fit[10];
    Int_t size=sizeof(Fit)/sizeof(Fit[0]); 
    infile = fopen (tempfile,"r");
    if(infile!=NULL){
      printf("     The contents of %s are ",tempfile);
      for(Int_t l=0;l<size;l++) {
	if(fscanf(infile,"%f, ",&Fit[l])==-1)
	  Fit[l]=0;
	if(Fit[l]){//added
	  if(l<2) change[l]=Fit[l];
	  param[detno-start][l+1+index]=Fit[l];//note use of index
	  printf("%9.4f ",param[detno-start][l+1+index]);
	}
      }
      printf("\n");
      fclose(infile);
    }
    else
      printf("File is NULL!\n");

    double new[2]={change[0]*old[0],change[0]*old[1]+change[1]}; 
    printf("The new calibration constants are %9.4f %9.4f\n",new[0],new[1]);
    param[detno-start][0+1+index]=new[0];
    param[detno-start][1+1+index]=new[1];
 
    FILE * outfile;
    if(!overwrite)
      calfile="output.txt";
    outfile=fopen(calfile,"w");
    printf(" Output file is \"%s\"\n",calfile);
    printf("The contents of \"%s\" are approximately:\n",calfile);
    for(Int_t i=0;i<dets;i++){
      fprintf(outfile,"%2.0f ",param[i][0]);
      printf("%2.0f ",param[i][0]);
      for(Int_t j=1;j<k-1;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%5.0f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
    }
    fclose(outfile);
  }
  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n",calfile,size,errorline);
}

void rewritetempa(Char_t *calfile="calibration.cal", Int_t detno=-1, Char_t *tempfile="temp.lst",Int_t index=0,Bool_t overwrite=1,Bool_t showtest=0,const int dets=24,Int_t start=0)
{//re-writes calibration constants after testing fit.
  if(detno==-1)
    detno=det;
  printf("Input detector number is %d\n",det);
  printf("Number of detectors = %d, ",dets);
  Float_t param[dets][50]; 
  Int_t errorline=-1;
  Int_t size=sizeof(param[0])/sizeof(param[0][0]);
  if(showtest)  printf("param array size is: [%d][%d].\n",sizeof(param)/sizeof(param[0]),size); 
  Bool_t fit=kFALSE;
  for(Int_t i=0;i<dets;i++)
    for(Int_t j=0;j<size;j++)
      param[i][j]=0;//initializes all array elements to zero
  
  FILE * infile;
  Int_t k=1;
  
  while(!fit&&k<=(size)){
    infile = fopen (calfile,"r");
    if(infile!=NULL){
      for(Int_t i=0;i<dets;i++){
	for(Int_t j=0;j<k;j++){
	  fscanf(infile,"%f",&param[i][j]);
	}
      }
      fclose(infile);
    }
    else{
      printf("File is NULL!\n");
      //for(Int_t l=0;l<size;l++){
      //	Fit[l]=0;
    }
      
    if(showtest) printf("Testing array length %d:\n",k);
      
    if(param[0][0]==start)
      fit=kTRUE;
    else
      fit=kFALSE;
      
    for(Int_t i=0;i<dets;i++){
      fit=(fit&&(param[i][0]==(i+start)));	
      if(fit){
	if(showtest) printf("%2.0f ",param[i][0]);
	for(Int_t j=1;j<k;j++){
	  if(showtest) printf("%7.2f ",param[i][j]);
	}
	if(showtest) printf("\n");
	if((i+1)>errorline)errorline=i+1;
      }
    }
           
    if(fit)printf("File \"%s\" has %d elements per line.\n",calfile,k);
     
    k++;
  }
  double old[2]={param[detno-start][1+index],param[detno-start][1+1+index]}; 
  printf("The old calibration constants are %9.4f %9.4f\n",old[0],old[1]);
  double change[2]={0,0};
  
  if(fit) {//readin successful   
    Float_t Fit[10];
    Int_t size=sizeof(Fit)/sizeof(Fit[0]); 
    infile = fopen (tempfile,"r");
    if(infile!=NULL){
      printf("     The contents of %s are ",tempfile);
      for(Int_t l=0;l<size;l++) {
	if(fscanf(infile,"%f, ",&Fit[l])==-1)
	  Fit[l]=0;
	if(Fit[l]){//added
	  if(l<2) change[l]=Fit[l];
	  param[detno-start][l+1+index]=Fit[l];//note use of index
	  printf("%9.4f ",param[detno-start][l+1+index]);
	}
      }
      printf("\n");
      fclose(infile);
    }
    else
      printf("File is NULL!\n");

    double new[2]={old[0],old[1]+change[1]}; 
    printf("The new calibration constants are %9.4f %9.4f\n",new[0],new[1]);
    param[detno-start][0+1+index]=new[0];
    param[detno-start][1+1+index]=new[1];
 
    FILE * outfile;
    if(!overwrite)
      calfile="output.txt";
    outfile=fopen(calfile,"w");
    printf(" Output file is \"%s\"\n",calfile);
    printf("The contents of \"%s\" are approximately:\n",calfile);
    for(Int_t i=0;i<dets;i++){
      fprintf(outfile,"%2.0f ",param[i][0]);
      printf("%2.0f ",param[i][0]);
      for(Int_t j=1;j<k-1;j++){
	fprintf(outfile,"%g ",param[i][j]);
	printf("%5.5f ",param[i][j]);
      }
      fprintf(outfile,"\n");
      printf("\n");
    }
    fclose(outfile);
  }
  else
    printf("File \"%s\" has more than %d elements per line, or there is an error on line %d.\n",calfile,size,errorline);
}

//-------------------------------------------------------------------------------------
// 5d). Fill (dump) a histogram from (to) a file---------------------------------------
void dump(Char_t *histname, Char_t *filename, Int_t ierr=0, Int_t zsupp=1)
{//copied form util.cc
  //   gDirectory->pwd();
  Int_t whatsit=0;
  whatsit=whatis(histname);
   
  switch(whatsit) {
  case 1:
    TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
    break;
  case 2:
    TH1D *hist1=(TH1D *) gROOT->FindObject(histname);
    break;
  case 3:
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    break;
  case 4:
    TH2D *hist2=(TH2D *) gROOT->FindObject(histname);
    break;
  case 5:
    TH3F *hist3=(TH3F *) gROOT->FindObject(histname);
    break;
  case 6:
    TProfile *hist1=(TProfile *) gROOT->FindObject(histname);
    break;
  }
   
  ofstream outfile(filename);
   
  if (whatsit==1 || whatsit==2 || whatsit==6) {//1D object
      
    for (Int_t ibin=0; ibin < hist1->GetNbinsX()+1;ibin++) {//includes underflow bin
      if (ierr==0) {
	if ((zsupp==1 && hist1->GetBinContent(ibin)!=0) ||
	    zsupp==0) {
	  outfile<< hist1->GetBinCenter(ibin) << " " <<
	    hist1->GetBinContent(ibin)<<endl;
	}
      } else {
	if ((zsupp==1 && hist1->GetBinContent(ibin)!=0) ||
	    zsupp==0) {
	  outfile<<hist1->GetBinCenter(ibin)<<" "<<
	    hist1->GetBinContent(ibin)<<" "<<
	    hist1->GetBinError(ibin)<<endl;
	}
      }
    }
  }
   
  if (whatsit==3 ||  whatsit==4) {//2D object
    for (Int_t ixbin=0; ixbin < hist2->GetNbinsX();ixbin++) {
      for (Int_t iybin=0; iybin<hist2->GetNbinsY();iybin++) {
	    
	Float_t content=hist2->GetBinContent(ixbin,iybin);
	Float_t xbin=hist2->GetXaxis()->GetBinCenter(ixbin);
	Float_t ybin=hist2->GetYaxis()->GetBinCenter(iybin);
	if (ierr==0) {
	  if ((zsupp==1 && content!=0) ||
	      zsupp==0) {
	    outfile<< xbin << " " << ybin <<" "<<content<<endl;
	  }
	} else {
	  if ((zsupp==1 && content!=0) ||
	      zsupp==0) {
	    outfile<<xbin<<" "<< ybin<<" "<<content<<endl;
	  }
	}
      }
    }
  }
  outfile.close();
}

//---------------------------------------------------------------------------
// 5di). Fill a 1D histogram from a file-------------------------------------
void fillhist0(Char_t *filename, Char_t *histname)
{//copied form util.cc
  Float_t x;
  TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
  hist1->Reset();
  ifstream infile(filename);
  while (infile >> x) {
    hist1->Fill(x);
  }
  hist1->Draw();
}

void fillhist(Char_t *filename, Char_t *histname, Int_t opt1=0, Int_t ierr=0)
{//copied form util.cc
  // if opt1=0 use x as channel to fill, else use bin number
  // if ierr=1 also read in errors and set bin errors

  Float_t x,y,dy,xval;
  Int_t xbin=1;
  TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
  hist1->Reset();
  ifstream infile(filename);
  switch (ierr) {
  case 0:
    while (infile >> x) {
      infile >> y;
      xval=x;
      if (opt1==1) {xval=(Float_t) xbin;}
      hist1->Fill(xval,y);
      xbin++;
    }
    hist1->Draw();
    break;
  case 1:
    while (infile >> x) {
      infile>>y;
      infile>>dy;
      xval=x;
      if (opt1==1) {xval=(Float_t) xbin;}
      hist1->Fill(xval,y);
      hist1->SetBinError(xbin,dy);
      xbin++;
    }
    hist1->Draw("E");
    break;
  }
}

void fill1(Char_t *filename,Char_t *histname,Int_t reset=1)
{//extension to fillhist0() from util.cc, includes weight and reset option.  
 //Fills a 1-dimensional histogram from a text file.
 //File is to be formatted as x-value, weight.
  Float_t x,y,w;
  TH1F *hist1=(TH1F *) gROOT->FindObject(histname);
  if (reset){
    printf("Resetting histogram \"%s\"\n",histname);
    hist1->Reset();
  }
  ifstream infile(filename);
  while (infile >> x) {
    //infile >> y;
    infile >> w;
    hist1->Fill(x,w);
  }
  hist1->Draw();
}

void fillgraph(Char_t *filename, Char_t *graphname, Int_t npts, Int_t ierr=0)
{//copied form util.cc
  gr=new TGraphErrors(npts);
  Double_t x,y,dx,dy;
  ifstream infile(filename);
  Int_t i=0;
  while (infile >> x) {
    infile >> y;
    infile >> dy;
    gr->SetPoint(i,x,y);
    gr->SetPointError(i,dx,dy);
    i++;
  }
  gr->SetName(graphname);
  gr->SetTitle("graph test");
  gr->SetMarkerColor(4);
  gr->SetMarkerStyle(21);
  gr->Draw("AP");
}

//---------------------------------------------------------------------------
// 5dii). Fill a 2D histogram from a file------------------------------------
void fillhist2(Char_t *filename,Char_t *histname)
{//copied form util.cc
  Float_t x,y,z;
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  hist2->Reset();
  ifstream infile(filename);
  while (infile >> x) {
    infile >> y;
    infile >> z;
    hist2->Fill(x,y,z);
  }
  hist2->Draw("col2");
}

void fill2(Char_t *filename,Char_t *histname,Int_t reset=1)
{//Extension to fillhist2(), includes reset option.
 //Fills a 2-dimensional histogram from a text file.
 //File is to be formatted as x-value, y-value, weight.
  Float_t x,y,w;
  TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
  if (reset){
    printf("Resetting histogram \"%s\"\n",histname);
    hist2->Reset();
  }
  ifstream infile(filename);
  while (infile >> x) {
    infile >> y;
    infile >> w;
    hist2->Fill(x,y,w);
  }
  hist2->Draw("col2");
}

//-------------------------------------------------------------------------------------
// 5e). Cuts and Cut File Utilities----------------------------------------------------
void cutg(Char_t *histname,Char_t * newcutname, Int_t graphit=1, Int_t npts=0, Char_t *xvar="", Char_t *yvar="")
{//copied form util.cc
  if (graphit==1) {
      
    if (!(TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1"))
      TCanvas *c1=new TCanvas("c1","c1");
    c1->cd();
    if ((TCutG *) gROOT->GetListOfSpecials()->FindObject("CUTG"))
      CUTG->Delete();
    if ((TCutG *) gROOT->GetListOfSpecials()->FindObject(newcutname)) {
      (TCutG *) gROOT->GetListOfSpecials()->FindObject(newcutname)->Delete();
    }
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    hist2->Draw("col2");
    c1->EditorBar();
    cout << "Click on \"Graphical Cut\" to create cut"<<endl;
    cout << "When finished don't forget to setname(\"CUTG\",\""<<newcutname<<"\")"<<endl;
  }
  else {
    if (npts==0) {
      cout << "enter number of points in graphical cut: ";
      cin >> npts;
    }
    TCutG *newcut=new TCutG(newcutname,npts);
    Double_t x,y;
    for (Int_t i=0; i<npts-1; i++) {
      cout <<"enter x, y for point "<<i<<", one per line:"<<endl;
      cin>>x>>y;
      newcut->SetPoint(i,x,y);
    }
    newcut->GetPoint(0,x,y);
    newcut->SetPoint(npts-1,x,y);
    newcut->Draw();
    newcut->SetVarX(xvar);
    newcut->SetVarY(yvar);
    //      newcut->Print();
    newcut->SetName(newcutname);      
  }
}   

void drawcut(Char_t *cutname) 
{ if( c1 == 0 ) {
    cout<<"please display the histogram first.\n";
    return;
  }
  char path[124]=gROOT->GetPath();
  readcuts("cut_file.root");
  TCutG *cut1 = (TCutG*) gROOT->FindObject(cutname);
  if( cut1 == 0 ) { 
    cout<<"Sorry, "<<cutname<<" does not exist.\n";
  } else {
    c1->cd();
    cut1->SetLineColor(2);
    cut1->SetLineWidth(2);
    cut1->Draw();
  }
  gROOT->cd(path);
}

void readcuts(Char_t *filename)
{//copied form util.cc
  char path[124]=gROOT->GetPath();//added
  cout<<path<<endl;
  TFile *fin=new TFile(filename);
  TList *speclist=(TList*) fin->GetListOfSpecials();
  speclist->Read();
  //the following 3 lines are copied from a similar block of code...
  fin->ReadAll();
  fin->Purge();
  fin->ls("-d");
  fin->Close();
  gROOT->cd(path);
}

void deletecuts(Char_t *filename, Char_t *cut_name) 
{
  char path[124]=gROOT->GetPath();
  cout<<path<<endl;
  TFile *fin=new TFile(filename,"update");
  fin->ReadAll();
  TString cutname=TString(cut_name)+";*";
  cout<<"Do you want to delete "<<cutname<<" ? (y/n)"<<endl;
  char opt;
  cin>>opt;
  if( opt == 'y' ) {
    fin->Delete( cutname.Data() );
    cout<<cutname<<" is deleted."<<endl;
  }
  fin->Close();
  gROOT->cd(path);
}

void savecuts(Char_t *filename)
{//copied form util.cc
  TList *speclist=gROOT->GetListOfSpecials();
  TFile *fout=new TFile(filename,"recreate");
  fout->cd();
  speclist->Write();
  fout->Purge();//added
  fout->ls();
  fout->Close();
  cout << "Special objects written to file "<<filename<<endl;
}

void listcuts()
{    
  TList *speclist=gROOT->GetListOfSpecials();
  cout<<"To be implemented."<<endl;
}

void setcutg(Char_t *cutname="CUTG", Char_t *xvar="",Char_t *yvar="")
{//copied form util.cc
  TCutG *gcut=(TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname);
  gcut->SetVarX(xvar);
  gcut->SetVarY(yvar);
}

void intcut(Char_t *filename="b13_cuts.root", Char_t *histname="hEDE0", 
	    Char_t *cutname="cEDE0_B13_big")
{
  //TCutG *tempcut;  
  gROOT->ProcessLine("TDirectory *_home=gDirectory"); 
  if(gROOT->FindObject("_file0"))
    _filename0=_file0;
  dr(histname);
  getfile(filename);
  cWindow=(TCutG *) gROOT->FindObject(cutname);
  cWindow->Draw();
  if(gROOT->FindObject("_file0"))
    _file0->cd();
  pjxwin(histname,cutname);
  //hProj=(TH1F *) gROOT->FindObject("xwproj");
  xwproj->Integral();
  printf("The integral of %s in %s is %d\n",cutname,histname,xwproj->Integral());
  _home->cd();
  //  hInput=(TH2F *) gROOT->FindObject(hname.Data());
  dr(histname);
  cWindow->Draw();
}

// 5e-). Project Cuts--------------------------------------------------------
void pjxwin(Char_t *histname, Char_t *cutname)
{//copied form util.cc
  TCutG *tempcut;
  Int_t whatsit=whatis(histname,0);
  if (whatsit != 3 && whatsit !=4) {
    cout << "histogram is not a 2D or is not found!"<<endl;
    return;
  }
  switch(whatsit) {
  case 3:
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    break ;
  case 4:
    TH2D *hist2=(TH2D *) gROOT->FindObject(histname);
    break;
  }
   
  if (!(TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname)) {
    cout << "Graphical cut "<<cutname<<" is not found!"<<endl;
    return;
  } else {
    tempcut = (TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname);
  }
   
  if ((TH1D *) gROOT->FindObject("xwproj")) xwproj->Delete();
  TH1D * xwproj=new TH1D("xwproj","X projection",hist2->GetNbinsX(),
			 hist2->GetXaxis()->GetXmin(),
			 hist2->GetXaxis()->GetXmax());
   
  for (Int_t xbin=0; xbin < hist2->GetNbinsX();xbin++) {
    for (Int_t ybin=0; ybin < hist2->GetNbinsY(); ybin++) {
      if (tempcut->IsInside(hist2->GetXaxis()->GetBinCenter(xbin),
			    hist2->GetYaxis()->GetBinCenter(ybin))) {
	xwproj->Fill(hist2->GetXaxis()->GetBinCenter(xbin),
		     hist2->GetBinContent(xbin,ybin));
      }
    }
  }
  xwproj->Draw();
}	 

void pjywin(Char_t *histname, Char_t *cutname)
{//copied form util.cc
  TCutG *tempcut;
  Int_t whatsit=whatis(histname,0);
  if (whatsit != 3 && whatsit !=4) {
    cout << "histogram is not a 2D or is not found!"<<endl;
    return;
  }
  switch(whatsit) {
  case 3:
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    break ;
  case 4:
    TH2D *hist2=(TH2D *) gROOT->FindObject(histname);
    break;
  }
   
  if (!(TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname)) {
    cout << "Graphical cut "<<cutname<<" is not found!"<<endl;
    return;
  } else {
    tempcut = (TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname);
  }
   
  if ((TH1D *) gROOT->FindObject("ywproj")) ywproj->Delete();
  TH1D * ywproj=new TH1D("ywproj","Y projection",hist2->GetNbinsY(),
			 hist2->GetYaxis()->GetXmin(),
			 hist2->GetYaxis()->GetXmax());
   
  for (Int_t xbin=0; xbin < hist2->GetNbinsX();xbin++) {
    for (Int_t ybin=0; ybin < hist2->GetNbinsY(); ybin++) {
      if (tempcut->IsInside(hist2->GetXaxis()->GetBinCenter(xbin),
			    hist2->GetYaxis()->GetBinCenter(ybin))) {
	ywproj->Fill(hist2->GetYaxis()->GetBinCenter(ybin),
		     hist2->GetBinContent(xbin,ybin));
      }
    }
  }
  ywproj->Draw();
}	 

void pj2win(Char_t *histname, Char_t *cutname)
{//copied form util.cc
  TCutG *tempcut;
  Int_t whatsit=whatis(histname,0);
  if (whatsit != 3 && whatsit !=4) {
    cout << "histogram is not a 2D or is not found!"<<endl;
    return;
  }
  switch(whatsit) {
  case 3:
    TH2F *hist2=(TH2F *) gROOT->FindObject(histname);
    break ;
  case 4:
    TH2D *hist2=(TH2D *) gROOT->FindObject(histname);
    break;
  }
   
  if (!(TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname)) {
    cout << "Graphical cut "<<cutname<<" is not found!"<<endl;
    return;
  } else {
    tempcut = (TCutG *) gROOT->GetListOfSpecials()->FindObject(cutname);
  }
   
  if ((TH2F *) gROOT->FindObject("xywproj")) xywproj->Delete();
  TH2F * xywproj=new TH2F("xywproj","XY copy",hist2->GetNbinsX(),
			  hist2->GetXaxis()->GetXmin(),
			  hist2->GetXaxis()->GetXmax(),
			  hist2->GetNbinsY(),
			  hist2->GetYaxis()->GetXmin(),
			  hist2->GetYaxis()->GetXmax());
   
  for (Int_t xbin=0; xbin < hist2->GetNbinsX();xbin++) {
    for (Int_t ybin=0; ybin < hist2->GetNbinsY(); ybin++) {
      if (tempcut->IsInside(hist2->GetXaxis()->GetBinCenter(xbin),
			    hist2->GetYaxis()->GetBinCenter(ybin))) {
	xywproj->Fill(hist2->GetXaxis()->GetBinCenter(xbin),
		      hist2->GetYaxis()->GetBinCenter(ybin),
		      hist2->GetBinContent(xbin,ybin));
      }
    }
  }
  xywproj->Draw("col2");
}

void GraphErrors()
{
	TCanvas* c=new TCanvas();
	TGraphErrors Graph("./Q4Probe.dat","%lg %lg %lg");
	Graph.SetTitle("Q4 Probe Calibration;
			NMR Probe Field (mT)
			Hall Probe Field (mT)");
	Graph.SetMarkerStyle(kCircle);
	Graph.Draw();
}
