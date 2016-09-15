{
  //gROOT->LoadMacro("C:/root/macros/fit.cc");
  //gROOT->LoadMacro("C:/root/macros/load_and_plot.cc");
  //gROOT->LoadMacro("C:/root/macros/PlotSim.cxx");
  
  //Load general macros and settings
  gROOT->LoadMacro("/home/matthew/Documents/EMMA/QuadAnalysis/fit.cc");
  setdisplay(); //calls function in fit.cc
  settime(true);
  sethome();
  
  //Load specfic macros and settings
  //gROOT->LoadMacro("/home/emma/offline/lighthall/rootana/analysis/scripts/load_and_plot.cc");
  gStyle->SetStatH(0.05);//reduces the size of the stat box

  //Load PGAC simulation
  //gROOT->LoadMacro("/home/emma/offline/lighthall/rootana/analysis/scripts/generator.cc");
  //setangles(0,4.5);
}
