{
	TTree *MyTree = new TTree("MyTree", "MyTree");
	MyTree->ReadFile("output2.txt", "Energy_1:Energy_2:Angle_1:Angle_2");
	TH1D *h1 = new TH1D("h1","21Na Experiment",100,0,100);
	TH1D *h2 = new TH1D("h2","21Na Experiment",100,0,100);
	MyTree->Draw("Energy_1>>h1","Angle_1<3.5");
	MyTree->Draw("Energy_2>>h2","Angle_2<3.5","same");
	h1->SetLineColor(2);
	h2->SetLineColor(4);
	
	cout << "(d,p) mean: " << h1->GetMean() << endl;
	cout << "(d,n) mean: " << h2->GetMean() << endl;

	TLegend *leg1 = new TLegend(0.6,0.7,0.9,0.9);
  	leg1->SetHeader("Legend"); // option "C" allows to center the header
  	leg1->AddEntry(h1,"^{21}Na(d,p)^{22}Na Recoils");
  	leg1->AddEntry(h2,"^{21}Na(d,n)^{22}Mg Recoils");
  	leg1->Draw();
	
	TCanvas *c2 = new TCanvas("c2");

	TH2D *h3 = new TH2D("h3","21Na Experiment (5 #mum Au Degrader)",100,0,100,200,0,20);
	MyTree->Draw("Angle_1:Energy_1>>h3");
	h3->SetMarkerStyle(20);
	h3->SetMarkerColor(2);
	//h3->GetXaxis()->SetAxisTitle("Post-Degrader Recoil Energy (MeV)");
	//h3->GetYaxis()->SetAxisTitle("Post-Degrader Recoil Angle (deg)");
	
	TH2D *h4 = new TH2D("h4","21Na Experiment",100,0,100,200,0,20);
	MyTree->Draw("Angle_2:Energy_2>>h4","","same");
	h4->SetMarkerStyle(21);
	h4->SetMarkerColor(4);

	
	TLine *l1 = new TLine(0.,3.5,100.,3.5);
	l1->SetLineColor(1);
	l1->SetLineStyle(2);
	l1->SetLineWidth(2);
	l1->Draw();


	const int n=4;

	Double_t x[n] = {65.7,65.7,80.3,80.3};
	Double_t y[n] = {0.,20.,20.,0.};

	TGraph *g1 = new TGraph(n,x,y);

	g1->SetFillColor(3);
	g1->SetFillStyle(3004);
   	g1->Draw("f"); 

	TLegend *leg2 = new TLegend(0.6,0.7,0.9,0.9);
  	leg2->SetHeader("Legend"); // option "C" allows to center the header
  	leg2->AddEntry(h3,"^{21}Na(d,p)^{22}Na Recoils");
  	leg2->AddEntry(h4,"^{21}Na(d,n)^{22}Mg Recoils");
	leg2->AddEntry(l1,"Angular Acceptance Limit","l");
	leg2->AddEntry(g1,"#pm10% Central Energy","f");
  	leg2->Draw();

	
	
}
