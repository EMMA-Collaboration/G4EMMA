//Calculates fringe field integrals of ED and MD
//compile: g++ EFLcalculator.cc -o EFLcalculator
//run: ./EFLcalculator filename

#include "globals.hh"

int main(int argc, char *argv[]){
  char line[100];
  string str;
  string first;
  char z[20],f[20];
  bool entrance,exit,doarray,calcdone;
  double z1,f1,z2,f2;
  double zstart,zstop,fstart,fstop,zeff[3];
  double I0,I1,I2,I4;
  double zb,G0;
  int header;
  float dummy;
  double fieldmax; //max field strength

  if(argc!=2){
    cerr<<argv[0]<<"Usage: <input file>"<<endl;
    std::exit(1);
  }
  
  cout<<setprecision(10);

//-----------------------------------------------------------------------------------------------//
//------------------------------ for ED and MDs -------------------------------------------------//
//-----------------------------------------------------------------------------------------------//

  FILE *file;
  string fname = argv[1]; //get file name from command line
  file = fopen(fname.c_str(),"r"); //open data file
  
  //file = fopen("effFieldOpticalAxis_n6.dat","r"); //open data file
  if (file == NULL) perror ("Error opening file");

  FILE *outfile;
  outfile = fopen("FFIntegrals.dat","w"); //open output fils
    
  //initialise parameters
  entrance=true;
  exit=doarray=calcdone=false;
  header=0;
  z1=f1=99999999999; //arbitrary number
  zb=0;
  G0=1;
  I0=0;
  
  while(!feof(file) && calcdone==false){
    fgets(line,100,file);
    
    if(first=="ED1" || first=="MD" || first=="ED2"){
    //if(first=="ED2"){
      if(header<1){ //read in header parameters
        sscanf(line,"%s %s",z,f); //read formatted data from string
        fieldmax=atof(f); //max field strength
        //cout<<fieldmax<<endl;
        header++;
        continue;
      }
      if(entrance==true){ //entrance fringe field integral calculations
        sscanf(line,"%s %s",z,f); //read formatted data from string
        z2=atof(z); //convert string to double
        f2=atof(f);
        
        //calculate integral numerically
        if(z1!=f1){
          I0+=fabs(z2-z1)*(f1+f2)/(2*fieldmax);
        }
        if(f2/fieldmax>0.99995){
          fstart=f2; //field strength at boundary of constant field
          zstart=z2; //distance where constant field starts
//---------------------------------Change these conditions----------------------------------------//
          zb=0; //path length which has constant field
          if(first=="ED1" || first=="ED2")G0=6.25; //aperture radius (cm)
          else if(first=="MD")G0=6;
//---------------------------------Change these conditions----------------------------------------//
          
          //effective field boundary calculations
          zeff[0]=zstart-I0;
          
          //change condition such that it calculates the integrals for the exit fringe field next
          entrance=false;
          exit=true;
          I0=0;
          continue;
        }
        z1=z2;
        f1=f2;
      }
      if(exit==true){ //exit fringe field integral calculations
        sscanf(line,"%s %s",z,f); //read formatted data from string
        z2=atof(z); //convert string to double
        f2=atof(f);
        
        //if field values start decreasing put values into array
        if(doarray==false && f2/fieldmax<0.99995){
          fstop=f1; //field strength at boundary of constant field
          zstop=z1; //distance where constant field stops
          doarray=true;
        }
        if(doarray==true && !isalpha(line[0])){ //put values into array
          I0+=fabs(z2-z1)*(f1+f2)/(2*fieldmax); //calculate integral numerically
        }
        z1=z2;
        f1=f2;
        //if you reached the last value for the element do the integration
        if(isalpha(line[0]) || feof(file)){
//---------------------------------Change these conditions----------------------------------------//
          zb=0; //path length which has constant field
          if(first=="ED1" || first=="ED2")G0=6.25; //aperture radius (cm)
          else if(first=="MD")G0=6;
//---------------------------------Change these conditions----------------------------------------//

          //effective field boundary calculations
          zeff[1]=zstop+I0;
          zeff[2]=zeff[1]-zeff[0];

          //print to screen
          cout<<"\n"<<first<<endl; 
          cout<<"Const field = "<<fstart<<" starts at z = "<<zstart<<" cm"<<endl;
          cout<<"Const field = "<<fstop<<" stops at z = "<<zstop<<" cm"<<endl;
          cout<<"Eff field starts at z = "<<zeff[0]<<" cm"<<endl;
          cout<<"Eff field stops at z = "<<zeff[1]<<" cm"<<endl;
          cout<<"Effective field length: "<<zeff[2]<<" cm"<<endl;
          //print to file
          stringstream ssout,ssout2;
          ssout2<<first<<" effective field length: "<<zeff[2]<<" cm\n\n";
          fputs(ssout2.str().c_str(),outfile);

          if(feof(file)) calcdone=true;
        }
      }
    }
    if(isalpha(line[0])){
      stringstream ss; 
      ss << line; //get first word from line
      ss >> first; //put first word into variable
      //reset values for next element
      I0=0;
      header=0;
      entrance=true;
      exit=doarray=false;
      z1=f1=99999999999; //arbitrary number
      continue;
    }

  }
  fclose(file);

//-----------------------------------------------------------------------------------------------//
//------------------------------ for QUADs ------------------------------------------------------//
//-----------------------------------------------------------------------------------------------//

  file = fopen(fname.c_str(),"r"); //open data file
  if (file == NULL) perror ("Error opening file");

  //declare new variables
  double zmid,r;
  double zpath3[ARRAYDIM],field3[ARRAYDIM];
  int imid,nn,nmax;

  //initialise parameters
  entrance=true;
  exit=doarray=calcdone=false;
  header=0;
  z1=f1=99999999999; //arbitrary number
  zb=0;
  G0=1;
  I0=0;
  
  while(!feof(file) && calcdone==false){
    fgets(line,100,file);
    
    if(first=="Q1" || first=="Q2" || first=="Q3" || first=="Q4"){
    //if(first=="dummy"){
      if(header<2){ //read in header parameters
        sscanf(line,"%s %s",z,f); //read formatted data from string
        if(header==0)r=atof(f); //radial distance from z=0 axis at which field was measured
        if(header==1)fieldmax=atof(f); //max field strength
        if(header==1)fieldmax/=r;
        header++;
        continue;
      }
      if(entrance==true){ //entrance fringe field integral calculations
        sscanf(line,"%s %s",z,f); //read formatted data from string
        z2=atof(z); //convert string to double
        f2=atof(f);
        f2/=r;
        
        //calculate integral numerically
        if(z1!=f1){
          I0+=fabs(z2-z1)*(f1+f2)/(2*fieldmax);
        }
        if(f2/fieldmax>0.99995){
          fstart=f2; //field strength at boundary of constant field
          zstart=z2; //distance where constant field starts
//---------------------------------Change these conditions----------------------------------------//
          zb=0; //path length which has constant field
          if(first=="Q1")G0=1; //aperture radius (cm)
//---------------------------------Change these conditions----------------------------------------//
          
          //effective field boundary calculations
          zeff[0]=zstart-I0;
          
          //change condition such that it calculates the integrals for the exit fringe field next
          entrance=false;
          exit=true;
          I0=0;
          continue;
        }
        z1=z2;
        f1=f2;
      }
      if(exit==true){ //exit fringe field integral calculations
        sscanf(line,"%s %s",z,f); //read formatted data from string
        z2=atof(z); //convert string to double
        f2=atof(f);
        f2/=r;
        
        //if field values start decreasing put values into array
        if(doarray==false && f2/fieldmax<0.99995){
          fstop=f1; //field strength at boundary of constant field
          zstop=z1; //distance where constant field stops
          doarray=true;
        }
        if(doarray==true && !isalpha(line[0])){ //put values into array
          I0+=fabs(z2-z1)*(f1+f2)/(2*fieldmax); //calculate integral numerically
        }
        z1=z2;
        f1=f2;
        //if you reached the last value for the element do the integration
        if(isalpha(line[0]) || feof(file)){
//---------------------------------Change these conditions----------------------------------------//
          zb=0; //path length which has constant field
          if(first=="Q1")G0=1; //aperture radius (cm)
//---------------------------------Change these conditions----------------------------------------//

          //effective field boundary calculations
          zeff[1]=zstop+I0;
          zeff[2]=zeff[1]-zeff[0];

          //print to screen
          cout<<"\n"<<first<<endl; 
          cout<<"Const field gradient = "<<fstart<<" starts at z = "<<zstart<<" cm"<<endl;
          cout<<"Const field gradient = "<<fstop<<" stops at z = "<<zstop<<" cm"<<endl;
          cout<<"Eff field starts at z = "<<zeff[0]<<" cm"<<endl;
          cout<<"Eff field stops at z = "<<zeff[1]<<" cm"<<endl;
          cout<<"Effective field length: "<<zeff[2]<<" cm"<<endl;
          //print to file
          stringstream ssout,ssout2;
          ssout2<<first<<" effective field length: "<<zeff[2]<<" cm\n\n";
          fputs(ssout2.str().c_str(),outfile);
          
          if(feof(file)) calcdone=true;
        }
      }
    }
    if(isalpha(line[0])){
      stringstream ss; 
      ss << line; //get first word from line
      ss >> first; //put first word into variable
      //reset values for next element
      I0=0;
      header=0;
      entrance=true;
      exit=doarray=false;
      z1=f1=99999999999; //arbitrary number
      continue;
    }
  }
  fclose(file);


  
  return 0;
}
