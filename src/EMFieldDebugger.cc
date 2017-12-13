// Use this routine to calculate E and B fields at certain locations
// It MUST be used after SpectrometerConstruction.cc is initialised since it assigns lengths and sizes
// of the elements needed to calculate the field stengths in the BGField classes
//
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......
#include "EMFieldDebugger.hh"
#include "EMMAGlobalField.hh"
#include "G4UnitsTable.hh"
#include <iomanip> 
#include <fstream> //Stream class to both read and write from/to files
using namespace std;
#include <stdlib.h>     /* abort, NULL */
#include "G4SystemOfUnits.hh"
using CLHEP::pi;



//....oooOO0OOooo........oooOO0OOooo........oooOO0OOooo........oooOO0OOooo......

EMFieldDebugger::EMFieldDebugger(int icomp)
{
  G4cout<<std::setprecision(14);

  //electric and magnetic fields
/*  Field1 = new BGField1(0,zQ1fieldbegins,Q1before,Q1after,Q1Logical,G4ThreeVector(0*cm,0*cm,Q1Solidz));
  Field2 = new BGField2(0,zQ2fieldbegins,Q2before,Q2after,Q2Logical,G4ThreeVector(0*cm,0*cm,Q2Solidz));
  Field3 = new BGField3(0,zED1fieldbegins,ED1before,ED1after,ED1Logical,G4ThreeVector(0*cm,0*cm,Pipe5z));
  Field4 = new BGField4(xMDfieldbegins,zMDfieldbegins,MDbefore,MDafter,MDLogical,posPipe1MD);
  Field5 = new BGField5(xED2fieldbegins,zED2fieldbegins,ED2before,ED2after,ED2Logical,G4ThreeVector(0*cm,0*cm,Pipe10z));
  Field6 = new BGField6(0,zQ3fieldbegins,Q3before,Q3after,Q3Logical,G4ThreeVector(0*m,0*cm,Pipe11z));
  Field7 = new BGField7(0,zQ4fieldbegins,Q4before,Q4after,Q4Logical,G4ThreeVector(0*cm,0*cm,Q4z));
  Field1->ScaleFieldStrength( magneticScaling );
  Field2->ScaleFieldStrength( magneticScaling );
  Field3->ScaleFieldStrength( electricScaling );
  Field4->ScaleFieldStrength( magneticScaling );
  Field5->ScaleFieldStrength( electricScaling );
  Field6->ScaleFieldStrength( magneticScaling );
  Field7->ScaleFieldStrength( magneticScaling );
*/
  //Print field strengths
/*  G4cout << G4endl;
  G4cout << "Field strengths: " << G4endl;
  G4cout << "Q1: " << Field1->GetFieldStrength() << " Tm" << G4endl;
  G4cout << "Q2: " << Field2->GetFieldStrength() << " Tm" << G4endl;
  G4cout << "ED1: " << Field3->GetFieldStrength() << "kV/cm" << G4endl;
  G4cout << "MD: " << Field4->GetFieldStrength() << " T" << G4endl;
  G4cout << "ED2: " << Field5->GetFieldStrength() << "kV/cm" << G4endl;
  G4cout << "Q3: " << Field6->GetFieldStrength() << " Tm" << G4endl;
  G4cout << "Q4: " << Field7->GetFieldStrength() << " Tm" << G4endl;
  G4cout << G4endl;
*/
  
  G4int n,nn;
  G4double angle = 20*pi/180; //angle of EDs and half angle of MD
  worldpos[0]=worldpos2[0]=0;
  worldpos[1]=worldpos2[1]=0;
  worldpos[2]=worldpos2[2]=0;
  G4double pathlength,pathlength2;
  G4double theta;
  G4double delz,deltube;
  G4double r;
  G4double zmiddle,pathmiddle,pathmax;
  G4double fieldend,fieldmax;
  G4double length;

  GlobalField = EMMAGlobalField::GetObject();
  
  //change the step length with nn. To calculate the effective field length use nn=10000.
  //To look at the field shapes nn=3 is sufficient.
  nn=10000;
  G4bool transpose = FALSE; //The fields for the quads are cutoff at the boundary of the volume. The
                            //interference of the B fields of the quads has not been implemented in
                            //gemma1.7. Therefore if you want to calculate the effective field length
                            //set transpose=TRUE. This transposes the field from the full tail onto
                            //the missing end.
  
  std::ofstream fieldFile(fieldFileName, std::ios::app); //Declared in EMMAPrimaryGeneratorAction.cc
//----------------------------------------------------------------------------------------------//
//The format of the file outputs is used in EFLcalculator.cc in the /UserDir/Results/fringefields
//folder. If you change the output format then program won't read in the file properly or calculate
//the effective field correctly.
//----------------------------------------------------------------------------------------------//
  
  if(icomp==9){
    G4cout<<"Custom Field Debug \n";
    fieldFile<<"Custom fields along optical axis: zoptical=(cm) xfield=(T) yfield=(T) zfield=(T) \n";
    //central field
    r=4*cm; //radial distance from center
    worldpos[0]=r;
    n=nn*10; //n changes such that the z step lengths are roughly equal through each element
             //n changes for each element since they don't have equal lengths

    for(int i=0;i<n;i++){
      worldpos[2]=zED2fieldends + i*(zQ4fieldends - zED2fieldends)/n; //world position through vacuum volume containing field	
      GlobalField->GetFieldValue(worldpos,field); //calculate field at position <-----------
      pathlength=worldpos[2]/cm; //pathlength along optical axis within element
      fieldFile<<pathlength<<"\t"<<10*field[1]/(volt*s/m2)<<"\n"; //write to file
    }
     
  }


  if(icomp==7){
    G4cout<<"Custom Field Debug \n";
    fieldFile<<"Custom fields along optical axis: zoptical=(cm) xfield=(T) yfield=(T) zfield=(T) \n";
    //central field
    r=2*cm; //radial distance from center
    worldpos[0]=r;
    n=nn*10; //n changes such that the z step lengths are roughly equal through each element
             //n changes for each element since they don't have equal lengths

    for(int i=0;i<n;i++){
      worldpos[2]=i*(zED1fieldbegins)/n; //world position through vacuum volume containing field	
      GlobalField->GetFieldValue(worldpos,field); //calculate field at position <-----------
      pathlength=worldpos[2]/cm; //pathlength along optical axis within element
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n"; //write to file
    }
     
  }
     
  if(icomp==0){
    G4cout<<"Calculated Q1 fields along optical axis: \n";
    fieldFile<<" Calculated Q1 fields along optical axis: zoptical=(cm) yfield=(T) \n";
    
    
    //central field
    r=2*cm; //radial distance from center
    worldpos[0] = r;
    worldpos[1] = 0;
    worldpos[2]=zQ1begins+(zQ1ends-zQ1begins)/2; //world position
    GlobalField->GetFieldValue(worldpos,field); //calculate field at position
    fieldmax=field[1]; //maximum field strength
    zmiddle=worldpos[2]; //middle of quadrupole
    fieldFile<<"radius(cm)"<<"\t"<<r/cm<<"\n"; //write to file
    fieldFile<<"Maxfield(T)"<<"\t"<<field[1]/(volt*s/m2)<<"\n";

    n=nn*20; //n changes such that the z step lengths are roughly equal through each element
             //n changes for each element since they don't have equal lengths
    for(int i=0;i<n;i++){
      worldpos[2]=zQ1fieldbegins+i*(zQ2fieldbegins-zQ1fieldbegins)/n; //world position through vacuum volume containing field
      GlobalField->GetFieldValue(worldpos,field); //calculate field at position
      pathlength=worldpos[2]/cm; //pathlength along optical axis within element
      fieldFile<<pathlength<<"\t"<<sqrt((pow(field[2]/(volt*s/m2),2)+pow(field[1]/(volt*s/m2),2)+pow(field[0]/(volt*s/m2),2)))<<"\n"; //write to file
    }
    n=nn*10;
    if(transpose==TRUE){
      fieldend=field[1]; //Bfield value of last position in volume
      for(int i=0;i<n;i++){ //add Bfield from lower tail to missing upper tail (field is symmetric around zmiddle)
        worldpos2[0]=r;
        worldpos2[2]=zQ2fieldbegins-i*(zQ2fieldbegins-zQ1fieldbegins)/n; //straight line
        if(zmiddle+(zmiddle-worldpos2[2])>worldpos[2]){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(zmiddle+(zmiddle-worldpos2[2]))/cm; //pathlength along optical axis within element
          if(field[1]<fieldend)fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n"; //write to file
        }
      }
    } //if(transpose==TRUE)
  }

  if(icomp==1){
    G4cout<<"Q2 fields along optical axis: \n";
    fieldFile<<"Q2 fields along optical axis: zoptical=(cm) yfield=(T) \n";
    
    //central field
    r=2*cm; //radial distance from center
    worldpos[0]=r;
    worldpos[2]=zQ2begins+(zQ2ends-zQ2begins)/2; //world position
    GlobalField->GetFieldValue(worldpos,field); //calculate field at position
    fieldmax=field[1]; //maximum field strength
    zmiddle=worldpos[2]; //middle of quadrupole
    fieldFile<<"radius(cm)"<<"\t"<<r/cm<<"\n";
    fieldFile<<"Maxfield(T)"<<"\t"<<field[1]/(volt*s/m2)<<"\n";

    //get field value at beginning of volume
    worldpos[0]=r;
    worldpos[2]=zQ2fieldbegins;
    GlobalField->GetFieldValue(worldpos,field);
    fieldend=field[1]; //Bfield value of first position in volume
    
    n=nn*10;
    if(transpose==TRUE){
      for(int i=0;i<n;i++){ //add Bfield from upper tail to missing lower tail (field is symmetric around zmiddle)
        worldpos2[0]=r;
        worldpos2[2]=zQ2fieldends-i*(zQ2fieldends-zQ2fieldbegins)/n;//world position through vacuum volume containing field
        if(zmiddle-(worldpos2[2]-zmiddle)<worldpos[2]){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(zmiddle-(worldpos2[2]-zmiddle)-zQ2fieldbegins)/cm; //pathlength along optical axis within element
          if(fabs(field[1])<fabs(fieldend))fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
        }
      }
    } //if(transpose==TRUE)
    n=nn*20;
    for(int i=0;i<n;i++){
      worldpos[0]=r;
      worldpos[2]=zQ2fieldbegins+i*(zQ2fieldends-zQ2fieldbegins)/n;
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(worldpos[2]-zQ2fieldbegins)/cm;
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }
  }		

  if(icomp==2){
    G4cout<<"ED1 fields along optical axis: \n";
    fieldFile<<"ED1 fields along optical axis: zoptical=(cm) xopticalfield=(kV/cm) \n";

    //central field
    theta=angle/2;
    worldpos[0]=xED1center+rED*cos(theta);
    worldpos[2]=zED1center+rED*sin(theta);
    GlobalField->GetFieldValue(worldpos,field); //calculate field at middle of element
    fieldFile<<"Maxfield(kV/cm)"<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    G4cout<<"Maxfield(kV/cm)"<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<G4endl;

    //n=nn*20;
	
    if(transpose==TRUE){
      worldpos[0]=0;
      worldpos[2]=zED1fieldbegins; //straight line
      GlobalField->GetFieldValue(worldpos,field);
      fieldend=sqrt(pow(field[3],2)+pow(field[5],2)); //Bfield value of last position in volume
      pathmiddle=(zED1center-zED1fieldbegins)+rED*(angle/2); //path length to middle of ED2
      for(int i=0;i<n;i++){ //add Efield from lower tail to missing upper tail (field is symmetric around zmiddle)
        delz=i*(zED1fieldends-(zED1center+rED*sin(angle)))/n;
        worldpos2[0]=(xED1center+rED*cos(angle))-((zED1fieldends-(zED1center+rED*sin(angle)))-delz)*tan(angle); //straight line at a 20deg angle
        worldpos2[2]=zED1fieldends-delz;
        pathlength2=(zED1center-zED1fieldbegins+rED*angle
            +sqrt(pow((zED1fieldends-(zED1center+rED*sin(angle))-delz)*tan(angle),2)+pow(zED1fieldends-(zED1center+rED*sin(angle))-delz,2))); // = 2*pi*rED*angle/(2*pi)
        if(pathmiddle-(pathlength2-pathmiddle)<0){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(pathmiddle-(pathlength2-pathmiddle))/cm; //pathlength along optical axis within element
          if(fabs(sqrt(pow(field[3],2)+pow(field[5],2)))<fabs(fieldend))fieldFile<<pathlength<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
        }
      }
    } 

    n=109288;

    for(int i=0;i<n;i++){ //before ED1
      worldpos[0]=0;
      worldpos[2]=i*(zED1center)/n; //straight line
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(worldpos[2])/cm;
      fieldFile<<pathlength<<"\t"<<10000*sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
    n=174533;
    for(int i=0;i<n;i++){ //inside ED1
      theta = i*angle/n;
      worldpos[0]=xED1center+rED*cos(theta); // the optical axis bends
      worldpos[2]=zED1center+rED*sin(theta);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(zED1center+rED*theta)/cm; // = 2*pi*rED*theta/(2*pi)
      fieldFile<<pathlength<<"\t"<<10000*sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
    n=122500;
    for(int i=0;i<n;i++){ //after ED1
      delz=i*(zED1fieldends-(zED1center+rED*sin(angle)))/n;
      worldpos[2]=(zED1center+rED*sin(angle))+delz; //straight line at a 20deg angle
      worldpos[0]=(xED1center+rED*cos(angle))-delz*tan(angle);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(zED1center+rED*angle+sqrt(pow(delz*tan(angle),2)+pow(delz,2)))/cm;
      fieldFile<<pathlength<<"\t"<<10000*sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
  }

  G4double ED1endsOptAxis = (zED1center + rED*angle + Pipe7HL)/cm;
  G4double ED1endsX = Pipe7HL*sin(angle) + rED*sin(angle);
  G4double ED1endsZ = Pipe7HL*cos(angle) + rED*sin(angle) + zED1center;

  if(icomp==3){
    G4cout<<"MD fields along optical axis: \n";
    fieldFile<<"MD fields along optical axis: zoptical=(cm) yfield=(T) \n";   
    
    G4double zMDbegins = zMDcenter+rMD*sin(-angle);
    G4double zMDends = zMDcenter+rMD*sin(angle);
    G4double xMDends = xMDcenter-rMD*cos(angle);
    
    //central field
    theta = 0;
    worldpos[0]=xMDcenter-rMD*cos(theta);
    worldpos[2]=zMDcenter+rMD*sin(theta);
    GlobalField->GetFieldValue(worldpos,field); //calculate field at middle of element
    fieldFile<<"Maxfield(kV/cm)"<<"\t"<<field[1]/(volt*s/m2)<<"\n";

   // compare with bruker center line:
   n=nn*20;
   zMDbegins = zMDbegins - 500*mm;
   zMDends = zMDends + 500*mm;
   worldpos[0]=xMDcenter-rMD*cos(theta);
   for(int i=0;i<n;i++){
	delz=i*(zMDends - zMDbegins)/n;
	worldpos[2] = zMDbegins + delz;
	GlobalField->GetFieldValue(worldpos,field);
	pathlength = (worldpos[2] - (zMDcenter + rMD*sin(theta)))/mm;
	fieldFile<<pathlength<<"\t"<<(-1)*field[1]/(volt*s/m2)<<"\n";
   }

   /*
    n=nn*20;
    for(int i=0;i<n;i++){ //before MD
      delz=i*(zMDbegins-zMDfieldbegins)/n;
      worldpos[2]=zMDfieldbegins+delz; //straight line at a 20deg angle
      worldpos[0]=xMDfieldbegins-delz*tan(angle);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=ED1endsOptAxis + sqrt(pow(delz*tan(angle),2)+pow(delz,2))/cm;
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }
    delz=zMDbegins-zMDfieldbegins;
    for(int i=0;i<n;i++){ //inside MD
      theta = -angle+i*(2*angle)/n;
      worldpos[0]=xMDcenter-rMD*cos(theta); // the optical axis bends
      worldpos[2]=zMDcenter+rMD*sin(theta);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=ED1endsOptAxis + (sqrt(pow(delz*tan(angle),2)+pow(delz,2))+rMD*(theta+angle))/cm;
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }
    pathlength2=pathlength;	//(sqrt(pow(zMDbegins*tan(angle),2)+pow(zMDbegins,2))+rMD*(2*angle));
    G4cout << "path: " << pathlength2 << G4endl;
    for(int i=0;i<n;i++){ //after MD
      delz=i*(zMDfieldends-zMDends)/n;
      worldpos[2]=zMDends+delz; //straight line at a 20deg angle
      worldpos[0]=xMDends+delz*tan(angle);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=pathlength2 + (sqrt(pow(delz*tan(angle),2)+pow(delz,2)))/cm;
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }*/
  }

  if(icomp==4){
    G4cout<<"ED2 fields along optical axis: \n";
    fieldFile<<"ED2 fields along optical axis: zoptical=(cm) xopticalfield=(kV/cm) \n";
    
    G4double zED2begins = zED2center-rED*sin(angle);
    G4double xED2begins = -(rED-rED*cos(angle));     

    //central field
    theta = angle/2;
    worldpos[0]=xED2center+rED*cos(theta);
    worldpos[2]=zED2center-rED*sin(theta);
    GlobalField->GetFieldValue(worldpos,field); //calculate field at middle of element
    fieldFile<<"Maxfield(kV/cm)"<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";

    n=nn*20;
    for(int i=0;i<n;i++){ //before ED2
      delz=i*(zED2begins-zED2fieldbegins)/n;
      worldpos[2]=zED2fieldbegins+delz; //straight line at a 20deg angle
      worldpos[0]=xED2fieldbegins+delz*tan(angle);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength = (sqrt(pow(delz*tan(angle),2)+pow(delz,2)))/cm;
      fieldFile<<pathlength<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
    delz=zED2begins-zED2fieldbegins;
    n=nn*60;
    for(int i=0;i<n;i++){ //inside ED2
      theta = angle-i*angle/n;
      worldpos[0]=xED2center+rED*cos(theta); // the optical axis bends
      worldpos[2]=zED2center-rED*sin(theta);
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(sqrt(pow(delz*tan(angle),2)+pow(delz,2))+rED*(angle-theta))/cm;
      fieldFile<<pathlength<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
    pathlength2=sqrt(pow(delz*tan(angle),2)+pow(delz,2))+rED*(angle);
    n=nn*10;
    for(int i=0;i<n;i++){ //after ED2
      delz=i*(zED2fieldends-zED2center)/n;
      worldpos[0]=0;
      worldpos[2]=zED2center+delz; //straight line
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(pathlength2+delz)/cm;
      fieldFile<<pathlength<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
    }
    n=nn*20;
    if(transpose==TRUE){
      fieldend=sqrt(pow(field[3],2)+pow(field[5],2)); //Bfield value of last position in volume
      pathmax=pathlength*cm;
      pathmiddle=sqrt(pow((zED2begins-zED2fieldbegins)*tan(angle),2)+pow((zED2begins-zED2fieldbegins),2))+rED*(angle/2); //path length to middle of ED2
      for(int i=0;i<n;i++){ //add Efield from lower tail to missing upper tail (field is symmetric around zmiddle)
        delz=i*(zED2begins-zED2fieldbegins)/n;
        worldpos2[0]=xED2begins-delz*tan(angle);
        worldpos2[2]=zED2begins-delz; //straight line at a 20deg angle
        deltube=zED2begins-zED2fieldbegins;
        pathlength2=sqrt(pow((deltube-delz)*tan(angle),2)+pow((deltube-delz),2));
        if(pathmiddle+(pathmiddle-pathlength2)>pathmax){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(pathmiddle+(pathmiddle-pathlength2))/cm; //pathlength along optical axis within element
          if(fabs(sqrt(pow(field[3],2)+pow(field[5],2)))<fabs(fieldend))fieldFile<<pathlength<<"\t"<<sqrt(pow(field[3],2)+pow(field[5],2))<<"\n";
        }
      }
    } //if(transpose==TRUE)
  }
  
  if(icomp==5){
    G4cout<<"Q3 fields along optical axis: \n";
    fieldFile<<"Q3 fields along optical axis: zoptical=(cm) yfield=(T) \n";
    
    //central field
    r=7*cm; //radial distance from center
    worldpos[0]=r;
    worldpos[2]=zQ3begins+(zQ3ends-zQ3begins)/2; //world position
    GlobalField->GetFieldValue(worldpos,field); //calculate field at position
    fieldmax=field[1]; //maximum field strength
    zmiddle=worldpos[2]; //middle of quadrupole
    fieldFile<<"radius(cm)"<<"\t"<<r/cm<<"\n";
    fieldFile<<"Maxfield(T)"<<"\t"<<field[1]/(volt*s/m2)<<"\n";

    n=nn*20;
    for(int i=0;i<n;i++){
     worldpos[0]=r;
     worldpos[2]=zQ3fieldbegins+i*(zQ4fieldbegins-zQ3fieldbegins)/n; //straight line
     GlobalField->GetFieldValue(worldpos,field);
     pathlength=(worldpos[2]-zQ3fieldbegins)/cm;
     fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }
    n=nn*10;
    if(transpose==TRUE){
      fieldend=field[1]; //Bfield value of last position in volume
      for(int i=0;i<n;i++){ //add Bfield from lower tail to missing upper tail (field is symmetric around zmiddle)
        worldpos2[0]=r;
        worldpos2[2]=zQ4fieldbegins-i*(zQ4fieldbegins-zQ3fieldbegins)/n; //straight line
        if(zmiddle+(zmiddle-worldpos2[2])>worldpos[2]){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(zmiddle+(zmiddle-worldpos2[2])-zQ3fieldbegins)/cm; //pathlength along optical axis within element
          if(fabs(field[1])<fabs(fieldend))fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
        }
      }
    } //if(transpose==TRUE)
  }
  if(icomp==6){
    G4cout<<"Q4 fields along optical axis: \n";
    fieldFile<<"Q4 fields along optical axis: zoptical=(cm) yfield=(T) \n";
    
    //central field
    r=9*cm; //radial distance from center
    worldpos[0]=r;
    worldpos[2]=zQ4begins+(zQ4ends-zQ4begins)/2; //straight line
    GlobalField->GetFieldValue(worldpos,field); //calculate field at position
    fieldmax=field[1]; //maximum field strength
    zmiddle=worldpos[2]; //middle of quadrupole
    fieldFile<<"radius(cm)"<<"\t"<<r/cm<<"\n";
    fieldFile<<"Maxfield(T)"<<"\t"<<field[1]/(volt*s/m2)<<"\n";

    //get field value at beginning of volume
    worldpos[0]=r;
    worldpos[2]=zQ4fieldbegins;
    GlobalField->GetFieldValue(worldpos,field);
    fieldend=field[1]; //Bfield value of first position in volume
    
    n=nn*10;
    if(transpose==TRUE){
      for(int i=0;i<n;i++){ //add Bfield from upper tail to missing lower tail (field is symmetric around zmiddle)
        worldpos2[0]=r;
        worldpos2[2]=zQ4fieldends-i*(zQ4fieldends-zQ4fieldbegins)/n; //straight line
        if(zmiddle-(worldpos2[2]-zmiddle)<worldpos[2]){
          GlobalField->GetFieldValue(worldpos2,field); //calculate field at position
          pathlength=(zmiddle-(worldpos2[2]-zmiddle)-zQ4fieldbegins)/cm; //pathlength along optical axis within element
          if(fabs(field[1])<fabs(fieldend))fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
        }
      }
    } //if(transpose==TRUE)
    n=nn*20;
    for(int i=0;i<n;i++){
      worldpos[0]=r;
      worldpos[2]=zQ4fieldbegins+i*(zQ4fieldends-zQ4fieldbegins)/n; //straight line
      GlobalField->GetFieldValue(worldpos,field);
      pathlength=(worldpos[2]-zQ4fieldbegins)/cm;
      fieldFile<<pathlength<<"\t"<<field[1]/(volt*s/m2)<<"\n";
    }
  }
  
  fieldFile.close();

}

EMFieldDebugger::~EMFieldDebugger(){ }

