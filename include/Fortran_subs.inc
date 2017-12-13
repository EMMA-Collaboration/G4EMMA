// Include all the fortran subrouting definitions here

extern "C" 
{
  extern void mitray_dipole__(const double* info, double* position, double* bfld);
  extern void mitray_edipol__(const double* info, double* position, double* bfld);
  extern void mitray_poles__(const double* info, double* position, double* bfld);
//  extern void mitray_sasp__(const double* info, double* position, double* bfld);
//  extern void mitray_solnd__(const double* info, double* position, double* bfld);
}
