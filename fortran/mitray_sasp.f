C***********************************************************************
C                               SASP SUBROUTINES
C***********************************************************************
C
      SUBROUTINE MITRAY_SASP(DATA,XPOS,BFLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     SUBROUTINE TO CALCULATE FIELD IN SASP DIPOLE.                    C
C                                                                      C
C     This is the same as a normal MTYP=4 (clamshell) dipole except    C
C     that sagging of the high end magnetic field due to saturation    C
C     is taken into account empirically.  The variation of the         C
C     B-field with XA is measured with scans of the Rawson-Lush        C
C     rotating coil Gaussmeter along the central axis of the dipole.   C
C                                                                      C
C     S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA                          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT NONE
C
      INTEGER MTYP,I
      REAL*8 DATA(75),BFLD(3),XPOS(3),BFLD_IDEAL(3)
      REAL*8 XA,YA,ZA,BF0,RATIO
C
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'		!local
C
C     ZERO THE OUTPUT B-FIELD VECTOR
C
      DO I=1,3
        BFLD(I)=0.0
      ENDDO
C
C     Extract the A-axis coordinates
      XA=XPOS(1)
      YA=XPOS(2)
      ZA=XPOS(3)
C
C     EXTRACT THE PARAMETERS FOR THE DIPOLE MAGNET
C
      MTYP = DATA(  5)
      BF0  = DATA( 15)
C
C     Check that MTYP=4 as required for SASP dipole
C
      IF(MTYP.NE.4)THEN
         WRITE(lout,100)MTYP
100      FORMAT(' **ERROR** IN SUBROUTINE SASP'/
     1   ' NEED MTYP=4 FOR SASP'/
     2   ' BUT MTYP = ',I5,' WAS SPECIFIED')
         STOP
      ENDIF 
C
C     Calculate the B-field at point (XA,YA,ZA)=(XPOS(1),XPOS(2),XPOS(3))
C     as if it were an ideal clamshell with no saturation
C
      CALL MITRAY_DIPOLE(DATA,XPOS,BFLD_IDEAL)
C
C     We assume that the field departs from the ideal shape as a function
C     of XA only. We calculate the ratio:
C                 RATIO=B_MEAS(XA,YA=0,ZA=0)/B_IDEAL(XA,YA=0,ZA=0)
C     and apply this correction to the ideal B-field.
C
      CALL MITRAY_SASPRATIO(BF0,XA,RATIO)
C
      DO I=1,3
        BFLD(I)=BFLD_IDEAL(I)*RATIO
      ENDDO
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_SASPRATIO(BMEAS0,XA,RATIO)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine to calculate the ratio of measured to ideal B-field   C
C     strength                                                         C
C                                                                      C
C       RATIO = B_MEAS / B_IDEAL                                       C
C                                                                      C
C     at the specified value of XA, for the TRIUMF SASP dipole.        C
C     RATIO departs most markedly from 1 for large values of BF0,      C
C     because of saturation of the steel in the SASP dipole.           C
C                                                                      C
C     input                                                            C
C     -----                                                            C
C     BMEAS0   actual measured B-field at XA=0, as measured with the   C
C              Rawson-Lush rotating Gaussmeter (kiloGauss)             C
C     XA       X-position of the field point, in the dipole            C
C              XA system (cm). The "X=0" reference point of the        C
C              RL-probe corresponds to XA=0.                           C
C                                                                      C
C     output                                                           C
C     ------                                                           C
C     RATIO    = B_MEAS / B_IDEAL                                      C
C                                                                      C
C     We now have to interpolate the following table to get the        C
C     ratio of measured to ideal fields.                               C
C                                                                      C
C     file usr0:[mrsbeam.map]rl_parameters.dat                         C
C     also file prv1:[stan.map]rl_parameters.dat                       C
C                                                                      C
C     These are parameterizations of the field of the SASP dipole as   C
C     measured by the Rawson-Lush probe, along the center line of the  C
C     dipole.  The data can be found in the files                      C
C             ERICH::PRV1:[STAN.MAP]RL_PROFILE_???AMPS.DAT.            C
C     PLOTDATA macro ERICH::PRV1:[STAN.MAP]FITRL.PCM was used to fit   C
C     these.                                                           C
C     First the region x>40 cm (which is least saturated) was fitted   C
C     with a function of the ideal clamshell form                      C
C                                                                      C
C     B(x) = B0/(1+n*x/R), where n=1300 and R=220000.                  C
C                                                                      C
C     where B0 is varied to achieve the best fit.                      C
C                                                                      C
C     Next, B0 is held fixed,and the entire field is fit with the form C
C                                                                      C
C     B(x) = B0/(1+n*x/R) * (a0 + a1*x + ... + a7*x**7)                C
C                                                                      C
C     This procedure is able to reproduce the data to better than      C
C     5.E-4 (1 part in 2000) over the most of the region of interest.  C
C                                                                      C
Ccurrent    B0      a0          a1            a2            a3            a4            a5            a6            a7              
C                                                                                                                                   
C  100   2.010635  .9958276  +1.954237E-3  -2.283502E-4  +1.229721E-5  -3.522838E-7  +5.528341E-9  -4.481606E-11  +1.466508E-13     
C  180   3.590952  .9954798  +1.913474E-3  -2.153433E-4  +1.125749E-5  -3.134953E-7  +4.790139E-9  -3.790939E-11  +1.214844E-13     
C  350   6.955757  .9950759  +1.952778E-3  -2.244842E-4  +1.217696E-5  -3.531899E-7  +5.617057E-9  -4.613114E-11  +1.527785E-13     
C  500   9.919624  .9947577  +2.073851E-3  -2.376060E-4  +1.274280E-5  -3.634150E-7  +5.668429E-9  -4.563387E-11  +1.482475E-13     
C  625   12.31721  .9940234  +1.995845E-3  -2.214544E-4  +1.170371E-5  -3.297804E-7  +5.084942E-9  -4.048612E-11  +1.301461E-13     
C  725   14.18361  .9917550  +2.303983E-3  -2.474834E-4  +1.308128E-5  -3.716997E-7  +5.790985E-9  -4.659174E-11  +1.512609E-13     
C  829   16.04223  .9854187  +2.737002E-3  -2.627983E-4  +1.344560E-5  -3.771425E-7  +5.834937E-9  -4.673327E-11  +1.512125E-13     
C  875   16.80113  .9810218  +3.008759E-3  -2.755810E-4  +1.400980E-5  -3.929814E-7  +6.079107E-9  -4.863025E-11  +1.570053E-13     
C  950   17.97734  .9721475  +3.513326E-3  -2.974198E-4  +1.483416E-5  -4.116604E-7  +6.301599E-9  -4.987789E-11  +1.593526E-13     
C 1000   18.66637  .965898   +3.856533E-3  -3.076932E-4  +1.501107E-5  -4.121208E-7  +6.270180E-9  -4.945806E-11  +1.577158E-13     
C                                                                      C
C     Since B(x) = B0 / (1+n*x/R) * (a0 + a1*x + ... + a7*x**7),       C
C     we pull out a constant factor a0 and multiply it with B0 to give C
C                                                                      C
C     BMEAS0 = B0*a0, and C1=a1/a0, C2=a2/a0, C3=a3/a0, ..., C7=a7/a0. C
C                                                                      C
C     Now                                                              C
C                                                                      C
C     B(x) = BMEAS0 / (1+n*x/R) * (1 + C1*x + C2*x**2 + ... + C7*x**7) C
C                                                                      C
C     where BMEAS0 is the measured value of the B-field at reference   C
C     point x=0.                                                       C
C                                                                      C
C  I      BF          C1          C2            C3              C4            C5           C6            C7                         
C amps   kGauss                                                                                                                     
C 100.   2.00225  1.962394E-03 -2.293013E-04  1.234833E-05 -3.537459E-07  5.551259E-09 -4.500169E-11  1.472579E-13                  
C 180.   3.57472  1.922171E-03 -2.163218E-04  1.130863E-05 -3.149187E-07  4.811879E-09 -3.808138E-11  1.220355E-13                  
C 350.   6.92151  1.962442E-03 -2.255950E-04  1.223721E-05 -3.549376E-07  5.644852E-09 -4.635942E-11  1.535346E-13                  
C 500.   9.86762  2.084780E-03 -2.388581E-04  1.280996E-05 -3.653301E-07  5.698301E-09 -4.587436E-11  1.490287E-13                  
C 625.  12.24360  2.007846E-03 -2.227859E-04  1.177408E-05 -3.317632E-07  5.115515E-09 -4.072955E-11  1.309286E-13                  
C 725.  14.06667  2.323137E-03 -2.495408E-04  1.319004E-05 -3.747899E-07  5.839129E-09 -4.697908E-11  1.525184E-13                  
C 829.  15.80831  2.777501E-03 -2.666869E-04  1.364456E-05 -3.827231E-07  5.921277E-09 -4.742479E-11  1.534500E-13                  
C 875.  16.48228  3.066965E-03 -2.809122E-04  1.428083E-05 -4.005838E-07  6.196709E-09 -4.957102E-11  1.600426E-13                  
C 950.  17.47663  3.613984E-03 -3.059410E-04  1.525917E-05 -4.234547E-07  6.482142E-09 -5.130691E-11  1.639181E-13                  
C1000.  18.02981  3.992692E-03 -3.185566E-04  1.554105E-05 -4.266711E-07  6.491555E-09 -5.120422E-11  1.632841E-13                  
C                                                                      C
C     Since the measured field has been parameterized with the form    C
C                                                                      C
C     B_meas (x) = BMEAS0 / (1+n*x/R) * (1 + C1*x + C2*x**2 + ... + C7*x**7)
C                                                                      C
C     and the ideal field has the form                                 C
C                                                                      C
C     B_ideal (x) = BMEAS0 / (1+n*x/R)                                 C
C                                                                      C
C     The ratio of B_meas/B_ideal is simply                            C
C                                                                      C
C     RATIO = (1 + C1*x + C2*x**2 + ... + C7*x**7)                     C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT NONE
C
      REAL*8 BMEAS0,XA,RATIO,RATIO1,RATIO2,ABSBF0,DIFF,FRAC
      REAL*8 BXEQ0(10),C1(10),C2(10),C3(10)
      REAL*8 C4(10),C5(10),C6(10),C7(10),XIESTIMATE
      REAL*8 RATIOIB,RATIOIC,RATIOJB,RATIOJC,RATIOB,RATIOC
      REAL*8 XRLB,XRLC,XRL,SLOPE
      INTEGER I,J,INDEX1,INDEX2,ISTART,ISTOP
C
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'		!local
C
      DATA BXEQ0(1),C1(1),C2(1),C3(1),C4(1),C5(1),C6(1),C7(1)
     1 /2.00225,  1.962394E-03, -2.293013E-04,  1.234833E-05,
     2 -3.537459E-07,  5.551259E-09, -4.500169E-11,  1.472579E-13 /
C
      DATA BXEQ0(2),C1(2),C2(2),C3(2),C4(2),C5(2),C6(2),C7(2)
     1 /3.57472,  1.922171E-03, -2.163218E-04,  1.130863E-05,
     2 -3.149187E-07,  4.811879E-09, -3.808138E-11,  1.220355E-13 /
C
      DATA BXEQ0(3),C1(3),C2(3),C3(3),C4(3),C5(3),C6(3),C7(3)
     1 / 6.92151,  1.962442E-03, -2.255950E-04,  1.223721E-05,
     2  -3.549376E-07,  5.644852E-09, -4.635942E-11,  1.535346E-13/
C
      DATA BXEQ0(4),C1(4),C2(4),C3(4),C4(4),C5(4),C6(4),C7(4)
     1 / 9.86762,  2.084780E-03, -2.388581E-04,  1.280996E-05,
     2  -3.653301E-07,  5.698301E-09, -4.587436E-11,  1.490287E-13/
C
      DATA BXEQ0(5),C1(5),C2(5),C3(5),C4(5),C5(5),C6(5),C7(5)
     1 /  12.24360,  2.007846E-03, -2.227859E-04,  1.177408E-05,
     2  -3.317632E-07,  5.115515E-09, -4.072955E-11,  1.309286E-13/
C
      DATA BXEQ0(6),C1(6),C2(6),C3(6),C4(6),C5(6),C6(6),C7(6)
     1 /  14.06667,  2.323137E-03, -2.495408E-04,  1.319004E-05,
     2  -3.747899E-07,  5.839129E-09, -4.697908E-11,  1.525184E-13/
C
      DATA BXEQ0(7),C1(7),C2(7),C3(7),C4(7),C5(7),C6(7),C7(7)
     1 /  15.80831,  2.777501E-03, -2.666869E-04,  1.364456E-05,
     2  -3.827231E-07,  5.921277E-09, -4.742479E-11,  1.534500E-13/
C
      DATA BXEQ0(8),C1(8),C2(8),C3(8),C4(8),C5(8),C6(8),C7(8)
     1 /  16.48228,  3.066965E-03, -2.809122E-04,  1.428083E-05,
     2  -4.005838E-07,  6.196709E-09, -4.957102E-11,  1.600426E-13/
C
      DATA BXEQ0(9),C1(9),C2(9),C3(9),C4(9),C5(9),C6(9),C7(9)
     1 / 17.47663,  3.613984E-03, -3.059410E-04,  1.525917E-05,
     2  -4.234547E-07,  6.482142E-09, -5.130691E-11,  1.639181E-13/
C
      DATA BXEQ0(10),C1(10),C2(10),C3(10),C4(10),C5(10),C6(10),C7(10)
     1 / 18.02981,  3.992692E-03, -3.185566E-04,  1.554105E-05,
     2  -4.266711E-07,  6.491555E-09, -5.120422E-11,  1.632841E-13/
C
C     For an MTYP=4 dipole (Clamshell dipole), the XA coordinate is the
C     same as the RL-probe coordinate.
C
      XRL=XA
C
C     Find the value of INDEX such that BXEQ0(INDEX) <= BF0 <= BXEQ0(INDEX+1)
C     INDEX1 and INDEX2 are the two values to use for interpolation
C
      ABSBF0=ABS(BMEAS0)
      IF(ABSBF0.LE.BXEQ0(1))THEN
        INDEX1=1
        INDEX2=1
        GOTO 100
      ELSE IF(ABSBF0.GE.BXEQ0(10))THEN
        INDEX1=10
        INDEX2=10
        GOTO 100
      ENDIF
C
C     MAKE AN ESTIMATE OF WHERE IN THE TABLE ONE SHOULD LOOK
C     GIVEN A VALUE OF B-FIELD STRENGTH ABSBF0, THIS POLYNOMIAL ESTIMATES
C     THE INDEX CORRECTLY TO WITHIN +/- 0.59 .  WE KEEP IT A FLOATING
C     POINT NUMBER FOR NOW
C
      XIESTIMATE=(((2.39218E-2*ABSBF0)+1.75226E-2)*ABSBF0+1.3203) + 0.5
C
C     CALCULATE THE RANGE OF INDEX VALUES OVER WHICH TO SEARCH.
C     ROUND DOWN ON ISTART, SO DON'T NEED TO DO ANYTHING
C
      ISTART=XIESTIMATE-0.6
C
C     ROUND UP ON ISTOP, SO ADD ANOTHER 0.5 TO 0.6 TO GIVE +1.1
C
      ISTOP=XIESTIMATE+1.1
      IF(ISTART.LT.1)ISTART=1
      IF(ISTOP.GT.9)ISTOP=9
      DO I=ISTART,ISTOP
        IF(ABSBF0.GE.BXEQ0(I) .AND. ABSBF0.LT.BXEQ0(I+1))THEN
          INDEX1=I
          INDEX2=I+1
          GOTO 100
        ENDIF
      ENDDO
C
C     Should never come here
C
      WRITE(lout,*)'**error** in subroutine MITRAY_SASPRATIO'
      WRITE(lout,*)'          failure of interpolation routine'
C
      RATIO=0.
      STOP
C
100   I=INDEX1
      J=INDEX2
C
C     FRAC=fraction of distance between BXEQ0(I) and BXEQ0(J) where the
C          actual B(x=0) value lies 
C
      IF(I.EQ.J)THEN
        FRAC=0.
      ELSE
        FRAC=(ABSBF0-BXEQ0(I))/(BXEQ0(J)-BXEQ0(I))
      ENDIF
C
C     THE POLYNOMIAL PARAMETERIZATION IS GOOD ONLY FOR XRL>0.
C     AND CAN WILDLY DIVERGE FOR XRL<0.  
C
      IF(XRL.GE.0.0)THEN
         RATIO1=1. + C1(I)*XRL + C2(I)*XRL**2 
     1             + C3(I)*XRL**3 + C4(I)*XRL**4
     1             + C5(I)*XRL**5 + C6(I)*XRL**6 
     1             + C7(I)*XRL**7
         RATIO2=1. + C1(J)*XRL + C2(J)*XRL**2 
     1             + C3(J)*XRL**3 + C4(J)*XRL**4
     1             + C5(J)*XRL**5 + C6(J)*XRL**6 
     1             + C7(J)*XRL**7
C
        DIFF=RATIO2-RATIO1
        RATIO=RATIO1+FRAC*DIFF
        RETURN
C
      ELSE
C
C       COME HERE IF XRL<0.
C       For XRL<0, there is no RL-probe data available to define the
C       shape of the SASP dipole magnetic field.
C       At the entrance and exit ends of SASP, we need values of XRL down
C       to as low as XRL=-25 cm.
C       There is some indication that the data points very near XRL=0
C       suffer from some "falling off" due to the proximity of the
C       dipole edge.  We will therefore determine RATIO at XRL=3. cm
C       and XRL=10. cm and use this slope to do a linear extrapolation
C       for XRL<0.
C
        XRLB=3.
        XRLC=10.
C
C       Now determine RATIO for XRL=XRLB (point B), for the two 
C       dipole current settings I and J
C
        RATIOIB=1. + C1(I)*XRLB + C2(I)*XRLB**2 
     1             + C3(I)*XRLB**3  + C4(I)*XRLB**4
     1             + C5(I)*XRLB**5 + C6(I)*XRLB**6 
     1             + C7(I)*XRLB**7
        RATIOJB=1. + C1(J)*XRLB + C2(J)*XRLB**2 
     1             + C3(J)*XRLB**3 +  C4(J)*XRLB**4
     1             + C5(J)*XRLB**5 + C6(J)*XRLB**6 
     1             + C7(J)*XRLB**7
C
        RATIOB=RATIOIB+FRAC*(RATIOJB-RATIOIB)
C
C
C       Now determine RATIO for XRL=XRLC (point C), for the two 
C       dipole current settings I and J
C
        RATIOIC=1. + C1(I)*XRLC + C2(I)*XRLC**2 
     1             + C3(I)*XRLC**3  + C4(I)*XRLC**4
     1             + C5(I)*XRLC**5 + C6(I)*XRLC**6 
     1             + C7(I)*XRLC**7
        RATIOJC=1. + C1(J)*XRLC + C2(J)*XRLC**2 
     1             + C3(J)*XRLC**3 +  C4(J)*XRLC**4
     1             + C5(J)*XRLC**5 + C6(J)*XRLC**6 
     1             + C7(J)*XRLC**7
C
        RATIOC=RATIOIC+FRAC*(RATIOJC-RATIOIC)
C
C       NOW WE HAVE THE TWO POINTS (XRL, RATIO) =  (XRLB, RATIOB),
C       AND (XRLC,RATIOC) .
        SLOPE=(RATIOB-RATIOC)/(XRLB-XRLC)
C
C       EXTRAPOLATE USING LINEAR EXTRAPOLATION, SUCH THAT RATIO=1.0 FOR XRL=0
C
        RATIO=1.0+XRL*SLOPE
        RETURN
      ENDIF
      END
C
