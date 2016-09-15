C***********************************************************************
C                          MULTI-POLES SUBROUTINE
C***********************************************************************
C
      SUBROUTINE MITRAY_POLES(DATA,XPOS,BFLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine for multipoles, GEANT implementation of MIT-RAYTRACE  C
C     adapted from:                                                    C
C     Subroutine POLES (NO, NP, T, TP, NUM) by S. Kowalski             C
C                                                                      C
C      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C
C     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Modification History
C     --------------------
C     Mar 26, 1998   S. Yen
C     Apr  5, 1999   S. Yen  Calculate overlapping entrance and exit fringe
C                            fields for very short multipole.
C 
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8  LF1, LF2, LU1, K, L
      REAL*8 GRAD1C, GRAD2C, GRAD3C, GRAD4C, GRAD5C
C
      COMMON  /MITRAY5/  XA, YA, ZA, VXA, VYA, VZA
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5
      COMMON  /MITRAY91/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY92/  IN
      COMMON  /MITRAY93/  DH, DO, DD, DDD, DSH, DSO, DSD, DSDD
C
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'     !local
      include 'diagnostic.inc'      !local
C
      REAL*8 DATA(75),XPOS(3),BFLD(3)
      REAL*8 TC(6), DTC(6)
      INTEGER*4 I,IZONE
C
C     First zero the output B-field in case of abort
C
	  LDIAG=.false.
      DO I=1,3
        BFLD(I)=0.
      ENDDO
C
C     EXTRACT THE MULTIPOLE PARAMETERS FROM THE INPUT DATA ARRAY
C
      LF1  = DATA(  1 )
      LU1  = DATA(  2 )
      LF2  = DATA(  3 )
      A    = DATA( 10 )
      B    = DATA( 11 )
      L    = DATA( 12 )
      RAD  = DATA( 13 )
      BQD  = DATA( 14 )
      BHX  = DATA( 15 )
      BOC  = DATA( 16 )
      BDC  = DATA( 17 )
      BDD  = DATA( 18 )
      Z11  = DATA( 19 )
      Z12  = DATA( 20 )
      Z21  = DATA( 21 )
      Z22  = DATA( 22 )
      FRH  = DATA( 35 )
      FRO  = DATA( 36 )
      FRD  = DATA( 37 )
      FRDD = DATA( 38 )
      DSH  = DATA( 39 )
      DSO  = DATA( 40 )
      DSD  = DATA( 41 )
      DSDD = DATA( 42 )
C     DTF1= LF1/ VEL
C     DTF2= LF2/ VEL
C     DTU = LU1/ VEL
      D = 2. * RAD
      IF( FRH  .EQ. 0. ) FRH  = 1.D0
      IF( FRO  .EQ. 0. ) FRO  = 1.D0
      IF( FRD  .EQ. 0. ) FRD  = 1.D0
      IF( FRDD .EQ. 0. ) FRDD = 1.D0
      DH  = FRH *D
      DO  = FRO *D
      DD  = FRD *D
      DDD = FRDD*D
C
C     Extract the A-axis coordinates from the XPOS array
C
      XA=XPOS(1)
      YA=XPOS(2)
      ZA=XPOS(3)
C
C     Calculate the B-axis coordinates (entrance VFB coordinates)
C
      XB=-XA
      YB=YA
      ZB=A-ZA
C
C     Calculate the C-axis coordinates (exit VFB coordinates)
C
      XC=-XB
      YC=YB
      ZC=-ZB-L
C
C
      BX = 0.
      BY = 0.
      BZ = 0.
      BT = 0.
      S = 0.
c
      IF(LDIAG)THEN
         WRITE(lout,900)XA,YA,ZA
900      FORMAT(1X,50('-')/' ENTER SUBROUTINE POLES'/
     1   ' XA,YA,ZA=',3F10.3/)
      ENDIF
C
C     Print out coordinates if diagnostic mode
C
      IF(LDIAG)THEN
         WRITE(lout,901)XB,YB,ZB
901         FORMAT(' XB,YB,ZB=',3F10.3)
         WRITE(lout,902)XC,YC,ZC
902         FORMAT(' XC,YC,ZC=',3F10.3)
         WRITE(lout,903)Z11,Z12,Z21,Z22
903         FORMAT(' Z11=',F10.3,'  Z12=',F10.3,
     1      '  Z21=',F10.3,'  Z22=',F10.3)
         WRITE(lout,*)' '
      END IF
C
C     Determine which zone of the multipole we are in by calling MITRAY_ZONE.
c     IZONE=0  far entrance or exit (B=0)
C     IZONE=1  entrance fringe field
C     IZONE=2  uniform field region
C     IZONE=3  exit fringe field
C     IZONE=4  overlapping entrance and exit fringe fields for short magnet
C              In this case, we calculate for the B-field at any point
C              B=B(entrance) + B(exit) - B(uniform).  This simulates
c              action of MIT-RAYTRACE in integrating *backwards* from end of
c              the entrance fringe field, to the start of the exit fringe
c              field, using the field of the uniform field region.
C     IZONE=-1 error
C
      CALL MITRAY_ZONE(ZB,ZC,Z11,Z12,Z21,Z22,IZONE)
C
C     For each zone, set variable IN accordingly, and call MITRAY_BPOLES
C     to evaluate the magnetic field for that zone.
C     Note that for IZONE=4 (overlapping entrance and exit fringe fields,
C     for a very short multipole), the B-field that we want is
C     B(total) = B(entrance fringe) + B(exit fringe) - B(uniform region)
C
      IF(IZONE.EQ.0)THEN
C        
C        ***********************************
C        * FAR ENTRANCE OR EXIT ZONES, B=0 *
C        ***********************************
C
         IF(LDIAG)THEN
           WRITE(lout,904)'A SYSTEM',0., 0., 0.
904        FORMAT(1X,A,'  BX,BY,BZ=',3F10.3)
         ENDIF
         RETURN
      ENDIF
C
C
C     Calculate the gradients for the various multipole components.
C     The gradients evaluated below (with no minus signs) are correct when
c     we are working in the C-axis system
C     (uniform field and exit fringe field regions), but the signs of
C     the quadrupole, octopole and dodecapole gradients must be changed
C     when we work in the B-axis system (i.e. in the entrance fringe field
C     region).
c
      GRAD1C = BQD/RAD
      GRAD2C = BHX/RAD**2
      GRAD3C = BOC/RAD**3
      GRAD4C = BDC/RAD**4
      GRAD5C = BDD/RAD**5
C
      IF(IZONE.EQ.2 .OR. IZONE.EQ.4)THEN
C
C        Come here for either uniform field region (IZONE=1) or
C        overlapping entrance and exit fringe fields (IZONE=4).
C
C        *****************************************************
C        * INTERIOR ("UNIFORM FIELD") ZONE FIELD CALCULATION *
C        *****************************************************
C
C        Load the C-axis coordinates into array TC(i), which is what
C        subroutine BPOLES is expecting for the uniform field region
C        IN designates the region that we are in.
         IN=2
         S=0.
         TC(1)=XC
         TC(2)=YC
         TC(3)=ZC
C
C        We are working in the C-axis system, so use C-axis gradients
         GRAD1=GRAD1C
         GRAD2=GRAD2C
         GRAD3=GRAD3C
         GRAD4=GRAD4C
         GRAD5=GRAD5C
C
C        CALL BPOLES TO CALCULATE B-FIELD IN C-AXIS SYSTEM
C
         CALL MITRAY_BPOLES
C
         IF(LDIAG)THEN
           WRITE(lout,*)'UNIFORM FIELD REGION CALC.'
           WRITE(lout,904)'C SYSTEM',BX,BY,BZ
           WRITE(lout,904)'A SYSTEM',BX,BY,BZ
         ENDIF
c
C        THE B-FIELD COMPONENTS ARE IN THE C-AXIS SYSTEM; WHICH IS THE
C        SAME AS IN THE A-AXIS SYSTEM.  STORE THEM IN OUTPUT ARRAY BFLD(i).
C
         IF(IZONE.EQ.2)THEN
           BFLD(1)=BX
           BFLD(2)=BY
           BFLD(3)=BZ
           RETURN
         ELSE 
c          IZONE=4  overlapping entrance & exit fringe fields
c          We add the uniform field components BX,BY,BZ to the total
c          field BFLD(i).
c          Note minus sign, since B=B(entrance)+B(exit)-B(uniform)
           BFLD(1)=BFLD(1)-BX
           BFLD(2)=BFLD(2)-BY
           BFLD(3)=BFLD(3)-BZ
         ENDIF
      ENDIF
C
C
C
      IF(IZONE.EQ.1 .OR. IZONE.EQ.4)THEN
C
C        Come here for either pure entrance fringe field (IZONE=1) or
C        overlapping entrance and exit fringe fields (IZONE=4).
C
C        ******************************************
C        * ENTRANCE FRINGE FIELD ZONE CALCULATION *
C        ******************************************
C
C        Set IN=1 to designate entrance fringe field, and extract the
C        fringing field coefficients
         IN=1
         C0=DATA(23)
         C1=DATA(24)
         C2=DATA(25)
         C3=DATA(26)
         C4=DATA(27)
         C5=DATA(28)
C
C        Load the B-axis coordinates into the array TC(i), which is
C        what subroutine BPOLES is expecting
         TC(1)=XB
         TC(2)=YB
         TC(3)=ZB
C
C        We are working in the B-axis system, so change the signs of
C        the quadrupole, octopole and dodecapole gradients from those
C        of the C-axis system.
         GRAD1 = -GRAD1C
         GRAD2 =  GRAD2C
         GRAD3 = -GRAD3C
         GRAD4 =  GRAD4C
         GRAD5 = -GRAD5C
C
C
C        Call BPOLES to calculate B-field in B-axis system
C
         CALL MITRAY_BPOLES
C
         IF(LDIAG)THEN
           WRITE(lout,904)'ENTRANCE FRINGE FIELD CALC.'
           WRITE(lout,904)'B SYSTEM',BX,BY,BZ
           WRITE(lout,904)'A SYSTEM',-BX,BY,-BZ
         ENDIF
C
C        THE B-FIELD COMPONENTS BX,BY,BZ ARE IN THE B-AXIS SYSTEM; CONVERT TO
C        THE A-AXIS SYSTEM.  STORE THEM IN OUTPUT ARRAY BFLD(i).
C
         IF(IZONE.EQ.1)THEN
           BFLD(1)=-BX
           BFLD(2)=BY
           BFLD(3)=-BZ
           RETURN
         ELSE
C          ! IZONE.EQ.4  overlapping entrance & exit fringe fields
c          We add the entrance field components BX,BY,BZ to the total
c          field BFLD(i), since B(total)=B(entrance)+B(exit)-B(uniform)
c          Note the sign changes for BX and BZ since these are in the B-axis
c          system and we need to change them to the A-axis system.
           BFLD(1)=BFLD(1)-BX
           BFLD(2)=BFLD(2)+BY
           BFLD(3)=BFLD(3)-BZ
         ENDIF
      ENDIF
C
C
C
      IF(IZONE.EQ.3 .OR. IZONE.EQ.4) THEN
C
C        Come here for either pure exit fringe field (IZONE=3) or
C        overlapping entrance and exit fringe fields (IZONE=4).
C
C        **************************************
C        * EXIT FRINGE FIELD ZONE CALCULATION *
C        **************************************
C
C        Set IN=3 to designate exit fringe field, and extract fringing
C        coefficients for the exit fringe field.
         IN=3
         C0=DATA(29)
         C1=DATA(30)
         C2=DATA(31)
         C3=DATA(32)
         C4=DATA(33)
         C5=DATA(34)
C
C        Load the C-axis coordinates into the array TC(i), which is
C        what subroutine BPOLES is expecting
         TC(1)=XC
         TC(2)=YC
         TC(3)=ZC
C
C        We are working in the C-axis system, so use C-axis gradients
         GRAD1=GRAD1C
         GRAD2=GRAD2C
         GRAD3=GRAD3C
         GRAD4=GRAD4C
         GRAD5=GRAD5C
C
C
C        Call BPOLES to calculate B-field in C-axis system
C
         CALL MITRAY_BPOLES
C
         IF(LDIAG)THEN
           WRITE(lout,904)'EXIT FRINGE FIELD CALC.'
           WRITE(lout,904)'C SYSTEM',BX,BY,BZ
           WRITE(lout,904)'A SYSTEM',BX,BY,BZ
         ENDIF
C
C        THE B-FIELD COMPONENTS BX,BY,BZ ARE IN THE C-AXIS SYSTEM; CONVERT TO
C        THE A-AXIS SYSTEM (which is the same).  
C        STORE THEM IN OUTPUT ARRAY BFLD(i).
C
         IF(IZONE.EQ.3)THEN
           BFLD(1)=BX
           BFLD(2)=BY
           BFLD(3)=BZ
           RETURN
         ELSE
C          ! IZONE=4 overlapping entrance and exit fringe fields
c          We add the exit field components BX,BY,BZ to the total
c          field BFLD(i) since B(total)=B(entrance)+B(exit)-B(uniform)
           BFLD(1)=BFLD(1)+BX
           BFLD(2)=BFLD(2)+BY
           BFLD(3)=BFLD(3)+BZ
           RETURN
         ENDIF
      ENDIF
C
      IF(IZONE.EQ.-1)THEN
C        UNKNOWN FIELD REGION, RETURN WITH B=0
         WRITE(lout,*)'UNKNOWN MULTIPOLE FIELD REGION'
         WRITE(lout,*)'!!! Abort current event !!!'
         jstop  = 1
         ieotri = 1
         RETURN
      ENDIF
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_BPOLES
C****
C**** CALCULATION OF MULTIPOLE(POLES) FIELD COMPONENTS
C****
C****
C****
C**** 2 - QUADRUPOLE  (GRAD1)
C**** 3 - HEXAPOLE    (GRAD2)
C**** 4 - OCTAPOLE    (GRAD3)
C**** 5 - DECAPOLE    (GRAD4)
C**** 6 - DODECAPOLE  (GRAD5)
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5
      COMMON  /MITRAY91/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY92/  IN
      COMMON  /MITRAY93/  DH, DO, DD, DDD, DSH, DSO, DSD, DSDD
      DIMENSION TC(6), DTC(6)
      X = TC(1)
      Y = TC(2)
      Z = TC(3)
      X2 = X*X
      X3 = X2*X
      X4 = X3*X
      X5 = X4*X
      X6 = X5*X
      X7 = X6*X
      Y2 = Y*Y
      Y3 = Y2*Y
      Y4 = Y3*Y
      Y5 = Y4*Y
      Y6 = Y5*Y
      Y7 = Y6*Y
      GO TO ( 2, 1, 2 ) , IN
      WRITE(6,3) IN
    3 FORMAT( '  ERROR IN BPOLES IN= ' ,I5 ///)
      STOP
    1 CONTINUE
      B2X = GRAD1*Y
      B2Y = GRAD1*X
      B3X = GRAD2*2.*X*Y
      B3Y = GRAD2*(X2-Y2)
      B4X = GRAD3*(3.*X2*Y-Y3)
      B4Y = GRAD3*(X3-3.*X*Y2)
      B5X = GRAD4*4.*(X3*Y-X*Y3)
      B5Y = GRAD4*(X4-6.*X2*Y2+Y4)
      B6X = GRAD5*(5.*X4*Y-10.*X2*Y3+Y5)
      B6Y = GRAD5*(X5-10.*X3*Y2+5.*X*Y4)
      BX = B2X + B3X + B4X + B5X + B6X
      BY = B2Y + B3Y + B4Y + B5Y + B6Y
      BZ = 0.
      BT =   DSQRT( BX*BX + BY*BY )
      RETURN
C****
C****
C**** QUADRUPOLE
C****
    2 S = Z/D
      CALL MITRAY_BPLS( 2, D, S, RE, G1, G2, G3, G4, G5, G6 )
      B2X = GRAD1*( RE*Y - (G2/12.)*(3.*X2*Y + Y3) +
     1   (G4/384.)*(5.*X4*Y + 6.*X2*Y3 + Y5 ) -
     2   (G6/23040.)*(7.*X6*Y + 15.*X4*Y3 + 9.*X2*Y5 + Y7)  )
      B2Y = GRAD1*( RE*X - (G2/12.)*(X3 + 3.*X*Y2) +
     1   (G4/384.)*(X5 + 6.*X3*Y2 + 5.*X*Y4 ) -
     2   (G6/23040.)*(X7 + 9.*X5*Y2 + 15.*X3*Y4 + 7.*X*Y6) )
      B2Z = GRAD1*( G1*X*Y - (G3/12.)*(X3*Y + X*Y3 ) +
     1   (G5/384.)*(X5*Y +2.*X3*Y3 + X*Y5)  )
C****
C**** HEXAPOLE
C****
      SS = Z/DH  + DSH
      CALL MITRAY_BPLS( 3, DH, SS, RE, G1, G2, G3, G4, G5, G6 )
      B3X = GRAD2*( RE*2.*X*Y - (G2/48.)*(12.*X3*Y + 4.*X*Y3 ) )
      B3Y = GRAD2*( RE*(X2-Y2) - (G2/48.)*(3.*X4 + 6.*X2*Y2 - 5.*Y4 ) )
      B3Z = GRAD2*( G1*(X2*Y - Y3/3.) - (G3/48.)*(3.*X4*Y+2.*X2*Y3-Y5))
C****
C**** OCTAPOLE
C****
      SS = Z/DO  + DSO
      CALL MITRAY_BPLS( 4, DO, SS, RE, G1, G2, G3, G4, G5, G6 )
      B4X = GRAD3*( RE*(3.*X2*Y - Y3) - (G2/80.)*(20.*X4*Y - 4.*Y5 ) )
      B4Y = GRAD3*( RE*(X3 - 3.*X*Y2) - (G2/80.)*(4.*X5-20.*X*Y4 ) )
      B4Z = GRAD3*G1*(X3*Y - X*Y3 )
C****
C**** DECAPOLE
C****
      SS = Z/DD  + DSD
      CALL MITRAY_BPLS( 5, DD, SS, RE, G1, G2, G3, G4, G5, G6 )
      B5X = GRAD4*RE*(4.*X3*Y - 4.*X*Y3)
      B5Y = GRAD4*RE*(X4 - 6.*X2*Y2 + Y4 )
      B5Z = GRAD4*G1*(X4*Y - 2.*X2*Y3 + Y5/5. )
C****
C**** DODECAPOLE
C****
      SS = Z/DDD + DSDD
      CALL MITRAY_BPLS( 6, DDD,SS, RE, G1, G2, G3, G4, G5, G6 )
      B6X = GRAD5*RE*(5.*X4*Y - 10.*X2*Y3 + Y5 )
      B6Y = GRAD5*RE*(X5 - 10.*X3*Y2 + 5.*X*Y4 )
      B6Z = 0.
C****
C**** TOTAL FIELD
C****
      BX = B2X + B3X + B4X + B5X + B6X
      BY = B2Y + B3Y + B4Y + B5Y + B6Y
      BZ = B2Z + B3Z + B4Z + B5Z + B6Z
      BT =   DSQRT( BX*BX + BY*BY + BZ*BZ )
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_BPLS (IGP,D,S,RE,G1,G2,G3,G4,G5,G6)
C****
C****
C****
      IMPLICIT REAL*8 (A-H,O-Z)
C****
C****
      COMMON  /MITRAY91/  C0, C1, C2, C3, C4, C5
C****
C****
      S2 = S*S
      S3 = S2*S
      S4 = S2*S2
      S5 = S4*S
      CS = C0 + C1*S + C2*S2 + C3*S3 + C4*S4 + C5*S5
      CP1 =(C1 + 2.*C2*S + 3.*C3*S2 + 4.*C4*S3 + 5.*C5*S4) / D
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S2 + 20.*C5*S3  ) / (D*D)
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S2 ) / (D**3)
      CP4 = ( 24.*C4 + 120.*C5*S ) / (D**4)
C****
      CP5 = 120.*C5/(D**5)
C****
C****
C****
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )
      E = DEXP(CS)
      RE = 1./(1. + E)
      ERE = E*RE
      ERE1= ERE*RE
      ERE2= ERE*ERE1
      ERE3= ERE*ERE2
      ERE4= ERE*ERE3
C****
      ERE5= ERE*ERE4
      ERE6= ERE*ERE5
C****
C****
      CP12 = CP1*CP1
      CP13 = CP1*CP12
      CP14 = CP12*CP12
      CP22 = CP2*CP2
C****
      CP15 = CP12*CP13
      CP16 = CP13*CP13
      CP23 = CP2*CP22
      CP32 = CP3*CP3
C****
C****
      IF( IGP .EQ. 6 ) RETURN
      G1 = -CP1*ERE1
C****
C****
      IF( IGP .EQ. 5 ) RETURN
      G2 =-( CP2+CP12   )*ERE1    + 2.*CP12 * ERE2
      IF( IGP .EQ. 4 ) RETURN
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE1      +
     1   6.*(CP1*CP2 + CP13)*ERE2 - 6.*CP13*ERE3
C****
C****
      IF( IGP .EQ. 3 ) RETURN
1     G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE1  +
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2    -
     2   36.*(CP12*CP2 + CP14)*ERE3       + 24.*CP14*ERE4
C****
C****
      IF( IGP .NE. 2 ) RETURN
      G5 = (-CP5 - 5.*CP1*CP4 - 10.*CP2*CP3 - 10.*CP12*CP3 -
     1     15.*CP1*CP22 - 10.*CP13*CP2 - CP15)*ERE1 +
     2     (10.*CP1*CP4 +20.*CP2*CP3 +60.*CP12*CP3 + 90.*CP1*CP22 +
     3     140.*CP13*CP2 +30.*CP15)*ERE2 + (-60.*CP12*CP3 -
     4     90.*CP1*CP22 - 360.*CP13*CP2 - 150.*CP15)*ERE3 +
     5     (240.*CP13*CP2 +240.*CP15)*ERE4 + (-120.*CP15)*ERE5
      G6 = (-6.*CP1*CP5 - 15.*CP2*CP4 - 15.*CP12*CP4 - 10.*CP32 -
     1     60.*CP1*CP2*CP3 - 20.*CP13*CP3 - 15.*CP23 - 45.*CP12*CP22 -
     2     15.*CP14*CP2 - CP16)*ERE1 + (12.*CP1*CP5 + 30.*CP2*CP4 +
     3     90.*CP12*CP4 +20.*CP32 + 360.*CP1*CP2*CP3 +280.*CP13*CP3 +
     4     90.*CP23 + 630.*CP12*CP22 + 450.*CP14*CP2 + 62.*CP16)*ERE2 +
     5     (-90.*CP12*CP4 - 360.*CP1*CP2*CP3 -720.*CP13*CP3 -90.*CP23 -
     6     1620.*CP12*CP22 -2250.*CP14*CP2 - 540.*CP16)*ERE3 +
     7     (480.*CP13*CP3 + 1080.*CP12*CP22 + 3600.*CP14*CP2 +
     8     1560.*CP16)*ERE4 + (-1800.*CP14*CP2 - 1800.*CP16)*ERE5 +
     9     720.*CP16*ERE6
C****
      RETURN
      END
C
