C
      SUBROUTINE MITRAY_EDIPOL(DATA,XPOS,EFLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine for electric dipole, in GEANT implementation of       C
C     MIT-RAYTRACE                                                     C
C     adapted from:                                                    C
C     Subroutine EDIPL (NO, NP, T, TP, NUM)  by S. Kowalski            C
C                                                                      C
C      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C
C     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C
C     T = TIME                                                         C
C                                                                      C
C     Input:  DATA(i)    array containing parameters of the            C
C                        electric dipole                               C
C             XPOS(i),   i=1,3  contain the X,Y,Z coordinates of the   C
C                        field point, in the A-axis coordinate system  C
C                        of the electric dipole.                       C
C                                                                      C
C     Output: EFLD(i),   i=1,3  contain the electric field components  C
C                        Ex, Ey, Ez at the specified field point, in   C
C                        the A-axis coordinate system of the E-dipole. C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8  LF1, LF2, LU1, K
      REAL*8  DATA(75), EFLD(3),XPOS(3)
C
      DIMENSION TC(6), DTC(6), DS(6), ES(6)
      CHARACTER REGION*2
      EXTERNAL MITRAY_EDIP
C
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY11/  EX, EY, EZ, QMC, IVEC
      COMMON  /MITRAY20/  EC2, EC4, WE, WC
      COMMON  /MITRAY22/  D, DG, S, EF, ET, DUM
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IDUM1, IDUM2
      COMMON  /MITRAY_EDIPO/A,B,PHI
C
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'         ! local
C
C     S.Yen addition  July 31, 1999
C     Extract coordinates of the field point in the
C     A-axis coordinate system of the electric dipole
C
C	  LDIAG=.true.
 	  LDIAG=.false.
	  XA=XPOS(1)
      YA=XPOS(2)
      ZA=XPOS(3)
      REGION=' '
      IF(LDIAG)THEN
	 WRITE(lout,900)XA,YA,ZA
900      FORMAT(/1X,50('-')/' ENTER SUBROUTINE EDIPOLE'/
     1   ' XA,YA,ZA=',3F10.3/)
      ENDIF
C
C     Extract the parameters for the electric dipole
C
      LF1  = DATA(  1 )
      LU1  = DATA(  2 )
      LF2  = DATA(  3 )
      DG   = DATA(  4 )
      A    = DATA( 11 )
      B    = DATA( 12 )
      D    = DATA( 13 )
      RB   = DATA( 14 )
      EFF  = DATA( 15 )
      PHI  = DATA( 16 )
      EC2  = DATA( 17 )
      EC4  = DATA( 18 )
      WE   = DATA( 19 )
      WC   = DATA( 20 )
      Z11  = DATA( 25 )
      Z12  = DATA( 26 )
      Z21  = DATA( 27 )
      Z22  = DATA( 28 )
C
C     SY Addition Sept 20/99
C     We must define the bounds of the entrance fringe field
C     in the XB axis system.  The entrance fringe field region is
C     defined by ( XBMIN < XB < XBMAX )  &  ( Z12 < ZB < Z11 )
C     If the field point is outside the gap width between the 
C     electrodes, then we assume that the field is zero.
C
      XBMAX=(0.5*D)
      XBMIN=-XBMAX
C
C     Similarly the bounds of the exit fringe field are defined in
C     the XC axis system.  The exit fringe field is defined by
C     ( XCMIN < XC < XCMAX ) & ( Z21 < ZC < Z22 )
C
      XCMAX=XBMAX
      XCMIN=XBMIN
C
C     SY Addition Sept 20/99
C     First zero the E-field components in case we need to abort
C
      DO I=1,3
	EFLD(I)=0.
      ENDDO
C
      IF (WE .EQ. 0.) WE = 1000. * RB
      BX = 0.
      BY = 0.
      BZ = 0.
      EX = 0.
      EY = 0.
      EZ = 0.
      ET = 0.
      S = 0.
C
C     TRANSFORM FROM INITIAL ENTRANCE (A-SYSTEM) COORDINATES TO ENTRANCE
C     EFB (B-SYSTEM) COORDINATES.
C
      XB =  -  XA
      YB = YA
      ZB = ( A-ZA )
C
C     TRANSFORM FROM THE B-SYSTEM COORDINATES TO THE EXIT EFB (C-SYSTEM)
C     COORDINATES.
C
C     (These are the same equations used for B-->C tranformation for
C      the magnetic dipole, but with ALPHA=BETA=0)
C
      COPAB =COS( (PHI)/57.29578)
      SIPAB =SIN( (PHI)/57.29578)
      COSPB =COS( (PHI/2.)/57.29578 )
      SINPB =SIN( (PHI/2.)/57.29578 )
      SIP2 =SIN( (PHI/2.)/57.29578 )
      ZC = - ZB  *COPAB +  XB  *SIPAB -2.*RB*SIP2*COSPB
      XC = - ZB  *SIPAB -  XB  *COPAB -2.*RB*SIP2*SINPB
      YC = YB
C
C     Print out coordinates if in diagostic mode      
C
      IF(LDIAG)THEN
	  WRITE(lout,901)XB,YB,ZB
901       FORMAT(' XB,YB,ZB=',3F10.3)
	  WRITE(lout,902)XC,YC,ZC
902       FORMAT(' XC,YC,ZC=',3F10.3)
	  WRITE(lout,903)Z11,Z12,Z21,Z22,XBMIN,XBMAX,XCMIN,XCMAX
903       FORMAT(' Z11=',F10.3,'  Z12=',F10.3,
     1    '  Z21=',F10.3,'  Z22=',F10.3/
     2    ' XBMIN=',F10.3,'  XBMAX=',F10.3,
     3    ' XCMIN=',F10.3,'  XCMAX=',F10.3)
	  WRITE(lout,*)' '
      ENDIF
C
C     Now determine which region we are in.  Choices are:
C     Before start of entrance fringe field ("entrance far field") IN=-99
C     entrance fringe field region  (IN=1)
C     "uniform" field region (IN=2)
C     exit fringe field region (IN=3)
C     after the end of the exit fringe field ("exit far field") IN=+99
C     Because of the possibility of confusing entrance and exit regions,
C     we first check for entrance/exit fringe field, then uniform field,
C     then entrance/exit far field regions, to make sure that we get the
C     most important regions first.
C
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN
C
      IF(ZB.LE.Z11 .AND. ZB.GT.Z12 .AND. XB.GE.XBMIN .AND.
     1   XB.LE.XBMAX)THEN
C
C        *************************
C        *                       *
C        * ENTRANCE FRINGE FIELD *
C        *                       *
C        *************************
C
C        Entrance fringe field region, B-axis coordinates are used.
C
	IN = 1
	XC_OFFSET = RB
	ZC_OFFSET = 0.0
	EF = EFF
C
C       Get Enge coefficients Cn for entrance fringe field.      
C
	C0   = DATA(29)
	C1   = DATA(30)
	C2   = DATA(31)
	C3   = DATA(32)
	C4   = DATA(33)
	C5   = DATA(34)
C
C       Load the B-axis coordinates into TC(1), TC(2), TC(3), because this 
C       is where the subroutine EDIP expects to find the coordinates
C
	TC(1)=XB
	TC(2)=YB
	TC(3)=ZB
C
C       Call subroutine EDIP to calculate the E-field components
C
	CALL MITRAY_EDIP
C
C       The E-field components EX, EY, EZ have been passed back
C       via common block /MITRAY11/, and are in the B-axis system.
C       Transform the E-field components back to the A-axis system.
C       Actually there is no change, since the A-axis and B-axis systems
C       are related by a simple translation for the electrostatic dipole,
C       with no rotation.
C
	EXA=EX
	EYA=EY
	EZA=EZ
C
C       Print out diagnostics if required
C
	IF(LDIAG)THEN
	   WRITE(lout,*)'ENTRANCE FRINGE FIELD REGION'
	   WRITE(lout,*)'B SYSTEM',EX,EY,EZ
	   WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	   WRITE(lout,*)'ENTRANCE FRINGE FIELD REGION'
	   WRITE(lout,904)'B SYSTEM',EX,EY,EZ
	   WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
904        FORMAT(1X,A,'  EX,EY,EZ=',3F10.3)
	ENDIF
C
C       Load EXA, EYA, EZA into array EFLD and exit.
C
	EFLD(1)=EXA
	EFLD(2)=EYA
	EFLD(3)=EZA
	RETURN
C
C     -------------------------------------
C
      ELSE IF(ZC.GT.Z21 .AND. ZC.LE.Z22 .AND. XC.GE.XCMIN
     1        .AND. XC.LE.XCMAX)THEN
C
C        *********************
C        *                   *
C        * EXIT FRINGE FIELD *
C        *                   *
C        *********************
C
C        SET INDICATOR IN=3 for exit fringe field
C
	 IN=3
         XC_OFFSET=-RB     ! ADDED NOV 23/99
         ZC_OFFSET=0.      ! ADDED NOV 23/99
         EF=-EFF           ! ADDED NOV 23/99
C
C        Get Enge coefficients Cn for the exit fringe field      
C
	 C0   = DATA(35)
	 C1   = DATA(36)
	 C2   = DATA(37)
	 C3   = DATA(38)
	 C4   = DATA(39)
	 C5   = DATA(40)
C     
C        LOAD THE C AXIS COORDINATES INTO ARRAY TC, BECAUSE THIS IS
C        WHERE SUBROUTINE BDIP EXPECTS TO FIND THEM.
C
	 TC(1)=XC
	 TC(2)=YC
	 TC(3)=ZC
C
C        Call subroutine EDIP to calculate the E-field components
C        in the C-axis system
C
	 CALL MITRAY_EDIP
C
C        The electric field components Ex, Ey, Ez are in the C-axis
C        system.  Transform them to the A-axis system.
C
	 CALL MITRAY_EDIP_ECTOEA(EX,EY,EZ,EXA,EYA,EZA)
C
C        Print out diagnostics if required
C
	 IF(LDIAG)THEN
	    WRITE(lout,*)'EXIT FRINGE FIELD REGION'
	    WRITE(lout,904)'C SYSTEM',EX,EY,EZ
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	    WRITE(lout,*)'EXIT FRINGE FIELD REGION'
	    WRITE(lout,904)'C SYSTEM',EX,EY,EZ
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	 ENDIF
C
C       Load EXA, EYA, EZA into array EFLD and exit.
C
	EFLD(1)=EXA
	EFLD(2)=EYA
	EFLD(3)=EZA
	RETURN
C
C       --------------------------------------
C
      ELSE IF(ZB.LE.Z12 .AND. ZC.LE.Z21)THEN
C
C        ************************
C        *                      *
C        * UNIFORM FIELD REGION *
C        *                      *
C        ************************
C      
C        Set indicator IN=2 for uniform field region
C
	 IN = 2
	 XC_OFFSET = -RB
	 ZC_OFFSET = 0.0
	 EF = -EFF
	 S  = 0.
C      
C        LOAD THE C-AXIS COORDINATES INTO ARRAY TC BECAUSE THIS
C        IS WHERE SUBROUTINE EDIP EXPECTS TO FIND THEM
C
	 TC(1)=XC
	 TC(2)=YC
	 TC(3)=ZC
C
C        CALL SUBROUTINE EDIP TO CALCULATE THE E-FIELD COMPONENTS IN
C        THE C-AXIS SYSTEM
C
	 CALL MITRAY_EDIP
C
C        EX, EY, EZ ARE THE E-FIELD COMPONENTS IN THE C-SYSTEM.
C        TRANSFORM THEM TO THE A-SYSTEM
C
	 CALL MITRAY_EDIP_ECTOEA(EX,EY,EZ,EXA,EYA,EZA)
C
C        Print out diagnostics if required
C
	 IF(LDIAG)THEN
	    WRITE(lout, *)'UNIFORM FIELD REGION'
	    WRITE(lout,904)'C SYSTEM',EX,EY,EZ
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	    WRITE(lout,*)'UNIFORM FIELD REGION'
	    WRITE(lout,904)'C SYSTEM',EX,EY,EZ
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	 ENDIF
C
C        Load EXA, EYA, EZA into array EFLD and exit.
C
	 EFLD(1)=EXA
	 EFLD(2)=EYA
	 EFLD(3)=EZA
	 RETURN
C
C     -----------------------------------------
C
      ELSE IF(ZB.GT.Z11 .AND. XB.GE.XBMIN .AND. 
     1        XB.LE.XBMAX)THEN
C
C        **********************
C        *                    *
C        * ENTRANCE FAR FIELD *
C        *                    *
C        **********************
C        
C        SET INDICATOR IN=-99
C
	 IN=-99
	 EXA=0.
	 EYA=0.
	 EZA=0.
C
C        PRINT DIAGNOSTICS IF REQUIRED
C
	 IF(LDIAG)THEN
	    WRITE(lout,*)'DIPOLE ENTRANCE FAR FIELD REGION'
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	    WRITE(lout,*)'DIPOLE ENTRANCE FAR FIELD REGION'
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	 ENDIF
C
C        LOAD E-FIELD COMPONENTS INTO ARRAY EFLD AND EXIT
C
	 EFLD(1)=EXA
	 EFLD(2)=EYA
	 EFLD(3)=EZA
	 RETURN
C
C        -----------------------------------------
C
      ELSE IF(ZC.GT.Z22 .AND. XC.GT.XCMIN .AND. 
     1          XC.LE.XCMAX)THEN
C
C           ******************
C           *                *
C           * EXIT FAR FIELD *
C           *                *
C           ******************
C
C        SET INDICATOR IN=+99
C
	 IN=+99
	 EXA=0.
	 EYA=0.
	 EZA=0.
C
C        PRINT DIAGNOSTICS IF REQUIRED
C
	 IF(LDIAG)THEN
	    WRITE(lout,*)'DIPOLE EXIT FAR FIELD REGION'
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	    WRITE(lout,*)'DIPOLE EXIT FAR FIELD REGION'
	    WRITE(lout,904)'A SYSTEM',EXA,EYA,EZA
	 ENDIF
C
C        LOAD E-FIELD COMPONENTS INTO ARRAY EFLD AND EXIT
C
	 EFLD(1)=EXA
	 EFLD(2)=EYA
	 EFLD(3)=EZA
	 RETURN
C
C      ----------------------------------------------------
C
      ELSE
C        UNSPECIFIED FIELD REGION, RETURN WITH ZERO FIELD COMPONENTS
C
	 IF(LDIAG)THEN
C	IF (1 .eq. 2)THEN
	    WRITE(lout,*)'UNKNOWN ELECTIC DIPOLE REGION'
	    WRITE(lout,905)'A SYSTEM XA,YA,ZA=',XA,YA,ZA
	    WRITE(lout,905)'B SYSTEM XB,YB,ZB=',XB,YB,ZB
	    WRITE(lout,905)'C SYSTEM XC,YC,ZC=',XC,YC,ZC
	    WRITE(lout,*)'RETURN E-FIELD EXA=0, EYA=0, EZA=0'
905         FORMAT(1X,A,3F12.4)
	    WRITE(lout,*)'UNKNOWN ELECTIC DIPOLE REGION'
	    WRITE(lout,905)'A SYSTEM XA,YA,ZA=',XA,YA,ZA
	    WRITE(lout,905)'B SYSTEM XB,YB,ZB=',XB,YB,ZB
	    WRITE(lout,905)'C SYSTEM XC,YC,ZC=',XC,YC,ZC
	    WRITE(lout,*)'RETURN E-FIELD EXA=0, EYA=0, EZA=0'
C
C           SET ALL E-FIELD COMPONENTS TO ZERO
C
	    EFLD(1)=0
	    EFLD(2)=0
	    EFLD(3)=0
	    RETURN
         ENDIF
      ENDIF
      RETURN
      END      
C
C=========================================================================
C
      SUBROUTINE MITRAY_EDIP
C****
C**** CALCULATES E-FIELD COMPONENTS FOR A CYLINDRICAL
C**** ELECTROSTATIC DEFLECTOR
C****
      IMPLICIT REAL*8 (A-H, O-Z)
C
      REAL*8 K
C
      include 'gcunit.inc'          !geant
C
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY11/  EX, EY, EZ, QMC, IVEC
      COMMON  /MITRAY20/  EC2, EC4, WE, WC
      COMMON  /MITRAY22/  D, DG, S, EF, ET, DUM
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IDUM1, IDUM2
      DIMENSION TC(6), DTC(6)
C****
C****
100   FORMAT( ' ** ERROR ** -GO TO-  IN SUBROUTINE MITRAY_EDIP', 
     1  /'               INVALID VALUE  IN = ', I5 //)
101   FORMAT( ' **ERROR ** IN SUBROUTINE MITRAY_EDIP'/   
     1   '               X=', F12.3,  '  XC_OFFSET=', F12.3 //)
C****
C****
C       ADDED BY SY OCT 30/99  INITIAL VALUES OF E-FIELD        
C
	EX=0.
	EY=0.
	EZ=0.
C
	X = TC(1)
	Y = TC(2)
	Z = TC(3)
	DX = X - XC_OFFSET
	RP2 = DX * DX + Z * Z
	GO TO (1, 2, 1) , IN
	PRINT 100, IN
C       ADDED BY SY  OCT 30/99 RETURN WITH ZERO FIELD IN CASE OF ILLEGAL
C       VALUE OF 'IN'
	RETURN
C****
C****   UNIFORM FIELD REGION   IN=2
C****
2       EX = EF * RB * DX / RP2
	EY = 0.
	EZ = EF * RB * Z / RP2
	ET = DSQRT(EX * EX + EZ * EZ)
	RETURN
C****
C****   FRINGE FIELD REGION    IN=1 OR IN=3
C****
1     RP = DSQRT(RP2)
      IF( DABS(X) .LT. DABS(XC_OFFSET) ) GO TO 3
      PRINT 101, X, XC_OFFSET
      CALL EXIT
3     DR = RP-RB
      SINT = Z/RP
      COST = DABS(XC_OFFSET-X)/RP
      THETA = DASIN(SINT)
      S = THETA*RB/D + EC2*Y*Y/(WE*WE) + EC4*(Y/WE)**4
      CALL MITRAY_EDPP( D, S, RE, G1, G2, G3, G4, G5, G6 )
      DRR  = DR/RB
      DRR2 = DRR*DRR
      DRR3 = DRR2*DRR
      DRR4 = DRR3*DRR
      EFR = EF*RB*( RE-DRR2*G2/2.+DRR3*G2/2.+DRR4*(G4-11.*G2)/24.)/RP
      EFT = EF*RB*( DRR*G1-DRR2*G1/2.+DRR3*(2.*G1-G3)/6.-
     1                     DRR4*(G1-G3)/4.)/RP
      EX = EFR*COST - EFT*SINT
      EZ = EFR*SINT + EFT*COST
      ET = DSQRT( EX * EX + EY * EY + EZ * EZ)
      IF( IN .EQ. 1 ) EZ = -EZ
      RETURN
      END      
C
C========================================================================
C
      SUBROUTINE MITRAY_EDPP( D, S, RE, G1, G2, G3, G4, G5, G6 )
C****
C**** CALCULATE S; DETERMINE E-FIELD IN FRINGE REGIONS
C****
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8 K
C
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
C
      S2 = S*S
      S3 = S2*S
      S4 = S2*S2
      S5 = S4*S
      CS = C0 + C1*S + C2*S2 + C3*S3 + C4*S4 + C5*S5
      RBD = RB/D
      CP1 =(C1 + 2.*C2*S + 3.*C3*S2 + 4.*C4*S3 + 5.*C5*S4)*RBD
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S2 + 20.*C5*S3  ) *RBD*RBD
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S2 ) *RBD**3
      CP4 = ( 24.*C4 + 120.*C5*S ) *RBD**4
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
      CP12 = CP1*CP1
      CP13 = CP1*CP12
      CP14 = CP12*CP12
      CP22 = CP2*CP2
C****
      G1 = -CP1*ERE1
C****
      G2 =-( CP2+CP12   )*ERE1    + 2.*CP12 * ERE2
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE1      +
     1   6.*(CP1*CP2 + CP13)*ERE2 - 6.*CP13*ERE3
C****
1     G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE1  +
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2    -
     2   36.*(CP12*CP2 + CP14)*ERE3       + 24.*CP14*ERE4
      RETURN
      END
C
C============================================================================
C
      SUBROUTINE MITRAY_EDIP_ECTOEA(EXC,EYC,EZC,EXA,EYA,EZA)
C
C     TRANSFORM E-FIELD COMPONENTS FROM SYSTEM C TO SYSTEM A COORDINATES
C     OF AN ELECTOSTATIC DIPOLE.
C
C     INPUT:  EXC, EYC, EZC     E-FIELD COMPONENTS IN C-AXIS SYSTEM
C     OUTPUT: EXA, EYA, EZA     E-FIELD COMPONENTS IN A-AXIS SYSTEM
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON /MITRAY_EDIPO/A,B,PHI
C
C     We use here the same formulae as for the magnetic dipole, but
C     with ALPHA=BETA=0
C
      COPAB = DCOS(-PHI/57.29577951D0)
      SIPAB = DSIN(-PHI/57.29577951D0)
C
C     Next 3 lines not needed
C
C     COSPB = DCOS(-PHI/2. /57.29577951D0)
C     SINPB = DSIN(-PHI/2. /57.29577951D0)
C     SIP2 = DSIN(-PHI/2. /57.29577951D0)
C
C    NOW ROTATE TO GET E-FIELD COMPONENTS IN B-SYSTEM, IN TERMS OF C-SYSTEM
C
      EZB = -EZC * COPAB + EXC* SIPAB
      EXB = -EZC * SIPAB - EXC * COPAB
      EYB = EYC
C
C     A-axis and B-axis systems are related by simple translation and
C     no rotation for the electrostatic dipole, so the E-field components
C     are the same in both B-axis and A- axis systems.
C
      EXA=EXB
      EYA=EYB
      EZA=EZB
      RETURN
      END
C
