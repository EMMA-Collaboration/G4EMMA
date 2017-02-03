C***********************************************************************
C                          DIPOLE SUBROUTINES
C***********************************************************************
C
	SUBROUTINE MITRAY_DIPO_ATOB(XA,YA,ZA,XB,YB,ZB)
C
C	TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.
C	I.E. FROM SYSTEM A TO SYSTEM B OF STANDARD LAYOUT, FOR A DIPOLE.
C	ALPHA IS THE POLEFACE ROTATION ANGLE OF THE ENTRANCE FACE
C
C	INPUT:  XA, YA, ZA
C	OUTPUT: XB, YB, ZB
C	
	IMPLICIT REAL*8(A-H,O-Z)
C
	COMMON/MITRAY_DIPO/A,B,PHI,ALPHA,BETA,XCR1,XCR2
C
	COSA = DCOS( ALPHA/57.29577951D0 )
	SINA = DSIN( ALPHA/57.29577951D0 )
C
	XB = (A-ZA)*SINA - XA*COSA
	YB = YA
	ZB = (A-ZA)*COSA + XA*SINA
C
C	COORDINATE SYSTEM B IS DISPLACED BY XCR1 IN THE +XB DIRECTION
C
	XB=XB-XCR1
C
C	ALL COORDINATES ARE NOW IN TERMS OF COORDINATE SYSTEM B
C
	RETURN
	END
C
C=======================================================================
C
	SUBROUTINE MITRAY_DIPO_BTOC(XB,YB,ZB,XC,YC,ZC)
C
C	TRANFORM FROM SYSTEM B TO SYSTEM C COORDINATES OF A DIPOLE
C
	IMPLICIT REAL*8(A-H,O-Z)
C
	COMMON/MITRAY_DIPO/A,B,PHI,ALPHA,BETA,XCR1,XCR2
	COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
C
	COPAB = DCOS( (PHI-ALPHA-BETA)/57.29577951D0)
	SIPAB = DSIN( (PHI-ALPHA-BETA)/57.29577951D0)
	COSPB = DCOS( (PHI/2.-BETA)/57.29577951D0 )
	SINPB = DSIN( (PHI/2.-BETA)/57.29577951D0 )
	SIP2  = DSIN( (PHI/2.)/57.29577951D0 )
C
C	THE ORIGIN OF THE B SYSTEM IS DISPLACED FROM THE CENTRAL RAY
C	BY AMOUNT XCR1 ALONG THE +XB AXIS
C
	XT = XB
	ZT = ZB
C
C	TRANSFORM BACK TO B COORDINATE SYSTEM CENTERED ON CENTRAL RAY
C
	XT=XT+XCR1
C
C	NOW ROTATE/TRANSLATE TO GET COORDINATES IN TERMS OF C SYSTEM.
C
	ZC = - ZT  *COPAB + XT  *SIPAB -2.*RB*SIP2*COSPB
	XC = - ZT  *SIPAB - XT  *COPAB -2.*RB*SIP2*SINPB
C
C	SHIFT C COORDINATE AXES FROM CENTRAL RAY BY AMOUNT XCR2 ALONG
C	+XC AXIS
C
	XC=XC-XCR2
	YC=YB
C
C	COORDINATES ARE NOW IN TERMS OF THE NEW, SHIFTED C SYSTEM.
C
	RETURN
	END
C
C=======================================================================
C
	SUBROUTINE MITRAY_DIPO_BBTOBA(BXB,BYB,BZB,BXA,BYA,BZA)
C
C	SUBROUTINE TO TRANSFORM B FIELD FROM A TO B COORD SYSTEMS 
C       OF A DIPOLE, I.E. FROM SYSTEM A TO SYSTEM B OF STANDARD LAYOUT.
C	ALPHA IS THE POLEFACE ROTATION ANGLE OF THE ENTRANCE FACE
C
C	INPUT:  BXB, BYB, BZB 
C	OUTPUT: BXA, BYA, BZA
C
	IMPLICIT REAL*8(A-H,O-Z)
C
	COMMON/MITRAY_DIPO/A,B,PHI,ALPHA,BETA,XCR1,XCR2
C
	COSA = DCOS(-ALPHA/57.29577951D0)
	SINA = DSIN(-ALPHA/57.29577951D0)
C
	BXA = -BZB*SINA - BXB*COSA
	BYA = BYB
	BZA = -BZB*COSA + BXB*SINA
C
C	COORDINATE SYSTEM B IS DISPLACED BY XCR1 IN THE +XB DIRECTION
C	ALL B-FIELD COMPONENTS ARE NOW IN TERMS OF COORDINATE SYSTEM A
C
	RETURN
	END
C
C=======================================================================
C
	SUBROUTINE MITRAY_DIPO_BCTOBB(BXC,BYC,BZC,BXB,BYB,BZB)
C
C	TRANFORM B-FIELD COMPONENTS FROM SYSTEM C TO SYSTEM B COORDINATES
C       OF A DIPOLE.
C
C	INPUT:  BXC, BYC, BZC   B-FIELD IN C-AXIS SYSTEM
C	OUTPUT: BXB, BYB, BZB   B-FIELD IN B-AXIS SYSTEM
C
	IMPLICIT REAL*8(A-H,O-Z)
C
	COMMON/MITRAY_DIPO/A,B,PHI,ALPHA,BETA,XCR1,XCR2
C
	COPAB = DCOS(-(PHI-ALPHA-BETA)/57.29577951D0)
	SIPAB = DSIN(-(PHI-ALPHA-BETA)/57.29577951D0)
	COSPB = DCOS(-(PHI/2.-BETA)/57.29577951D0 )
	SINPB = DSIN(-(PHI/2.-BETA)/57.29577951D0 )
	SIP2 =  DSIN(-(PHI/2.)/57.29577951D0 )
C
C	NOW ROTATE TO GET COORDINATES IN TERMS OF C SYSTEM.
C
	BZB = - BZC  *COPAB + BXC  *SIPAB 
	BXB = - BZC  *SIPAB - BXC  *COPAB
	BYB=BYC
C
C	B-FIELD COMPONENTS ARE NOW IN TERMS OF THE B SYSTEM.
C
	RETURN
	END
C
C=======================================================================
C
      SUBROUTINE MITRAY_DIPOLE(DATA,XPOS,BFLD)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine for dipole, in GEANT implementation of MIT-RAYTRACE   C
C     adapted from:                                                    C
C     Subroutine DIPOLE ( NO, NP, T, TP ,NUM ) by S. Kowalski          C
C                                                                      C
C      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C
C     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8  LF1, LF2, LU1, K, NDX
      REAL*8 DATA(75),BFLD(3),XPOS(3)
C
      DIMENSION TC(6), DTC(6)
      CHARACTER REGION*2
      EXTERNAL MITRAY_BDIP
C
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      COMMON /MITRAY_AXES/XA,YA,ZA,XB,YB,ZB,XC,YC,ZC
      COMMON /MITRAY_BOUNDS/XBMIN,XBMAX,XCMIN,XCMAX
      COMMON /MITRAY_DIPO/A,B,PHI,ALPHA,BETA,XCR1,XCR2
C
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'	    !local
      include 'diagnostic.inc'      !local
C
C     SY  Addition Feb 10/98
C     Extract coordinates of the field point,
C     in the A-axis system of the device.
C

	LDIAG=.false.
      XA=XPOS(1)
      YA=XPOS(2)
      ZA=XPOS(3)
C
      REGION=' '
      IF(LDIAG)THEN
         WRITE(lout,900)XA,YA,ZA
900      FORMAT(/1X,50('-')/' ENTER SUBROUTINE DIPOLE'/
     1   ' XA,YA,ZA=',3F10.3/)
      ENDIF
C
C     EXTRACT THE PARAMETERS FOR THE DIPOLE MAGNET
C
      IR = 0
      LF1  = DATA(  1)
      LU1  = DATA(  2)
      LF2  = DATA(  3)
      DG   = DATA(  4)
      MTYP = DATA(  5)
      IMAP = DATA(  6)
      A    = DATA( 11)
      B    = DATA( 12)
      D    = DATA( 13)
      RB   = DATA( 14)
      BF   = DATA( 15)
      PHI  = DATA( 16)
      ALPHA= DATA( 17)
      BETA = DATA( 18)
      NDX  = DATA( 19)
      BET1 = DATA( 20)
      GAMA = DATA( 21)
      DELT = DATA( 22)
      Z11  = DATA( 25)
      Z12  = DATA( 26)
      Z21  = DATA( 27)
      Z22  = DATA( 28)
      BR1  = DATA( 41)
      BR2  = DATA( 42)
      XCR1 = DATA( 43)
      XCR2 = DATA( 44)
      WDE  = DATA( 49)
      WDX  = DATA( 50)
C
C     SY  Addition Feb 10/98
C     These parameters define the bounds of the entrance fringe
C     field, in the XB axis system.  The entrance fringe field
C     is defined by XBMIN < XB < XBMAX  .AND.  Z12 < ZB < Z11
C
      XBMAX=WDE/2.
      XBMIN=-XBMAX
C
C     Similarly, the bounds of the exit fringe field region are
C     defined in the XC axis system.  The exit fringe field is 
C     defined by XCMIN < XC < XCMAX .AND. Z21 < ZC < Z22
C
      XCMAX=WDX/2.
      XCMIN=-XCMAX
C
      IF( MTYP .EQ. 0  )  MTYP = 1
C
C     SY  Addition Feb 10/98
C     We first zero the B-field components in case we need to abort
C
      DO I=1,3
         BFLD(I)=0.
      ENDDO
C
      BX = 0.
      BY = 0.
      BZ = 0.
      BT = 0.
      S = 0.
      BR = BR1
C
C     Transform from the A to the B coordinate system
      CALL MITRAY_DIPO_ATOB(XA,YA,ZA,XB,YB,ZB)
C     Transform from the B to the C coordinate system
      CALL MITRAY_DIPO_BTOC(XB,YB,ZB,XC,YC,ZC)
C
C     Print out coordinates if diagnostic mode
C
      IF(LDIAG)THEN
         WRITE(lout,901)XB,YB,ZB
901         FORMAT(' XB,YB,ZB=',3F10.3)
         WRITE(lout,902)XC,YC,ZC
902         FORMAT(' XC,YC,ZC=',3F10.3)
         WRITE(lout,903)Z11,Z12,Z21,Z22,XBMIN,XBMAX,XCMIN,XCMAX
903         FORMAT(' Z11=',F10.3,'  Z12=',F10.3,
     1      '  Z21=',F10.3,'  Z22=',F10.3/
     2      ' XBMIN=',F10.3, '  XBMAX=',F10.3,
     3      '  XCMIN=',F10.3,'  XCMAX=',F10.3)
         WRITE(lout,*)' '
      END IF
C
C   Now determine what region we are in -- 
C   Before start of the entrance fringe field ("entrance far field", IN=-99 )
C   entrance fringe field (IN=1), 
C   "uniform" field region (IN=2), 
C   exit fringe field region (IN=3),
C   after the end of the exit fringe field ("exit far field", IN=+99)
C   Because of the possibility of confusing entrance and exit regions, we
C   first check for entrance/exit fringe field, then uniform field, then
C   entrance/exit far field regions, to be sure that we get the most
C   important regions first
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
         IR = 1
         IN = 1
         XC_OFFSET= RB*DCOS( ALPHA/ 57.29578 )
         ZC_OFFSET=-RB*DSIN( ALPHA/ 57.29578 )
C
C        Load the B-axis coordinates into TC(1),TC(2),TC(3), because this
C        is where subroutine BDIP expects to find the coordinates.
C
         TC(1)=XB
         TC(2)=YB
         TC(3)=ZB
C
         C0   = DATA( 29 )
         C1   = DATA( 30 )
         C2   = DATA( 31 )
         C3   = DATA( 32 )
         C4   = DATA( 33 )
         C5   = DATA( 34 )
         DELS = DATA( 45 )
         RCA  = DATA( 47 )
         WDIP = DATA( 49 )
C
C        S2...S8 are the coefficients for the entrance face curvature
C        SY Feb 20/98   Calculate powers of RB, use DATA(57)/RB4/RB3 
C        instead of RB**7 to avoid exponent overflow in case of large RB
C
         RB2 = RB**2
         RB3 = RB2*RB
         RB4 = RB3*RB
         S2   = DATA( 51 ) / RB    + RCA/2.D0
         S3   = DATA( 52 ) / RB2
         S4   = DATA( 53 ) / RB3 + RCA**3/8.D0
         S5   = DATA( 54 ) / RB4
         S6   = DATA( 55 ) / RB3/RB2 + RCA**5/16.D0
         S7   = DATA( 56 ) / RB3/RB3
         S8   = DATA( 57 ) / RB4/RB3 + RCA**7/25.6D0
C
C        CHECK IF WE HAVE A FLAT BOUNDARY
C                 NSRF=0 FLAT
C                     =1 CURVED
C
         NSRF = 1
         IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
C
C        Call BDIP to calculate the B-field components
C
         CALL MITRAY_BDIP
C
C	BX, BY, BZ ARE IN B-AXIS SYSTEM;  TRANSFORM TO A-AXIS SYSTEM
C
         CALL MITRAY_DIPO_BBTOBA(BX,BY,BZ,BXA,BYA,BZA)
C
         IF(LDIAG)THEN
           WRITE(lout,*)'ENTRANCE FRINGE FIELD REGION'
           WRITE(lout,904)'B SYSTEM',BX,BY,BZ
           WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
904        FORMAT(1X,A,'  BX,BY,BZ=',3F10.3)
        END IF
        BFLD(1)=BXA
        BFLD(2)=BYA
        BFLD(3)=BZA
        RETURN
C
C        -------------------------
C
      ELSE IF(ZC.GT.Z21 .AND. ZC.LE.Z22 .AND. XC.GE.XCMIN .AND.
     1   XC.LE.XCMAX)THEN
C
C        *********************
C        *                   *
C        * EXIT FRINGE FIELD *
C        *                   *
C        *********************
C
C        Exit fringe field region, C-axis coordinates are used.
C        Setup for second fringe field and integration
C        IN=3 designates exit fringe field

         IN=3
         IR=2
         XC_OFFSET=-RB*DCOS( BETA / 57.29577951D0 )
         ZC_OFFSET=-RB*DSIN( BETA / 57.29577951D0 )
C
C        Load the C axis coordinates into array TC, because this is
C        where subroutine BDIP expects to find the coordinates.
C
         TC(1)=XC
         TC(2)=YC
         TC(3)=ZC
C
         BR   = BR2
C        C0,...,C5 are the expansion coefficients for the exit fringe field
         C0   = DATA( 35 )
         C1   = DATA( 36 )
         C2   = DATA( 37 )
         C3   = DATA( 38 )
         C4   = DATA( 39 )
         C5   = DATA( 40 )
         DELS = DATA( 46 )
C        RCA is inverse radius of curvature of exit boundary
         RCA  = DATA( 48 )
         WDIP = DATA( 50 )
C
C        S2...S8 are expansion coefficients for shape of exit
C        face of dipole
C        SY Feb 20, 1998  Calculate powers of RB, use DATA(64)/RB4/RB3 
C        to avoid calculating RB**7, which could suffer exponent overflow
C        in case of very large RB value, as in clamshell dipole.
C
         RB2=RB**2
         RB3=RB2*RB
         RB4=RB3*RB
         S2   = DATA( 58 ) / RB    + RCA/2.D0
         S3   = DATA( 59 ) / RB2
         S4   = DATA( 60 ) / RB3 + RCA**3/8.D0
         S5   = DATA( 61 ) / RB4
         S6   = DATA( 62 ) / RB3/RB2 + RCA**5/16.D0
         S7   = DATA( 63 ) / RB3/RB3
         S8   = DATA( 64 ) / RB4/RB3 + RCA**7/25.6D0
C
C        CHECK IF WE HAVE A FLAT BOUNDARY
C                 NSRF=0 FLAT
C                     =1 CURVED
C
         NSRF = 1
         IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
C
C        Call BDIP to calculate magnetic field components
C
         CALL MITRAY_BDIP
C
C        BX, BY, BZ ARE IN C-AXIS SYSTEM;  FIRST TRANSFORM TO B-AXIS SYSTEM
C
         CALL MITRAY_DIPO_BCTOBB(BX,BY,BZ,BXB,BYB,BZB)
C
C        THEN TRANSFORM FROM B TO A-AXIS SYSTEM
C
         CALL MITRAY_DIPO_BBTOBA(BXB,BYB,BZB,BXA,BYA,BZA)
C
         IF(LDIAG)THEN
           WRITE(lout,*)'EXIT FRINGE FIELD REGION'
           WRITE(lout,904)'C SYSTEM',BX,BY,BZ
           WRITE(lout,904)'B SYSTEM',BXB,BYB,BZB
           WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
         END IF
         BFLD(1)=BXA
         BFLD(2)=BYA
         BFLD(3)=BZA
         RETURN
C
C        -------------------------
C
      ELSE IF(ZB.LE.Z12 .AND. ZC.LE.Z21)THEN
C
C        ************************
C        *                      *
C        * UNIFORM FIELD REGION *
C        *                      *
C        ************************
C    
C       UNIFORM FIELD REGION;  C-AXIS COORDINATES ARE USED
C
         S = 0.
         IN = 2
         XC_OFFSET=-RB*DCOS( BETA / 57.29577951D0 )
         ZC_OFFSET=-RB*DSIN( BETA / 57.29577951D0 )
C
C        Load the C axis coordinates into array TC, because this is where
C        subroutine BDIP expects to find the coordinates.
         TC(1)=XC
         TC(2)=YC
         TC(3)=ZC
C
         DELS=0.
         SCOR=0.
C
         CALL MITRAY_BDIP
C
C        BX, BY, BZ ARE IN C-AXIS SYSTEM.  TRANSFORM FIRST TO B-AXIS SYSTEM
C
         CALL MITRAY_DIPO_BCTOBB(BX,BY,BZ,BXB,BYB,BZB)
C
C        THEN TRANSFORM FIELD FROM B TO A-AXIS SYSTEM
C
         CALL MITRAY_DIPO_BBTOBA(BXB,BYB,BZB,BXA,BYA,BZA)
C
         IF(LDIAG)THEN
            WRITE(lout,*)'UNIFORM FIELD REGION'
            WRITE(lout,904)'C SYSTEM',BX,BY,BZ
            WRITE(lout,904)'B SYSTEM',BXB,BYB,BZB
            WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
         END IF
         BFLD(1)=BXA
         BFLD(2)=BYA
         BFLD(3)=BZA
C
      ELSE IF(ZB.GT.Z11 .AND. XB.GE.XBMIN .AND.
     1   XB.LE.XBMAX)THEN
C
C        **********************
C        *                    *
C        * ENTRANCE FAR FIELD *
C        *                    *
C        **********************
C
         IN=-99
         BXC=0.
         BYC=0.
         BZC=0.
         BXB=0.
         BYB=0.
         BZB=0.
         BXA=0.
         BYA=0.
         BZA=0.
C
         IF(LDIAG)THEN
            WRITE(lout,*)'DIPOLE ENTRANCE FAR FIELD REGION'
            WRITE(lout,904)'C SYSTEM',BXC,BYC,BZC
            WRITE(lout,904)'B SYSTEM',BXB,BYB,BZB
            WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
         END IF
         BFLD(1)=BXA
         BFLD(2)=BYA
         BFLD(3)=BZA
         RETURN
C
C        -------------------------
C
      ELSE IF(ZC.GT.Z22 .AND. XC.GT.XCMIN .AND. 
     1        XC.LE.XCMAX)THEN
C
C        ******************
C        *                *
C        * EXIT FAR FIELD *
C        *                *
C        ******************
C
         IN=99
         IN=-99
         BXC=0.
         BYC=0.
         BZC=0.
         BXB=0.
         BYB=0.
         BZB=0.
         BXA=0.
         BYA=0.
         BZA=0.
C
         IF(LDIAG)THEN
            WRITE(lout,*)'DIPOLE EXIT FAR FIELD REGION'
            WRITE(lout,904)'C SYSTEM',BXC,BYC,BZC
            WRITE(lout,904)'B SYSTEM',BXB,BYB,BZB
            WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
         END IF
         BFLD(1)=BXA
         BFLD(2)=BYA
         BFLD(3)=BZA
         RETURN
C
C        -------------------------
C
      ELSE
C        UNSPECIFIED FIELD REGION, RETURN WITH ZERO FIELD COMPONENTS
		IF (LDIAG) THEN
         WRITE(lout,*)'UNKNOWN DIPOLE REGION'
         WRITE(lout,904)'C SYSTEM',BXC,BYC,BZC
         WRITE(lout,904)'B SYSTEM',BXB,BYB,BZB
         WRITE(lout,904)'A SYSTEM',BXA,BYA,BZA
         WRITE(lout,*)'!!! Abort current event !!!'
		END IF
         jstop  = 1
         ieotri = 1
         BFLD(1)=0.
         BFLD(2)=0.
         BFLD(3)=0.
         RETURN
      ENDIF
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_BDIP
C****
C****
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE
C**** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION
C**** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R)
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION
C**** MTYP=6  :    PRETZEL MAGNET
C****
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO
C**** AXES (Z,X) IS GIVEN BY
C****
C****
C****
C**** B0  = B( 0, 0 )
C**** B1  = B( 1, 0 )
C**** B2  = B( 2, 0 )
C**** B3  = B( 1, 1 )
C**** B4  = B( 1,-1 )
C**** B5  = B( 0, 1 )
C**** B6  = B( 0, 2 )
C**** B7  = B( 0,-1 )
C**** B8  = B( 0,-2 )
C**** B9  = B(-1, 0 )
C**** B10 = B(-2, 0 )
C**** B11 = B(-1, 1 )
C**** B12 = B(-1,-1 )
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8  NDX, K
C
      include 'gcunit.inc'          !geant
C
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      DIMENSION TC(6), DTC(6)
C****
C****
      GO TO ( 10,10,6,6,10,21 )     ,MTYP
      WRITE(lout,*)'**error** in MITRAY_BDIP'
      WRITE(lout,*)'          Illegal value MTYP=',MTYP
      WRITE(lout,*)'!!! Abort current run !!!'
      STOP
    6 CALL MITRAY_NDIP
      RETURN
   21 CALL MITRAY_BPRETZ
      RETURN
C****
C**** MTYP = 1 , 2, 5
C**** UNIFORM FIELD MAGNETS
C****
   10 CONTINUE
      GO TO( 2, 1, 2, 4 ) , IN
    7 WRITE(lout,8) IN
    8 FORMAT(  35H0 ERROR -GO TO -  IN BFUN   IN=        ,I5 )
    1 BX = 0.
      BY = BF
      BZ = 0.
      BT = BF
c      WRITE(lout,*)'!!! Abort current run !!!'
c      STOP
	  return
C****
C****
    2 X = TC(1)
      Y = TC(2)
      Z = TC(3)
C****
C**** MTYP=1,2,5 MAP ROUTINES/INTERPOLATE
C****
      IF( IMAP .EQ. 0 ) GO TO 5
      CALL MITRAY_BDMP ( B0, Z, X )
      S0 = 0.
      IF( Y .NE. 0. )   GO TO 11
      BX = 0.
      BY = B0
      BZ = 0.
      BT = B0
      RETURN
   11 CALL MITRAY_BDMP ( B1 , Z + DG, X  )
      CALL MITRAY_BDMP ( B2 , Z + 2.*DG, X  )
      CALL MITRAY_BDMP ( B3 , Z + DG, X + DG  )
      CALL MITRAY_BDMP ( B4 , Z + DG, X - DG  )
      CALL MITRAY_BDMP ( B5 , Z , X + DG  )
      CALL MITRAY_BDMP ( B6 , Z , X + 2.*DG  )
      CALL MITRAY_BDMP ( B7 , Z , X - DG  )
      CALL MITRAY_BDMP ( B8 , Z , X - 2.*DG  )
      CALL MITRAY_BDMP ( B9 , Z - DG, X  )
      CALL MITRAY_BDMP ( B10, Z - 2.*DG, X  )
      CALL MITRAY_BDMP ( B11, Z - DG, X + DG  )
      CALL MITRAY_BDMP ( B12, Z - DG, X - DG  )
      GO TO 9
C****
C**** MTYP = 1,2,5   STANDARD ROUTINES
C****
    5 CALL MITRAY_BDPP ( B0, Z, X )
      S0 = S
      IF( Y .NE. 0. )   GO TO 3
      BX = 0.
      BY = B0
      BZ = 0.
      BT = B0
      RETURN
C****
C****
    3 CONTINUE
C****
C****
      IF( MTYP .EQ. 2 ) GO TO 12
C****
C****
C**** MTYP = 1,5
C**** NON-MIDPLANE FRINGING FIELD REGION
C****
      CALL MITRAY_BDPP ( B1 , Z + DG, X  )
      CALL MITRAY_BDPP ( B2 , Z + 2.*DG, X  )
      CALL MITRAY_BDPP ( B3 , Z + DG, X + DG  )
      CALL MITRAY_BDPP ( B4 , Z + DG, X - DG  )
      CALL MITRAY_BDPP ( B5 , Z , X + DG  )
      CALL MITRAY_BDPP ( B6 , Z , X + 2.*DG  )
      CALL MITRAY_BDPP ( B7 , Z , X - DG  )
      CALL MITRAY_BDPP ( B8 , Z , X - 2.*DG  )
      CALL MITRAY_BDPP ( B9 , Z - DG, X  )
      CALL MITRAY_BDPP ( B10, Z - 2.*DG, X  )
      CALL MITRAY_BDPP ( B11, Z - DG, X + DG  )
      CALL MITRAY_BDPP ( B12, Z - DG, X - DG  )
      GO TO 9
C****
C**** MTYP = 2
C**** NON-MIDPLANE FRINGING FIELD REGION
C****
   12 CALL MITRAY_BDPPX(  B1 , 1, 0 )
      CALL MITRAY_BDPPX(  B2 , 2, 0 )
      CALL MITRAY_BDPPX(  B3 , 1, 1 )
      CALL MITRAY_BDPPX(  B4 , 1,-1 )
      CALL MITRAY_BDPPX(  B5 , 0, 1 )
      CALL MITRAY_BDPPX(  B6 , 0, 2 )
      CALL MITRAY_BDPPX(  B7 , 0,-1 )
      CALL MITRAY_BDPPX(  B8 , 0,-2 )
      CALL MITRAY_BDPPX(  B9 ,-1, 0 )
      CALL MITRAY_BDPPX(  B10,-2, 0 )
      CALL MITRAY_BDPPX(  B11,-1, 1 )
      CALL MITRAY_BDPPX(  B12,-1,-1 )
C****
C**** CALCULATE BX, BY, AND BZ
C****
    9 S = S0
      YG1 = Y/DG
      YG2 = YG1*YG1
      YG3 = YG2*YG1
      YG4 = YG3*YG1
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )
      BT = DSQRT(BX*BX + BY*BY + BZ*BZ)
      RETURN
C****
C**** CONSTANT FIELD REGION
C****
    4 BX = 0.
      BY = BR
      BZ = 0.
      BT = BR
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE  MITRAY_BDPP ( BFLD, Z, X )
C****
C****
C****
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE
C****              MORE ACCURATE 3'RD AND HIGHER ORDER CURVATURES
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  NDX, K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      DIMENSION TC(6), DTC(6)
C****
      GO TO (10,2,6,6,11,6 ) , MTYP
    6 RETURN
C****
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION
C****
   10 S = ( Z-XMITRAY_ZEFB(X) )/D + DELS
      GO TO 13
C****
C**** MTYP=2  :    UNIFORM FIELD, ITERATIVE CALCULATION
C****
    2 CALL MITRAY_SDIP( X,Z )
      GO TO 13
C****
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION
C****
   11 IF( DABS(RCA)  .GE. 1.D-08  ) GO TO 12
      S = Z/D + DELS
      GO TO 13
   12 A = 1./RCA
      S = ( DSIGN(1.D0,A) * DSQRT( (Z+A)**2 + X*X ) - A ) / D + DELS
      GO TO 13
C****
C**** ENTRY FOR OFF MIDPLANE FIELD
C****
      ENTRY MITRAY_BDPPX( BFLD, I, J )
      CALL MITRAY_SIJ( I, J )
   13 CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )
      E=DEXP(CS)
      P0 = 1.0 + E
      DB=BF-BR
      BFLD=BR + DB/P0
C****
C**** WRITE(6,100)X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_NDIP
C****
C****
C**** MTYP = 3 OR 4
C**** THIS VERSION OF BFUN IS MAINLY FOR NONUNIFORM FIELD MAGNETS
C**** THE CENTRAL FIELD REGION IS REPRESENTED TO 3'RD ORDER ON-AND-
C**** OFF THE MIDPLANE BY ANALYTIC EXPRESSIONS. SEE SLAC NO. 75
C**** FRINGE FIELD REGIONS REPRESENTED BY FERMI TYPE FALL-OFF
C**** ALONG WITH RADIAL FALL-OFF
C**** COMPONENTS OF 'B' IN FRINGE REGION EVALUATED BY NUMERICAL METHODS
C****
C****
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO
C**** AXES (Z,X) IS GIVEN BY
C****
C****
C**** B0  = B( 0, 0 )
C**** B1  = B( 1, 0 )
C**** B2  = B( 2, 0 )
C**** B3  = B( 1, 1 )
C**** B4  = B( 1,-1 )
C**** B5  = B( 0, 1 )
C**** B6  = B( 0, 2 )
C**** B7  = B( 0,-1 )
C**** B8  = B( 0,-2 )
C**** B9  = B(-1, 0 )
C**** B10 = B(-2, 0 )
C**** B11 = B(-1, 1 )
C**** B12 = B(-1,-1 )
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  NDX, K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      DIMENSION TC(6), DTC(6)
      X = TC(1)
      Y = TC(2)
      Z = TC(3)
      DX = X - XC_OFFSET
      DZ = Z - ZC_OFFSET
      RP =DSQRT( DX*DX + DZ*DZ )
      DR = RP - RB
      GO TO ( 1, 2, 1, 14 ), IN
    7 WRITE(6,8) IN, MTYP
      STOP
    8 FORMAT (    '0 ERROR -GO TO -  IN BFUN   IN=', I3, '   MTYP=',I4 )
    2 DRR1 = DR/RB
      DRR2 = DRR1*DRR1
      DRR3 = DRR2*DRR1
      DRR4 = DRR3*DRR1
      IF( Y .NE. 0. )  GO TO 4
C****
C**** MID-PLANE UNIFORM FIELD REGION
C****
      BX = 0.
      BY = 0.
      IF( MTYP .EQ. 3) BY=
     1     BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 )
      IF( MTYP .EQ. 4) BY= BF/ (1. + NDX*DRR1 )
      BZ = 0.
      BT = BY
      RETURN
C****
C**** NON MID-PLANE UNIFORM FIELD REGION
C****
    4 YR1 = Y/RB
      YR2 = YR1*YR1
      YR3 = YR2*YR1
      YR4 = YR3*YR1
      RR1 = RB/RP
      RR2 = RR1*RR1
      RR3 = RR2*RR1
      IF( MTYP .EQ. 3 ) GO TO 11
      IF( MTYP .EQ. 4 ) GO TO 12
      GO TO 7
C****
C**** MTYP = 3
C****
   11 BRR = BF*( ( -NDX + 2.*BET1*DRR1 + 3.*GAMA*DRR2 + 4.*DELT*DRR3 )
     1   *YR1 - (NDX*RR2 + 2.*BET1*RR1*(1.-RR1*DRR1) +
     2   3.*GAMA*( 2. + 2.*RR1*DRR1 - RR2*DRR2 ) +
     3   4.*DELT*( 6.*DRR1 + 3.*RR1*DRR2 - RR2*DRR3 ))*YR3/6. )
      BY = BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 -
     1   .5*YR2*( -NDX*RR1 + 2.*BET1*( 1. + RR1*DRR1) +
     2   3.*GAMA*DRR1*( 2. + RR1*DRR1) + 4.*DELT*DRR2*(3. + RR1*DRR1) )
     3   + YR4*( -NDX*RR3 + 2.*BET1*( RR3*DRR1 - RR2) +
     4   3.*GAMA*( 4.*RR1 - 2.*RR2*DRR1 + RR3*DRR2 ) +
     5   4.*DELT*( 6. + 12.*RR1*DRR1 - 3.*RR2*DRR2 + RR3*DRR3 ) )/24. )
      GO TO 13
C****
C**** MTYP = 4
C****
   12 DNR1 = 1. + NDX*DRR1
      DNR2 = DNR1*DNR1
      DNR3 = DNR2*DNR1
      DNR4 = DNR3*DNR1
      DNR5 = DNR4*DNR1
      BRR = BF*NDX*( -YR1/DNR2 + YR3*( 6.*NDX*NDX/DNR4 -
     1   2.*NDX*RR1/DNR3 - RR2/DNR2 ) /6.  )
      BY = BF*( 1./DNR1 + .5*YR2*NDX*( -2.*NDX/DNR3 + RR1/DNR2) +
     2   YR4*NDX*( 24.*NDX**3 /DNR5 - 12.*NDX*NDX*RR1/DNR4 -
     3   2.*NDX*RR2/DNR3 - RR3/DNR2 ) /24.  )
C****
C****
   13 BX = BRR*DX/RP
      BZ = BRR*DZ/RP
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)
      RETURN
C****
C**** FRINGING FIELD ZONES
C****
C**** CHECK IF FIELD MAP CALCULATED
C****
    1 CONTINUE
      IF( IMAP .EQ. 0 ) GO TO 3
C****
C**** MTYP=3,4 MAP ROUTINES/INTERPOLATE
C****
C****
      CALL MITRAY_BDMP ( B0, Z, X )
      IF( Y .NE. 0. )   GO TO 5
      BX = 0.
      BY = B0
      BZ = 0.
      BT = B0
      RETURN
    5 CALL MITRAY_BDMP ( B1 , Z + DG, X  )
      CALL MITRAY_BDMP ( B2 , Z + 2.*DG, X  )
      CALL MITRAY_BDMP ( B3 , Z + DG, X + DG  )
      CALL MITRAY_BDMP ( B4 , Z + DG, X - DG  )
      CALL MITRAY_BDMP ( B5 , Z , X + DG  )
      CALL MITRAY_BDMP ( B6 , Z , X + 2.*DG  )
      CALL MITRAY_BDMP ( B7 , Z , X - DG  )
      CALL MITRAY_BDMP ( B8 , Z , X - 2.*DG  )
      CALL MITRAY_BDMP ( B9 , Z - DG, X  )
      CALL MITRAY_BDMP ( B10, Z - 2.*DG, X  )
      CALL MITRAY_BDMP ( B11, Z - DG, X + DG  )
      CALL MITRAY_BDMP ( B12, Z - DG, X - DG  )
      GO TO 15
C****
C**** MTYP=3, 4  STANDARD ROUTINES
C****
    3 ZFB = XMITRAY_ZEFB(X)
      IF( Z .GT. ZFB ) DR=DSQRT(DX*DX+(ZFB-ZC_OFFSET)**2)-RB
      CALL MITRAY_NDPP( B0, Z, X, DR      )
      IF( Y  .NE. 0. )  GO TO 6
C****
C**** MID-PLANE FRINGING FIELD REGION
C****
      BX = 0.
      BY = B0
      BZ = 0.
      BT   = B0
      RETURN
C****
C**** NON MID-PLANE FRINGING FIELD REGION
C****
    6 IF( Z .GT. ZFB )  GO TO 9
      DR1  =       (DSQRT( DX*DX + (DZ+DG)**2 ) - RB )
      DR2  =       (DSQRT( DX*DX + (DZ+2.*DG)**2 ) - RB )
      DR3  =       (DSQRT( (DX+DG)**2 + (DZ+DG)**2 )  - RB )
      DR4  =       (DSQRT( (DX-DG)**2 + (DZ+DG)**2 )  - RB )
      DR5  =       (DSQRT( (DX+DG)**2 + DZ*DZ ) - RB )
      DR6  =       (DSQRT( (DX+ 2.*DG)**2 + DZ*DZ ) - RB )
      DR7  =       (DSQRT( (DX-DG)**2 + DZ*DZ ) - RB )
      DR8  =       (DSQRT( (DX- 2.*DG)**2 + DZ*DZ ) - RB )
      DR9  =       (DSQRT( DX*DX + (DZ-DG)**2 ) - RB )
      DR10 =       (DSQRT( DX*DX + (DZ-2.*DG)**2 ) - RB )
      DR11 =       (DSQRT( (DX+DG)**2 + (DZ-DG)**2 )  - RB )
      DR12 =       (DSQRT( (DX-DG)**2 + (DZ-DG)**2 )  - RB )
      GO TO 10
    9 CONTINUE
      DR1  = DR
      DR2  = DR
      DR9  = DR
      DR10 = DR
      XP = X+DG
      ZFB = XMITRAY_ZEFB(XP)
      DX = XP-XC_OFFSET
      DR3  = DSQRT( DX*DX + (ZFB-ZC_OFFSET)**2 ) - RB
      DR5  = DR3
      DR11 = DR3
      XP = X-DG
      ZFB = XMITRAY_ZEFB(XP)
      DX = XP-XC_OFFSET
      DR4  = DSQRT( DX*DX + (ZFB-ZC_OFFSET)**2 ) - RB
      DR7  = DR4
      DR12 = DR4
      XP = X+2.*DG
      ZFB = XMITRAY_ZEFB(XP)
      DX = XP-XC_OFFSET
      DR6  = DSQRT( DX*DX + (ZFB-ZC_OFFSET)**2 ) - RB
      XP = X-2.*DG
      ZFB = XMITRAY_ZEFB(XP)
      DX = XP-XC_OFFSET
      DR8  = DSQRT( DX*DX + (ZFB-ZC_OFFSET)**2 ) - RB
C****
C****
   10 CONTINUE
C**** CALL NDPP ( B1 , Z + DG, X  , DR1 )
C**** CALL NDPP ( B2 , Z + 2.*DG, X  , DR2 )
C**** CALL NDPP ( B3 , Z + DG, X + DG  , DR3 )
C**** CALL NDPP ( B4 , Z + DG, X - DG  , DR4 )
C**** CALL NDPP ( B5 , Z , X + DG , DR5 )
C**** CALL NDPP ( B6 , Z , X + 2.*DG  , DR6 )
C**** CALL NDPP ( B7 , Z , X - DG , DR7 )
C**** CALL NDPP ( B8 , Z , X - 2.*DG  , DR8 )
C**** CALL NDPP ( B9 , Z - DG, X  , DR9 )
C**** CALL NDPP ( B10, Z - 2.*DG, X, DR10 )
C**** CALL NDPP ( B11, Z - DG, X + DG  , DR11 )
C**** CALL NDPP ( B12, Z - DG, X - DG  , DR12 )
C****
C****
      CALL MITRAY_NDPPX(  B1 , 1, 0, DR1 )
      CALL MITRAY_NDPPX(  B2 , 2, 0, DR2 )
      CALL MITRAY_NDPPX(  B3 , 1, 1, DR3 )
      CALL MITRAY_NDPPX(  B4 , 1,-1, DR4 )
      CALL MITRAY_NDPPX(  B5 , 0, 1, DR5 )
      CALL MITRAY_NDPPX(  B6 , 0, 2, DR6 )
      CALL MITRAY_NDPPX(  B7 , 0,-1, DR7 )
      CALL MITRAY_NDPPX(  B8 , 0,-2, DR8 )
      CALL MITRAY_NDPPX(  B9 ,-1, 0, DR9 )
      CALL MITRAY_NDPPX(  B10,-2, 0, DR10)
      CALL MITRAY_NDPPX(  B11,-1, 1, DR11)
      CALL MITRAY_NDPPX(  B12,-1,-1, DR12)
C****
C**** OFF-MIDPLANE FIELD COMPONENTS BX, BY, AND BZ
C****
   15 YG1 = Y/DG
      YG2 = YG1*YG1
      YG3 = YG2*YG1
      YG4 = YG3*YG1
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)
      RETURN
   14 BX = 0.
      BY = BR
      BZ = 0.
      BT = BR
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE  MITRAY_NDPP ( BFLD, Z, X , DR )
C****
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  NDX, K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      DIMENSION TC(6), DTC(6)
C****
C****
      CALL MITRAY_SDIP( X, Z )
      GO TO 1
C****
C****
C**** ENTRY FOR OFF MIDPLANE FIELDS
C****
      ENTRY MITRAY_NDPPX( BFLD, I, J, DR )
      CALL MITRAY_SIJ(I, J )
    1 CONTINUE
      DRR1 = DR/RB
      DRR2 = DRR1*DRR1
      DRR3 = DRR2*DRR1
      DRR4 = DRR3*DRR1
      CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )
      E=DEXP(CS)
      P0 = 1.0 + E
      DB=BF-BR
      BFLD = 0.
      IF( MTYP .EQ. 3 ) BFLD =
     1       BR +( 1. - NDX*DRR1 + BET1*DRR2+GAMA*DRR3+DELT*DRR4)*DB/P0
      IF( MTYP .EQ. 4 ) BFLD = BR + ( 1./(1. +NDX*DRR1) )*DB/P0
C****
C**** WRITE(6,100) X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_BPRETZ
C****
C****
C**** MTYP=6
C****
C****
C**** PRETZEL MAGNET FIELD COMPONENTS
C**** DG = SMALL NEGATIVE NUMBER
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  NDX, K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      DIMENSION TC(6), DTC(6)
C****
C****
      G1 = BF/D
      Y = TC(2)
      Z = TC(3)
      IF( Z .LE. DG ) GO TO 1
      BX = 0.
      BY = 0.
      BZ = 0.
      RETURN
    1 BY0 = G1*DABS(Z)**NDX
      BY1 = BY0*NDX/Z
      BY2 = BY1*(NDX-1.)/Z
      BY3 = BY2*(NDX-2.)/Z
      BY4 = BY3*(NDX-3.)/Z
      BX = 0.
      BY = BY0 - Y*Y*BY2/2. + Y**4*BY4/24.
      BZ = Y*BY1 - Y**3*BY3/6.
      BT = DSQRT(BX*BX + BY*BY + BZ*BZ)
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_SDIP( X, Z )
C****
C****
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE
C**** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION
C**** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R)
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  NDX, K
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY23/  C0, C1, C2, C3, C4, C5
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      COMMON  /MITRAYBLSDIP/  XO,ZO,SS,DCS,DSN
      DIMENSION TC(6), DTC(6)
C****
C****
C****
C**** MTYP=2,3,4  :
C****
C****
C**** FIELD POINT (X,Z)
C****
C****
C**** CHECK TO SEE IF BOUNDARY IS FLAT
C****
      IF( NSRF .NE. 0. ) GO TO 1
      S = Z/D + DELS
      SS= S
      DCS = 1.0
      DSN = 0.0
      ZO  = XMITRAY_ZEFB(X)
      RETURN
C****
C**** FIND POINT ON EFFECTIVE FIELD BOUNDARY THROUGH FIELD POINT
C**** PARALLEL TO Z-AXIS
C****
    1 ZP    = XMITRAY_ZEFB(X)
C****
C**** INTERVAL OF SEARCH, AZ
C****
      AZ = (Z-ZP)/5.D0
      ZSIGN = DSIGN(1.D0,AZ)
      AZMAX = DSQRT( X*X + Z*Z )/5.D0
      IF( AZ .GT. AZMAX ) AZ = AZMAX
C****
C****
      AZ = DABS(AZ)
      XP = X-5*AZ
      IXP = 1
      DP = 1.D15
      DO 2 I=1,11
      ZP = XMITRAY_ZEFB(XP)
      XXP = X-XP
      ZZP = Z-ZP
      DD =  XXP*XXP + ZZP*ZZP
      IF( DD .GE. DP ) GO TO 3
      IXP = I
      DP = DD
    3 XP = XP+AZ
    2 CONTINUE
C****
C****  DIVIDE INTERVAL AND REPEAT FOR MORE EXACT
C****  SHORTEST DISTANCE.
C****
      X1 = X+AZ*(IXP-6)
      AZ = AZ/5.D0
      XP = X1-5*AZ
      IXP = 1
      DP = 1.D15
      DO 4 I=1,11
      ZP = XMITRAY_ZEFB(XP)
      XXP = X-XP
      ZZP = Z-ZP
      DD = XXP*XXP + ZZP*ZZP
      IF( DD .GE. DP ) GO TO 5
      IXP = I
      DP = DD
    5 XP = XP+AZ
    4 CONTINUE
C****
C****
      XO = X1+AZ*(IXP-6)
      ZO = XMITRAY_ZEFB(XO)
      XPO = X - XO
      ZPO = Z - ZO
      RO = XPO*XPO + ZPO*ZPO
C****
C**** INTERPOLATE FOR MORE ACCURATE LOCATION
C****
      IF( (IXP .EQ. 1 ) .OR. (IXP .EQ. 11)  ) GO TO 8
      XP  = XO + AZ
      ZP  = XMITRAY_ZEFB(XP)
      XXP = X-XP
      ZZP = Z-ZP
      R1  = XXP*XXP + ZZP*ZZP
C****
C**** CALCULATE POINT ON THE OTHER SIDE
C****
      XPM = XO - AZ
      ZPM = XMITRAY_ZEFB(XPM)
      XXP = X-XPM
      ZZP = Z-ZPM
      R2  = XXP*XXP + ZZP*ZZP
      IF( R1 .LE. R2 ) GO TO 9
C****
C**** SWAP POINTS
C****
      XP  = XO
      ZP  = ZO
      R1  = RO
      XO  = XPM
      ZO  = ZPM
      RO  = R2
9     X12 = XP-XO
      Z12 = ZP-ZO
      CC  = X12*X12 + Z12*Z12
      XO  = XO + (CC+RO-R1)*AZ/(2*CC)
      ZO  = XMITRAY_ZEFB(XO)
      XPO = X - XO
      ZPO = Z - ZO
      RO = XPO*XPO + ZPO*ZPO
    8 CONTINUE
C****
C****
      IF( RO .LT. 1.D-15 ) RO = 1.D-15
      IF( RO .GT. 1.D+15 ) RO = 1.D+15
      DZDXO = XMITRAY_DZDX(XO)
      COSTH = DSQRT ( 1. / (1. + DZDXO*DZDXO) )
      DELTAX = DSQRT(RO) * COSTH/4.D0
C****
C****
C**** WRITE(6,100) X, Z, XO, ZO, COSTH, DELTAX
C****
C**** PREPARE TO CALCULATE A PAIR OF EQUALLY SPACED IN X
C**** DISTANCES ON EITHER SIDE OF RO
C****
      RINV4 = 1.D0/(RO*RO)
C****
C**** CALCULATE REPRESENTATIVE DISTANCE
C****
      CX = XO - 2*DELTAX
      DO 6 J=1,5
      IF( J .EQ. 3 ) GO TO 7
      ZP = XMITRAY_ZEFB(CX)
      XDI = X - CX
      ZDI = Z - ZP
      RR  = XDI*XDI + ZDI*ZDI
      IF( RR .LT. 1.D-15 ) RR = 1.D-15
      IF( RR .GT. 1.D+15 ) RR = 1.D+15
      RINV4 = RINV4 + 1.0D0 / ( RR*RR )
   7  CX = CX+DELTAX
   6  CONTINUE
      DP2= DSQRT( 1.D0/RINV4 )
      DP = DSQRT( DP2 )
C****
C****
      S = 1.41875D0* ZSIGN * DP/D + DELS
C****
C**** Parameters for off midplane calculation
C****
      SS= S
      DELTA = DATAN(XMITRAY_DZDX(XO))
      DCS   = DCOS(DELTA)
      DSN   = DSIN(DELTA)
C****
C*100 FORMAT( 1P6D15.4 )
C**** WRITE(6,100) X, Z, DELS, S
C****
      RETURN
C****
C**** ENTRY FOR NON MIDPLANE 'S'
C****
      ENTRY MITRAY_SIJ( IZ, JX )
C****
C****
      A   = ( JX*DCS + IZ*DSN )*DG
      DSD = -DCS*( XMITRAY_ZEFB(XO+A*DCS) - ZO - A*DSN )
      S   = SS + ( ( IZ*DCS - JX*DSN )*DG + DSD )/D
      RETURN
      END
C
C=======================================================================
C
      REAL*8 FUNCTION XMITRAY_ZEFB(XP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
C
      XP2 = XP*XP
      XP3 = XP2*XP
      XP4 = XP3 * XP
      ZEFB= -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +
     1       S7*XP4*XP3 + S8*XP4*XP4 )
      XMITRAY_ZEFB=ZEFB
      RETURN
      END
C
C=======================================================================
C
      REAL*8 FUNCTION XMITRAY_DZDX(XP)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
C
      XP2 = XP*XP
      XP3 = XP2*XP
      XP4 = XP3 * XP
      DZDX= -(2.*S2*XP + 3.*S3*XP2+ 4.*S4*XP3 + 5.*S5*XP4 +
     1   6.*S6*XP4*XP + 7.*S7*XP4*XP2 + 8.*S8*XP4*XP3 )
      XMITRAY_DZDX=DZDX
      RETURN
      END
C
C=======================================================================
C
      REAL*8 FUNCTION XMITRAY_DZDX2(XP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
C
      XP2 = XP*XP
      XP3 = XP2*XP
      XP4 = XP3 * XP
      DZDX2 = -(2.*S2 + 6.*S3*XP+ 12.*S4*XP2 + 20.*S5*XP3 +
     1   30.*S6*XP4 + 42.*S7*XP4*XP + 56.*S8*XP4*XP2 )
      XMITRAY_DZDX2=DZDX2
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_BDMP( BZZ, Z, X )
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8  K, NDX
      COMMON  /MITRAY10/  BX, BY, BZ, K, TC, DTC
      COMMON  /MITRAY20/  NDX,BET1,GAMA,DELT
      COMMON  /MITRAY21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8
      COMMON  /MITRAY22/  D, DG, S, BF, BT, WDIP
      COMMON  /MITRAY24/  RB, XC_OFFSET, ZC_OFFSET
      COMMON  /MITRAY25/  IN, MTYP, NSRF, IMAP, IR
      COMMON  /MITRAY26/  JMAP(5), IX, IZ, IDUM, BZMAP(101,101,2,5)
      DIMENSION TC(6), DTC(6)
      DIMENSION FZ(3)
C****
C****
C****
      DX = IX + X/DG
      DZ = IZ + Z/DG
C**** NXP = DX
C**** NZQ = DZ
      NXP = DX + 0.5
      NZQ = DZ + 0.5
      PX = DX - NXP
      QZ = DZ - NZQ
C****
C****
C**** 6-POINT BIVARIATE INTERPOLATION 'ABRAMOWITZ'
C****
C****
C****      BZZ =  BF*( ( QZ*(QZ-1.) * BZMAP( NXP, NZQ-1, IR, IMAP ) +
C****     1        PX*(PX-1.) * BZMAP( NXP-1, NZQ, IR, IMAP ) +
C****     2        PX*(PX-2.*QZ+1.) * BZMAP( NXP+1, NZQ, IR, IMAP ) +
C****     3        QZ*(QZ-2.*PX+1.)*BZMAP( NXP, NZQ+1, IR, IMAP )  )/2.
C****     4        PX*QZ * BZMAP( NXP+1, NZQ+1, IR, IMAP ) +
C****     5        (1.+PX*QZ-PX*PX-QZ*QZ) * BZMAP( NXP, NZQ, IR, IMAP )
C****
C****
      QZ2 = QZ*QZ
      QZ3 = QZ2*QZ
      QZ4 = QZ3*QZ
      DO 1 I=1,3
      NXX = NXP-2+I
      BM2 = BZMAP(NXX, NZQ-2, IR, IMAP)
      BM1 = BZMAP(NXX, NZQ-1, IR, IMAP)
      B00 = BZMAP(NXX, NZQ  , IR, IMAP)
      BP1 = BZMAP(NXX, NZQ+1, IR, IMAP)
      BP2 = BZMAP(NXX, NZQ+2, IR, IMAP)
      A1  = ( (BP1-BM1)*8 - BP2 +BM2 )/12
      A2  = ( (BP1+BM1)*16- BP2 -BM2 -30*B00 )/24
      A3  = ( (BM1-BP1)*2 + BP2 -BM2 )/12
      A4  = ( -4*(BP1 + BM1) + BP2 +BM2 + 6*B00)/24
      FZ(I) = B00 + A1*QZ + A2*QZ2 + A3*QZ3 + A4*QZ4
    1 CONTINUE
      C1  = ( FZ(3)-FZ(1) )/2
      C2  = ( FZ(3)+FZ(1)-2*FZ(2) )/2
      BZZ = BF*( FZ(2) + C1*PX + C2*PX*PX )
C****
C****
      RETURN
      END
C
C=======================================================================

