C***********************************************************************
C                         SOLENOID SUBROUTINES
C***********************************************************************
C
      SUBROUTINE mitray_solnd(data, xpos, bfld)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Subroutine for solenoid, in GEANT implementation of MIT-RAYTRACE C
C     adapted from:                                                    C
C     Subroutine SOLND (NO, NP, T, TP ,NUM ) by S. Kowalski            C
C     by Stanley Yen (TRIUMF)                                          C
C                                                                      C
C      TC(1) to  TC(6) =  (  X,  Y,  Z, VX, VY, VZ )                   C
C     DTC(1) to DTC(6) =  ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )          C
C                                                                      C
C     BF (positive) : Solenoid field in beam direction                 C
C                                                                      C
C     MODIFICATION HISTORY                                             C
C     --------------------                                             C
C                                                                      C
C     Mar 16, 1998    S.Yen     Original adaptation from MIT-RAYTRACE  C
C                                                                      C
C     S. Yen (TRIUMF)  e-mail  STAN@TRIUMF.CA                          C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT none
C
      REAL*8 DATA(75), XPOS(3), BFLD(3)
C
      REAL*8 XA, YA, ZA
      REAL*8 A, B, L, D, Z11, Z22
C
      REAL*8 BX, BY, BZ, TC(6), DTC(6)
      COMMON  /MITRAY10/  BX, BY, BZ, TC, DTC
C
      REAL*8 BF, AL, RAD
      COMMON  /MITRAY30/  BF, AL, RAD
C
      REAL*8 S, BT
      COMMON  /MITRAY31/  S, BT
C
C     Extract the A-axes coordinates from XPOS array
C
      XA = XPOS(1)
      YA = XPOS(2)
      ZA = XPOS(3)
C
C     EXTRACT THE SOLENOID PARAMETERS FROM THE ARRAY DATA(i)
C
      A   = DATA(10)
      B   = DATA(11)
      L   = DATA(12)
      D   = DATA(13)
      BF  = DATA(14)
      Z11 = DATA(15)
      Z22 = DATA(16)
C
      AL  = L/2.
      RAD = D/2.
      BX  = 0.
      BY  = 0.
      BZ  = 0.
      BT  = 0.
      S   = 0.
C
C     Load position coordinates into TC(1),TC(2),TC(3) since this is
C     what subroutine BSOL expects.  Note that these are relative to
C     the geometric center of the solenoid.
C
      TC(1) =  XA
      TC(2) =  YA
      TC(3) =  ZA - A - AL
C
C     If outside the integration regions defined by Z11 before the 
C     entrance edge of the solenoid, to Z22 beyond the exit edge of 
C     the solenoid, then return without calculating anything
C
      IF(TC(3).LT.(-(AL+Z11)) .OR. TC(3).GT.(AL+Z22))RETURN
C
C     CALL BSOL TO CALCULATE B-FIELD COMPONENTS BX,BY,BZ, 
C     RETURNED IN COMMON BLOCK
C
      CALL mitray_bsol
C
      BFLD(1) = BX
      BFLD(2) = BY
      BFLD(3) = BZ
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE mitray_bsol
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C     Routine valid for fields outside central zone of elemental       C
C                              Solenoid                                C
C                                                                      C
C     BF    = field at center of infinite solenoid; curr. den. (NI/M)  C
C                                                                      C
C     M.W.GARRETTT  JOURNAL OF APP. PHYS. 34,(1963),P2567              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      REAL*8 BX, BY, BZ, TC(6), DTC(6)
      COMMON  /MITRAY10/  BX, BY, BZ, TC, DTC
C
      REAL*8 BF, AL, RAD
      COMMON  /MITRAY30/  BF, AL, RAD
C
      REAL*8 S, BT
      COMMON  /MITRAY31/  S, BT
C
      REAL*8 PI4
      DATA PI4/12.566370616D0 /
C
      X = TC(1)
      Y = TC(2)
      Z = TC(3)
C
      R = DSQRT( X*X + Y*Y )
C
      IF( R  .LT.  (RAD/1.D4)  )  GO TO 5
C
      RADR = RAD+R
      AAPR = 4.D0*RAD/RADR
      AAMR = (RAD-R)/(2.D0*RAD)
      RCSQ = 4.D0*RAD*R/(RADR*RADR)
C
C *** Solenoid left hand source
C
      ZZ   = -(AL+Z)
      R1SQ = RADR*RADR + ZZ*ZZ
      R1   = DSQRT(R1SQ)
      RKSQ = 4.D0*RAD*R/R1SQ
C
      CALL MITRAY_FB01AD(RKSQ, VKS, VES )
      CALL MITRAY_FB03AD(RCSQ, RKSQ, P )
C
      BZS1 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1
      BRS1 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)
C
C *** Solenoid right hand source
C
      ZZ   = AL-Z
      R1SQ = RADR*RADR + ZZ*ZZ
      R1   = DSQRT(R1SQ)
      RKSQ = 4.D0*RAD*R/R1SQ
C
      CALL MITRAY_FB01AD(RKSQ, VKS, VES )
      CALL MITRAY_FB03AD(RCSQ, RKSQ, P )
C
      BZS2 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1
      BRS2 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)
C
      BZ = BF*( BZS2-BZS1 )/PI4
      BR = BF*( BRS2-BRS1 )/(R*PI4)
      BX = BR * X /R
      BY = BR *  Y/R
      BT = DSQRT( BX**2 + BY**2 + BZ**2 )
C
      RETURN
C
    5 CONTINUE
C
      COSA = (AL-Z) / DSQRT( RAD*RAD + (AL-Z)**2 )
      COSB =-(AL+Z) / DSQRT( RAD*RAD + (AL+Z)**2 )
C
      BX = 0.
      BY = 0.
      BZ = BF*(COSA-COSB)/2.D0
      BT = DABS(BZ)
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_FB01AD(C, VK,VE)
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DATA XLG/'7FFFFFFFFFFFFFFF'X/
c      REAL * 8 XLG/'7FFFFFFFFFFFFFFF'X/    ! modified by O Kirsebom Jan 17 2013
C
      D = 1D0 - C
      IF(D .GT. 0D0)E=-DLOG(D)
C
C *** Harwell version of FB01AD
C
      IF(C .GE. 1D0)GO TO 2
           VE=E*((((((((((
     A     3.18591956555015718D-5*D  +.989833284622538479D-3)*D
     B    +.643214658643830177D-2)*D +.16804023346363385D-1)*D
     C    +.261450147003138789D-1)*D +.334789436657616262D-1)*D
     D    +.427178905473830956D-1)*D +.585936612555314917D-1)*D
     E    +.937499997212031407D-1)*D +.249999999999901772D0)*D)
     F    +(((((((((
     G     .149466217571813268D-3*D  +.246850333046072273D-2)*D
     H    +.863844217360407443D-2)*D+.107706350398664555D-1)*D
     I    +.782040406095955417D-2)*D +.759509342255943228D-2)*D
     J    +.115695957452954022D-1)*D +.218318116761304816D-1)*D
     K    +.568051945675591566D-1)*D +.443147180560889526D0)*D
     L    +1D0
C
C *** Routine modified to calculate VD and VE always
C
           VK=E*((((((((((
     A     .297002809665556121D-4*D   +.921554634963249846D-3)*D
     B    +.597390429915542916D-2)*D  +.155309416319772039D-1)*D
     C    +.239319133231107901D-1)*D  +.301248490128989303D-1)*D
     D    +.373777397586236041D-1)*D  +.48828041906862398D-1)*D
     E    +.703124997390383521D-1)*D  +.124999999999908081D0)*D
     F    +.5D0)+(((((((((
     G     .139308785700664673D-3*D   +.229663489839695869D-2)*D
     H    +.800300398064998537D-2)*D  +.984892932217689377D-2)*D
     I    +.684790928262450512D-2)*D  +.617962744605331761D-2)*D
     J    +.878980187455506468D-2)*D  +.149380135326871652D-1)*D
     K    +.308851462713051899D-1)*D  +.965735902808562554D-1)*D
     L    +1.38629436111989062D0
C
      RETURN
C
    2 VE=1D0
      VK=XLG
C
      RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_FB02AD(CAYSQ,SINP,COSP,E,F)
C
      IMPLICITREAL*8(A-H,O-Z)
C
      PHI = DATAN(SINP/COSP)
C
      IF(CAYSQ*SINP*SINP-0.5D0)1,1,5
C
    1 H    = 1.0D0
      A    = PHI
      N    = 0
      SIG1 = 0.D0
      SIG2 = 0.D0
      SIN2 = SINP*SINP
      TERM = SINP*COSP*0.5D0
      CRIT = PHI
C
    2 N = N + 1
C
      RECIP = 1.0D0/N
      FACT  = (N-.5D0)*RECIP
      H1    = H
      H     = FACT*CAYSQ*H
      A     = FACT*A-TERM*RECIP
      TERM  = TERM*SIN2
      CRIT  = CRIT*SIN2
      DEL1  = H*A
      DEL2  = -.5D0*RECIP*CAYSQ*H1*A
      SIG1  = SIG1+DEL1
      SIG2  = SIG2+DEL2
C
      IF(DABS(DEL1)-4.0D-16)4,3,3
C
    3 IF(DABS(CRIT)-DABS(A))4,2,2
C
    4 F = PHI+SIG1
      E = PHI+SIG2
      GO TO 8
C
    5 CFI  = 1.D0
      CFJ  = 1.D0
      CFL  = 0.D0
      CFM  = 0.D0
      CFN  = 0.D0
      SIG1 = 0.D0
      SIG2 = 0.D0
      SIG3 = 0.D0
      SIG4 = 0.D0
C
      N = 0
C
      FACT1  = 1.0D0-CAYSQ*SINP*SINP
      FACTOR = .5D0*COSP*DSQRT(CAYSQ/FACT1)
      FACTRO = FACTOR+FACTOR
      CAYDSQ = 1.0D0-CAYSQ
C
    6 N = N + 1
C
      RECIP  = 1.0D0/N
      FACTN  = RECIP*(N-.5D0)
      FACTM  = (N+.5D0)/(N+1.0D0)
      FACTOR = FACTOR*FACT1
      CFI1   = CFI
      CFJ1   = CFJ
      CFI    = CFI*FACTN
      CFJ    = CFJ*FACTN*FACTN*CAYDSQ
      CFL    = CFL+.5D0/(N*(N-.5D0))
      CFM    = (CFM-FACTOR*RECIP*CFI)*FACTM*FACTM*CAYDSQ
      CFN    = (CFN-FACTOR*RECIP*CFI1)*FACTN*FACTM*CAYDSQ
      DEL1   = CFM-CFJ*CFL
      DEL2   = CFN-(FACTN*CFL-.25D0*RECIP*RECIP)*CAYDSQ*CFJ1
      DEL3   = CFJ
      DEL4   = FACTM*CFJ
      SIG1   = SIG1+DEL1
      SIG2   = SIG2+DEL2
      SIG3   = SIG3+DEL3
      SIG4   = SIG4+DEL4
C
      IF(DABS (DEL1)-4.0D-16)7,6,6
C
    7 CAYMOD = DSQRT(CAYSQ)
      FLOG1  = DLOG(4.0D0/(DSQRT(FACT1)+CAYMOD*COSP))
      T1=(1.0D0+SIG3)*FLOG1+FACTRO*DLOG(.5D0+.5D0*CAYMOD*DABS (SINP))
      T2=(.5D0+SIG4)*CAYDSQ*FLOG1+1.0D0-FACTRO*(1.0D0-CAYMOD*DABS(SINP))
      F = T1+SIG1
      E = T2+SIG2
C
    8 RETURN
      END
C
C=======================================================================
C
      SUBROUTINE MITRAY_FB03AD( GN,CACA,P )
C
      IMPLICITREAL*8(A-H,O-Z)
C
      IF(GN)1,2,2
C
    1 IF(CACA)3,3,4
C
    3 P = 1.5707963268/DSQRT(1.D0-GN)
      RETURN
C
    4 STH  = DSQRT(-GN/(CACA-GN))
      CTH  = DSQRT(1.D0-STH*STH)
      CADA = 1.D0-CACA
C
      CALL MITRAY_FB01AD(CACA,CAPK,CAPE)
      CALL MITRAY_FB02AD(CADA,STH,CTH,E,F)
C
      BR = CAPE*F-CAPK*(F-E)
      P  = CAPK*CTH*CTH+STH*BR/DSQRT(1.D0-GN)
      RETURN
C
    2 IF(GN-CACA)10,30,20
C
   10 STH = DSQRT(GN/CACA)
      CTH = DSQRT(1.D0-STH*STH)
C
      CALL MITRAY_FB01AD(CACA,CAPK,CAPE)
      CALL MITRAY_FB02AD(CACA,STH,CTH,E,F)
C
      BR = CAPK*E-CAPE*F
      P  = CAPK+BR*STH/(CTH*DSQRT(1.D0-GN))
      RETURN
C
   30 CALL MITRAY_FB01AD(CACA,CAPK,CAPE)
      P = CAPE/(1.D0-CACA)
      RETURN
C
   20 CADA = 1.D0-CACA
      PI   = 3.1415926536
      STH  = DSQRT((1.D0-GN)/CADA)
      CTH  = DSQRT(1.D0-STH*STH)
C
      CALL MITRAY_FB01AD(CACA,CAPK,CAPE)
      CALL MITRAY_FB02AD(CADA,STH,CTH,E,F)
C
      BR = PI/2.+CAPK*(F-E)-CAPE*F
      P  = CAPK+BR*DSQRT(GN)/(CADA*STH*CTH)
C
      RETURN
      END
C
