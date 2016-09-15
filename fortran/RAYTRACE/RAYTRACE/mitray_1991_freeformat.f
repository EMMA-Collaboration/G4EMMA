C****                                                       
C**** RAY TRACE  -  MIT VERSION 1989   (02/19/89)
C**** Prof. STANLEY KOWALSKI                                            RAY00030
C**** MASS INST OF TECH                                                 RAY00040
C**** BLDG 26-427                                                       RAY00050
C**** CAMBRIDGE MASS 02139                                              RAY00060
C**** PH 617+253-4288                                                   RAY00070
C****                                                                   RAY00080
C**** Copyright - 1991 S. Kowalski
C**** All rights reserved. No part of this code may be reproduced or
C**** transmitted in any form or by any means, electronic, mechanical, 
C**** photocopying, recording, or otherwise, without the written
C**** permission of the author.
C****
C**** REVISIONS:
C****   28-MAY-02 SY  NWD, NWORD and ITITLE made CHAR*4
C****   08-OCT-99 SY  COMM  comment cards as  device type 4
c****   08-OCT-99 SY  Free format for all numerical read
C****   02-FEB-91 SK  NSKIP Matrix selection
C****   20-SEP-90 SK  PRINT Routine C in calc. of RHO
C****   15-FEB-90 SK  Radius print in solenoid routine
C****   15-AUG-89 SK  Change sign of EZ in IN=1 for EDIP
C****   04-MAY-89 SK  Corrected path length calculations to account
C****                 for varying particle velocities in EINTZEL 
C****                 and ACCEL
C****   28-FEB-89 SK  MODIFIED EDIP Fringe Field Algorithim
C****   21-FEB-89 SK  Changed default of PMASS to non-zero
C****   19-FEB-89 SK  ERROR IN FIELD DERIV ( G5- Accel, Poles )
C****   16-FEB-89 SK  EINZEL LENS
C****   07-FEB-89 SK  SDIP Passing variables on ENTRY stmnt for PC's
C****   31-JAN-89 SK  FNMIRK re HALFH
C****   05-NOV-88 SK  Accelerator Section
C****   06-OCT-88 SK  Matrix Element TTPP corrected
C****   09-JAN-88 SK  Print format re POLES routine and final coord.
C****   13-JAN-87 SK  DMAP - Corrected indexing variable K
C****   21-JUN-86 SK  Major error in BDPP corrected re MTYP=1,2,5
C****   30-APR-86 SK  Analytic algorithim for DIPOLE- s
C****   17-APR-86 SK  Modified BDMP algorithim
C****   29-MAR-86 SK  Field Map Routines for MTYP=3, 4
C****   20-MAR-86 SK  Field Map Generation Routines
C****   21-MAR-86 SK  SHROT - Translate Particle to end of System
C****   19-MAR-86 SK  BDIP, BDPP, NDIP, NDPP Reference to Y
C****   17-MAR-86 SK  Remove reference to CSC and SCOR ; not used
C****   17-MAR-86 SK  BDIP - Removed some extraneous coding.
C****   10-OCT-85 SK  Waist printout in routine MTRX1
C****   07-SEP-85 SK  MTYP=2,3,4 Modified Algorithm for s.
C****   04-SEP-85 SK  Modified Algorithm for fringe field
C****                 MTYP=3,4 Magnetic Dipole
C****   03-SEP-85 SK  Velocity of light C=2.99792458
C****                 C**2 = 8.98755
C****                 Atomic mass = 931.5016 Mev/amu
C****   03-SEP-85 SK  Print Control; JPRT
C****                 Electrostatic Dipole, (Z/D+1.)
C****                 Velocity Selector,    (Z/D+1.)
C****                 Poles Error (Octapole - G2)
C****                 Lens; Chromatic Aberration
C****                 
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY00090
      REAL*8   K                                                        RAY00100
      LOGICAL LPLT
      CHARACTER*9 DAET
      CHARACTER*8 TYME
      CHARACTER*4 NTITLE, NT1, NT2, NWD, ITITLE, NWORD
      COMMON  /BLCK00/  LPLT
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY00110
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP                 RAY00120
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL        RAY00130
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY00140
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY00150
      COMMON  /BLCK 6/  NP, JFOCAL                                      RAY00160
      COMMON  /BLCK 7/  NCODE                                           RAY00170
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY00180
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           RAY00190
      COMMON  /BLCK15/  TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX              RAY00195
C*IBM DIMENSION DAET(5), TYME(2)
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DIMENSION XI(999), YI(999), ZI(999), VXI(999), VYI(999), VZI(999),RAY00210
     1          DELP(999)                                               RAY00220
      DIMENSION NWORD(16),DATA(75,200),IDATA(200),NTITLE(20),ITITLE(200)RAY00230
      DIMENSION TC(6), DTC(6), R(6,6), T2(5,6,6)                        RAY00240
      DATA NWORD/4HSENT, 4HDIPO, 4HEINZ, 4HCOMM, 4HXXXX, 4HXXXX, 4HEDIP,RAY00250
     1           4HVELS, 4HPOLE, 4HMULT, 4HSHRT, 4HDRIF, 4HCOLL, 4HSOLE,RAY00260
     2           4HLENS, 4HACCE/
      DATA C /2.99792458D10/                                            RAY00270
      DATA TYME/ '        ' /
      DATA NT1, NT2/' RT9','0.1 '/
C****                                                                   RAY00280
C****                                                                   RAY00290
  100 FORMAT( 8F10.5 )                                                  RAY00300
  101 FORMAT( 20A4 )                                                    RAY00310
  102 FORMAT(10I5)                                                      RAY00320
  103 FORMAT( ///  10X, 'KEY WORD DOES NOT MATCH - NWD= ',    A4)       RAY00330
  104 FORMAT( // 10X, ' GO TO STATEMENT IN MAIN FELL THROUGH - I= ',I5/)RAY00340
  105 FORMAT( 1H1, 10X, 20A4  )                                         RAY00350
  106 FORMAT( 1H1 )                                                     RAY00360
  107 FORMAT( 6F10.5/ 5F10.5/3F10.5/4F10.5/ 4F10.5/ 6F10.5/ 6F10.5/     RAY00370
     1        6F10.5/ 4F10.5/ 7F10.5/ 7F10.5                           )RAY00380
  108 FORMAT('1',62X, 'RAY ', I4, //  30X, 'ENERGY=',F8.3,' MEV ', 7X,  RAY00390
     1   'PMOM=', F8.3, ' MEV/C', 6X, 'VELC=', 1PD11.3, ' CM/SEC'    /  RAY00400
     2   30X, 'DELE/E=', 0PF8.3, ' (PC)', 5X, 'DELP/P=', F8.3,          RAY00410
     3   ' (PC) ', 4X, 'DELV/V=', F7.3, '     (PC)'        /)           RAY00420
  109 FORMAT( 2F10.5/ 5F10.5/ 2F10.5/ 6F10.5            )               RAY00430
  111 FORMAT( 2F10.5/ 6F10.5/ 2F10.5/ 6F10.5/ 3F10.5 )                  RAY00440
  112 FORMAT( 3F10.5/ 4F10.5/ 5F10.5/ 4F10.5/ 6F10.5/ 6F10.5 / 8F10.5 ) RAY00450
  113 FORMAT( A4, 16X, A4  )                                            RAY00460
  114 FORMAT( 1F10.5 / 5F10.5 / 2F10.5  )                               RAY00470
  116 FORMAT( /10X, '  PARTICLE ENERGY =', F10.4,  '  MEV'      /       RAY00500
     1         10X, 'PARTICLE MOMENTUM =', F10.4,  '  MEV/C'    /       RAY00510
     2         10X, 'PARTICLE VELOCITY =',1PD14.4, '  CM/SEC'  /        RAY00520
     3         10X, '             MASS =',0PF10.4, '  AMU'     /        RAY00530
     4         10X, '           CHARGE =', F10.4,  '  EQ'           )   RAY00540
  117 FORMAT( 10X, A9, 1X, A8, I12, ' CPU.SEC'    )                     RAY00541
C*IBM117   FORMAT( 10X, 3A4, 1X, 2A4, 2A4 )
  118 FORMAT(4F10.5/5F10.5/F10.5/4F10.5/4F10.5/6F10.5/6F10.5)
  119 FORMAT( /// '  MAXIMUM NUMBER OF BEAM ELEMENTS EXCEEDED  ' /// )
  120 FORMAT( 3F10.5/4F10.5/1F10.5/4F10.5/6F10.5/6F10.5 )
C****                                                                   RAY00550
      CALL DATE(DAET)                                                   RAY00551
      CALL TIME(TYME)                                                   RAY00552
C*IBM CALL WHEN(DAET)
C**** CALL ERRSET( NUMBER, CONT, COUNT, TYPE, LOG, MAXLIN     )         RAY00553
c      CALL ERRSET( 63, .TRUE., .FALSE., .FALSE., .FALSE., 2048)
c      CALL ERRSET( 72, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)         RAY00560
c      CALL ERRSET( 74, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)         RAY00560
c      CALL ERRSET( 88, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)         RAY00560
c      CALL ERRSET( 89, .TRUE., .FALSE., .FALSE., .TRUE.,  2560)         RAY00570
C*IBM CALL ERRSET( 207, 256, 1 )
C*IBM CALL ERRSET( 208, 256, 1 )
C*IBM CALL ERRSET( 209, 256, 1 )
C*IBM CALL ERRSET( 210, 256, 1 )                                        RAY00590
C****                                                                   RAY00600
C****                                                                   RAY00610
    5 LPLT = .FALSE.
      IVEC = 0                                                          RAY00620
      LNEN = 0                                                          RAY00630
      NMAX = 200
      DO 1  I=1,NMAX                                                    RAY00640
      IDATA(I)= 0                                                       RAY00650
      DO 1  J=1,75                                                      RAY00660
      DATA(J,I) = 0.                                                    RAY00670
    1 CONTINUE                                                          RAY00680 
      READ ( 5,101,END=99) NTITLE                                       RAY00690
        NTITLE(19) = NT1
        NTITLE(20) = NT2
c     Change to format-free read of numerical values  SY  Jan 20, 2000
c
      READ (5,*)NR, IP, NSKIP, JFOCAL, JPRT,  JNR, NPLT                 RAY00700
      READ (5,*) ENERGY, DEN, XNEN, PMASS, Q0                           RAY00710
      IF( NPLT .NE. 0 ) LPLT = .TRUE.
      IF( NR .GT. 999) NR=999                                           RAY00720
      IF( Q0 .EQ. 0. )  Q0 = 1.                                         RAY00730
      IF( PMASS .EQ. 0. ) PMASS = 1.D-10
      EMASS = PMASS*931.5016                                            RAY00740
      QMC = EMASS/(8.98755D10*Q0)                                       RAY00750
      ETOT = EMASS + ENERGY                                             RAY00760
      VEL = ( DSQRT( ( 2.*EMASS + ENERGY)*ENERGY) / ETOT ) * C          RAY00770
      VEL0 = VEL                                                        RAY00780
      EN0 = ENERGY                                                      RAY00790
      PMOM0 = DSQRT( (2.*EMASS + EN0)*EN0)                              RAY00800
      NEN = XNEN                                                        RAY00810
      IF( NEN  .EQ.  0 ) NEN = 1                                        RAY00820
      NO = 1                                                            RAY00830
    2 IF( NO .LE. NMAX ) GO TO 6
      PRINT 119
      CALL EXIT
    6 READ (5,*) NWD, ITITLE(NO)                                        RAY00840
c
      PRINT *,'NOW READING INPUT FOR DEVICE TYPE ',NWD
      IF(NWD.EQ.'SENT')GOTO 11
      IF(NWD.EQ.'DIPO')GOTO 12
      IF(NWD.EQ.'EINZ')GOTO 13
      IF(NWD.EQ.'COMM')GOTO 2
      IF(NWD.EQ.'EDIP')GOTO 17
      IF(NWD.EQ.'VELS')GOTO 18
      IF(NWD.EQ.'POLE')GOTO 19
      IF(NWD.EQ.'MULT')GOTO 20
      IF(NWD.EQ.'SHRT')GOTO 21
      IF(NWD.EQ.'DRIF')GOTO 22
      IF(NWD.EQ.'COLL')GOTO 23
      IF(NWD.EQ.'SOLE')GOTO 24
      IF(NWD.EQ.'LENS')GOTO 25
      IF(NWD.EQ.'ACCE')GOTO 26
      PRINT *,' '
      PRINT *,'UNKNOWN DEVICE TYPE ',NWD
      PRINT *,'EXECUTION OF MIT-RAYTRACE TERMINATED'
 99   STOP
C
C
C     DO 3  I=1,16                                                      RAY00850
C     IF( NWD  .EQ. NWORD(I) ) GO TO 4                                  RAY00860
C   3 CONTINUE                                                          RAY00870
C     PRINT 103, NWD                                                    RAY00880
C  99 CALL EXIT                                                         RAY00890
C--   SY 08-OCT-99  NWD=4 is COMM (comment) card
C   4 GO TO(11,12,13,2,14,14,17,18,19,20,21,22,23,24,25,26),NWD         RAY00900
C****                                                                   RAY00910
C****                                                                   RAY00920
C****                                                                   RAY00930
C****                                                                   RAY00940
C  14 PRINT 104,  I                                                     RAY00950
C     CALL EXIT                                                         RAY00960
C****                                                                   RAY00970
C**** DIPOLE  LENS   (DIPO)  TYPE = 2                                   RAY00980
C****                                                                   RAY00990
   12 IDATA(NO) = 2                                                     RAY01000
C     Switch to format-free read   SY  Jan 14,2000
      READ (5,*) ( DATA( J,NO ) , J=1,6 )
      READ (5,*) ( DATA(J,NO),J=11,15)
      READ(5,*)(DATA(J,NO),J=16,18)
      READ(5,*)(DATA(J,NO),J=19,22)
      READ(5,*)(DATA(J,NO),J=25,28)
      READ(5,*)(DATA(J,NO),J=29,34)
      READ(5,*)(DATA(J,NO),J=35,40)
      READ(5,*)(DATA(J,NO),J=41,46)
      READ(5,*)(DATA(J,NO),J=47,50)
      READ(5,*)(DATA(J,NO),J=51,57)
      READ(5,*)(DATA(J,NO),J=58,64)
      NO = NO + 1                                                       RAY01030
      GO TO 2                                                           RAY01040
C****                                                                   RAY01050
C**** EINZEL LENS (EINZ)                                                RAY01060
C****                                                                   RAY01110
   13 IDATA(NO) = 3                                                     RAY01120
C     Switch to format-free read  SY  Jan 14, 2000
      READ(5,*)(DATA(J,NO ) , J=1,2 )
      READ(5,*)(DATA(J,NO ), J=10,14 )
      READ(5,*)(DATA(J,NO),J=15,16)
      READ(5,*)(DATA(J,NO),J=17,22)
      NO = NO + 1                                                       RAY01140
      GO TO 2                                                           RAY01150
C****   
C****   ELECTROSTATIC DEFLECTOR (EDIP)  TYPE=7
17      IDATA(NO) = 7
C       Switch to format-free read  SY  Jan 14, 2000
        READ(5,*) (DATA(J, NO), J=1, 4) 
        READ(5,*)(DATA(J, NO), J=11,15)
        READ(5,*)DATA(16, NO)
        READ(5,*)(DATA(J, NO), J=17,20)
        READ(5,*)(DATA(J, NO), J=25,28)
        READ(5,*)(DATA(J, NO), J=29,34)
        READ(5,*)(DATA(J, NO), J=35,40)
        NO = NO + 1
        GO TO 2
C****                                                                   RAY01160
C**** VELOCITY SELECTOR  (VELS)    TYPE = 8                             RAY01170
C****                                                                   RAY01180
c     Switch to format-free read  SY  Jan 17, 2000
   18 IDATA(NO) = 8                                                     RAY01190
      READ (5,*) ( DATA(J,NO),J=1,4)  ! record 2
      READ(5,*)(DATA(J,NO), J=7,11 )  ! record 3                        RAY01200
      READ(5,*)( DATA(J,NO),J=12,13)  ! record 4
      READ(5,*)(DATA(J,NO),J=16,19)   ! record 5
      READ(5,*)(DATA(J,NO),J=20,23)   ! record 6
      READ(5,*)(DATA(J,NO),J=24,27)   ! record 7
      READ(5,*)(DATA(J,NO),J=28,33)   ! record 8
      READ(5,*)(DATA(J,NO),J=34,39)   ! record 9
      READ(5,*)(DATA(J,NO),J=40,45)   ! record 10
      READ(5,*)(DATA(J,NO),J=46,51)   ! record 11
      NO = NO + 1                                                       RAY01210
      GO TO 2                                                           RAY01220
C****                                                                   RAY01230
C**** MULTIPOLE (POLES)      TYPE =  9                                  RAY01240
C****                                                                   RAY01250
c     Switch to free format   SY  Jan. 17, 2000
   19 IDATA(NO) = 9
      READ(5,*)(DATA(J,NO),J=1,3)     ! record 2
      READ(5,*)(DATA(J,NO),J=10,13)   ! record 3
      READ(5,*)(DATA(J,NO),J=14,18)   ! record 4
      READ(5,*)(DATA(J,NO),J=19,22)   ! record 5
      READ(5,*)(DATA(J,NO),J=23,28)   ! record 6
      READ(5,*)(DATA(J,NO),J=29,34)   ! record 7
      READ(5,*)(DATA(J,NO),J=35,42)   ! record 8
      NO = NO + 1                                                       RAY01280
      GO TO 2                                                           RAY01290
C****                                                                   RAY01300
C**** MULTIPOLE LENS (LENS)  TYPE = 10                                  RAY01310
C****                                                                   RAY01320
C     Change to free format    SY   Jan. 17, 2000
   20 IDATA(NO) = 10                                                    RAY01330
      READ(5,*)(DATA(J,NO),J=1,2)     ! record 2
      READ(5,*)(DATA(J,NO),J=10,15)   ! record 3
      READ(5,*)(DATA(J,NO),J=16,17)   ! record 4
      READ(5,*)(DATA(J,NO),J=20,25)   ! record 5
      READ(5,*)(DATA(J,NO),J=26,28)   ! record 6
      NO = NO + 1                                                       RAY01360
      GO TO 2                                                           RAY01370
C****                                                                   RAY01380
C**** SHIFT AND ROTATE  (SHRT)   TYPE = 11                              RAY01390
C****                                                                   RAY01400
C     Change to free format   SY   Jan. 19, 2000
   21 IDATA(NO) = 11                                                    RAY01410
      READ (5,*) ( DATA( J,NO ) , J=1,6 )                               RAY01420
      NO = NO + 1                                                       RAY01430
      GO TO 2                                                           RAY01440
C****                                                                   RAY01450
C**** DRIFT     (DRIF)       TYPE = 12                                  RAY01460
C****
C     Change to free format   SY   Jan. 19, 2000                        RAY01470
   22 IDATA(NO) = 12                                                    RAY01480
      READ (5,*)   DATA( 1,NO )                                         RAY01490
      NO = NO + 1                                                       RAY01500
      GO TO 2                                                           RAY01510
C****
C**** COLLIMATOR  (COLL)       TYPE = 13
C****
C     Change to free format   SY   Jan. 19, 2000
   23 IDATA(NO) = 13
      READ(5,*)  (DATA(J,NO),J=1,5)
      NO = NO+1
      GO TO 2
C****                                                                   RAY01520
C**** SOLENOID    (SOLE)       TYPE = 14                                RAY01530
C****                                                                   RAY01540
C     Change to free format   SY   Jan. 19, 2000
   24 IDATA(NO) = 14                                                    RAY01550
      READ (5,*)DATA(1,NO)
      READ(5,*)(DATA(J,NO),J=10,14)
      READ(5,*)(DATA(J,NO),J=15,16)
      NO = NO+1                                                         RAY01570
      GO TO 2                                                           RAY01580
C****                                                                   RAY01590
C**** LENS     (LENS)        TYPE = 15                                  RAY01600
C****                                                                   RAY01610
C     Change to free format   SY   Jan. 19, 2000
   25 IDATA(NO) = 15                                                    RAY01620
      READ(5,*)(DATA(J,NO),J=1,8)
      READ(5,*)(DATA(J,NO),J=9,11)
      NO = NO+1                                                         RAY01640
      GO TO 2                                                           RAY01650
C****
C****
C**** ACCELERATOR  (ACCE)     TYPE = 16
C****
C****
C     Change to free format    SY   Jan. 19, 2000
   26 IDATA(NO) = 16
      READ(5,*)(DATA(J,NO),J=1,3)
      READ(5,*)(DATA(J,NO),J=10,13)
      READ(5,*)DATA(J,14)
      READ(5,*)(DATA(J,NO),J=15,18)
      READ(5,*)(DATA(J,NO),J=19,24)
      READ(5,*)(DATA(J,NO),J=25,30)
      NO = NO+1
      GO TO 2
C****                                                                   RAY01660
C**** SYSTEM END             TYPE = 1                                   RAY01670
C****                                                                   RAY01680
   11 IDATA(NO) = 1                                                     RAY01690
C****
C****
C**** CALCULATE FIELD MAPS
C****
C****
      CALL FMAP( IDATA,NO,IP )
      ICPU = ITCPU( )/100
      PRINT 117, DAET, TYME, ICPU                                       RAY02631
C****                                                                   RAY01700
C**** STANDARD RAYS AUTOMATIC SET-UP                                    RAY01710
C**** IF( NR .GT. JNR ) APPEND ADDITIONAL RAYS FROM INPUT
C****
        IF (JNR.EQ.0) GO TO 66                                          RAY01711
C       Change to free-format   SY   Jan. 19, 2000
        PRINT *,'NOW READING PARAMS FOR PROGRAM-GENERATED RAYS'
        READ (5,*) TMIN,PMIN,XMAX,TMAX,YMAX,PMAX,DMAX                   RAY01713
        CALL RAYS(JNR)                                                  RAY01714
        PRINT *,JNR,' RAYS AUTOMATICALLY GENERATED'
      IF( JNR .GE. NR ) GO TO 52
      JNRP = JNR+1
      DO 49 J=JNRP,NR
c       Change to format-free read  SY  Jan. 19, 2000
        READ(5,*,END=60) XI(J),VXI(J),YI(J),VYI(J),ZI(J),VZI(J),
     1                    DELP(J)
49    CONTINUE
        GO TO 52                                                        RAY01715
C****
C**** INPUT RAYS
C****
   66 DO 56  J=1,NR                                                     RAY01720
C        Change to format-free read   SY  Jan. 19, 2000
      READ(5,*,END=60 )XI(J),VXI(J),YI(J),VYI(J),ZI(J),VZI(J),DELP(J)   RAY01730
   56 CONTINUE                                                          RAY01740
      GO TO 52                                                          RAY01750
   60 NR = J-1                                                          RAY01760
   52 DO 53 JEN=1,NEN                                                   RAY01770
C****                                                                   RAY01780
C****                                                                   RAY01790
C****                                                                   RAY01800
      NP = IP                                                           RAY01810
      IF( (NP .LE. 100)  .OR.  (NP .GE. 200)  ) GO TO 65                RAY01820
      IF( JEN   .EQ.   (NEN/2+1)  )  NP = IP-100                        RAY01830
   65 CONTINUE                                                          RAY01840
      IF( (NP .GT. 100)  .AND.  (JEN .NE. 1) )  GO TO 55                RAY01850
      PRINT 105, NTITLE                                                 RAY01860
      PRINT 117, DAET, TYME                                             RAY01861
      PRINT 116, EN0, PMOM0, VEL0, PMASS, Q0                            RAY01870
      DO 54  NO = 1,200                                                 RAY01880
      ITYPE = IDATA(NO)                                                 RAY01890
      IF( ITYPE .EQ. 1 ) GO TO 55                                       RAY01900
   54 CALL PRNT( ITYPE, NO )                                            RAY01910
   55 CONTINUE
C**** IF( ( NP .GT. 100) .AND. (JEN .EQ. 1 ) ) PRINT 106                RAY01920
      DO 57  J=1,NR                                                     RAY01930
      ENERGY = (1.+DELP(J)/100. ) *EN0                                  RAY01940
      ETOT = EMASS + ENERGY                                             RAY01950
      VEL = ( DSQRT( (2.*EMASS + ENERGY) *ENERGY) /ETOT)*C              RAY01960
      PMOM =  DSQRT( (2.*EMASS + ENERGY) *ENERGY)                       RAY01970
      K = (Q0/ETOT)*8.98755D10                                          RAY01980
C****                                                                   RAY01990
      T = 0.                                                            RAY02000
      TP= 0.
      NUM = 0
      XA = XI(J)                                                        RAY02010
      YA = YI(J)                                                        RAY02020
      ZA = ZI(J)                                                        RAY02030
      VXA =VEL*DSIN( VXI(J)/1000. ) * DCOS( VYI(J)/1000. )              RAY02040
      VYA =VEL*DSIN( VYI(J)/1000. )                                     RAY02050
      VZA =VEL*DCOS( VXI(J)/1000. ) * DCOS( VYI(J)/1000. )              RAY02060
      XDVEL = (VEL-VEL0)*100./VEL0                                      RAY02070
      DELTP = (PMOM-PMOM0)*100./PMOM0                                   RAY02080
      IF( NP .LE. 100) PRINT 108,J, ENERGY,PMOM,VEL,DELP(J),DELTP,XDVEL RAY02090
      DO 50 NO =1,200                                                   RAY02100
      ITYPE = IDATA(NO )                                                RAY02110
      GO TO( 31,32,33,30,30,30,37,38,39,40,41,42,46,44,45,47)   ,ITYPE  RAY02120
   30 CALL EXIT                                                         RAY02130
C****                                                                   RAY02140
C****                                                                   RAY02150
   32 CALL DIPOLE ( NO, NP, T, TP ,NUM )                                RAY02160
      GO TO 51                                                          RAY02170
   33 IVEC = 1
      CALL EINZEL ( NO, NP, T, TP ,NUM )                                RAY02190
      IVEC = 0
      GO TO 51                                                          RAY02200
   37 IVEC = 1
      CALL EDIPL (NO, NP, T, TP, NUM)
      IVEC = 0
      GO TO 51
   38 IVEC = 1                                                          RAY02210
      CALL VELS   ( NO, NP, T, TP ,NUM )                                RAY02220
      IVEC = 0                                                          RAY02230
      GO TO 51                                                          RAY02240
   39 CALL POLES  ( NO, NP, T, TP ,NUM )                                RAY02250
      GO TO 51                                                          RAY02260
   40 CALL MULT   ( NO, NP, T, TP ,NUM )                                RAY02270
      GO TO 51                                                          RAY02280
   41 CALL SHROT  ( NO, NP, T, TP ,NUM )                                RAY02290
      GO TO 50                                                          RAY02300
   42 CALL DRIFT  ( NO, NP, T, TP ,NUM )                                RAY02310
      GO TO 50                                                          RAY02320
   44 CALL SOLND  ( NO, NP, T, TP ,NUM )                                RAY02330
      GO TO 51                                                          RAY02340
   45 CALL LENS   ( NO, NP, T, TP ,NUM )                                RAY02350
      GO TO 50                                                          RAY02360
   46 CALL COLL   ( NO,  J, IFLAG      )
      IF( IFLAG .NE. 0 ) GO TO 57
      GO TO 50
   47 IVEC = 1
      CALL ACCEL  ( NO, NP, T, TP ,NUM )
      IVEC = 0
      GO TO 51
C****
C****
C****
   51 XA = TC(1)                                                        RAY02370
      YA = TC(2)                                                        RAY02380
      ZA = TC(3)                                                        RAY02390
      VXA= TC(4)                                                        RAY02400
      VYA= TC(5)                                                        RAY02410
      VZA= TC(6)                                                        RAY02420
   50 CONTINUE                                                          RAY02430
   31 CONTINUE                                                          RAY02440
      CALL OPTIC( J, JFOCAL, NP, T, TP )                                RAY02450
      IF (LPLT ) CALL PLTOUT ( JEN, J, NUM )
   57 CONTINUE                                                          RAY02460
      ENERGY = EN0                                                      RAY02470
      VEL = VEL0
      IF( NP .GT. 100 ) GO TO 59                                        RAY02480
      PRINT 105, NTITLE                                                 RAY02490
      PRINT 117, DAET,TYME                                              RAY02491
      PRINT 116, EN0, PMOM0, VEL0, PMASS, Q0                            RAY02500
      DO 58 NO =1,200                                                   RAY02510
      ITYPE = IDATA(NO )                                                RAY02520
      IF ( ITYPE  .EQ.  1 ) GO TO 59                                    RAY02530
   58 CALL PRNT( ITYPE, NO )                                            RAY02540
   59 CONTINUE                                                          RAY02550
      IF( NSKIP .GT. 1 ) GO TO 61                                       RAY02560
      IF( (NR .GE. 46) .AND. (NSKIP .EQ. 1 ) ) GO TO 63
      IF( NR  .GE.  46  )  GO TO 62                                     RAY02570
      IF( NR  .GE.  14  )  GO TO 63                                     RAY02580
      IF( NR  .GE.   6  )  GO TO 64                                     RAY02590
      GO TO 61                                                          RAY02600
   62 CALL MATRIX(R,T2)                                                 RAY02610
      GO TO 61                                                          RAY02620
   63 PRINT 105, NTITLE                                                 RAY02630
      ICPU = ITCPU( )/100
      PRINT 117, DAET, TYME, ICPU                                       RAY02631
      CALL MTRX1( 0, JEN, NR, ENERGY, JPRT  )                           RAY02640
      LNEN = 1                                                          RAY02650
      GO TO 61                                                          RAY02660
   64 PRINT 105, NTITLE                                                 RAY02670
      ICPU = ITCPU( )/100
      PRINT 117, DAET, TYME, ICPU                                       RAY02671
      CALL MTRX1( 1, JEN, NR, ENERGY, JPRT )                            RAY02680
      LNEN = 1                                                          RAY02690
   61 IF( JPRT .EQ. 3 ) GO TO 67
      if( JPRT .EQ. 0 ) GO TO 68
      IF( .NOT. ( (JPRT .EQ. 1) .AND. (JEN .EQ. (NEN/2+1) ) ) ) GO TO 67
   68 CALL PRNT1 ( NR )                                                 RAY02700
   67 EN0 = EN0 + DEN                                                   RAY02710
      ENERGY = EN0                                                      RAY02720
      ETOT = EMASS + EN0                                                RAY02730
      VEL0 = ( DSQRT( ( 2.*EMASS + EN0)*EN0 ) /ETOT)*C                  RAY02740
      PMOM0 = DSQRT( (2.*EMASS + EN0)*EN0)                              RAY02750
   53 CONTINUE                                                          RAY02760
      IF(  (LNEN .EQ. 0 )  .OR.   (NEN .EQ. 1 )  )  GO TO 5             RAY02770
      PRINT 105, NTITLE                                                 RAY02780
C**** CALL TIME(TYME)
      ICPU = ITCPU( )/100
      PRINT 117, DAET, TYME, ICPU                                       RAY02781
C*IBM CALL WHEN(DAET)
      CALL MPRNT( NEN )                                                 RAY02790
      PRINT 106                                                         RAY02800
      GO TO 5                                                           RAY02810
      END                                                               RAY02820
        SUBROUTINE  RAYS(NR)                                            RAYK0010
C****                                                                   RAYK0020
        IMPLICIT REAL*8(A-H,O-Z)                                        RAYK0030
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP                 RAY00120
      COMMON  /BLCK15/  TMIN, PMIN, XMAX, TMAX, YMAX, PMAX, DMAX        RAY00195
      DIMENSION XI(999), YI(999), ZI(999), VXI(999), VYI(999), VZI(999),RAYK0060
     1        DELP(999)                                                 RAYK0070
100     FORMAT (///10X, 'JNR = ', I10 ///)                              RAYK0080
C****                                                                   RAYK0090
C****                                                                   RAYK0100
        DO 1 I=1,999                                                    RAYK0110
        XI(I)=0.                                                        RAYK0120
        YI(I)=0.                                                        RAYK0130
        ZI(I)=0.                                                        RAYK0140
        VXI(I)=0.                                                       RAYK0150
        VYI(I)=0.                                                       RAYK0160
        VZI(I)=0.                                                       RAYK0170
        DELP(I)=0.                                                      RAYK0180
1       CONTINUE                                                        RAYK0190
        IF (TMIN.EQ.0.) TMIN=1.0                                        RAYK0200
        IF (PMIN.EQ.0.) PMIN=1.0                                        RAYK0210
        TMAX2 = TMAX/2.0                                                RAYK0220
        TMAX3 = TMAX/3.0                                                RAYK0230
        PMAX2 = PMAX/2.0                                                RAYK0240
        PMAX3 = 2.*PMAX/3.0                                             RAYK0250
        IF (NR.EQ.2) GO TO 2
        IF (NR.EQ.6) GO TO 2                                            RAYK0260
        IF (NR.EQ.14) GO TO 2                                           RAYK0270
        IF (NR.EQ.46) GO TO 3                                           RAYK0280
        PRINT 100, NR                                                   RAYK0290
        CALL EXIT                                                       RAYK0300
2       VXI(2)=TMIN                                                     RAYK0310
        VYI(2)=PMIN                                                     RAYK0320
        IF (NR.EQ.2) GO TO 5
        VXI(3)=TMAX2                                                    RAYK0330
        VXI(4)=-TMAX2                                                   RAYK0340
        VXI(5)=TMAX                                                     RAYK0350
        VXI(6)=-TMAX                                                    RAYK0360
        IF (NR.EQ.6) GO TO 5                                            RAYK0370
        VYI(7)=PMAX2                                                    RAYK0380
        VXI(8)=TMAX2                                                    RAYK0390
        VYI(8)=PMAX2                                                    RAYK0400
        VXI(9)=-TMAX2                                                   RAYK0410
        VYI(9)=PMAX2                                                    RAYK0420
        VXI(10)=TMAX                                                    RAYK0430
        VYI(10)=PMAX2                                                   RAYK0440
        VXI(11)=-TMAX                                                   RAYK0450
        VYI(11)=PMAX2                                                   RAYK0460
        VYI(12)=PMAX                                                    RAYK0470
        VXI(13)=TMAX2                                                   RAYK0480
        VYI(13)=PMAX                                                    RAYK0490
        VXI(14)=-TMAX2                                                  RAYK0500
        VYI(14)=PMAX                                                    RAYK0510
C****
C****
C****
    5   DO 4 I=1,NR
        XI(I) = XMAX
        YI(I) = YMAX
    4   DELP(I) = DMAX
        RETURN                                                          RAYK0520
C****                                                                   RAYK0530
C****                                                                   RAYK0540
C****                                                                   RAYK0550
3       VXI(2)=TMIN                                                     RAYK0560
        VYI(2)=PMIN                                                     RAYK0570
        XI(3)=XMAX                                                      RAYK0580
        XI(4)=-XMAX                                                     RAYK0590
        VXI(5)=TMAX3                                                    RAYK0600
        VXI(6)=-TMAX3                                                   RAYK0610
        YI(7)=YMAX                                                      RAYK0620
        YI(8)=-YMAX                                                     RAYK0630
        VYI(9)=PMAX3                                                    RAYK0640
        VYI(10)=-PMAX3                                                  RAYK0650
        DELP(11)=DMAX                                                   RAYK0660
        DELP(12)=-DMAX                                                  RAYK0670
        XI(13)=XMAX                                                     RAYK0680
        VXI(13)=TMAX3                                                   RAYK0690
        XI(14)=-XMAX                                                    RAYK0700
        VXI(14)=-TMAX3                                                  RAYK0710
        XI(15)=XMAX                                                     RAYK0720
        DELP(15)=DMAX                                                   RAYK0730
        XI(16)=-XMAX                                                    RAYK0740
        DELP(16)=-DMAX                                                  RAYK0750
        VXI(17)=TMAX3                                                   RAYK0760
        DELP(17)=DMAX                                                   RAYK0770
        VXI(18)=-TMAX3                                                  RAYK0780
        DELP(18)=-DMAX                                                  RAYK0790
        YI(19)=YMAX                                                     RAYK0800
        VYI(19)=PMAX3                                                   RAYK0810
        YI(20)=-YMAX                                                    RAYK0820
        VYI(20)=PMAX3                                                   RAYK0830
        XI(21)=XMAX                                                     RAYK0840
        YI(21)=YMAX                                                     RAYK0850
        XI(22)=-XMAX                                                    RAYK0860
        YI(22)=YMAX                                                     RAYK0870
        XI(23)=XMAX                                                     RAYK0880
        VYI(23)=PMAX3                                                   RAYK0890
        XI(24)=-XMAX                                                    RAYK0900
        VYI(24)=PMAX3                                                   RAYK0910
        VXI(25)=TMAX3                                                   RAYK0920
        YI(25)=YMAX                                                     RAYK0930
        YI(26)=YMAX                                                     RAYK0940
        VXI(27)=TMAX3                                                   RAYK0950
        VYI(27)=PMAX3                                                   RAYK0960
        VXI(28)=-TMAX3                                                  RAYK0970
        VYI(28)=PMAX3                                                   RAYK0980
        YI(29)=YMAX                                                     RAYK0990
        DELP(29)=DMAX                                                   RAYK1000
        YI(30)=YMAX                                                     RAYK1010
        DELP(30)=-DMAX                                                  RAYK1020
        VYI(31)=PMAX3                                                   RAYK1030
        DELP(31)=DMAX                                                   RAYK1040
        VYI(32)=PMAX3                                                   RAYK1050
        DELP(32)=-DMAX                                                  RAYK1060
        VXI(33)=TMAX                                                    RAYK1070
        VXI(34)=-TMAX                                                   RAYK1080
        XI(35)=XMAX                                                     RAYK1090
        VXI(35)=TMAX                                                    RAYK1100
        XI(36)=-XMAX                                                    RAYK1110
        VXI(36)=TMAX                                                    RAYK1120
        XI(37)=XMAX                                                     RAYK1130
        VXI(37)=-TMAX                                                   RAYK1140
        XI(38)=-XMAX                                                    RAYK1150
        VXI(38)=-TMAX                                                   RAYK1160
        VXI(39)=TMAX                                                    RAYK1170
        DELP(39)=DMAX                                                   RAYK1180
        VXI(40)=TMAX                                                    RAYK1190
        DELP(40)=-DMAX                                                  RAYK1200
        VXI(41)=-TMAX                                                   RAYK1210
        DELP(41)=DMAX                                                   RAYK1220
        VXI(42)=-TMAX                                                   RAYK1230
        DELP(42)=-DMAX                                                  RAYK1240
        VYI(43)=PMAX                                                    RAYK1250
        VXI(44)=TMAX                                                    RAYK1260
        VYI(44)=PMAX                                                    RAYK1270
        DELP(45)=3.*DMAX                                                RAYK128 
        DELP(46)=-3.*DMAX                                               RAYK1290
        RETURN                                                          RAYK1300
        END                                                             RAYK1310
      SUBROUTINE FMAP(IDATA, NO, NP)
C****                                                                   RAY02910
C****                                                                   RAY02920
C**** Control Section for calculating Dipole Field Maps on a 
C**** Rectangular grid.
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY02930
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY02960
      COMMON  /BLCK26/  JMAP(5), IX, IZ, BZMAP(101,101,2,5)
      DIMENSION IDATA(200)
      DIMENSION DATA(  75,200 ) ,ITITLE(200)                            RAY03060
      DATA PRNT /0/
C****
C****
C****
  120 FORMAT( / ' *** FATAL ERROR **** - ELEMENT NO=', I5,
     1'    EXCEEDS MAXIMUM FIELD MAP INDEX' , / '  IMAP=', I5 /)
  121 FORMAT( / ' ***WARNING*** FIELD MAPS NOT IMPLIMENTED FOR THIS
     1  MTYP:  NO=',I5, '  MTYP=',I5, '  IMAP=',I5  /)
  122 FORMAT( '1', // ' FIELD MAP PARAMETERS ', / 5X, 
     1  'IMAP   NO   MTYP   IR   NXLO  NXHI  NZLO  NZHI  '   )
C****
C****
C**** BZMAP(NX,NZ,IR,IMAP)
C****    NX         : X-POSITION INDEX
C****    NZ         : Z-POSITION INDEX
C****    IR=1       : ENTRANCE FRINGE FIELD
C****    IR=2       : EXIT FRINGE FIELD
C****    IMAP=(1-5) : IDENTIFIES FIELD MAP
C****
C****
C****
C**** CLEAR FIELD MAP AND INDEX ARRAYS
C****
      DO 1 I=1,5
    1 JMAP(I)=0
      DO 2 I1=1,101
      DO 2 I2=1,101
      DO 2 I3=1,2
      DO 2 I4=1,5
    2 BZMAP(I1,I2,I3,I4)=0.
C****
C**** CYCLE THROUGH ELEMENTS TO FIND DIPOLES WHICH NEED FIELD MAPS
C**** CALCULATED.
C****
      DO 3 I=1,NO
      IF( IDATA(I) .NE. 2 ) GO TO 3
C****
C**** CHECK FOR MAP INDEX AND WHETHER DIPOLE NEEDS A MAP TO BE
C**** CALCULATED
C****
      MTYP = DATA(5,I)
      IMAP = DATA(6,I)
      IF( IMAP .EQ. 0 ) GO TO 3
C****
C**** CHECK FOR VALID MTYP'S
C****
      IF( MTYP .LE. 5 ) GO TO 6
      PRINT 121, I, MTYP, IMAP
C****
C**** RESET INVALID FIELD-MAP
C****
      DATA(6,I) = 0.
      GO TO 3
C****
C**** CHECK TO SEE IF MAP WITH THIS INDEX IMAP=(1-5) HAS ALLREADY
C**** BEEN CALCULATED
C**** CHECK IMAP INDEX LIMITS
C****
    6 CONTINUE
      IF( IMAP .LE. 5 ) GO TO 4
      PRINT 120, I, IMAP
      CALL EXIT
    4 CONTINUE
      IF( JMAP(IMAP) .NE. 0 ) GO TO 5
C****
C**** IDENTIFY DIPOLE MAGNETIC ELEMENT USED TO CALCULATE FIELD MAP
C**** FOR INDEX IMAP.
C****
      JMAP(IMAP)=I
C****
C**** CALCULATE FIELD MAP FOR INDEX IMAP
C****
      IF( PRNT .EQ. 0 ) PRINT 122
      PRNT = 1
      CALL DMAP( I, NP )
      GO TO 3
    5 CONTINUE
C****
C**** CHECK TO SEE THAT DIPOLE HAS EXACTLY THE SAME FIELD DESCRIPTION 
C**** AS DIPOLE WHOSE MAP HAS ALLREADY BEEN CALCULATED
C****
    3 CONTINUE
      RETURN
      END
      SUBROUTINE DMAP( NO, NP )
C****
C****
C**** CALCULATE FIELD MAPS
C****
C****                                                                   RAY03090
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY02930
      CHARACTER*4 ITITLE
      REAL*8  LF1, LF2, K, NDX                                          RAY02940
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY02960
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY02970
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY02980
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY02990
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY03020
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY03030
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY03050
      COMMON  /BLCK26/  JMAP(5), IX, IZ, BZMAP(101,101,2,5)
      DIMENSION DATA(  75,200 ) ,ITITLE(200)                            RAY03060
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY03070
      DIMENSION NXLMT(5,2,2), NZLMT(5,2,2)
C****
C****
      DATA NXMAX /101/
      DATA NZMAX /101/
      DATA NZ1MAX/ 60/
      DATA NZ2MAX/ 40/
C****
C****
      IX = NXMAX/2+1
      IZ = NZ2MAX +1
C****
C****
  100 FORMAT( '1',// '   ENTRANCE FRINGING FIELD MAP :  IMAP=', I3 / )
  101 FORMAT( '1',// '   EXIT FRINGING FIELD MAP :  IMAP=', I3 / )
  102 FORMAT( '1' )
  103 FORMAT( '    NX', 4X, 15I8 )
  104 FORMAT( I4, F8.3, 15F8.4 )
  105 FORMAT( /' ***WARNING*** MAP INDICES EXCEED LIMITS -RESET-'/
     1 ' NO=',I4, ' IMAP=',I4, 'IR=',I4, 
     2 ' NXLO=',I4,' NXHI=',I4,' NZLO=',I4,' NZHI=',I4 /)
  106 FORMAT( 3X, 8I6 )
  107 FORMAT( '     X', 6X, 15F8.3 )
  108 FORMAT( '  NZ     Z' )
  110 FORMAT( /'***WARNING*** INPUT DG CHANGED TO STAY WITHIN ARRAY
     1 LIMITS : DG(Input)=',F10.3, '   DG(Cacl.)=',F10.3 /)
C****
C****
C****
C****
      LF1  = DATA(  1,NO )
      LF2  = DATA(  3,NO )
      DG   = DATA(  4,NO )                                              RAY03130
      MTYP = DATA(  5,NO )                                              RAY03140
      IMAP = DATA(  6,NO )
      D    = DATA( 13,NO )                                              RAY03170
      RB   = DATA( 14,NO )                                              RAY03180
      BF   = DATA( 15,NO )                                              RAY03190
      ALPHA= DATA( 17,NO )                                              RAY03210
      BETA = DATA( 18,NO )                                              RAY03220
      NDX  = DATA( 19,NO )                                              RAY03230
      BET1 = DATA( 20,NO )                                              RAY03240
      GAMA = DATA( 21,NO )                                              RAY03250
      DELT = DATA( 22,NO )                                              RAY03260
      Z11  = DATA( 25,NO )                                              RAY03270
      Z12  = DATA( 26,NO )                                              RAY03280
      Z21  = DATA( 27,NO )                                              RAY03290
      Z22  = DATA( 28,NO )                                              RAY03300
      BR1  = DATA( 41,NO )                                              RAY03310
      BR2  = DATA( 42,NO )                                              RAY03320
      WDE  = DATA( 49,NO )
      WDX  = DATA( 50,NO )
      IF( MTYP .EQ. 0  )  MTYP = 1                                      RAY03350
      IF( WDE .EQ. 0. ) THEN
      WDE = 5.*D
      DATA( 49,NO ) = WDE
      END IF
      IF( WDX .EQ. 0. ) THEN
      WDX = 5.*D
      DATA( 50,NO ) = WDX
      END IF
C****                                                                   RAY03570
C****
C****
      DX1 = (WDE + 2.*DABS(Z11)*DTAN( DABS(ALPHA/57.29578)))/(NXMAX-7)
      DX2 = (WDX + 2.*DABS(Z22)*DTAN( DABS(BETA/57.29578)) )/(NXMAX-7)
      DZ11 = (LF1+DABS(Z11))/(NZ1MAX-3)
      DZ12 = (LF1+DABS(Z12))/(NZ2MAX-3)
      DZ21 = (LF2+DABS(Z21))/(NZ2MAX-3)
      DZ22 = (LF2+DABS(Z22))/(NZ1MAX-3)
      DGI = DG
      IF( DX1  .GT. DG ) DG = DX1
      IF( DX2  .GT. DG ) DG = DX2
      IF( DZ11 .GT. DG ) DG = DZ11
      IF( DZ12 .GT. DG ) DG = DZ12
      IF( DZ21 .GT. DG ) DG = DZ21
      IF( DZ22 .GT. DG ) DG = DZ22
      IF( DG   .NE. DGI) THEN
           DATA(4,NO) = DG
           PRINT 110, DGI, DG
      END IF
C****
C**** IR=1
C****
C****
      IFLAG = 0
      IR = 1
      NDX1 = ( WDE+2.*DABS(Z11)*DTAN( DABS(ALPHA/57.29578) ) )/(2.*DG)
      NXLO = IX-NDX1-3
      NXHI = IX+NDX1+3
      NZLO = IZ-3+(Z12-LF1)/DG
      NZHI = IZ+3+(Z11+LF1)/DG
      NXLMT( IMAP,IR,1 )  = NXLO
      NXLMT( IMAP,IR,2 )  = NXHI
      NZLMT( IMAP,IR,1 )  = NZLO
      NZLMT( IMAP,IR,2 )  = NZHI
C****
C**** CHECK IF INDEX .LT. 1 ; PRINT WARNING
C**** CHECK IF NX .GT. NXMAX  ; PRINT WARNING
C**** CHECK IF NZ .GT. NZMAX  ; PRINT WARNING
C****
      IF( NXLO .LE. 0 ) THEN
           NXLMT(IMAP,IR,1) = 1
           IFLAG = 1
      END IF
      IF( NXHI .GT. NXMAX ) THEN
           NXLMT(IMAP,IR,2) = NXMAX
           IFLAG = 1
      END IF
C****
C****
      IF( NZLO .LE. 0 ) THEN
           NZLMT(IMAP,IR,1) = 1
           IFLAG = 1
      END IF
      IF( NZHI .GT. NZMAX ) THEN
           NZLMT(IMAP,IR,2) = NZMAX
           IFLAG = 1
      END IF
C****
C****
      IF(IFLAG .NE. 0) PRINT 105, NO, IMAP,IR, NXLO, NXHI, NZLO, NZHI
C****
C**** IR=2
C****
      IFLAG = 0
      IR = 2
      NDX1 = ( WDX+2.*DABS(Z22)*DTAN( DABS(BETA/57.29578) ) )/(2.*DG)
      NXLO = IX-NDX1-3
      NXHI = IX+NDX1+3
      NZLO = IZ-3+(Z21-LF2)/DG
      NZHI = IZ+3+(Z22+LF2)/DG
      NXLMT( IMAP,IR,1 )  = NXLO
      NXLMT( IMAP,IR,2 )  = NXHI
      NZLMT( IMAP,IR,1 )  = NZLO
      NZLMT( IMAP,IR,2 )  = NZHI
C****
C**** CHECK IF INDEX .LT. 1 ; PRINT WARNING
C**** CHECK IF NX .GT. NXMAX  ; PRINT WARNING
C**** CHECK IF NZ .GT. NZMAX  ; PRINT WARNING
C****
      IF( NXLO .LE. 0 ) THEN
           NXLMT(IMAP,IR,1) = 1
           IFLAG = 1
      END IF
      IF( NXHI .GT. NXMAX ) THEN
           NXLMT(IMAP,IR,2) = NXMAX
           IFLAG = 1
      END IF
C****
C****
      IF( NZLO .LE. 0 ) THEN
           NZLMT(IMAP,IR,1) = 1
           IFLAG = 1
      END IF
      IF( NZHI .GT. NZMAX ) THEN
           NZLMT(IMAP,IR,2) = NZMAX
           IFLAG = 1
      END IF
C****
C****
      IF(IFLAG .NE. 0) PRINT 105, NO, IMAP,IR, NXLO, NXHI, NZLO, NZHI
C****
C**** CALCULATE MAPS FOR ENTRANCE AND EXIT FRINGE FIELDS
C****
      DO 1 IR=1,2
      IF( IR .EQ. 1 ) THEN
C****
C**** SETUP ENTRANCE FRINGE FIELD PARAMETERS
C****
      XC= RB*DCOS( ALPHA/ 57.29578 )                                    RAY04070
      ZC=-RB*DSIN( ALPHA/ 57.29578 )                                    RAY04080
      BR = BR1                                                          RAY03440
      C0   = DATA( 29,NO )                                              RAY04100
      C1   = DATA( 30,NO )                                              RAY04110
      C2   = DATA( 31,NO )                                              RAY04120
      C3   = DATA( 32,NO )                                              RAY04130
      C4   = DATA( 33,NO )                                              RAY04140
      C5   = DATA( 34,NO )                                              RAY04150
      DELS = DATA( 45,NO )                                              RAY04160
      RCA  = DATA( 47,NO )                                              RAY04170
      S2   = DATA( 51,NO ) / RB    + RCA/2.D0                           RAY04200
      S3   = DATA( 52,NO ) / RB**2                                      RAY04210
      S4   = DATA( 53,NO ) / RB**3 + RCA**3/8.D0                        RAY04220
      S5   = DATA( 54,NO ) / RB**4                                      RAY04230
      S6   = DATA( 55,NO ) / RB**5 + RCA**5/16.D0                       RAY04240
      S7   = DATA( 56,NO ) / RB**6                                      RAY04250
      S8   = DATA( 57,NO ) / RB**7 + RCA**7/25.6D0                      RAY04260
C****
C**** CHECK IF WE HAVE A FLAT BOUNDARY
C****       NSRF=0 FLAT
C****           =1 CURVED
C****
      NSRF = 1
      IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
C****                                                                   RAY05150
      END IF
C****
C****
      IF( IR .EQ. 2 ) THEN
C****                                                                   RAY05160
C**** SETUP EXIT FRINGE FIELD PARAMETERS
C****                                                                   RAY05180
C****                                                                   RAY05190
      XC=-RB*DCOS( BETA / 57.29578 )                                    RAY04070
      ZC=-RB*DSIN( BETA / 57.29578 )                                    RAY04080
      BR   = BR2                                                        RAY05200
      C0   = DATA( 35,NO )                                              RAY05210
      C1   = DATA( 36,NO )                                              RAY05220
      C2   = DATA( 37,NO )                                              RAY05230
      C3   = DATA( 38,NO )                                              RAY05240
      C4   = DATA( 39,NO )                                              RAY05250
      C5   = DATA( 40,NO )                                              RAY05260
      DELS = DATA( 46,NO )                                              RAY05270
      RCA  = DATA( 48,NO )                                              RAY05280
      S2   = DATA( 58,NO ) / RB    + RCA/2.D0                           RAY05310
      S3   = DATA( 59,NO ) / RB**2                                      RAY05320
      S4   = DATA( 60,NO ) / RB**3 + RCA**3/8.D0                        RAY05330
      S5   = DATA( 61,NO ) / RB**4                                      RAY05340
      S6   = DATA( 62,NO ) / RB**5 + RCA**5/16.D0                       RAY05350
      S7   = DATA( 63,NO ) / RB**6                                      RAY05360
      S8   = DATA( 64,NO ) / RB**7 + RCA**7/25.6D0                      RAY05370
C****
C**** CHECK IF WE HAVE A FLAT BOUNDARY
C****       NSRF=0 FLAT
C****           =1 CURVED
C****
      NSRF = 1
      IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
C****                                                                   RAY05150
C****
      END IF
C****
      NXLO = NXLMT(IMAP,IR,1)
      NXHI = NXLMT(IMAP,IR,2)
      NZLO = NZLMT(IMAP,IR,1)
      NZHI = NZLMT(IMAP,IR,2)
C****
C**** MTYP = 3, 4
C****
      IF( (MTYP .EQ. 3) .OR. (MTYP .EQ. 4) ) THEN
           DO 3 I=NXLO,NXHI
           DO 3 J=NZLO,NZHI
           X = (I-IX)*DG
           Z = (J-IZ)*DG
           DX = X-XC
           DZ = Z-ZC
           ZFB = ZEFB(X)
           IF( Z .GT. ZFB ) DZ = ZFB-ZC
           DR = DSQRT( DX*DX + DZ*DZ ) - RB
           CALL NDPP( B0,Z,X,DR)
           BZMAP(I, J, IR, IMAP ) = B0/BF
    3      CONTINUE
           GO TO 4
      END IF
C****
C**** MTYP = 0, 1, 2, 5
C****
      DO 2 I=NXLO,NXHI
      DO 2 J=NZLO,NZHI
      X = (I-IX)*DG
      Z = (J-IZ)*DG
      CALL BDPP( B0,Z,X)
      BZMAP(I, J, IR, IMAP ) = B0/BF
    2 CONTINUE
C****
C****
    4 PRINT 106, IMAP, NO, MTYP, IR, NXLO, NXHI, NZLO, NZHI
    1 CONTINUE
C****
C**** PRINT MAPS
C***
      IF( NP .LE. 100 ) THEN
      DO 10 IR=1,2
      IF( IR .EQ. 1 ) PRINT 100, IMAP
      IF( IR .EQ. 2 ) PRINT 101, IMAP
      NXLO = NXLMT(IMAP,IR,1)
      NXHI = NXLMT(IMAP,IR,2)
      NZLO = NZLMT(IMAP,IR,1)
      NZHI = NZLMT(IMAP,IR,2)
      DO 11 I=NXLO,NXHI,15
      J1 = I
      J2 = I+14
      IF( J1 .GT. NXLO ) PRINT 102
      IF( J2 .GT. NXHI ) J2 = NXHI
      PRINT 103, (J, J=J1,J2)
      PRINT 107, ( (J-51)*DG, J=J1,J2)
      PRINT 108
      DO 12 KSK=NZLO,NZHI
      L = KSK
      PRINT 104, L, (L-IZ)*DG, (BZMAP(J, L, IR, IMAP), J=J1,J2 )
   12 CONTINUE
   11 CONTINUE
   10 CONTINUE
      END IF
C****
      RETURN
      END
      SUBROUTINE BDMP( BZZ, Z, X )
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY02930
      REAL*8  K, NDX                                                    RAY02940
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY02990
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY03020
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY03050
      COMMON  /BLCK26/  JMAP(5), IX, IZ, BZMAP(101,101,2,5)
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY03070
      DIMENSION NXLMT(5,2,2), NZLMT(5,2,2)
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
C****     3        QZ*(QZ-2.*PX+1.)*BZMAP( NXP, NZQ+1, IR, IMAP )  )/2. +
C****     4        PX*QZ * BZMAP( NXP+1, NZQ+1, IR, IMAP ) +
C****     5        (1.+PX*QZ-PX*PX-QZ*QZ) * BZMAP( NXP, NZQ, IR, IMAP ) )
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
      SUBROUTINE DIPOLE ( NO, NP, T, TP ,NUM )                          RAY02830
C****                                                                   RAY02840
C****                                                                   RAY02850
C**** SINGLE MAGNET RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIALRAY02860
C**** EQUATIONS OF MOTION.                                              RAY02870
C     T = TIME                                                          RAY02880
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY02890
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY02900
C****                                                                   RAY02910
C****                                                                   RAY02920
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY02930
      CHARACTER*4 ITITLE
      REAL*8  LF1, LF2, LU1, K, NDX                                     RAY02940
      EXTERNAL BDIP                                                     RAY02950
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY02960
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY02970
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY02980
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY02990
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY03020
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY03030
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY03050
      DIMENSION DATA(  75,200 ) ,ITITLE(200)                            RAY03060
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY03070
C**** DATA  C/ 2.99792458D10/                                           RAY03080
C****                                                                   RAY03090
      IR = 0
      LF1  = DATA(  1,NO )                                              RAY03100
      LU1  = DATA(  2,NO )                                              RAY03110
      LF2  = DATA(  3,NO )                                              RAY03120
      DG   = DATA(  4,NO )                                              RAY03130
      MTYP = DATA(  5,NO )                                              RAY03140
      IMAP = DATA(  6,NO )
      A    = DATA( 11,NO )                                              RAY03150
      B    = DATA( 12,NO )                                              RAY03160
      D    = DATA( 13,NO )                                              RAY03170
      RB   = DATA( 14,NO )                                              RAY03180
      BF   = DATA( 15,NO )                                              RAY03190
      PHI  = DATA( 16,NO )                                              RAY03200
      ALPHA= DATA( 17,NO )                                              RAY03210
      BETA = DATA( 18,NO )                                              RAY03220
      NDX  = DATA( 19,NO )                                              RAY03230
      BET1 = DATA( 20,NO )                                              RAY03240
      GAMA = DATA( 21,NO )                                              RAY03250
      DELT = DATA( 22,NO )                                              RAY03260
      Z11  = DATA( 25,NO )                                              RAY03270
      Z12  = DATA( 26,NO )                                              RAY03280
      Z21  = DATA( 27,NO )                                              RAY03290
      Z22  = DATA( 28,NO )                                              RAY03300
      BR1  = DATA( 41,NO )                                              RAY03310
      BR2  = DATA( 42,NO )                                              RAY03320
      XCR1 = DATA( 43,NO )                                              RAY03330
      XCR2 = DATA( 44,NO )                                              RAY03340
      IF( MTYP .EQ. 0  )  MTYP = 1                                      RAY03350
      DTF1= LF1/ VEL                                                    RAY03360
      DTF2= LF2/ VEL                                                    RAY03370
      DTU = LU1/ VEL                                                    RAY03380
      BX = 0.                                                           RAY03390
      BY = 0.                                                           RAY03400
      BZ = 0.                                                           RAY03410
      BT = 0.                                                           RAY03420
      S = 0.                                                            RAY03430
      BR = BR1                                                          RAY03440
      IF( NP  .GT. 100 ) GO TO 5                                        RAY03450
      PRINT 100, ITITLE(NO)                                             RAY03460
  100 FORMAT(  ' DIPOLE  ****  ', A4,'  ****************************'/) RAY03470
      PRINT 101                                                         RAY03480
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,RAY03490
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY03500
     2   6HPHI MR , 6X, 1HB             )                               RAY03510
      CALL PRNT2 (TP,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )RAY03520
      PRINT 103                                                         RAY03530
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  RAY03540
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY03550
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY03560
C****                                                                   RAY03570
    5 COSA =DCOS( ALPHA/57.29578)                                       RAY03580
      SINA =DSIN( ALPHA/57.29578)                                       RAY03590
      TC(1) = ( A-ZA ) * SINA - ( XA + XCR1 ) * COSA                    RAY03600
      TC(2) = YA                                                        RAY03610
      TC(3) = ( A-ZA ) * COSA + ( XA + XCR1 ) * SINA                    RAY03620
      TC(4) = -VZA * SINA - VXA * COSA                                  RAY03630
      TC(5) = VYA                                                       RAY03640
      TC(6) = -VZA * COSA + VXA * SINA                                  RAY03650
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY03660
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY03670
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 RAY03680
C****                                                                   RAY03690
C****                                                                   RAY03700
      IF(  BR1  .EQ.  0. ) GO TO 20                                     RAY03710
      IN = 4                                                            RAY03720
      XDTF1 = DTF1                                                      RAY03730
      IF(  Z11  .GT.  TC(3) )  XDTF1 = -DTF1                            RAY03740
      IF( NP  .LE. 100) PRINT 108                                       RAY03750
  108 FORMAT(/ ' CONSTANT FIELD CORRECTION IN FRINGE FIELD REGION    ' )RAY03760
      NSTEP = 0                                                         RAY03770
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            RAY03780
   21 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY03790
      DO 22  I=1,NP                                                     RAY03800
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            RAY03810
      NSTEP = NSTEP + 1                                                 RAY03820
      NUM = NUM+1
      TP = TP+XDTF1*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY03821
      IF(  XDTF1  .LT.  0. )  GO TO 23                                  RAY03830
      IF(  Z11  .GE.  TC(3) )  GO TO 24                                 RAY03840
      GO TO 22                                                          RAY03850
   23 IF(  Z11  .LE.  TC(3) )  GO TO 24                                 RAY03860
   22 CONTINUE                                                          RAY03870
      GO TO 21                                                          RAY03880
   24 DO 2 I=1,2                                                        RAY03890
      XDTF1 = (TC(3) - Z11) / DABS(TC(6))                               RAY03900
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            RAY03910
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            RAY03920
    2 TP = TP+XDTF1*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY03930
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY03940
C****                                                                   RAY03950
C****                                                                   RAY03960
C****                                                                   RAY03970
   20 TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              RAY03980
      TC(1) = TC(1) + TDT * TC(4)                                       RAY03990
      TC(2) = TC(2) + TDT * TC(5)                                       RAY04000
      TC(3) = TC(3) + TDT * TC(6)                                       RAY04010
      T = T + TDT                                                       RAY04020
      TP = TP+TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY04030
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN                             RAY04040
C****                                                                   RAY04050
      IR = 1
      IN = 1                                                            RAY04060
      XC= RB*DCOS( ALPHA/ 57.29578 )                                    RAY04070
      ZC=-RB*DSIN( ALPHA/ 57.29578 )                                    RAY04080
C****                                                                   RAY04090
      C0   = DATA( 29,NO )                                              RAY04100
      C1   = DATA( 30,NO )                                              RAY04110
      C2   = DATA( 31,NO )                                              RAY04120
      C3   = DATA( 32,NO )                                              RAY04130
      C4   = DATA( 33,NO )                                              RAY04140
      C5   = DATA( 34,NO )                                              RAY04150
      DELS = DATA( 45,NO )                                              RAY04160
      RCA  = DATA( 47,NO )                                              RAY04170
      WDIP = DATA( 49,NO )
      S2   = DATA( 51,NO ) / RB    + RCA/2.D0                           RAY04200
      S3   = DATA( 52,NO ) / RB**2                                      RAY04210
      S4   = DATA( 53,NO ) / RB**3 + RCA**3/8.D0                        RAY04220
      S5   = DATA( 54,NO ) / RB**4                                      RAY04230
      S6   = DATA( 55,NO ) / RB**5 + RCA**5/16.D0                       RAY04240
      S7   = DATA( 56,NO ) / RB**6                                      RAY04250
      S8   = DATA( 57,NO ) / RB**7 + RCA**7/25.6D0                      RAY04260
C****
C**** CHECK IF WE HAVE A FLAT BOUNDARY
C****       NSRF=0 FLAT
C****           =1 CURVED
C****
      NSRF = 1
      IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
      IF( NP  .LE. 100) PRINT 104                                       RAY04270
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY04280
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BDIP,  0    )            RAY04290
      NSTEP = 0                                                         RAY04300
    6 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04310
      DO 7 I = 1, NP                                                    RAY04320
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BDIP,  1    )            RAY04330
      NSTEP = NSTEP + 1                                                 RAY04340
      NUM = NUM+1
      TP = TP+DTF1*VEL
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04341
      IF( Z12 .GE. TC(3) ) GO TO 8                                      RAY04350
    7 CONTINUE                                                          RAY04360
      GO TO 6                                                           RAY04370
    8 CONTINUE                                                          RAY04380
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            RAY04390
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  0    )            RAY04400
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, BDIP,  1    )            RAY04410
      TP = TP+XDTF1*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04420
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04430
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY04440
C***                                                                    RAY04450
C***  UNIFORM FIELD REGION                                              RAY04460
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              RAY04470
C***                                                                    RAY04480
      COPAB =DCOS( (PHI-ALPHA-BETA)/57.29578)                           RAY04490
      SIPAB =DSIN( (PHI-ALPHA-BETA)/57.29578)                           RAY04500
      COSPB =DCOS( (PHI/2.-BETA)/57.29578 )                             RAY04510
      SINPB =DSIN( (PHI/2.-BETA)/57.29578 )                             RAY04520
      SIP2 =DSIN( (PHI/2.)/57.29578 )                                   RAY04530
      XT = TC(1)                                                        RAY04540
      ZT = TC(3)                                                        RAY04550
      VXT = TC(4)                                                       RAY04560
      VZT = TC(6)                                                       RAY04570
      TC(3) = - ZT  *COPAB +  XT  *SIPAB -2.*RB*SIP2*COSPB              RAY04580
      TC(1) = - ZT  *SIPAB -  XT  *COPAB -2.*RB*SIP2*SINPB              RAY04590
      TC(6) = - VZT *COPAB +  VXT *SIPAB                                RAY04600
      TC(4) = - VZT *SIPAB -  VXT *COPAB                                RAY04610
C****                                                                   RAY04620
C****                                                                   RAY04630
C**** UNIFORM FIELD INTEGRATION REGION                                  RAY04640
C****                                                                   RAY04650
C****                                                                   RAY04660
      S = 0.
      IN = 2                                                            RAY04670
      XC=-RB*DCOS( BETA / 57.29578 )                                    RAY04680
      ZC=-RB*DSIN( BETA / 57.29578 )                                    RAY04690
      IF( NP  .LE. 100) PRINT 106                                       RAY04700
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            RAY04710
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   RAY04720
C****                                                                   RAY04730
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT RAY04740
C****                                                                   RAY04750
      IF( NP  .LE. 100) PRINT 102                                       RAY04760
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         RAY04770
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES, BDIP,  0    )            RAY04780
      NSTEP = 0                                                         RAY04790
   16 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04800
      DO 17  I =1, NP                                                   RAY04810
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES, BDIP,  1    )            RAY04820
      TP = TP-DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY04830
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04831
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  RAY04840
   17 CONTINUE                                                          RAY04850
      GO TO 16                                                          RAY04860
   18 CONTINUE                                                          RAY04870
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY04880
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  0    )            RAY04890
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  1    )            RAY04900
      TP = TP+XDTU*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04910
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04920
      IF( NP  .LE. 100) PRINT 107                                       RAY04930
  107 FORMAT( / )                                                       RAY04940
      GO TO 19                                                          RAY04950
C****                                                                   RAY04960
C****                                                                   RAY04970
   15 CONTINUE                                                          RAY04980
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BDIP,  0    )            RAY04990
      NSTEP = 0                                                         RAY05000
    9 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05010
      DO 10  I =1, NP                                                   RAY05020
      CALL FNMIRK( 6, T, DTU, TC, DTC, DS, ES, BDIP,  1    )            RAY05030
      TP = TP+DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY05040
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY05041
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  RAY05050
   10 CONTINUE                                                          RAY05060
      GO TO 9                                                           RAY05070
   11 CONTINUE                                                          RAY05080
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY05090
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  0    )            RAY05100
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, BDIP,  1    )            RAY05110
      TP = TP+XDTU*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05120
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY05130
   19 CONTINUE                                                          RAY05140
C****                                                                   RAY05160
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     RAY05170
C****                                                                   RAY05180
      BR   = BR2                                                        RAY05200
      C0   = DATA( 35,NO )                                              RAY05210
      C1   = DATA( 36,NO )                                              RAY05220
      C2   = DATA( 37,NO )                                              RAY05230
      C3   = DATA( 38,NO )                                              RAY05240
      C4   = DATA( 39,NO )                                              RAY05250
      C5   = DATA( 40,NO )                                              RAY05260
      DELS = DATA( 46,NO )                                              RAY05270
      RCA  = DATA( 48,NO )                                              RAY05280
      WDIP = DATA( 50,NO )
      S2   = DATA( 58,NO ) / RB    + RCA/2.D0                           RAY05310
      S3   = DATA( 59,NO ) / RB**2                                      RAY05320
      S4   = DATA( 60,NO ) / RB**3 + RCA**3/8.D0                        RAY05330
      S5   = DATA( 61,NO ) / RB**4                                      RAY05340
      S6   = DATA( 62,NO ) / RB**5 + RCA**5/16.D0                       RAY05350
      S7   = DATA( 63,NO ) / RB**6                                      RAY05360
      S8   = DATA( 64,NO ) / RB**7 + RCA**7/25.6D0                      RAY05370
C****
C**** CHECK IF WE HAVE A FLAT BOUNDARY
C****       NSRF=0 FLAT
C****           =1 CURVED
C****
      NSRF = 1
      IF( (S2 .EQ. 0.) .AND. (S3 .EQ. 0.) .AND. (S4 .EQ. 0.) .AND.
     1    (S5 .EQ. 0.) .AND. (S6 .EQ. 0.) .AND. (S7 .EQ. 0.) .AND.
     2    (S8 .EQ. 0.) )  NSRF = 0
      IR = 2
      IN = 3                                                            RAY05380
      IF( NP  .LE. 100) PRINT 104                                       RAY05390
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BDIP,  0    )            RAY05400
      NSTEP = 0                                                         RAY05410
   12 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05420
      DO 13  I =1, NP                                                   RAY05430
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BDIP,  1    )            RAY05440
      TP = TP+DTF2*VEL
      NSTEP = NSTEP + 1                                                 RAY05450
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY05451
      IF( TC(3) .GE. Z22 )  GO TO 14                                    RAY05460
   13 CONTINUE                                                          RAY05470
      GO TO 12                                                          RAY05480
   14 CONTINUE                                                          RAY05490
      XDTF2 = ( Z22 - TC(3) ) /DABS( TC(6) )                            RAY05500
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            RAY05510
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            RAY05520
      TP = TP+XDTF2*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05530
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY05540
C****                                                                   RAY05550
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY05560
C****                                                                   RAY05570
      COSB =DCOS( BETA/57.29578 )                                       RAY05580
      SINB =DSIN( BETA/57.29578 )                                       RAY05590
      XT = TC(1)                                                        RAY05600
      ZT = TC(3)                                                        RAY05610
      VXT = TC(4)                                                       RAY05620
      VZT = TC(6)                                                       RAY05630
      TC(3) = ZT*COSB - XT*SINB - B                                     RAY05640
      TC(1) = ZT*SINB + XT*COSB - XCR2                                  RAY05650
      TC(6) = VZT*COSB - VXT*SINB                                       RAY05660
      TC(4) = VZT*SINB + VXT*COSB                                       RAY05670
      IF( NP  .LE. 100) PRINT 109                                       RAY05680
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05690
C****                                                                   RAY05700
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY05710
C****                                                                   RAY05720
      IF(  BR2  .EQ.  0. ) GO TO 30                                     RAY05730
      IN = 4                                                            RAY05740
      XDTF2 = DTF2                                                      RAY05750
      IF( TC(3)  .GT. 0. ) XDTF2 = -DTF2                                RAY05760
      IF( NP  .LE. 100) PRINT 108                                       RAY05770
      NSTEP = 0                                                         RAY05780
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            RAY05790
   31 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05800
      DO 32  I=1,NP                                                     RAY05810
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            RAY05820
      TP = TP+XDTF2*VEL
      NSTEP = NSTEP + 1                                                 RAY05830
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY05831
      IF( XDTF2  .LT. 0. ) GO TO 33                                     RAY05840
      IF( TC(3)  .GE. 0. ) GO TO 34                                     RAY05850
      GO TO 32                                                          RAY05860
   33 IF( TC(3)  .LE. 0. ) GO TO 34                                     RAY05870
   32 CONTINUE                                                          RAY05880
      GO TO 31                                                          RAY05890
   34 DO 3 I=1,2                                                        RAY05900
      XDTF2 = -TC(3) / DABS(TC(6))                                      RAY05910
      TP = TP+XDTF2*VEL
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  0    )            RAY05920
    3 CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BDIP,  1    )            RAY05930
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY05940
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY05950
C****                                                                   RAY05960
C****                                                                   RAY05970
C****                                                                   RAY05980
   30 TDT = -TC(3) /DABS( TC(6) )                                       RAY05990
      TC(1) = TC(1) + TDT * TC(4)                                       RAY06000
      TC(2) = TC(2) + TDT * TC(5)                                       RAY06010
      TC(3) = TC(3) + TDT * TC(6)                                       RAY06020
      T = T + TDT                                                       RAY06030
      TP = TP+TDT*VEL
      BX = 0.                                                           RAY06050
      BY = 0.                                                           RAY06060
      BZ = 0.                                                           RAY06070
      BT = 0.                                                           RAY06080
      S  = 0.                                                           RAY06090
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
      RETURN                                                            RAY06250
99      CALL PRNT4(NO, IN)                                              RAY06251
        RETURN                                                          RAY06252
      END                                                               RAY06260
      SUBROUTINE BDIP                                                   RAY06270
C****                                                                   RAY06280
C****                                                                   RAY06290
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 RAY06300
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           RAY06310
C**** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION              RAY06320
C**** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R)                    RAY06330
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  RAY06340
C**** MTYP=6  :    PRETZEL MAGNET
C****                                                                   RAY06350
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO RAY06360
C**** AXES (Z,X) IS GIVEN BY                                            RAY06370
C****                                                                   RAY06380
C****                                                                   RAY06390
C****                                                                   RAY06400
C**** B0  = B( 0, 0 )                                                   RAY06410
C**** B1  = B( 1, 0 )                                                   RAY06420
C**** B2  = B( 2, 0 )                                                   RAY06430
C**** B3  = B( 1, 1 )                                                   RAY06440
C**** B4  = B( 1,-1 )                                                   RAY06450
C**** B5  = B( 0, 1 )                                                   RAY06460
C**** B6  = B( 0, 2 )                                                   RAY06470
C**** B7  = B( 0,-1 )                                                   RAY06480
C**** B8  = B( 0,-2 )                                                   RAY06490
C**** B9  = B(-1, 0 )                                                   RAY06500
C**** B10 = B(-2, 0 )                                                   RAY06510
C**** B11 = B(-1, 1 )                                                   RAY06520
C**** B12 = B(-1,-1 )                                                   RAY06530
C****                                                                   RAY06540
C****                                                                   RAY06550
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      REAL*8  NDX, K                                                    RAY06570
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY06580
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY06610
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY06620
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY06630
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY06640
      DIMENSION TC(6), DTC(6)                                           RAY06650
C****                                                                   RAY06700
C****                                                                   RAY06710
      GO TO ( 10,10,6,6,10,21 )     ,MTYP                               RAY06720
      CALL EXIT                                                         RAY06730
      RETURN                                                            RAY06740
    6 CALL NDIP                                                         RAY06750
      RETURN                                                            RAY06760
   21 CALL BPRETZ
      RETURN
C****                                                                   RAY06770
C**** MTYP = 1 , 2, 5                                                   RAY06780
C**** UNIFORM FIELD MAGNETS                                             RAY06790
C****                                                                   RAY06800
   10 CONTINUE                                                          RAY06810
      GO TO( 2, 1, 2, 4 ) , IN                                          RAY06820
    7 PRINT 8, IN                                                       RAY06830
    8 FORMAT(  35H0 ERROR -GO TO -  IN BFUN   IN=        ,I5 )          RAY06840
    1 BX = 0.                                                           RAY06850
      BY = BF                                                           RAY06860
      BZ = 0.                                                           RAY06870
      BT = BF                                                           RAY06880
      RETURN                                                            RAY06890
C****
C****
    2 X = TC(1)                                                         RAY06900
      Y = TC(2)                                                         RAY06910
      Z = TC(3)                                                         RAY06920
C****
C**** MTYP=1,2,5 MAP ROUTINES/INTERPOLATE
C****
      IF( IMAP .EQ. 0 ) GO TO 5
      CALL BDMP ( B0, Z, X )                                            RAY08030
      S0 = 0.                                                           RAY08040
      IF( Y .NE. 0. )   GO TO 11                                        RAY08050
      BX = 0.                                                           RAY08060
      BY = B0                                                           RAY08070
      BZ = 0.                                                           RAY08080
      BT = B0                                                           RAY08090
      RETURN                                                            RAY08100
   11 CALL BDMP ( B1 , Z + DG, X  )                                     RAY08110
      CALL BDMP ( B2 , Z + 2.*DG, X  )                                  RAY08120
      CALL BDMP ( B3 , Z + DG, X + DG  )                                RAY08130
      CALL BDMP ( B4 , Z + DG, X - DG  )                                RAY08140
      CALL BDMP ( B5 , Z , X + DG  )                                    RAY08150
      CALL BDMP ( B6 , Z , X + 2.*DG  )                                 RAY08160
      CALL BDMP ( B7 , Z , X - DG  )                                    RAY08170
      CALL BDMP ( B8 , Z , X - 2.*DG  )                                 RAY08180
      CALL BDMP ( B9 , Z - DG, X  )                                     RAY08190
      CALL BDMP ( B10, Z - 2.*DG, X  )                                  RAY08200
      CALL BDMP ( B11, Z - DG, X + DG  )                                RAY08210
      CALL BDMP ( B12, Z - DG, X - DG  )                                RAY08220
      GO TO 9
C****
C**** MTYP = 1,2,5   STANDARD ROUTINES
C****
    5 CALL BDPP ( B0, Z, X )                                            RAY08030
      S0 = S                                                            RAY08040
      IF( Y .NE. 0. )   GO TO 3                                         RAY08050
      BX = 0.                                                           RAY08060
      BY = B0                                                           RAY08070
      BZ = 0.                                                           RAY08080
      BT = B0                                                           RAY08090
      RETURN                                                            RAY08100
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
      CALL BDPP ( B1 , Z + DG, X  )                                     RAY08110
      CALL BDPP ( B2 , Z + 2.*DG, X  )                                  RAY08120
      CALL BDPP ( B3 , Z + DG, X + DG  )                                RAY08130
      CALL BDPP ( B4 , Z + DG, X - DG  )                                RAY08140
      CALL BDPP ( B5 , Z , X + DG  )                                    RAY08150
      CALL BDPP ( B6 , Z , X + 2.*DG  )                                 RAY08160
      CALL BDPP ( B7 , Z , X - DG  )                                    RAY08170
      CALL BDPP ( B8 , Z , X - 2.*DG  )                                 RAY08180
      CALL BDPP ( B9 , Z - DG, X  )                                     RAY08190
      CALL BDPP ( B10, Z - 2.*DG, X  )                                  RAY08200
      CALL BDPP ( B11, Z - DG, X + DG  )                                RAY08210
      CALL BDPP ( B12, Z - DG, X - DG  )                                RAY08220
      GO TO 9
C****     
C**** MTYP = 2
C**** NON-MIDPLANE FRINGING FIELD REGION
C****
   12 CALL BDPPX(  B1 , 1, 0 )
      CALL BDPPX(  B2 , 2, 0 )
      CALL BDPPX(  B3 , 1, 1 )
      CALL BDPPX(  B4 , 1,-1 )
      CALL BDPPX(  B5 , 0, 1 )
      CALL BDPPX(  B6 , 0, 2 )
      CALL BDPPX(  B7 , 0,-1 )
      CALL BDPPX(  B8 , 0,-2 )
      CALL BDPPX(  B9 ,-1, 0 )
      CALL BDPPX(  B10,-2, 0 )
      CALL BDPPX(  B11,-1, 1 )
      CALL BDPPX(  B12,-1,-1 )
C****
C**** CALCULATE BX, BY, AND BZ
C****
    9 S = S0                                                            RAY08240
      YG1 = Y/DG                                                        RAY08250
      YG2 = YG1*YG1                                                     RAY08260
      YG3 = YG2*YG1                                                     RAY08270
      YG4 = YG3*YG1                                                     RAY08280
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     RAY08290
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             RAY08300
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               RAY08310
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            RAY08320
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     RAY08330
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  RAY08340
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       RAY08350
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      RAY08360
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              RAY08370
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              RAY08380
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                RAY08390
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             RAY08400
      BT = DSQRT(BX*BX + BY*BY + BZ*BZ)                                 RAY08410
      RETURN                                                            RAY08420
C****
C**** CONSTANT FIELD REGION
C****
    4 BX = 0.                                                           RAY08430
      BY = BR                                                           RAY08440
      BZ = 0.                                                           RAY08450
      BT = BR                                                           RAY08460
      RETURN                                                            RAY08470
      END                                                               RAY08480
      SUBROUTINE  BDPP ( BFLD, Z, X )                                   RAY08490
C****                                                                   RAY08500
C****                                                                   RAY08510
C****                                                                   RAY08520
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 RAY08530
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           RAY08540
C****              MORE ACCURATE 3'RD AND HIGHER ORDER CURVATURES       RAY08550
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  RAY08560
C****                                                                   RAY08570
C****                                                                   RAY08580
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY08590
      REAL*8  NDX, K                                                    RAY08600
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY08610
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY08640
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY08650
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY08660
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY08670
      DIMENSION TC(6), DTC(6)                                           RAY08680
C****                                                                   RAY08690
      GO TO (10,2,6,6,11,6 ) , MTYP                                     RAY08700
    6 CALL EXIT                                                         RAY08710
      RETURN                                                            RAY08720
C****                                                                   RAY08730
C**** MTYP=1  :    UNIFORM FIELD STANDARD APPROXIMATION                 RAY08740
C****                                                                   RAY08750
   10 S = ( Z-ZEFB(X) )/D + DELS
      GO TO 13                                                          RAY08810
C****
C**** MTYP=2  :    UNIFORM FIELD, ITERATIVE CALCULATION
C****
    2 CALL SDIP( X,Z )
      GO TO 13
C****                                                                   RAY08820
C**** MTYP=5  :    UNIFORM FIELD, CIRCULAR POLE OPTION                  RAY08830
C****                                                                   RAY08840
   11 IF( DABS(RCA)  .GE. 1.D-08  ) GO TO 12                            RAY08850
      S = Z/D + DELS                                                    RAY08860
      GO TO 13                                                          RAY08870
   12 A = 1./RCA                                                        RAY08880
      S = ( DSIGN(1.D0,A) * DSQRT( (Z+A)**2 + X*X ) - A ) / D + DELS    RAY08890
      GO TO 13
C****
C**** ENTRY FOR OFF MIDPLANE FIELD
C****
      ENTRY BDPPX( BFLD, I, J )
      CALL SIJ( I, J )
   13 CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                            RAY08900
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )               RAY08910
      E=DEXP(CS)                                                        RAY08920
      P0 = 1.0 + E                                                      RAY08930
      DB=BF-BR                                                          RAY08940
      BFLD=BR + DB/P0                                                   RAY08950
C****
C**** PRINT 100, X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN                                                            RAY08960
      END                                                               RAY08970
      SUBROUTINE NDIP                                                   RAY08980
C****                                                                   RAY08990
C****                                                                   RAY09000
C**** MTYP = 3 OR 4                                                     RAY09010
C**** THIS VERSION OF BFUN IS MAINLY FOR NONUNIFORM FIELD MAGNETS       RAY09020
C**** THE CENTRAL FIELD REGION IS REPRESENTED TO 3'RD ORDER ON-AND-     RAY09030
C**** OFF THE MIDPLANE BY ANALYTIC EXPRESSIONS. SEE SLAC NO. 75         RAY09040
C**** FRINGE FIELD REGIONS REPRESENTED BY FERMI TYPE FALL-OFF           RAY09050
C**** ALONG WITH RADIAL FALL-OFF                                        RAY09060
C**** COMPONENTS OF 'B' IN FRINGE REGION EVALUATED BY NUMERICAL METHODS RAY09070
C****                                                                   RAY09080
C****                                                                   RAY09090
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO RAY09100
C**** AXES (Z,X) IS GIVEN BY                                            RAY09110
C****                                                                   RAY09120
C****                                                                   RAY09130
C**** B0  = B( 0, 0 )                                                   RAY09140
C**** B1  = B( 1, 0 )                                                   RAY09150
C**** B2  = B( 2, 0 )                                                   RAY09160
C**** B3  = B( 1, 1 )                                                   RAY09170
C**** B4  = B( 1,-1 )                                                   RAY09180
C**** B5  = B( 0, 1 )                                                   RAY09190
C**** B6  = B( 0, 2 )                                                   RAY09200
C**** B7  = B( 0,-1 )                                                   RAY09210
C**** B8  = B( 0,-2 )                                                   RAY09220
C**** B9  = B(-1, 0 )                                                   RAY09230
C**** B10 = B(-2, 0 )                                                   RAY09240
C**** B11 = B(-1, 1 )                                                   RAY09250
C**** B12 = B(-1,-1 )                                                   RAY09260
C****                                                                   RAY09270
C****                                                                   RAY09280
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY09290
      REAL*8  NDX, K                                                    RAY09300
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY09310
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY09340
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY09350
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY09360
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY09370
      DIMENSION TC(6), DTC(6)                                           RAY09380
      X = TC(1)                                                         RAY09390
      Y = TC(2)                                                         RAY09400
      Z = TC(3)                                                         RAY09410
      DX = X - XC                                                       RAY09420
      DZ = Z - ZC                                                       RAY09430
      RP =DSQRT( DX*DX + DZ*DZ )                                        RAY09440
      DR = RP - RB                                                      RAY09450
      GO TO ( 1, 2, 1, 14 ), IN                                         RAY09460
    7 PRINT 8, IN, MTYP                                                 RAY09470
      CALL EXIT                                                         RAY09480
    8 FORMAT (    '0 ERROR -GO TO -  IN BFUN   IN=', I3, '   MTYP=',I4 )RAY09490
    2 DRR1 = DR/RB                                                      RAY09500
      DRR2 = DRR1*DRR1                                                  RAY09510
      DRR3 = DRR2*DRR1                                                  RAY09520
      DRR4 = DRR3*DRR1                                                  RAY09530
      IF( Y .NE. 0. )  GO TO 4                                          RAY09540
C****
C**** MID-PLANE UNIFORM FIELD REGION
C****
      BX = 0.                                                           RAY09550
      BY = 0.                                                           RAY09560
      IF( MTYP .EQ. 3) BY=                                              RAY09570
     1     BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 )    RAY09580
      IF( MTYP .EQ. 4) BY= BF/ (1. + NDX*DRR1 )                         RAY09590
      BZ = 0.                                                           RAY09600
      BT = BY                                                           RAY09610
      RETURN                                                            RAY09620
C****
C**** NON MID-PLANE UNIFORM FIELD REGION
C****
    4 YR1 = Y/RB                                                        RAY09630
      YR2 = YR1*YR1                                                     RAY09640
      YR3 = YR2*YR1                                                     RAY09650
      YR4 = YR3*YR1                                                     RAY09660
      RR1 = RB/RP                                                       RAY09670
      RR2 = RR1*RR1                                                     RAY09680
      RR3 = RR2*RR1                                                     RAY09690
      IF( MTYP .EQ. 3 ) GO TO 11                                        RAY09700
      IF( MTYP .EQ. 4 ) GO TO 12                                        RAY09710
      GO TO 7                                                           RAY09720
C****
C**** MTYP = 3
C****
   11 BRR = BF*( ( -NDX + 2.*BET1*DRR1 + 3.*GAMA*DRR2 + 4.*DELT*DRR3 )  RAY09730
     1   *YR1 - (NDX*RR2 + 2.*BET1*RR1*(1.-RR1*DRR1) +                  RAY09740
     2   3.*GAMA*( 2. + 2.*RR1*DRR1 - RR2*DRR2 ) +                      RAY09750
     3   4.*DELT*( 6.*DRR1 + 3.*RR1*DRR2 - RR2*DRR3 ))*YR3/6. )         RAY09760
      BY = BF* ( 1. - NDX*DRR1 + BET1*DRR2 + GAMA*DRR3 + DELT*DRR4 -    RAY09770
     1   .5*YR2*( -NDX*RR1 + 2.*BET1*( 1. + RR1*DRR1) +                 RAY09780
     2   3.*GAMA*DRR1*( 2. + RR1*DRR1) + 4.*DELT*DRR2*(3. + RR1*DRR1) ) RAY09790
     3   + YR4*( -NDX*RR3 + 2.*BET1*( RR3*DRR1 - RR2) +                 RAY09800
     4   3.*GAMA*( 4.*RR1 - 2.*RR2*DRR1 + RR3*DRR2 ) +                  RAY09810
     5   4.*DELT*( 6. + 12.*RR1*DRR1 - 3.*RR2*DRR2 + RR3*DRR3 ) )/24. ) RAY09820
      GO TO 13                                                          RAY09830
C****
C**** MTYP = 4
C****
   12 DNR1 = 1. + NDX*DRR1                                              RAY09840
      DNR2 = DNR1*DNR1                                                  RAY09850
      DNR3 = DNR2*DNR1                                                  RAY09860
      DNR4 = DNR3*DNR1                                                  RAY09870
      DNR5 = DNR4*DNR1                                                  RAY09880
      BRR = BF*NDX*( -YR1/DNR2 + YR3*( 6.*NDX*NDX/DNR4 -                RAY09890
     1   2.*NDX*RR1/DNR3 - RR2/DNR2 ) /6.  )                            RAY09900
      BY = BF*( 1./DNR1 + .5*YR2*NDX*( -2.*NDX/DNR3 + RR1/DNR2) +       RAY09910
     2   YR4*NDX*( 24.*NDX**3 /DNR5 - 12.*NDX*NDX*RR1/DNR4 -            RAY09920
     3   2.*NDX*RR2/DNR3 - RR3/DNR2 ) /24.  )                           RAY09930
C****
C****
   13 BX = BRR*DX/RP                                                    RAY09940
      BZ = BRR*DZ/RP                                                    RAY09950
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 RAY09960
      RETURN                                                            RAY09970
C****
C**** FRINGING FIELD ZONES
C****                                                                   RAY09980
C**** CHECK IF FIELD MAP CALCULATED
C****                                                                   RAY09990
    1 CONTINUE
      IF( IMAP .EQ. 0 ) GO TO 3
C****
C**** MTYP=3,4 MAP ROUTINES/INTERPOLATE
C****
C****
      CALL BDMP ( B0, Z, X )                                            RAY08030
      IF( Y .NE. 0. )   GO TO 5                                         RAY08050
      BX = 0.                                                           RAY08060
      BY = B0                                                           RAY08070
      BZ = 0.                                                           RAY08080
      BT = B0                                                           RAY08090
      RETURN                                                            RAY08100
    5 CALL BDMP ( B1 , Z + DG, X  )                                     RAY08110
      CALL BDMP ( B2 , Z + 2.*DG, X  )                                  RAY08120
      CALL BDMP ( B3 , Z + DG, X + DG  )                                RAY08130
      CALL BDMP ( B4 , Z + DG, X - DG  )                                RAY08140
      CALL BDMP ( B5 , Z , X + DG  )                                    RAY08150
      CALL BDMP ( B6 , Z , X + 2.*DG  )                                 RAY08160
      CALL BDMP ( B7 , Z , X - DG  )                                    RAY08170
      CALL BDMP ( B8 , Z , X - 2.*DG  )                                 RAY08180
      CALL BDMP ( B9 , Z - DG, X  )                                     RAY08190
      CALL BDMP ( B10, Z - 2.*DG, X  )                                  RAY08200
      CALL BDMP ( B11, Z - DG, X + DG  )                                RAY08210
      CALL BDMP ( B12, Z - DG, X - DG  )                                RAY08220
      GO TO 15
C****
C**** MTYP=3, 4  STANDARD ROUTINES
C****
    3 ZFB = ZEFB(X)
      IF( Z .GT. ZFB ) DR = DSQRT( DX*DX + (ZFB-ZC)**2 ) - RB
      CALL NDPP( B0, Z, X, DR      )                                    RAY10040
      IF( Y  .NE. 0. )  GO TO 6                                         RAY10050
C****
C**** MID-PLANE FRINGING FIELD REGION
C****
      BX = 0.                                                           RAY10060
      BY = B0                                                           RAY10070
      BZ = 0.                                                           RAY10080
      BT   = B0                                                         RAY10090
      RETURN                                                            RAY10100
C****                                                                   RAY10110
C**** NON MID-PLANE FRINGING FIELD REGION
C****                                                                   RAY10120
    6 IF( Z .GT. ZFB )  GO TO 9                                         RAY10130
      DR1  =       (DSQRT( DX*DX + (DZ+DG)**2 ) - RB )                  RAY10140
      DR2  =       (DSQRT( DX*DX + (DZ+2.*DG)**2 ) - RB )               RAY10150
      DR3  =       (DSQRT( (DX+DG)**2 + (DZ+DG)**2 )  - RB )            RAY10160
      DR4  =       (DSQRT( (DX-DG)**2 + (DZ+DG)**2 )  - RB )            RAY10170
      DR5  =       (DSQRT( (DX+DG)**2 + DZ*DZ ) - RB )                  RAY10180
      DR6  =       (DSQRT( (DX+ 2.*DG)**2 + DZ*DZ ) - RB )              RAY10190
      DR7  =       (DSQRT( (DX-DG)**2 + DZ*DZ ) - RB )                  RAY10200
      DR8  =       (DSQRT( (DX- 2.*DG)**2 + DZ*DZ ) - RB )              RAY10210
      DR9  =       (DSQRT( DX*DX + (DZ-DG)**2 ) - RB )                  RAY10220
      DR10 =       (DSQRT( DX*DX + (DZ-2.*DG)**2 ) - RB )               RAY10230
      DR11 =       (DSQRT( (DX+DG)**2 + (DZ-DG)**2 )  - RB )            RAY10240
      DR12 =       (DSQRT( (DX-DG)**2 + (DZ-DG)**2 )  - RB )            RAY10250
      GO TO 10                                                          RAY10260
    9 CONTINUE
      DR1  = DR
      DR2  = DR                                                         RAY10280
      DR9  = DR                                                         RAY10290
      DR10 = DR                                                         RAY10300
      XP = X+DG                                                         RAY06970
      ZFB = ZEFB(XP)
      DX = XP-XC
      DR3  = DSQRT( DX*DX + (ZFB-ZC)**2 ) - RB                          RAY10310
      DR5  = DR3                                                        RAY10320
      DR11 = DR3                                                        RAY10330
      XP = X-DG                                                         RAY06970
      ZFB = ZEFB(XP)
      DX = XP-XC
      DR4  = DSQRT( DX*DX + (ZFB-ZC)**2 ) - RB                          RAY10310
      DR7  = DR4                                                        RAY10350
      DR12 = DR4                                                        RAY10360
      XP = X+2.*DG                                                      RAY06970
      ZFB = ZEFB(XP)
      DX = XP-XC
      DR6  = DSQRT( DX*DX + (ZFB-ZC)**2 ) - RB                          RAY10310
      XP = X-2.*DG                                                      RAY06970
      ZFB = ZEFB(XP)
      DX = XP-XC
      DR8  = DSQRT( DX*DX + (ZFB-ZC)**2 ) - RB                          RAY10310
C****                                                                   RAY10390
C****                                                                   RAY10400
   10 CONTINUE
C**** CALL NDPP ( B1 , Z + DG, X  , DR1 )                               RAY10410
C**** CALL NDPP ( B2 , Z + 2.*DG, X  , DR2 )                            RAY10420
C**** CALL NDPP ( B3 , Z + DG, X + DG  , DR3 )                          RAY10430
C**** CALL NDPP ( B4 , Z + DG, X - DG  , DR4 )                          RAY10440
C**** CALL NDPP ( B5 , Z , X + DG , DR5 )                               RAY10450
C**** CALL NDPP ( B6 , Z , X + 2.*DG  , DR6 )                           RAY10460
C**** CALL NDPP ( B7 , Z , X - DG , DR7 )                               RAY10470
C**** CALL NDPP ( B8 , Z , X - 2.*DG  , DR8 )                           RAY10480
C**** CALL NDPP ( B9 , Z - DG, X  , DR9 )                               RAY10490
C**** CALL NDPP ( B10, Z - 2.*DG, X, DR10 )                             RAY10500
C**** CALL NDPP ( B11, Z - DG, X + DG  , DR11 )                         RAY10510
C**** CALL NDPP ( B12, Z - DG, X - DG  , DR12 )                         RAY10520
C****
C****
      CALL NDPPX(  B1 , 1, 0, DR1 )
      CALL NDPPX(  B2 , 2, 0, DR2 )
      CALL NDPPX(  B3 , 1, 1, DR3 )
      CALL NDPPX(  B4 , 1,-1, DR4 )
      CALL NDPPX(  B5 , 0, 1, DR5 )
      CALL NDPPX(  B6 , 0, 2, DR6 )
      CALL NDPPX(  B7 , 0,-1, DR7 )
      CALL NDPPX(  B8 , 0,-2, DR8 )
      CALL NDPPX(  B9 ,-1, 0, DR9 )
      CALL NDPPX(  B10,-2, 0, DR10)
      CALL NDPPX(  B11,-1, 1, DR11)
      CALL NDPPX(  B12,-1,-1, DR12)
C****
C**** OFF-MIDPLANE FIELD COMPONENTS BX, BY, AND BZ
C****
   15 YG1 = Y/DG                                                        RAY10530
      YG2 = YG1*YG1                                                     RAY10540
      YG3 = YG2*YG1                                                     RAY10550
      YG4 = YG3*YG1                                                     RAY10560
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     RAY10570
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             RAY10580
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               RAY10590
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            RAY10600
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     RAY10610
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  RAY10620
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       RAY10630
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      RAY10640
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              RAY10650
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              RAY10660
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                RAY10670
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             RAY10680
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 RAY10690
      RETURN                                                            RAY10700
   14 BX = 0.                                                           RAY10710
      BY = BR                                                           RAY10720
      BZ = 0.                                                           RAY10730
      BT = BR                                                           RAY10740
      RETURN                                                            RAY10750
      END                                                               RAY10760
      SUBROUTINE  NDPP ( BFLD, Z, X , DR )                              RAY10770
C****                                                                   RAY10780
C****                                                                   RAY10790
C****                                                                   RAY10800
C****                                                                   RAY10810
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY10820
      REAL*8  NDX, K                                                    RAY10830
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY10840
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY10870
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY10880
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY10890
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY10900
      DIMENSION TC(6), DTC(6)                                           RAY10910
C****                                                                   RAY06940
C****                                                                   RAY06960
      CALL SDIP( X, Z )
      GO TO 1
C****
C****
C**** ENTRY FOR OFF MIDPLANE FIELDS
C****
      ENTRY NDPPX( BFLD, I, J, DR )
      CALL SIJ(I, J )
    1 CONTINUE
      DRR1 = DR/RB                                                      RAY10920
      DRR2 = DRR1*DRR1                                                  RAY10930
      DRR3 = DRR2*DRR1                                                  RAY10940
      DRR4 = DRR3*DRR1                                                  RAY10950
      CS=C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                            RAY11290
      IF( DABS(CS)  .GT.  70.  )  CS =DSIGN( 70.D0 ,CS  )               RAY11300
      E=DEXP(CS)                                                        RAY11310
      P0 = 1.0 + E                                                      RAY11320
      DB=BF-BR                                                          RAY11330
      BFLD = 0.                                                         RAY11340
      IF( MTYP .EQ. 3 ) BFLD =                                          RAY11350
     1       BR +( 1. - NDX*DRR1 + BET1*DRR2+GAMA*DRR3+DELT*DRR4)*DB/P0 RAY11360
      IF( MTYP .EQ. 4 ) BFLD = BR + ( 1./(1. +NDX*DRR1) )*DB/P0         RAY11370
C****
C**** PRINT 100, X, Y, Z,  DR, S, BFLD
C*100 FORMAT( 1P6D15.4 )
C****
      RETURN                                                            RAY11380
      END                                                               RAY11390
      SUBROUTINE BPRETZ
C****
C****
C**** MTYP=6
C****
C****
C**** PRETZEL MAGNET FIELD COMPONENTS
C**** DG = SMALL NEGATIVE NUMBER
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      REAL*8  NDX, K                                                    RAY06570
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY06580
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY06610
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY06620
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY06630
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY06640
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
      SUBROUTINE SDIP( X, Z )
C****                                                                   RAY06280
C****                                                                   RAY06290
C**** MTYP=2  :    UNIFORM FIELD MODIFIED ITERATIVE PROCEDURE           RAY06310
C**** MTYP=3  :    NONUNIFORM FIELD STANDARD APPROXIMATION              RAY06320
C**** MTYP=4  :    NONUNIFORM FIELD  B=BF/(1+N*DR/R)                    RAY06330
C****                                                                   RAY06350
C****                                                                   RAY06400
C****                                                                   RAY06550
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      REAL*8  NDX, K                                                    RAY06570
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY06580
      COMMON  /BLCK20/  NDX,BET1,GAMA,DELT                              RAY03000
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY03010
      COMMON  /BLCK22/  D, DG, S, BF, BT, WDIP                          RAY06610
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY06620
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY06630
      COMMON  /BLCK25/  IN, MTYP, NSRF, IMAP, IR                        RAY06640
      COMMON  /BLSDIP/  XO,ZO,SS,DCS,DSN 
      DIMENSION TC(6), DTC(6)                                           RAY06650
C****
C****                                                                   RAY06710
C****                                                                   RAY06940
C**** MTYP=2,3,4  :
C****                                                                   RAY06960
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
      ZO  = ZEFB(X)
      RETURN
C****
C**** FIND POINT ON EFFECTIVE FIELD BOUNDARY THROUGH FIELD POINT
C**** PARALLEL TO Z-AXIS
C****
    1 ZP    = ZEFB(X)
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
      DO 2 I=1,11                                                       RAY07040
      ZP = ZEFB(XP)
      XXP = X-XP                                                        RAY07110
      ZZP = Z-ZP                                                        RAY07120
      DD =  XXP*XXP + ZZP*ZZP                                           RAY07150
      IF( DD .GE. DP ) GO TO 3
      IXP = I
      DP = DD
    3 XP = XP+AZ
    2 CONTINUE                                                          RAY07160
C****     
C****  DIVIDE INTERVAL AND REPEAT FOR MORE EXACT
C****  SHORTEST DISTANCE.
C****
      X1 = X+AZ*(IXP-6)
      AZ = AZ/5.D0
      XP = X1-5*AZ
      IXP = 1
      DP = 1.D15
      DO 4 I=1,11                                                       RAY07040
      ZP = ZEFB(XP)
      XXP = X-XP                                                        RAY07110
      ZZP = Z-ZP                                                        RAY07120
      DD = XXP*XXP + ZZP*ZZP                                            RAY07150
      IF( DD .GE. DP ) GO TO 5
      IXP = I
      DP = DD
    5 XP = XP+AZ
    4 CONTINUE                                                          RAY07160
C****
C****
      XO = X1+AZ*(IXP-6)
      ZO = ZEFB(XO)
      XPO = X - XO
      ZPO = Z - ZO
      RO = XPO*XPO + ZPO*ZPO
C****
C**** INTERPOLATE FOR MORE ACCURATE LOCATION
C****
      IF( (IXP .EQ. 1 ) .OR. (IXP .EQ. 11)  ) GO TO 8
      XP  = XO + AZ
      ZP  = ZEFB(XP)
      XXP = X-XP
      ZZP = Z-ZP
      R1  = XXP*XXP + ZZP*ZZP
C****
C**** CALCULATE POINT ON THE OTHER SIDE
C****
      XPM = XO - AZ
      ZPM = ZEFB(XPM)
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
      ZO  = ZEFB(XO)
      XPO = X - XO
      ZPO = Z - ZO
      RO = XPO*XPO + ZPO*ZPO
    8 CONTINUE
C****
C****
      IF( RO .LT. 1.D-25 ) RO = 1.D-25
      IF( RO .GT. 1.D+25 ) RO = 1.D+25
      DZDXO = DZDX(XO)
      COSTH = DSQRT ( 1. / (1. + DZDXO*DZDXO) )
      DELTAX = DSQRT(RO) * COSTH/4.D0
C****
C****
C**** PRINT 100, X, Z, XO, ZO, COSTH, DELTAX
C****
C**** PREPARE TO CALCULATE A PAIR OF EQUALLY SPACED IN X 
C**** DISTANCES ON EITHER SIDE OF RO
C****
      RINV4 = 1.D0/(RO*RO)
C****
C**** CALCULATE REPRESENTATIVE DISTANCE
C****
      CX = XO - 2*DELTAX
      DO 6 J=1,5                                                        RAY07040
      IF( J .EQ. 3 ) GO TO 7
      ZP = ZEFB(CX)
      XDI = X - CX                                                      RAY07110
      ZDI = Z - ZP                                                      RAY07120
      RR  = XDI*XDI + ZDI*ZDI
      IF( RR .LT. 1.D-15 ) RR = 1.D-15
      IF( RR .GT. 1.D+15 ) RR = 1.D+15
      RINV4 = RINV4 + 1.0D0 / ( RR*RR )
   7  CX = CX+DELTAX
   6  CONTINUE                                                          RAY07160
      DP2= DSQRT( 1.D0/RINV4 )
      DP = DSQRT( DP2 )
C****
C****
      S = 1.41875D0* ZSIGN * DP/D + DELS
C****
C**** Parameters for off midplane calculation
C****
      SS= S
      DELTA = DATAN(DZDX(XO))
      DCS   = DCOS(DELTA)
      DSN   = DSIN(DELTA)
C****
C*100 FORMAT( 1P6D15.4 )
C**** PRINT 100, X, Z, DELS, S
C****
      RETURN
C****
C**** ENTRY FOR NON MIDPLANE 'S'
C****
      ENTRY SIJ( IZ, JX )
C****
C****
      A     = ( JX*DCS + IZ*DSN )*DG
      DSD   = -DCS*( ZEFB(XO+A*DCS) - ZO - A*DSN )
      S     = SS + ( ( IZ*DCS - JX*DSN )*DG + DSD )/D
      RETURN
      END
      REAL*8 FUNCTION ZEFB(XP)
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY06600
C****
C****
C****
      XP2 = XP*XP                                                       RAY06980
      XP3 = XP2*XP                                                      RAY06990
      XP4 = XP3 * XP                                                    RAY07000
      ZEFB= -(S2*XP2 + S3*XP3 + S4*XP4 + S5*XP4*XP + S6*XP4*XP2 +       RAY07010
     1       S7*XP4*XP3 + S8*XP4*XP4 )                                  RAY07020
      RETURN
      END
      REAL*8 FUNCTION DZDX(XP)
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY06600
C****
C****
C****
      XP2 = XP*XP                                                       RAY06980
      XP3 = XP2*XP                                                      RAY06990
      XP4 = XP3 * XP                                                    RAY07000
      DZDX= -(2.*S2*XP + 3.*S3*XP2+ 4.*S4*XP3 + 5.*S5*XP4 +             RAY07370
     1   6.*S6*XP4*XP + 7.*S7*XP4*XP2 + 8.*S8*XP4*XP3 )                 RAY07380
      RETURN
      END
      REAL*8 FUNCTION DZDX2(XP)
C****
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY06560
      COMMON  /BLCK21/  RCA,DELS,BR,S2,S3,S4,S5,S6,S7,S8                RAY06600
C****
C****
C****
      XP2 = XP*XP                                                       RAY06980
      XP3 = XP2*XP                                                      RAY06990
      XP4 = XP3 * XP                                                    RAY07000
      DZDX2 = -(2.*S2 + 6.*S3*XP+ 12.*S4*XP2 + 20.*S5*XP3 +             RAY07370
     1   30.*S6*XP4 + 42.*S7*XP4*XP + 56.*S8*XP4*XP2 )                  RAY07380
      RETURN
      END
      SUBROUTINE EDIPL( NO, NP, T, TP ,NUM )                            RAY02830
C****                                                                   RAY02840
C****                                                                   RAY02850
C**** SINGLE MAGNET RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIALRAY02860
C**** EQUATIONS OF MOTION.                                              RAY02870
C     T = TIME                                                          RAY02880
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY02890
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY02900
C****                                                                   RAY02910
C****                                                                   RAY02920
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY02930
      REAL*8  LF1, LF2, LU1, K                                          RAY02940
      CHARACTER*4 ITITLE
      EXTERNAL EDIP                                                     RAY02950
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY02960
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY02970
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY02980
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY02990
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           RAY00190
      COMMON  /BLCK20/  EC2, EC4, WE, WC
      COMMON  /BLCK22/  D, DG, S, EF, ET                                RAY03020
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY03030
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      COMMON  /BLCK25/  IN, MTYP, NSRF                                  RAY03050
      DIMENSION DATA(  75,200 ) ,ITITLE(200)                            RAY03060
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY03070
C****                                                                   RAY03090
      LF1  = DATA(  1,NO )                                              RAY03100
      LU1  = DATA(  2,NO )                                              RAY03110
      LF2  = DATA(  3,NO )                                              RAY03120
      DG   = DATA(  4,NO )                                              RAY03130
      A    = DATA( 11,NO )                                              RAY03150
      B    = DATA( 12,NO )                                              RAY03160
      D    = DATA( 13,NO )                                              RAY03170
      RB   = DATA( 14,NO )                                              RAY03180
      EFF  = DATA( 15,NO )                                              RAY03190
      PHI  = DATA( 16,NO )                                              RAY03200
      EC2  = DATA( 17,NO )
      EC4  = DATA( 18,NO )
      WE   = DATA( 19,NO )
      WC   = DATA( 20,NO )
      Z11  = DATA( 25,NO )                                              RAY03270
      Z12  = DATA( 26,NO )                                              RAY03280
      Z21  = DATA( 27,NO )                                              RAY03290
      Z22  = DATA( 28,NO )                                              RAY03300
      DTF1= LF1/ VEL                                                    RAY03360
      DTF2= LF2/ VEL                                                    RAY03370
      DTU = LU1/ VEL                                                    RAY03380
      IF (WE .EQ. 0.) WE = 1000. * RB
      BX = 0.
      BY = 0.
      BZ = 0.
      EX = 0.                                                           RAY03390
      EY = 0.                                                           RAY03400
      EZ = 0.                                                           RAY03410
      ET = 0.                                                           RAY03420
      S = 0.                                                            RAY03430
      IF( NP  .GT. 100 ) GO TO 5                                        RAY03450
      PRINT 100, ITITLE(NO)                                             RAY03460
  100 FORMAT(  ' E.S.-DIPOLE ****', A4,'  ***************************'/)RAY03470
      PRINT 101                                                         RAY03480
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HEX, 8X, 4HY CM , 7X, 2HEY,RAY03490
     1   8X, 4HZ CM, 7X, 2HEZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY03500
     2   6HPHI MR , 6X, 1HE             )                               RAY03510
      CALL PRNT5 (TP,S,XA   ,YA   ,ZA   ,EX,EY,EZ,ET,VXA  ,VYA  ,VZA   )RAY03520
      PRINT 103                                                         RAY03530
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  RAY03540
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY03550
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO EFB COORD.         RAY03560
C****                                                                   RAY03570
    5 CONTINUE                                                          RAY03580
      TC(1) =  -  XA                                                    RAY03600
      TC(2) = YA                                                        RAY03610
      TC(3) = ( A-ZA )                                                  RAY03620
      TC(4) = - VXA                                                     RAY03630
      TC(5) = VYA                                                       RAY03640
      TC(6) = -VZA                                                      RAY03650
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY03660
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY03950
C****                                                                   RAY03960
C****                                                                   RAY03970
   20 TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              RAY03980
      TC(1) = TC(1) + TDT * TC(4)                                       RAY03990
      TC(2) = TC(2) + TDT * TC(5)                                       RAY04000
      TC(3) = TC(3) + TDT * TC(6)                                       RAY04010
      T = T + TDT                                                       RAY04020
      TP = TP+TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY04030
C**** IN DESIGNATES MAGNET REGIONS FOR BFUN                             RAY04040
C****                                                                   RAY04050
      IN = 1                                                            RAY04060
      XC = RB                                                           RAY04070
      ZC = 0.0                                                          RAY04080
      EF = EFF
C****                                                                   RAY04090
      C0   = DATA( 29,NO )                                              RAY04100
      C1   = DATA( 30,NO )                                              RAY04110
      C2   = DATA( 31,NO )                                              RAY04120
      C3   = DATA( 32,NO )                                              RAY04130
      C4   = DATA( 33,NO )                                              RAY04140
      C5   = DATA( 34,NO )                                              RAY04150
      IF( NP  .LE. 100) PRINT 104                                       RAY04270
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY04280
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EDIP,  0    )            RAY04290
      NSTEP = 0                                                         RAY04300
    6 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04310
      DO 7 I = 1, NP                                                    RAY04320
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EDIP,  1    )            RAY04330
      TP = TP+DTF1*VEL
      NSTEP = NSTEP + 1                                                 RAY04340
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04341
      IF( Z12 .GE. TC(3) ) GO TO 8                                      RAY04350
    7 CONTINUE                                                          RAY04360
      GO TO 6                                                           RAY04370
    8 CONTINUE                                                          RAY04380
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            RAY04390
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, EDIP,  0    )            RAY04400
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES, EDIP,  1    )            RAY04410
      TP = TP+XDTF1*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04420
      NUM = NUM + 1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04430
  105 FORMAT( 10H   NSTEPS=   ,I5)                                      RAY04440
C***                                                                    RAY04450
C***  UNIFORM FIELD REGION                                              RAY04460
C**** TRANSFORM TO SECOND EFB COORD SYSTEM                              RAY04470
C***                                                                    RAY04480
      COPAB =DCOS( (PHI)/57.29578)                                      RAY04490
      SIPAB =DSIN( (PHI)/57.29578)                                      RAY04500
      COSPB =DCOS( (PHI/2.)/57.29578 )                                  RAY04510
      SINPB =DSIN( (PHI/2.)/57.29578 )                                  RAY04520
      SIP2 =DSIN( (PHI/2.)/57.29578 )                                   RAY04530
      XT = TC(1)                                                        RAY04540
      ZT = TC(3)                                                        RAY04550
      VXT = TC(4)                                                       RAY04560
      VZT = TC(6)                                                       RAY04570
      TC(3) = - ZT  *COPAB +  XT  *SIPAB -2.*RB*SIP2*COSPB              RAY04580
      TC(1) = - ZT  *SIPAB -  XT  *COPAB -2.*RB*SIP2*SINPB              RAY04590
      TC(6) = - VZT *COPAB +  VXT *SIPAB                                RAY04600
      TC(4) = - VZT *SIPAB -  VXT *COPAB                                RAY04610
C****                                                                   RAY04620
C****                                                                   RAY04630
C**** UNIFORM FIELD INTEGRATION REGION                                  RAY04640
C****                                                                   RAY04650
C****                                                                   RAY04660
      IN = 2                                                            RAY04670
      XC = -RB                                                          RAY04680
      ZC = 0.0                                                          RAY04690
      EF = -EFF
      S  = 0.
      IF( NP  .LE. 100) PRINT 106                                       RAY04700
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            RAY04710
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   RAY04720
C****                                                                   RAY04730
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT RAY04740
C****                                                                   RAY04750
      IF( NP  .LE. 100) PRINT 102                                       RAY04760
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         RAY04770
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES, EDIP,  0    )            RAY04780
      NSTEP = 0                                                         RAY04790
   16 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04800
      DO 17  I =1, NP                                                   RAY04810
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES, EDIP,  1    )            RAY04820
      TP = TP-DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY04830
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04831
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  RAY04840
   17 CONTINUE                                                          RAY04850
      GO TO 16                                                          RAY04860
   18 CONTINUE                                                          RAY04870
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY04880
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  0    )            RAY04890
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  1    )            RAY04900
      TP = TP+XDTU*VEL
      CALL PRNT5 (tp,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04910
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04920
      IF( NP  .LE. 100) PRINT 107                                       RAY04930
  107 FORMAT( / )                                                       RAY04940
      GO TO 19                                                          RAY04950
C****                                                                   RAY04960
C****                                                                   RAY04970
   15 CONTINUE                                                          RAY04980
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, EDIP,  0    )            RAY04990
      NSTEP = 0                                                         RAY05000
    9 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY05010
      DO 10  I =1, NP                                                   RAY05020
      CALL FNMIRK( 6, T, DTU, TC, DTC, DS, ES, EDIP,  1    )            RAY05030
      TP = TP+DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY05040
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY05041
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  RAY05050
   10 CONTINUE                                                          RAY05060
      GO TO 9                                                           RAY05070
   11 CONTINUE                                                          RAY05080
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY05090
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  0    )            RAY05100
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES, EDIP,  1    )            RAY05110
      TP = TP+XDTU*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY05120
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY05130
   19 CONTINUE                                                          RAY05140
C***                                                                    RAY05150
C***                                                                    RAY05160
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     RAY05170
C****                                                                   RAY05180
C****                                                                   RAY05190
      C0   = DATA( 35,NO )                                              RAY05210
      C1   = DATA( 36,NO )                                              RAY05220
      C2   = DATA( 37,NO )                                              RAY05230
      C3   = DATA( 38,NO )                                              RAY05240
      C4   = DATA( 39,NO )                                              RAY05250
      C5   = DATA( 40,NO )                                              RAY05260
      IN = 3                                                            RAY05380
      IF( NP  .LE. 100) PRINT 104                                       RAY05390
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EDIP,  0    )            RAY05400
      NSTEP = 0                                                         RAY05410
   12 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY05420
      DO 13  I =1, NP                                                   RAY05430
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EDIP,  1    )            RAY05440
      TP = TP+DTF2*VEL
      NSTEP = NSTEP + 1                                                 RAY05450
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY05451
      IF( TC(3) .GE. Z22 )  GO TO 14                                    RAY05460
   13 CONTINUE                                                          RAY05470
      GO TO 12                                                          RAY05480
   14 CONTINUE                                                          RAY05490
      XDTF2 = ( Z22 - TC(3) ) /DABS( TC(6) )                            RAY05500
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EDIP,  0    )            RAY05510
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EDIP,  1    )            RAY05520
      TP = TP+XDTF2*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY05530
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY05540
C****                                                                   RAY05550
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY05560
C****                                                                   RAY05570
      XT = TC(1)                                                        RAY05600
      ZT = TC(3)                                                        RAY05610
      VXT = TC(4)                                                       RAY05620
      VZT = TC(6)                                                       RAY05630
      TC(3) = ZT - B                                                    RAY05640
      TC(1) = XT                                                        RAY05650
      TC(6) = VZT                                                       RAY05660
      TC(4) = VXT                                                       RAY05670
      IF( NP  .LE. 100) PRINT 109                                       RAY05680
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY05690
C****                                                                   RAY05700
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY05710
C****                                                                   RAY05720
C****                                                                   RAY05960
C****                                                                   RAY05970
C****                                                                   RAY05980
   30 TDT = -TC(3) /DABS( TC(6) )                                       RAY05990
      TC(1) = TC(1) + TDT * TC(4)                                       RAY06000
      TC(2) = TC(2) + TDT * TC(5)                                       RAY06010
      TC(3) = TC(3) + TDT * TC(6)                                       RAY06020
      T = T + TDT                                                       RAY06030
      TP = TP + TDT*VEL                                                 RAY06040
      EX = 0.                                                           RAY06050
      EY = 0.                                                           RAY06060
      EZ = 0.                                                           RAY06070
      ET = 0.                                                           RAY06080
      S  = 0.                                                           RAY06090
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
      RETURN                                                            RAY06250
99      CALL PRNT4(NO, IN)                                              RAY06251
        RETURN                                                          RAY06252
      END                                                               RAY06260
      SUBROUTINE EDIP
C****
C**** CALCULATES E-FIELD COMPONENTS FOR A CYLINDRICAL 
C**** ELECTROSTATIC DEFLECTOR
C****
      IMPLICIT REAL*8 (A-H, O-Z)
      REAL*8 K
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY02990
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           RAY00190
      COMMON  /BLCK20/  EC2, EC4, WE, WC
      COMMON  /BLCK22/  D, DG, S, EF, ET                                RAY03020
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY03030
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      COMMON  /BLCK25/  IN, MTYP, NSRF                                  RAY03050
      DIMENSION TC(6), DTC(6)
C****
C****         
100   FORMAT( ' ERROR -GO TO-  IN EDIP IN = ', I5)
101   FORMAT( ' ****ERROR EDIP****   X=', F12.3, '  XC=', F12.3, ///)
C****
C****
        X = TC(1)
        Y = TC(2)
        Z = TC(3)
        DX = X - XC
        RP2 = DX * DX + Z * Z
        GO TO (1, 2, 1) , IN
        PRINT 100, IN
C****
C****   UNIFORM FIELD REGION
C****
2       EX = EF * RB * DX / RP2
        EY = 0.
        EZ = EF * RB * Z / RP2
        ET = DSQRT(EX * EX + EZ * EZ)
        RETURN
C****
C****   FRINGE FIELD REGION
C****
1     RP = DSQRT(RP2)
      IF( DABS(X) .LT. DABS(XC) ) GO TO 3
      PRINT 101, X, XC
      CALL EXIT
3     DR = RP-RB
      SINT = Z/RP
      COST = DABS(XC-X)/RP
      THETA = DASIN(SINT)
      S = THETA*RB/D + EC2*Y*Y/(WE*WE) + EC4*(Y/WE)**4
      CALL EDPP( D, S, RE, G1, G2, G3, G4, G5, G6 )
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
      SUBROUTINE EDPP( D, S, RE, G1, G2, G3, G4, G5, G6 )
C****
C**** CALCULATE S; DETERMINE E-FIELD IN FRINGE REGIONS
C****
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 K
      COMMON  /BLCK23/  C0, C1, C2, C3, C4, C5                          RAY03030
      COMMON  /BLCK24/  RB, XC, ZC                                      RAY03040
      S2 = S*S
      S3 = S2*S
      S4 = S2*S2
      S5 = S4*S
      CS = C0 + C1*S + C2*S2 + C3*S3 + C4*S4 + C5*S5                    RAY17260
      RBD = RB/D
      CP1 =(C1 + 2.*C2*S + 3.*C3*S2 + 4.*C4*S3 + 5.*C5*S4)*RBD          RAY17270
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S2 + 20.*C5*S3  ) *RBD*RBD        RAY17280
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S2 ) *RBD**3                    RAY17290
      CP4 = ( 24.*C4 + 120.*C5*S ) *RBD**4                              RAY17300
C****
C****
C****
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )                   RAY17310
      E = DEXP(CS)                                                      RAY17320
      RE = 1./(1. + E)                                                  RAY17330
      ERE = E*RE                                                        RAY17340
      ERE1= ERE*RE
      ERE2= ERE*ERE1                                                    RAY17350
      ERE3= ERE*ERE2                                                    RAY17360
      ERE4= ERE*ERE3                                                    RAY17370
C****
C****
      CP12 = CP1*CP1                                                    RAY17380
      CP13 = CP1*CP12                                                   RAY17400
      CP14 = CP12*CP12                                                  RAY17410
      CP22 = CP2*CP2                                                    RAY17390
C****
C****
      G1 = -CP1*ERE1                                                    RAY17420
C****
C****
      G2 =-( CP2+CP12   )*ERE1    + 2.*CP12 * ERE2                      RAY17430
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE1      +                    RAY17440
     1   6.*(CP1*CP2 + CP13)*ERE2 - 6.*CP13*ERE3                        RAY17450
C****
C****
1     G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE1  +   RAY17460
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2    -     RAY17470
     2   36.*(CP12*CP2 + CP14)*ERE3       + 24.*CP14*ERE4               RAY17480
      RETURN
      END
      SUBROUTINE POLES  ( NO, NP, T, TP ,NUM )                          RAY14570
C****                                                                   RAY14580
C****                                                                   RAY14590
C**** MULTIPOLE     RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIALRAY14600
C**** EQUATIONS OF MOTION.                                              RAY14610
C     T = TIME                                                          RAY14620
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY14630
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY14640
C****                                                                   RAY14650
C****                                                                   RAY14660
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY14670
      CHARACTER*4 ITITLE
      REAL*8  LF1, LF2, LU1, K, L                                       RAY14680
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY14690
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY14700
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY14710
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY14720
      COMMON  /BLCK90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5         RAY14730
      COMMON  /BLCK91/  C0, C1, C2, C3, C4, C5                          RAY14740
      COMMON  /BLCK92/  IN                                              RAY14750
      COMMON  /BLCK93/  DH, DO, DD, DDD, DSH, DSO, DSD, DSDD
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY14760
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY14770
      EXTERNAL  BPOLES                                                  RAY14780
C****                                                                   RAY14800
      LF1  = DATA(  1,NO )                                              RAY14810
      LU1  = DATA(  2,NO )                                              RAY14820
      LF2  = DATA(  3,NO )                                              RAY14830
      A    = DATA( 10,NO )                                              RAY14840
      B    = DATA( 11,NO )                                              RAY14850
      L    = DATA( 12,NO )                                              RAY14860
      RAD  = DATA( 13,NO )                                              RAY14870
      BQD  = DATA( 14,NO )                                              RAY14880
      BHX  = DATA( 15,NO )                                              RAY14890
      BOC  = DATA( 16,NO )                                              RAY14900
      BDC  = DATA( 17,NO )                                              RAY14910
      BDD  = DATA( 18,NO )                                              RAY14920
      Z11  = DATA( 19,NO )                                              RAY14930
      Z12  = DATA( 20,NO )                                              RAY14940
      Z21  = DATA( 21,NO )                                              RAY14950
      Z22  = DATA( 22,NO )                                              RAY14960
      FRH  = DATA( 35,NO )
      FRO  = DATA( 36,NO )
      FRD  = DATA( 37,NO )
      FRDD = DATA( 38,NO )
      DSH  = DATA( 39,NO )
      DSO  = DATA( 40,NO )
      DSD  = DATA( 41,NO )
      DSDD = DATA( 42,NO )
      DTF1= LF1/ VEL                                                    RAY14970
      DTF2= LF2/ VEL                                                    RAY14980
      DTU = LU1/ VEL                                                    RAY14990
      D = 2. * RAD                                                      RAY15000
      IF( FRH  .EQ. 0. ) FRH  = 1.D0
      IF( FRO  .EQ. 0. ) FRO  = 1.D0
      IF( FRD  .EQ. 0. ) FRD  = 1.D0
      IF( FRDD .EQ. 0. ) FRDD = 1.D0
      DH  = FRH *D
      DO  = FRO *D
      DD  = FRD *D
      DDD = FRDD*D
      GRAD1 = -BQD/RAD                                                  RAY15010
      GRAD2 =  BHX/RAD**2                                               RAY15020
      GRAD3 = -BOC/RAD**3                                               RAY15030
      GRAD4 =  BDC/RAD**4                                               RAY15040
      GRAD5 = -BDD/RAD**5                                               RAY15050
      BX = 0.                                                           RAY15060
      BY = 0.                                                           RAY15070
      BZ = 0.                                                           RAY15080
      BT = 0.                                                           RAY15090
      S = 0.                                                            RAY15100
C****                                                                   RAY15110
      IF( NP  .GT. 100 ) GO TO 5                                        RAY15120
      PRINT 100, ITITLE(NO)                                             RAY15130
  100 FORMAT(  ' MULTIPOLE(POLES)  ****  ', A4,'  ******************'/) RAY15140
C****                                                                   RAY15150
      PRINT 101                                                         RAY15160
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,RAY15170
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY15180
     2   6HPHI MR , 6X, 1HB             )                               RAY15190
      CALL PRNT2 (TP,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )RAY15200
      PRINT 103                                                         RAY15210
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  RAY15220
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY15230
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY15240
C****                                                                   RAY15250
    5 TC(1) = -XA                                                       RAY15260
      TC(2) = YA                                                        RAY15270
      TC(3) = A - ZA                                                    RAY15280
      TC(4) = -VXA                                                      RAY15290
      TC(5) = VYA                                                       RAY15300
      TC(6) = -VZA                                                      RAY15310
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY15320
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY15330
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 RAY15340
C****                                                                   RAY15350
      TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              RAY15360
C****                                                                   RAY15370
      TC(1) = TC(1) + TDT * TC(4)                                       RAY15380
      TC(2) = TC(2) + TDT * TC(5)                                       RAY15390
      TC(3) = TC(3) + TDT * TC(6)                                       RAY15400
      T = T + TDT                                                       RAY15410
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY15420
C**** IN DESIGNATES FIELD REGIONS FOR MULTIPOLE                         RAY15430
C****                                                                   RAY15440
      IN = 1                                                            RAY15450
      C0   = DATA( 23,NO )                                              RAY15460
      C1   = DATA( 24,NO )                                              RAY15470
      C2   = DATA( 25,NO )                                              RAY15480
      C3   = DATA( 26,NO )                                              RAY15490
      C4   = DATA( 27,NO )                                              RAY15500
      C5   = DATA( 28,NO )                                              RAY15510
      IF( NP  .LE. 100) PRINT 104                                       RAY15520
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY15530
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BPOLES,0    )            RAY15540
      NSTEP = 0                                                         RAY15550
    6 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY15560
      DO 7 I = 1, NP                                                    RAY15570
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BPOLES,1    )            RAY15580
      TP = TP + DTF1*VEL
      NSTEP = NSTEP + 1                                                 RAY15590
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY15591
      IF( Z12 .GE. TC(3) ) GO TO 8                                      RAY15600
    7 CONTINUE                                                          RAY15610
      GO TO 6                                                           RAY15620
    8 CONTINUE                                                          RAY15630
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            RAY15640
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BPOLES, 0    )            RAY15650
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BPOLES, 1    )            RAY15660
      TP = TP + XDTF1*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY15670
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY15680
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY15690
C***                                                                    RAY15700
C***  UNIFORM FIELD REGION                                              RAY15710
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              RAY15720
C***                                                                    RAY15730
      GRAD1 = -GRAD1                                                    RAY15740
      GRAD2 =  GRAD2                                                    RAY15750
      GRAD3 = -GRAD3                                                    RAY15760
      GRAD4 =  GRAD4                                                    RAY15770
      GRAD5 = -GRAD5                                                    RAY15780
      TC(1) = -TC(1)                                                    RAY15790
      TC(3) = -TC(3) - L                                                RAY15800
      TC(4) = -TC(4)                                                    RAY15810
      TC(6) = -TC(6)                                                    RAY15820
C****                                                                   RAY15830
C****                                                                   RAY15840
C**** UNIFORM FIELD INTEGRATION REGION                                  RAY15850
C****                                                                   RAY15860
C****                                                                   RAY15870
      S = 0.
      IN = 2                                                            RAY15880
      IF( NP  .LE. 100) PRINT 106                                       RAY15890
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            RAY15900
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   RAY04720
C****                                                                   RAY04730
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT RAY04740
C****                                                                   RAY04750
      IF( NP  .LE. 100) PRINT 102                                       RAY04760
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         RAY04770
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES,BPOLES, 0    )            RAY04780
      NSTEP = 0                                                         RAY04790
   16 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04800
      DO 17  I =1, NP                                                   RAY04810
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES,BPOLES, 1    )            RAY04820
      TP = TP - DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY04830
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04831
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  RAY04840
   17 CONTINUE                                                          RAY04850
      GO TO 16                                                          RAY04860
   18 CONTINUE                                                          RAY04870
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY04880
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 0    )            RAY04890
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 1    )            RAY04900
      TP = TP + XDTU*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY04910
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04920
      IF( NP  .LE. 100) PRINT 107                                       RAY04930
  107 FORMAT( / )                                                       RAY04940
      GO TO 19                                                          RAY04950
C****                                                                   RAY04960
C****                                                                   RAY04970
   15 CONTINUE                                                          RAY04980
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BPOLES,0    )            RAY15910
      NSTEP = 0                                                         RAY15920
    9 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY15930
      DO 10  I =1, NP                                                   RAY15940
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, BPOLES,1    )            RAY15950
      TP = TP + DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY15960
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY15961
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  RAY15970
   10 CONTINUE                                                          RAY15980
      GO TO 9                                                           RAY15990
   11 CONTINUE                                                          RAY16000
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY16010
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 0    )            RAY16020
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,BPOLES, 1    )            RAY16030
      TP = TP + XDTU*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY16040
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY16050
   19 CONTINUE                                                          RAY05140
C***                                                                    RAY16060
C***                                                                    RAY16070
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     RAY16080
C****                                                                   RAY16090
C****                                                                   RAY16100
      C0   = DATA( 29,NO )                                              RAY16110
      C1   = DATA( 30,NO )                                              RAY16120
      C2   = DATA( 31,NO )                                              RAY16130
      C3   = DATA( 32,NO )                                              RAY16140
      C4   = DATA( 33,NO )                                              RAY16150
      C5   = DATA( 34,NO )                                              RAY16160
      IN = 3                                                            RAY16170
      IF( NP  .LE. 100) PRINT 104                                       RAY16180
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BPOLES,0    )            RAY16190
      NSTEP = 0                                                         RAY16200
   12 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY16210
      DO 13  I =1, NP                                                   RAY16220
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, BPOLES,1    )            RAY16230
      TP = TP + DTF2*VEL
      NSTEP = NSTEP + 1                                                 RAY16240
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY16241
      IF( TC(3) .GE. Z22 )  GO TO 14                                    RAY16250
   13 CONTINUE                                                          RAY16260
      GO TO 12                                                          RAY16270
   14 CONTINUE                                                          RAY16280
      XDTF2 = ( Z22 - TC(3) ) / TC(6)                                   RAY16290
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BPOLES,0    )            RAY16300
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, BPOLES,1    )            RAY16310
      TP = TP + XDTF2*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY16320
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY16330
C****                                                                   RAY16340
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY16350
C****                                                                   RAY16360
      TC(3) = TC(3) - B                                                 RAY16370
      IF( NP  .LE. 100) PRINT 109                                       RAY16380
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY16390
C****                                                                   RAY16400
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY16410
C****                                                                   RAY16420
      TDT = -TC(3) /DABS( TC(6) )                                       RAY16430
      TC(1) = TC(1) + TDT * TC(4)                                       RAY16440
      TC(2) = TC(2) + TDT * TC(5)                                       RAY16450
      TC(3) = TC(3) + TDT * TC(6)                                       RAY16460
      T = T + TDT                                                       RAY16470
      TP = TP + TDT*VEL                                                 RAY16480
      BX = 0.                                                           RAY16490
      BY = 0.                                                           RAY16500
      BZ = 0.                                                           RAY16510
      BT = 0.                                                           RAY16520
      S  = 0.                                                           RAY16530
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
C****
      RETURN                                                            RAY16720
99      CALL PRNT4 (NO, IN)                                             RAY16721
        RETURN                                                          RAY16722
      END                                                               RAY16730
      SUBROUTINE BPOLES                                                 RAY16740
C****                                                                   RAY16750
C**** CALCULATION OF MULTIPOLE(POLES) FIELD COMPONENTS                  RAY16760
C****                                                                   RAY16770
C****                                                                   RAY16780
C****                                                                   RAY16790
C**** 2 - QUADRUPOLE  (GRAD1)                                           RAY16800
C**** 3 - HEXAPOLE    (GRAD2)                                           RAY16810
C**** 4 - OCTAPOLE    (GRAD3)                                           RAY16820
C**** 5 - DECAPOLE    (GRAD4)                                           RAY16830
C**** 6 - DODECAPOLE  (GRAD5)                                           RAY16840
C****                                                                   RAY16850
C****                                                                   RAY16860
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY16870
      REAL*8 K                                                          RAY16880
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY16890
      COMMON  /BLCK90/  D, S, BT, GRAD1,GRAD2,GRAD3,GRAD4,GRAD5         RAY16900
      COMMON  /BLCK91/  C0, C1, C2, C3, C4, C5                          RAY16910
      COMMON  /BLCK92/  IN                                              RAY16920
      COMMON  /BLCK93/  DH, DO, DD, DDD, DSH, DSO, DSD, DSDD
      DIMENSION TC(6), DTC(6)                                           RAY16930
      X = TC(1)                                                         RAY16940
      Y = TC(2)                                                         RAY16950
      Z = TC(3)                                                         RAY16960
      X2 = X*X                                                          RAY16970
      X3 = X2*X                                                         RAY16980
      X4 = X3*X                                                         RAY16990
      X5 = X4*X                                                         RAY17000
      X6 = X5*X
      X7 = X6*X
      Y2 = Y*Y                                                          RAY17010
      Y3 = Y2*Y                                                         RAY17020
      Y4 = Y3*Y                                                         RAY17030
      Y5 = Y4*Y                                                         RAY17040
      Y6 = Y5*Y
      Y7 = Y6*Y
      GO TO ( 2, 1, 2 ) , IN                                            RAY17050
      PRINT 3, IN                                                       RAY17060
    3 FORMAT( '  ERROR IN BPOLES IN= ' ,I5 ///)                         RAY17070
      CALL EXIT                                                         RAY17080
    1 CONTINUE                                                          RAY17090
      B2X = GRAD1*Y                                                     RAY17100
      B2Y = GRAD1*X                                                     RAY17110
      B3X = GRAD2*2.*X*Y                                                RAY17120
      B3Y = GRAD2*(X2-Y2)                                               RAY17130
      B4X = GRAD3*(3.*X2*Y-Y3)                                          RAY17140
      B4Y = GRAD3*(X3-3.*X*Y2)                                          RAY17150
      B5X = GRAD4*4.*(X3*Y-X*Y3)                                        RAY17160
      B5Y = GRAD4*(X4-6.*X2*Y2+Y4)                                      RAY17170
      B6X = GRAD5*(5.*X4*Y-10.*X2*Y3+Y5)                                RAY17180
      B6Y = GRAD5*(X5-10.*X3*Y2+5.*X*Y4)                                RAY17190
      BX = B2X + B3X + B4X + B5X + B6X                                  RAY17200
      BY = B2Y + B3Y + B4Y + B5Y + B6Y                                  RAY17210
      BZ = 0.                                                           RAY17220
      BT =   DSQRT( BX*BX + BY*BY )                                     RAY17230
      RETURN                                                            RAY17240
C****
C****
C**** QUADRUPOLE
C****
    2 S = Z/D                                                           RAY17250
      CALL BPLS( 2, D, S, RE, G1, G2, G3, G4, G5, G6 )
      B2X = GRAD1*( RE*Y - (G2/12.)*(3.*X2*Y + Y3) +                    RAY17490
     1   (G4/384.)*(5.*X4*Y + 6.*X2*Y3 + Y5 ) -                         RAY17500
     2   (G6/23040.)*(7.*X6*Y + 15.*X4*Y3 + 9.*X2*Y5 + Y7)  )
      B2Y = GRAD1*( RE*X - (G2/12.)*(X3 + 3.*X*Y2) +                    RAY17510
     1   (G4/384.)*(X5 + 6.*X3*Y2 + 5.*X*Y4 ) -                         RAY17520
     2   (G6/23040.)*(X7 + 9.*X5*Y2 + 15.*X3*Y4 + 7.*X*Y6) )
      B2Z = GRAD1*( G1*X*Y - (G3/12.)*(X3*Y + X*Y3 ) +                  RAY17530
     1   (G5/384.)*(X5*Y +2.*X3*Y3 + X*Y5)  )
C****
C**** HEXAPOLE
C****
      SS = Z/DH  + DSH
      CALL BPLS( 3, DH, SS, RE, G1, G2, G3, G4, G5, G6 )
      B3X = GRAD2*( RE*2.*X*Y - (G2/48.)*(12.*X3*Y + 4.*X*Y3 ) )        RAY17540
      B3Y = GRAD2*( RE*(X2-Y2) - (G2/48.)*(3.*X4 + 6.*X2*Y2 - 5.*Y4 ) ) RAY17550
      B3Z = GRAD2*( G1*(X2*Y - Y3/3.) - (G3/48.)*(3.*X4*Y+2.*X2*Y3-Y5)) RAY17560
C****
C**** OCTAPOLE
C****
      SS = Z/DO  + DSO
      CALL BPLS( 4, DO, SS, RE, G1, G2, G3, G4, G5, G6 )
      B4X = GRAD3*( RE*(3.*X2*Y - Y3) - (G2/80.)*(20.*X4*Y - 4.*Y5 ) )  RAY17570
      B4Y = GRAD3*( RE*(X3 - 3.*X*Y2) - (G2/80.)*(4.*X5-20.*X*Y4 ) )    RAY17580
      B4Z = GRAD3*G1*(X3*Y - X*Y3 )                                     RAY17590
C****
C**** DECAPOLE
C****
      SS = Z/DD  + DSD
      CALL BPLS( 5, DD, SS, RE, G1, G2, G3, G4, G5, G6 )
      B5X = GRAD4*RE*(4.*X3*Y - 4.*X*Y3)                                RAY17600
      B5Y = GRAD4*RE*(X4 - 6.*X2*Y2 + Y4 )                              RAY17610
      B5Z = GRAD4*G1*(X4*Y - 2.*X2*Y3 + Y5/5. )                         RAY17620
C****
C**** DODECAPOLE
C****
      SS = Z/DDD + DSDD
      CALL BPLS( 6, DDD,SS, RE, G1, G2, G3, G4, G5, G6 )
      B6X = GRAD5*RE*(5.*X4*Y - 10.*X2*Y3 + Y5 )                        RAY17630
      B6Y = GRAD5*RE*(X5 - 10.*X3*Y2 + 5.*X*Y4 )                        RAY17640
      B6Z = 0.                                                          RAY17650
C****
C**** TOTAL FIELD
C****
      BX = B2X + B3X + B4X + B5X + B6X                                  RAY17660
      BY = B2Y + B3Y + B4Y + B5Y + B6Y                                  RAY17670
      BZ = B2Z + B3Z + B4Z + B5Z + B6Z                                  RAY17680
      BT =   DSQRT( BX*BX + BY*BY + BZ*BZ )                             RAY17690
      RETURN                                                            RAY17700
      END                                                               RAY17710
      SUBROUTINE BPLS ( IGP, D, S, RE, G1, G2, G3, G4, G5, G6 )
C****
C****
C****
      IMPLICIT REAL*8 (A-H,O-Z)
C****
C****
      COMMON  /BLCK91/  C0, C1, C2, C3, C4, C5                          RAY16910
C****
C****
      S2 = S*S
      S3 = S2*S
      S4 = S2*S2
      S5 = S4*S
      CS = C0 + C1*S + C2*S2 + C3*S3 + C4*S4 + C5*S5                    RAY17260
      CP1 =(C1 + 2.*C2*S + 3.*C3*S2 + 4.*C4*S3 + 5.*C5*S4) / D          RAY17270
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S2 + 20.*C5*S3  ) / (D*D)         RAY17280
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S2 ) / (D**3)                   RAY17290
      CP4 = ( 24.*C4 + 120.*C5*S ) / (D**4)                             RAY17300
C****
      CP5 = 120.*C5/(D**5)
C****
C****
C****
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )                   RAY17310
      E = DEXP(CS)                                                      RAY17320
      RE = 1./(1. + E)                                                  RAY17330
      ERE = E*RE                                                        RAY17340
      ERE1= ERE*RE
      ERE2= ERE*ERE1                                                    RAY17350
      ERE3= ERE*ERE2                                                    RAY17360
      ERE4= ERE*ERE3                                                    RAY17370
C****
      ERE5= ERE*ERE4
      ERE6= ERE*ERE5
C****
C****
      CP12 = CP1*CP1                                                    RAY17380
      CP13 = CP1*CP12                                                   RAY17400
      CP14 = CP12*CP12                                                  RAY17410
      CP22 = CP2*CP2                                                    RAY17390
C****
      CP15 = CP12*CP13
      CP16 = CP13*CP13
      CP23 = CP2*CP22
      CP32 = CP3*CP3
C****
C****
      IF( IGP .EQ. 6 ) RETURN
      G1 = -CP1*ERE1                                                    RAY17420
C****
C****
      IF( IGP .EQ. 5 ) RETURN
      G2 =-( CP2+CP12   )*ERE1    + 2.*CP12 * ERE2                      RAY17430
      IF( IGP .EQ. 4 ) RETURN
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE1      +                    RAY17440
     1   6.*(CP1*CP2 + CP13)*ERE2 - 6.*CP13*ERE3                        RAY17450
C****
C****
      IF( IGP .EQ. 3 ) RETURN
1     G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE1  +   RAY17460
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2    -     RAY17470
     2   36.*(CP12*CP2 + CP14)*ERE3       + 24.*CP14*ERE4               RAY17480
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
      SUBROUTINE ACCEL  ( NO, NP, T, TP ,NUM )                          RAY14570
C****                                                                   RAY14580
C****                                                                   RAY14590
C**** ACCELERATOR RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL  RAY14600
C**** EQUATIONS OF MOTION.                                              RAY14610
C     T = TIME                                                          RAY14620
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY14630
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY14640
C****                                                                   RAY14650
C****                                                                   RAY14660
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY14670
      CHARACTER*4 ITITLE
      REAL*8  LF1, LF2, LU1, K, L                                       RAY14680
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY14690
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY14700
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY14710
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY14720
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC
      COMMON  /BLCK80/  D, S, ET, EFLD                                  RAY14730
      COMMON  /BLCK81/  C0, C1, C2, C3, C4, C5                          RAY14740
      COMMON  /BLCK82/  IN                                              RAY14750
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY14760
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY14770
      EXTERNAL  EACCEL                                                  RAY14780
C****                                                                   RAY14800
      LF1  = DATA(  1,NO )                                              RAY14810
      LU1  = DATA(  2,NO )                                              RAY14820
      LF2  = DATA(  3,NO )                                              RAY14830
      A    = DATA( 10,NO )                                              RAY14840
      B    = DATA( 11,NO )                                              RAY14850
      L    = DATA( 12,NO )                                              RAY14860
      RAD  = DATA( 13,NO )                                              RAY14870
      EFELD= DATA( 14,NO )                                              RAY14880
      Z11  = DATA( 15,NO )                                              RAY14930
      Z12  = DATA( 16,NO )                                              RAY14940
      Z21  = DATA( 17,NO )                                              RAY14950
      Z22  = DATA( 18,NO )                                              RAY14960
      DTF1= LF1/ VEL                                                    RAY14970
      DTF2= LF2/ VEL                                                    RAY14980
      DTU = LU1/ VEL                                                    RAY14990
      D = 2. * RAD                                                      RAY15000
      EFLD = -EFELD
      BX = 0.                                                           RAY15060
      BY = 0.                                                           RAY15070
      BZ = 0.                                                           RAY15080
      BT = 0.                                                           RAY15090
      EX = 0.
      EY = 0.
      EZ = 0.
      ET = 0.
      S = 0.                                                            RAY15100
C****                                                                   RAY15110
      IF( NP  .GT. 100 ) GO TO 5                                        RAY15120
      PRINT 100, ITITLE(NO)                                             RAY15130
  100 FORMAT(  ' ACCELERATOR       ****  ', A4,'  ******************'/) RAY15140
C****                                                                   RAY15150
      PRINT 101                                                         RAY15160
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HEX, 8X, 4HY CM , 7X, 2HEY,RAY15170
     1   8X, 4HZ CM, 7X, 2HEZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY15180
     2   6HPHI MR , 6X, 1HE             )                               RAY15190
      CALL PRNT5 (TP,S,XA   ,YA   ,ZA   ,EX,EY,EZ,ET,VXA  ,VYA  ,VZA   )RAY15200
      PRINT 103                                                         RAY15210
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM '       )  RAY15220
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY15230
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY15240
C****                                                                   RAY15250
    5 TC(1) = -XA                                                       RAY15260
      TC(2) = YA                                                        RAY15270
      TC(3) = A - ZA                                                    RAY15280
      TC(4) = -VXA                                                      RAY15290
      TC(5) = VYA                                                       RAY15300
      TC(6) = -VZA                                                      RAY15310
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY15320
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY15330
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 RAY15340
C****                                                                   RAY15350
      TDT = ( TC(3) - Z11 ) /DABS( TC(6) )                              RAY15360
C****                                                                   RAY15370
      TC(1) = TC(1) + TDT * TC(4)                                       RAY15380
      TC(2) = TC(2) + TDT * TC(5)                                       RAY15390
      TC(3) = TC(3) + TDT * TC(6)                                       RAY15400
      T = T + TDT                                                       RAY15410
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY15420
C**** IN DESIGNATES FIELD REGIONS FOR MULTIPOLE                         RAY15430
C****                                                                   RAY15440
      IN = 1                                                            RAY15450
      DTF1= LF1/ VEL                                                    RAY14970
      C0   = DATA( 19,NO )                                              RAY15460
      C1   = DATA( 20,NO )                                              RAY15470
      C2   = DATA( 21,NO )                                              RAY15480
      C3   = DATA( 22,NO )                                              RAY15490
      C4   = DATA( 23,NO )                                              RAY15500
      C5   = DATA( 24,NO )                                              RAY15510
      IF( NP  .LE. 100) PRINT 104                                       RAY15520
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY15530
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EACCEL,0    )            RAY15540
      NSTEP = 0                                                         RAY15550
    6 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY15560
      DO 7 I = 1, NP                                                    RAY15570
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EACCEL,1    )            RAY15580
      TP = TP + DTF1*VEL
      NSTEP = NSTEP + 1                                                 RAY15590
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY15591
      IF( Z12 .GE. TC(3) ) GO TO 8                                      RAY15600
    7 CONTINUE                                                          RAY15610
      GO TO 6                                                           RAY15620
    8 CONTINUE                                                          RAY15630
      XDTF1 =-( Z12 - TC(3) ) /DABS( TC(6) )                            RAY15640
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,EACCEL, 0    )            RAY15650
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,EACCEL, 1    )            RAY15660
      TP = TP + XDTF1*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY15670
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY15680
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY15690
C***                                                                    RAY15700
C***  UNIFORM FIELD REGION                                              RAY15710
C**** TRANSFORM TO SECOND VFB COORD SYSTEM                              RAY15720
C***                                                                    RAY15730
      EFLD = EFELD
      TC(1) = -TC(1)                                                    RAY15790
      TC(3) = -TC(3) - L                                                RAY15800
      TC(4) = -TC(4)                                                    RAY15810
      TC(6) = -TC(6)                                                    RAY15820
C****                                                                   RAY15830
C****                                                                   RAY15840
C**** UNIFORM FIELD INTEGRATION REGION                                  RAY15850
C****                                                                   RAY15860
C****                                                                   RAY15870
      S = 0.
      IN = 2                                                            RAY15880
      DTU = LU1/ VEL                                                    RAY14990
      IF( NP  .LE. 100) PRINT 106                                       RAY15890
  106 FORMAT(   '0UNIFORM FIELD REGION IN C AXIS SYSTEM '  )            RAY15900
      IF( TC(3)  .LT.  Z21 ) GO TO 15                                   RAY04720
C****                                                                   RAY04730
C**** THIS SECTION CORRECTS FOR MAGNETS WHOSE FRINGING FIELDS INTERSECT RAY04740
C****                                                                   RAY04750
      IF( NP  .LE. 100) PRINT 102                                       RAY04760
  102 FORMAT( / '   INTEGRATE BACKWARDS    '  )                         RAY04770
      CALL FNMIRK( 6, T,-DTU ,TC, DTC, DS, ES,EACCEL, 0    )            RAY04780
      NSTEP = 0                                                         RAY04790
   16 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04800
      DO 17  I =1, NP                                                   RAY04810
      CALL FNMIRK( 6, T,-DTU, TC, DTC, DS, ES,EACCEL, 1    )            RAY04820
      TP = TP - DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY04830
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY04831
      IF( TC(3)  .LE.  Z21 )  GO TO 18                                  RAY04840
   17 CONTINUE                                                          RAY04850
      GO TO 16                                                          RAY04860
   18 CONTINUE                                                          RAY04870
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY04880
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,EACCEL, 0    )            RAY04890
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,EACCEL, 1    )            RAY04900
      TP = TP + XDTU*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY04910
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY04920
      IF( NP  .LE. 100) PRINT 107                                       RAY04930
  107 FORMAT( / )                                                       RAY04940
      GO TO 19                                                          RAY04950
C****                                                                   RAY04960
C****                                                                   RAY04970
   15 CONTINUE                                                          RAY04980
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, EACCEL,0    )            RAY15910
      NSTEP = 0                                                         RAY15920
    9 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY15930
      DO 10  I =1, NP                                                   RAY15940
      CALL FNMIRK( 6, T, DTU ,TC, DTC, DS, ES, EACCEL,1    )            RAY15950
      TP = TP + DTU*VEL
      NSTEP = NSTEP + 1                                                 RAY15960
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY15961
      IF( TC(3)  .GE.  Z21 )  GO TO 11                                  RAY15970
   10 CONTINUE                                                          RAY15980
      GO TO 9                                                           RAY15990
   11 CONTINUE                                                          RAY16000
      XDTU  = ( Z21 - TC(3) ) /DABS( TC(6) )                            RAY16010
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,EACCEL, 0    )            RAY16020
      CALL FNMIRK( 6, T,XDTU ,TC, DTC, DS, ES,EACCEL, 1    )            RAY16030
      TP = TP + XDTU*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY16040
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY16050
   19 CONTINUE                                                          RAY05140
C***                                                                    RAY16060
C***                                                                    RAY16070
C**** SETUP FOR SECOND FRINGE FIELD AND INTEGRATION                     RAY16080
C****                                                                   RAY16090
C****                                                                   RAY16100
      C0   = DATA( 25,NO )                                              RAY16110
      C1   = DATA( 26,NO )                                              RAY16120
      C2   = DATA( 27,NO )                                              RAY16130
      C3   = DATA( 28,NO )                                              RAY16140
      C4   = DATA( 29,NO )                                              RAY16150
      C5   = DATA( 30,NO )                                              RAY16160
      IN = 3                                                            RAY16170
      DTF2= LF2/ VEL                                                    RAY14980
      IF( NP  .LE. 100) PRINT 104                                       RAY16180
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EACCEL,0    )            RAY16190
      NSTEP = 0                                                         RAY16200
   12 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY16210
      DO 13  I =1, NP                                                   RAY16220
      CALL FNMIRK( 6, T, DTF2,TC, DTC, DS, ES, EACCEL,1    )            RAY16230
      TP = TP + DTF2*VEL
      NSTEP = NSTEP + 1                                                 RAY16240
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY16241
      IF( TC(3) .GE. Z22 )  GO TO 14                                    RAY16250
   13 CONTINUE                                                          RAY16260
      GO TO 12                                                          RAY16270
   14 CONTINUE                                                          RAY16280
      XDTF2 = ( Z22 - TC(3) ) / TC(6)                                   RAY16290
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EACCEL,0    )            RAY16300
      CALL FNMIRK( 6, T,XDTF2,TC, DTC, DS, ES, EACCEL,1    )            RAY16310
      TP = TP + XDTF2*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY16320
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY16330
C****                                                                   RAY16340
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY16350
C****                                                                   RAY16360
      TC(3) = TC(3) - B                                                 RAY16370
      IF( NP  .LE. 100) PRINT 109                                       RAY16380
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY16390
C****                                                                   RAY16400
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY16410
C****                                                                   RAY16420
      TDT = -TC(3) /DABS( TC(6) )                                       RAY16430
      TC(1) = TC(1) + TDT * TC(4)                                       RAY16440
      TC(2) = TC(2) + TDT * TC(5)                                       RAY16450
      TC(3) = TC(3) + TDT * TC(6)                                       RAY16460
      T = T + TDT                                                       RAY16470
      TP = TP + TDT*VEL                                                 RAY16480
      EX = 0.                                                           RAY16490
      EY = 0.                                                           RAY16500
      EZ = 0.                                                           RAY16510
      ET = 0.                                                           RAY16520
      S  = 0.                                                           RAY16530
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY16600
      RETURN                                                            RAY16720
99      CALL PRNT4 (NO, IN)                                             RAY16721
        RETURN                                                          RAY16722
      END                                                               RAY16730
      SUBROUTINE EACCEL                                                 RAY16740
C****                                                                   RAY16750
C**** CALCULATION OF ACCELERATOR FIELD COMPONENTS                       RAY16760
C****                                                                   RAY16770
C****                                                                   RAY16860
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY16870
      REAL*8 K                                                          RAY16880
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY16890
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC
      COMMON  /BLCK80/  D, S, ET, EFLD                                  RAY14730
      COMMON  /BLCK81/  C0, C1, C2, C3, C4, C5                          RAY14740
      COMMON  /BLCK82/  IN                                              RAY14750
      DIMENSION TC(6), DTC(6)                                           RAY16930
C****
C****
      GO TO ( 2, 1, 2 ) , IN                                            RAY17050
      PRINT 3, IN                                                       RAY17060
    3 FORMAT( '  ERROR IN EACCEL IN= ' ,I5 ///)                         RAY17070
      CALL EXIT                                                         RAY17080
    1 CONTINUE                                                          RAY17090
      EX = 0.
      EY = 0.
      EZ = EFLD
      ET = EFLD
      RETURN                                                            RAY17240
C****
C**** ELECTRIC FIELDS
C****
    2 X = TC(1)                                                         RAY16940
      Y = TC(2)                                                         RAY16950
      Z = TC(3)                                                         RAY16960
      R = DSQRT( X*X + Y*Y )
      S = Z/D                                                           RAY17250
      CALL EACCS( D, S, RE, G1, G2, G3, G4, G5, G6 )
      R2 = R*R
      R3 = R2*R
      R4 = R3*R
      R5 = R4*R
      R6 = R5*R
      ER = EFLD*( -R*G1/2. + R3*G3/16. - R5*G5/384. )
      EZ = EFLD*( RE - R2*G2/4. + R4*G4/64. - R6*G6/2304. )
      EX = 0.
      EY = 0.
      IF( R .LT. 1.D-10 ) GO TO 5
      EX = ER*X/R
      EY = ER*Y/R
    5 CONTINUE
      ET =   DSQRT( EX*EX + EY*EY + EZ*EZ )                             RAY17690
      RETURN                                                            RAY17700
      END                                                               RAY17710
      SUBROUTINE EACCS ( D, S, RE, G1, G2, G3, G4, G5, G6 )
C****
C****
C****
      IMPLICIT REAL*8 (A-H,O-Z)
C****
C****
      COMMON  /BLCK81/  C0, C1, C2, C3, C4, C5                          RAY16910
C****
C****
      S2 = S*S
      S3 = S2*S
      S4 = S2*S2
      S5 = S4*S
      CS = C0 + C1*S + C2*S2 + C3*S3 + C4*S4 + C5*S5                    RAY17260
      CP1 =(C1 + 2.*C2*S + 3.*C3*S2 + 4.*C4*S3 + 5.*C5*S4) / D          RAY17270
      CP2 = (2.*C2 + 6.*C3*S + 12.*C4*S2 + 20.*C5*S3  ) / (D*D)         RAY17280
      CP3 = ( 6.*C3 + 24.*C4*S + 60.*C5*S2 ) / (D**3)                   RAY17290
      CP4 = ( 24.*C4 + 120.*C5*S ) / (D**4)                             RAY17300
C****
      CP5 = 120.*C5/(D**5)
C****
C****
C****
      IF( DABS(CS) .GT. 70. )  CS = DSIGN(70.D0, CS )                   RAY17310
      E = DEXP(CS)                                                      RAY17320
      RE = 1./(1. + E)                                                  RAY17330
      ERE = E*RE                                                        RAY17340
      ERE1= ERE*RE
      ERE2= ERE*ERE1                                                    RAY17350
      ERE3= ERE*ERE2                                                    RAY17360
      ERE4= ERE*ERE3                                                    RAY17370
C****
      ERE5= ERE*ERE4
      ERE6= ERE*ERE5
C****
C****
      CP12 = CP1*CP1                                                    RAY17380
      CP13 = CP1*CP12                                                   RAY17400
      CP14 = CP12*CP12                                                  RAY17410
      CP22 = CP2*CP2                                                    RAY17390
C****
      CP15 = CP12*CP13
      CP16 = CP13*CP13
      CP23 = CP2*CP22
      CP32 = CP3*CP3
C****
C****
      G1 = -CP1*ERE1                                                    RAY17420
C****
C****
      G2 =-( CP2+CP12   )*ERE1    + 2.*CP12 * ERE2                      RAY17430
      G3 =-(CP3 + 3.*CP1*CP2 + CP13  ) * ERE1      +                    RAY17440
     1   6.*(CP1*CP2 + CP13)*ERE2 - 6.*CP13*ERE3                        RAY17450
C****
C****
1     G4 = -(CP4 + 4.*CP1*CP3 + 3.*CP22 + 6.*CP12*CP2 + CP14)*ERE1  +   RAY17460
     1   (8.*CP1*CP3 + 36.*CP12*CP2 + 6.*CP22 + 14.*CP14)*ERE2    -     RAY17470
     2   36.*(CP12*CP2 + CP14)*ERE3       + 24.*CP14*ERE4               RAY17480
C****
C****
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
      SUBROUTINE EINZEL ( NO, NP, T, TP ,NUM )                          RAY25230
C****                                                                   RAY25240
C****                                                                   RAY25250
C**** EINZEL LENS RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIAL  RAY25260
C**** EQUATIONS OF MOTION.                                              RAY25270
C     T = TIME                                                          RAY25280
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY25290
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY25300
C****                                                                   RAY25330
C****                                                                   RAY25340
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY25350
      CHARACTER*4 ITITLE
      REAL*8  LF, K, L                                                  RAY25360
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY25370
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY25380
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY25390
      COMMON  /BLCK 7/ NCODE                                            RAY25400
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY25410
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC
      COMMON  /BLCK50/  V, D, AL, S, ET                                 RAY25420
      COMMON  /BLCK81/  C0, C1, C2, C3, C4, C5                          RAY25430
      COMMON  /BLCK52/  IN, LTYP                                        RAY25440
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY25450
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY25460
      EXTERNAL  EINZ                                                    RAY25470
C****                                                                   RAY25490
C****                                                                   RAY25500
      LF   = DATA(  1,NO )                                              RAY25510
      LTYP = DATA(  2,NO )
      A    = DATA( 10,NO )                                              RAY25520
      B    = DATA( 11,NO )                                              RAY25530
      L    = DATA( 12,NO )                                              RAY25540
      D    = DATA( 13,NO )                                              RAY25550
      V    = DATA( 14,NO )                                              RAY25560
      Z11  = DATA( 15,NO )                                              RAY25570
      Z22  = DATA( 16,NO )                                              RAY25580
      C0   = DATA( 17,NO )
      C1   = DATA( 18,NO )
      C2   = DATA( 19,NO )
      C3   = DATA( 20,NO )
      C4   = DATA( 21,NO )
      C5   = DATA( 22,NO )
      DTF1= LF/VEL                                                      RAY25590
      AL  = L/2.                                                        RAY25600
      RAD = D/2.                                                        RAY25610
      BX = 0.                                                           RAY25620
      BY = 0.                                                           RAY25630
      BZ = 0.                                                           RAY25640
      EX = 0.
      EY = 0.
      EZ = 0.
      ET = 0.
      S = 0.                                                            RAY25660
      IF( (LTYP .NE. 0) .AND. (LTYP .NE. 1) ) LTYP = 0
C****                                                                   RAY25670
C****                                                                   RAY25680
      IF( NP  .GT. 100 ) GO TO 5                                        RAY25690
  201 FORMAT(  ' EINZEL LENS ****  ', A4, '  ***********************'/) RAY25700
      PRINT 201, ITITLE(NO)                                             RAY25710
      PRINT 101
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HEX, 8X, 4HY CM , 7X, 2HEY,RAY25720
     1   8X, 4HZ CM, 7X, 2HEZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY25730
     2   6HPHI MR , 6X, 1HE             )                               RAY25740
      CALL PRNT5 (TP,S,XA   ,YA   ,ZA   ,EX,EY,EZ,ET,VXA  ,VYA  ,VZA   )RAY25750
      PRINT 103                                                         RAY25770
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO CENTERED AXIS SYSTEM ' ) RAY25780
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY25790
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY25800
C****                                                                   RAY25810
    5 TC(1) =  XA                                                       RAY25820
      TC(2) = YA                                                        RAY25830
      TC(3) = ZA-A-AL                                                   RAY25840
      TC(4) =  VXA                                                      RAY25850
      TC(5) = VYA                                                       RAY25860
      TC(6) =  VZA                                                      RAY25870
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY25880
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY25890
C**** TRANSLATE PARTICLE TO START OF       FRINGE FIELD                 RAY25900
C****                                                                   RAY25910
      TDT = (-TC(3) -Z11 -AL ) /DABS( TC(6) )                           RAY25920
C****                                                                   RAY25930
      TC(1) = TC(1) + TDT * TC(4)                                       RAY25940
      TC(2) = TC(2) + TDT * TC(5)                                       RAY25950
      TC(3) = TC(3) + TDT * TC(6)                                       RAY25960
      T = T + TDT                                                       RAY25970
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 104                                       RAY25980
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY25990
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EINZ , 0    )            RAY26000
      NSTEP = 0                                                         RAY26010
    6 CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY25880
      DO 7 I = 1, NP                                                    RAY26030
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, EINZ , 1    )            RAY26040
      TP = TP + DTF1*VEL
      NSTEP = NSTEP + 1                                                 RAY26050
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  1000)  GO TO 99                                RAY26051
      IF( (Z22+AL) .LE. TC(3) ) GO TO 8                                 RAY26060
    7 CONTINUE                                                          RAY26070
      GO TO 6                                                           RAY26080
    8 CONTINUE                                                          RAY26090
      XDTF1 =-( TC(3) -(Z22+AL)  ) / DABS( TC(6) )                      RAY26100
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,EINZ ,  0    )            RAY26110
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,EINZ ,  1    )            RAY26120
      TP = TP + XDTF1*VEL
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY25880
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY26140
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY26150
C****                                                                   RAY26160
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY26170
C****                                                                   RAY26180
      TC(3) = TC(3) - B - AL                                            RAY26190
      IF( NP  .LE. 100) PRINT 109                                       RAY26200
      CALL PRNT5 (TP,S,TC(1),TC(2),TC(3),EX,EY,EZ,ET,TC(4),TC(5),TC(6) )RAY25880
C****                                                                   RAY26220
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY26230
C****                                                                   RAY26240
      TDT = -TC(3) /DABS( TC(6) )                                       RAY26250
      TC(1) = TC(1) + TDT * TC(4)                                       RAY26260
      TC(2) = TC(2) + TDT * TC(5)                                       RAY26270
      TC(3) = TC(3) + TDT * TC(6)                                       RAY26280
      T = T + TDT                                                       RAY26290
      TP = TP + TDT*VEL                                                 RAY26300
      BX = 0.                                                           RAY26310
      BY = 0.                                                           RAY26320
      BZ = 0.                                                           RAY26330
      BT = 0.                                                           RAY26340
      S  = 0.                                                           RAY26350
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY26420
      RETURN                                                            RAY26510
99      CALL PRNT4(NO, IN )                                             RAY26511
        RETURN                                                          RAY26512
      END                                                               RAY26520
      SUBROUTINE EINZ                                                   RAY16740
C****                                                                   RAY16750
C**** CALCULATION OF EINZEL LENS FIELD COMPONENTS                       RAY16760
C****                                                                   RAY16770
C****                                                                   RAY16860
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY16870
      REAL*8 K                                                          RAY16880
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY16890
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC
      COMMON  /BLCK50/  V, D, AL, S, ET                                 RAY25420
      COMMON  /BLCK81/  C0, C1, C2, C3, C4, C5                          RAY25430
      COMMON  /BLCK52/  IN, LTYP                                        RAY25440
      DIMENSION TC(6), DTC(6)                                           RAY16930
C****
C**** ELECTRIC FIELDS
C****
      X = TC(1)                                                         RAY16940
      Y = TC(2)                                                         RAY16950
      Z = TC(3)                                                         RAY16960
      S1 = (-AL-Z)/D
      S2 = (-AL+Z)/D
      CALL EACCS( D, S1, RE1, G1, G2, G3, G4, G5, G6 )
      CALL EACCS( D, S2, RE2, H1, H2, H3, H4, H5, H6 )
      IF( LTYP .EQ. 1 ) GO TO 1
C****
C**** LTYP = 2  2-DIMENSIONAL EINZEL LENS
C****
      R = DSQRT( X*X + Y*Y )
      R2 = R*R
      R3 = R2*R
      R4 = R3*R
      R5 = R4*R
      ER = V*( R*(G2+H2)/2. - R3*(G4+H4)/16. + R5*(G6+H6)/384. )
      EZ = V*( -(-G1+H1) + R2*(-G3+H3)/4. - R4*(-G5+H5)/64. )
      EX = 0.
      EY = 0.
      IF( R .LT. 1.D-10 ) GO TO 5
      EX = ER*X/R
      EY = ER*Y/R
    5 CONTINUE
      ET =   DSQRT( EX*EX + EY*EY + EZ*EZ )                             RAY17690
      S = S1
      RETURN                                                            RAY17700
C****
C**** LTYP = 1     1-DIMENSIONAL EINZEL LENS
C****
    1 X2 = X*X
      X3 = X2*X
      X4 = X3*X
      X5 = X4*X
      EX = V*( X*(G2+H2) - X3*(G4+H4)/6. + X5*(G6+H6)/120. )
      EZ = V*( -(-G1+H1) + X2*(-G3+H3)/2. - X4*(-G5+H5)/24. )
      ET =   DSQRT( EX*EX + EY*EY + EZ*EZ )                             RAY17690
      S = S1
      RETURN
      END                                                               RAY17710
      SUBROUTINE VELS ( NO,NP,T,TP ,NUM )                               RAY17720
C****                                                                   RAY17730
C****                                                                   RAY17740
C     VELOCITY SELECTOR......ADDED JAN. 1976 BY W. R. BERNECKY          RAY17750
C****                                                                   RAY17760
C****                                                                   RAY17770
      IMPLICIT  REAL*8 (A-H,O-Z)                                        RAY17780
      REAL*8  K,LF1,LU1,LF2,L                                           RAY17790
      REAL*8  NDX
      CHARACTER*4 ITITLE
      EXTERNAL BEVC                                                     RAY17800
      COMMON /BLCK 0/  DATA, ITITLE                                     RAY17810
      COMMON /BLCK 4/  ENERGY, VEL, PMASS, Q0                           RAY17820
      COMMON /BLCK 5/  XA,YA,ZA,VXA,VYA,VZA                             RAY17830
      COMMON /BLCK10/  BX,BY,BZ,K,TC,DTC                                RAY17840
      COMMON /BLCK11/  EX, EY, EZ, QMC, IVEC                            RAY17850
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          RAY17860
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          RAY17870
      COMMON /BLCK73/  IN,NFLAG                                         RAY17880
      COMMON /BLCK74/  BF,EF,S,DG                                       RAY17890
      COMMON /BLCK75/  BC2,BC4,EC2,EC4                                  RAY17900
      COMMON /BLCK76/  DB,DE,WB,WE                                      RAY17910
      COMMON /BLCK77/  RB,NDX
C****                                                                   RAY17920
      DIMENSION  DATA(75,200) , ITITLE(200)                             RAY17930
      DIMENSION  TC(6),DTC(6),DS(6),ES(6)                               RAY17940
C****                                                                   RAY17960
      LF1=DATA( 1,NO)                                                   RAY17970
      LU1=DATA( 2,NO)                                                   RAY17980
      LF2=DATA( 3,NO)                                                   RAY17990
      DG =DATA( 4,NO)                                                   RAY18000
      A  =DATA( 7,NO)                                                   RAY18010
      B  =DATA( 8,NO)                                                   RAY18020
      L  =DATA( 9,NO)                                                   RAY18030
      BF =DATA(10,NO)                                                   RAY18040
      EF =DATA(11,NO)                                                   RAY18050
      RB =DATA(12,NO)
      NDX=DATA(13,NO)
      DB =DATA(16,NO)                                                   RAY18060
      DE =DATA(17,NO)                                                   RAY18070
      WB =DATA(18,NO)                                                   RAY18080
      WE =DATA(19,NO)                                                   RAY18090
      Z11=DATA(20,NO)                                                   RAY18100
      Z12=DATA(21,NO)                                                   RAY18110
      Z21=DATA(22,NO)                                                   RAY18120
      Z22=DATA(23,NO)                                                   RAY18130
      BC2=DATA(24,NO)                                                   RAY18140
      BC4=DATA(25,NO)                                                   RAY18150
      EC2=DATA(26,NO)                                                   RAY18160
      EC4=DATA(27,NO)                                                   RAY18170
      NFLAG = 0
      IF( NDX .NE. 0. ) NFLAG=1
      IF( RB  .EQ. 0. ) RB=1.D30
      EX = 0.                                                           RAY18180
      EY = 0.                                                           RAY18190
      EZ = 0.                                                           RAY18200
      S  = 0.                                                           RAY18210
      BX = 0.                                                           RAY18220
      BY = 0.                                                           RAY18230
      BZ = 0.                                                           RAY18240
      IF ( NP .GT. 100 ) GO TO 5                                        RAY18250
      PRINT 100, ITITLE(NO)                                             RAY18260
  100 FORMAT ('0VELOCITY SELECTOR****  ',A4,'  ******************'/ )   RAY18270
      PRINT 101                                                         RAY18280
  101 FORMAT (8H    T CM,6X,4HX CM,5X,2HBX,8X,2HEX,8X,4HY CM,5X,2HBY,8X,RAY18290
     1       2HEY,7X,4HZ CM,6X,2HBZ,8X,2HEZ,6X,8HTHETA MR,5X,6HPHI MR,  RAY18300
     2   2X, 'VEL/E9'   )                                               RAY18310
      CALL PRNT3( TP   ,XA,YA,ZA,BX,BY,BZ,EX,EY,EZ,VXA,VYA,VZA )        RAY18330
      PRINT 103                                                         RAY18340
  103 FORMAT ( '0COORDINATE TRANSFORMATION TO B AXIS SYSTEM' )          RAY18350
  109 FORMAT ( '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM' )          RAY18360
C****                                                                   RAY18370
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES                       RAY18380
C****                                                                   RAY18390
    5 TC(1) = -XA                                                       RAY18400
      TC(2) =  YA                                                       RAY18410
      TC(3) = A-ZA                                                      RAY18420
      TC(4) = -VXA                                                      RAY18430
      TC(5) =  VYA                                                      RAY18440
      TC(6) = -VZA                                                      RAY18450
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY18460
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY18470
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY18480
C**** TRANSLATE PARTICLE TO START OF FRINGE FIELD                       RAY18490
C****                                                                   RAY18500
      TDT = ( TC(3)-Z11 )/DABS( TC(6) )                                 RAY18510
      TC(1) = TC(1)+TDT*TC(4)                                           RAY18520
      TC(2) = TC(2)+TDT*TC(5)                                           RAY18530
      TC(3) = TC(3)+TDT*TC(6)                                           RAY18540
      T = T+TDT                                                         RAY18550
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY18560
C**** IN DESIGNATES MAGNET REGIONS FOR BEFN                             RAY18570
C****                                                                   RAY18580
      IN = 1                                                            RAY18590
      CB0=DATA(28,NO)                                                   RAY18600
      CB1=DATA(29,NO)                                                   RAY18610
      CB2=DATA(30,NO)                                                   RAY18620
      CB3=DATA(31,NO)                                                   RAY18630
      CB4=DATA(32,NO)                                                   RAY18640
      CB5=DATA(33,NO)                                                   RAY18650
      CE0=DATA(34,NO)                                                   RAY18660
      CE1=DATA(35,NO)                                                   RAY18670
      CE2=DATA(36,NO)                                                   RAY18680
      CE3=DATA(37,NO)                                                   RAY18690
      CE4=DATA(38,NO)                                                   RAY18700
      CE5=DATA(39,NO)                                                   RAY18710
      DTF1 = LF1/VEL                                                    RAY18720
      IF ( NP .LE. 100 ) PRINT 104                                      RAY18730
  104 FORMAT ( 22H0FRINGING FIELD REGION)                               RAY18740
      CALL FNMIRK (6,T,DTF1,TC,DTC,DS,ES,BEVC,0 )                       RAY18750
      NSTEP = 0                                                         RAY18760
    6 CONTINUE                                                          RAY18780
      CALL PRNT3 (TP,TC(1),TC(2),TC(3),BX,BY,BZ,                        RAY18790
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY18800
      DO 7 I=1,NP                                                       RAY18810
      CALL FNMIRK (6,T,DTF1,TC,DTC,DS,ES,BEVC,1 )                       RAY18820
      TP = TP + DTF1*VEL
      NSTEP = NSTEP+1                                                   RAY18830
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY18831
      IF ( Z12 .GE. TC(3) ) GO TO 8                                     RAY18850
    7 CONTINUE                                                          RAY18860
      GO TO 6                                                           RAY18870
    8 CONTINUE                                                          RAY18880
      XDTF1 = -( Z12-TC(3) )*DABS( TC(6) )/VEL**2                       RAY18890
      CALL FNMIRK (6,T,XDTF1,TC,DTC,DS,ES,BEVC,0 )                      RAY18900
      CALL FNMIRK (6,T,XDTF1,TC,DTC,DS,ES,BEVC,1 )                      RAY18910
      TP = TP + XDTF1*VEL
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY18930
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY18940
      IF ( NP .LE. 100 ) PRINT 105,NSTEP                                RAY18950
  105 FORMAT ( 10H   NSTEPS= ,I5 )                                      RAY18960
C****                                                                   RAY18970
C****    TRANSLATE TO 2ND EFB COORDINATE SYSTEM                         RAY18980
C****                                                                   RAY18990
      TC(1) = -TC(1)                                                    RAY19000
      TC(3) = -(TC(3)+L)                                                RAY19010
      TC(4) = -TC(4)                                                    RAY19020
      TC(6) = -TC(6)                                                    RAY19030
C****                                                                   RAY19040
C**** UNIFORM FIELD REGION                                              RAY19050
C****                                                                   RAY19060
      IN = 2                                                            RAY19070
      DTU = LU1/VEL                                                     RAY19080
      IF ( NP .LE. 100 ) PRINT 106                                      RAY19090
  106 FORMAT ( '0UNIFORM FIELD REGION IN C AXIS SYSTEM' )               RAY19100
      CALL FNMIRK (6,T,DTU,TC,DTC,DS,ES,BEVC,0 )                        RAY19110
      NSTEP = 0                                                         RAY19120
    9 CONTINUE                                                          RAY19130
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19140
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19150
      DO 10 I = 1,NP                                                    RAY19160
      CALL FNMIRK (6,T,DTU,TC,DTC,DS,ES,BEVC,1 )                        RAY19170
      TP = TP + DTU*VEL
      NSTEP = NSTEP+1                                                   RAY19180
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY19181
      IF ( TC(3) .GE. Z21 ) GO TO 11                                    RAY19200
   10 CONTINUE                                                          RAY19210
      GO TO 9                                                           RAY19220
   11 CONTINUE                                                          RAY19230
      XDTU = (Z21-TC(3) )*DABS( TC(6) )/VEL**2                          RAY19240
      CALL FNMIRK (6,T,XDTU,TC,DTC,DS,ES,BEVC,0)                        RAY19250
      CALL FNMIRK (6,T,XDTU,TC,DTC,DS,ES,BEVC,1 )                       RAY19260
      TP = TP + XDTU*VEL
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19280
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19290
      IF ( NP .LE. 100 ) PRINT 105, NSTEP                               RAY19300
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY19310
C**** SET UP FOR SECOND FRINGE FIELD INTEGRATION                        RAY19320
C****                                                                   RAY19330
      CB0=DATA(40,NO)                                                   RAY19340
      CB1=DATA(41,NO)                                                   RAY19350
      CB2=DATA(42,NO)                                                   RAY19360
      CB3=DATA(43,NO)                                                   RAY19370
      CB4=DATA(44,NO)                                                   RAY19380
      CB5=DATA(45,NO)                                                   RAY19390
      CE0=DATA(46,NO)                                                   RAY19400
      CE1=DATA(47,NO)                                                   RAY19410
      CE2=DATA(48,NO)                                                   RAY19420
      CE3=DATA(49,NO)                                                   RAY19430
      CE4=DATA(50,NO)                                                   RAY19440
      CE5=DATA(51,NO)                                                   RAY19450
      IN = 3                                                            RAY19460
      DTF2 = LF2/VEL                                                    RAY19470
      IF ( NP .LE. 100 ) PRINT 104                                      RAY19480
      CALL FNMIRK (6,T,DTF2,TC,DTC,DS,ES,BEVC,0 )                       RAY19490
      NSTEP=0                                                           RAY19500
   12 CONTINUE                                                          RAY19510
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19520
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19530
      DO 13  I=1,NP                                                     RAY19540
      CALL FNMIRK (6,T,DTF2,TC,DTC,DS,ES,BEVC,1 )                       RAY19550
      TP = TP + DTF2*VEL
      NSTEP = NSTEP+1                                                   RAY19560
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY19561
      IF ( TC(3) .GE. Z22 ) GO TO 14                                    RAY19580
   13 CONTINUE                                                          RAY19590
      GO TO 12                                                          RAY19600
   14 CONTINUE                                                          RAY19610
      XDTF2 = ( Z22-TC(3) )*TC(6)/VEL**2                                RAY19620
      CALL FNMIRK (6,T,XDTF2,TC,DTC,DS,ES,BEVC,0 )                      RAY19630
      CALL FNMIRK (6,T,XDTF2,TC,DTC,DS,ES,BEVC,1 )                      RAY19640
      TP = TP + XDTF2*VEL
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19660
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19670
      IF (NP .LE. 100) PRINT 105,NSTEP                                  RAY19680
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY19690
C**** TRANSLATE TO OUTPUT COORDINATES                                   RAY19700
C****                                                                   RAY19710
      TC(3) = TC(3)-B                                                   RAY19720
      IF ( NP .LE. 100 ) PRINT 109                                      RAY19730
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19740
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19750
      TDT =-TC(3)/DABS( TC(6) )                                         RAY19770
      TC(1) = TC(1)+TDT*TC(4)                                           RAY19780
      TC(2) = TC(2)+TDT*TC(5)                                           RAY19790
      TC(3) = TC(3)+TDT*TC(6)                                           RAY19800
      T = T+TDT                                                         RAY19810
      TP = TP + TDT*VEL
      BX = 0.                                                           RAY19820
      BY = 0.                                                           RAY19830
      BZ = 0.                                                           RAY19840
      EX = 0.                                                           RAY19850
      EY = 0.                                                           RAY19860
      EZ = 0.                                                           RAY19870
      S  = 0.                                                           RAY19880
      VXF    = 1000. *DATAN2( TC(4), TC(6)  )                           RAY19890
      VYF    = 1000. *DASIN ( TC(5)/ VEL    )                           RAY19900
      NUM = NUM+1
      NBR = 4
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF ( NP .GT. 100 ) GO TO 15                                       RAY19920
      CALL PRNT3 (TP   ,TC(1),TC(2),TC(3),BX,BY,BZ,                     RAY19930
     1            EX,EY,EZ,TC(4),TC(5),TC(6)  )                         RAY19940
   15 CONTINUE                                                          RAY19950
      ZDX = -TC(1)/( TC(4)/TC(6)+1.E-10 )                               RAY19960
      ZDY = -TC(2)/( TC(5)/TC(6)+1.E-10 )                               RAY19970
      IF (NP .LE. 100 ) PRINT 111,VXF,VYF,ZDX,ZDY                       RAY19980
  111 FORMAT (/'0INTERSECTIONS WITH VER. AND HOR. PLANES '              RAY19990
     X       /15X,5H  XP=F10.4,10H MR    YP=F10.4,3H MR / ,             RAY20000
     1    15X,5H Z0X=F10.2,10H CM   Z0Y=F10.2,3H CM   /   )             RAY20010
      RETURN                                                            RAY20020
99      CALL PRNT4(NO, IN)                                              RAY20021
        RETURN                                                          RAY20022
      END                                                               RAY20030
      SUBROUTINE BEFN (F,Z,X,Y,DR,IBEX)                                 RAY20040
C****                                                                   RAY20050
C****                                                                   RAY20060
C****    CALCULATES S, THEN DETERMINES B (OR E) FIELD.                  RAY20070
C****                                                                   RAY20080
C****                                                                   RAY20090
C****
C**** IBEX = 0   MAGNETIC FIELD COMPONENTS
C****      = 1   ELECTRIC FIELD COMPONENTS
C****
      IMPLICIT  REAL*8 (A-H,O-Z)                                        RAY20100
      REAL*8 NDX
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          RAY20110
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          RAY20120
      COMMON /BLCK73/  IN,NFLAG                                         RAY20130
      COMMON /BLCK74/  BF,EF,S,DG                                       RAY20140
      COMMON /BLCK75/  BC2,BC4,EC2,EC4                                  RAY20150
      COMMON /BLCK76/  DB,DE,WB,WE                                      RAY20160
      COMMON /BLCK77/  RB,NDX
C****                                                                   RAY20170
      IF (IBEX .NE. 0 ) GO TO 10                                        RAY20180
C****
C**** MAGNETIC FIELD COMPONENTS
C****
      F1 = BF                                                           RAY20190
      D = DB                                                            RAY20200
      C02 = BC2                                                         RAY20210
      C04 = BC4                                                         RAY20220
      W2 = WB*WB                                                        RAY20230
      C0 = CB0                                                          RAY20240
      C1 = CB1                                                          RAY20250
      C2 = CB2                                                          RAY20260
      C3 = CB3                                                          RAY20270
      C4 = CB4                                                          RAY20280
      C5 = CB5                                                          RAY20290
      GO TO 20                                                          RAY20300
C****                                                                   RAY20310
C**** ELECTRIC FIELD COMPONENTS
C****
   10 F1 = EF                                                           RAY20320
      IF( IN .EQ. 1 ) F1 = -EF                                          RAY20330
      D = DE                                                            RAY20340
      C02 = EC2                                                         RAY20350
      C04 = EC4                                                         RAY20360
      W2 = WE*WE                                                        RAY20370
      C0 = CE0                                                          RAY20380
      C1 = CE1                                                          RAY20390
      C2 = CE2                                                          RAY20400
      C3 = CE3                                                          RAY20410
      C4 = CE4                                                          RAY20420
      C5 = CE5                                                          RAY20430
   20 ZD1 = Z/D                                                         RAY20440
      ZD2 = C02*(ZD1+1.D0)*X*X/W2                                       RAY20450
      W4 = W2*W2                                                        RAY20460
      ZD3 = C04*(X**4)/W4                                               RAY20470
      S = ZD1+ZD2+ZD3                                                   RAY20480
      CS = C0+S*(C1+S*(C2+S*(C3+S*(C4+S*C5))))                          RAY20490
      IF ( DABS(CS) .GT. 70. ) CS = DSIGN ( 70.D0,CS )                  RAY20500
      E = DEXP(CS)                                                      RAY20510
      P0 = 1.0+E                                                        RAY20520
      F = F1/P0                                                         RAY20530
      IF( IBEX  .EQ.  1) RETURN
      IF( NFLAG .EQ.  1) F=F*(1.D0 - (F/F1)*NDX*DR/RB)
      RETURN                                                            RAY20540
      END                                                               RAY20550
      SUBROUTINE BEY (BEF,Z,X,Y,IBEX )                                  RAY20560
C****                                                                   RAY20570
C**** CALCULATE B OR E FIELD OFF THE MEDIAN PLANE                       RAY20580
C****                                                                   RAY20590
C****                                                                   RAY20600
C****
C**** IBEX = 0   MAGNETIC FIELD COMPONENTS
C****      = 1   ELECTRIC FIELD COMPONENTS
C****
      IMPLICIT  REAL*8 (A-H,O-Z)                                        RAY20610
      REAL*8 NDX
      COMMON /BLCK73/  IN,NFLAG                                         RAY20130
      COMMON /BLCK74/  BF,EF,S,DG                                       RAY20620
      COMMON /BLCK77/  RB,NDX
      DIMENSION BEF(3)                                                  RAY20630
C****                                                                   RAY20640
C****                                                                   RAY10110
C**** NON MID-PLANE FRINGING FIELD REGION
C****                                                                   RAY10120
      IF( IBEX  .EQ. 1 ) GO TO 1
      IF( NFLAG .EQ. 0 ) GO TO 1
      SINE = -1.
      IF( IN .EQ. 3 ) SINE=1.
      DR0  = X*SINE
      DR1  = SINE* X                                                    RAY10270
      DR2  = DR1                                                        RAY10280
      DR9  = DR1                                                        RAY10290
      DR10 = DR1                                                        RAY10300
      DR3  = SINE* ( X + DG )                                           RAY10310
      DR5  = DR3                                                        RAY10320
      DR11 = DR3                                                        RAY10330
      DR4  = SINE*( X - DG )                                            RAY10340
      DR7  = DR4                                                        RAY10350
      DR12 = DR4                                                        RAY10360
      DR6  = SINE* ( X + 2.*DG )                                        RAY10370
      DR8  = SINE* ( X - 2.*DG )                                        RAY10380
C****
C****
C****
    1 CALL BEFN(F0,Z,X,Y,       DR0, IBEX )                             RAY20650
      CALL BEFN(F1,Z+DG,X,Y,    DR1, IBEX )                             RAY20660
      CALL BEFN(F2,Z+2.*DG,X,Y, DR2, IBEX )                             RAY20670
      CALL BEFN(F3,Z+DG,X+DG,Y, DR3, IBEX )                             RAY20680
      CALL BEFN(F4,Z+DG,X-DG,Y, DR4, IBEX )                             RAY20690
      CALL BEFN(F5,Z   ,X+DG,Y, DR5, IBEX )                             RAY20700
      CALL BEFN(F6,Z,X+2.*DG,Y, DR6, IBEX )                             RAY20710
      CALL BEFN(F7,Z,X-DG,Y,    DR7, IBEX )                             RAY20720
      CALL BEFN(F8,Z,X-2.*DG,Y, DR8, IBEX )                             RAY20730
      CALL BEFN(F9,Z-DG,X,Y,    DR9, IBEX )                             RAY20740
      CALL BEFN(F10,Z-2.*DG,X,Y,DR10,IBEX )                             RAY20750
      CALL BEFN(F11,Z-DG,X+DG,Y,DR11,IBEX )                             RAY20760
      CALL BEFN(F12,Z-DG,X-DG,Y,DR12,IBEX )                             RAY20770
C****                                                                   RAY20780
      YG1 = Y/DG                                                        RAY20790
      YG2 = YG1**2                                                      RAY20800
      YG3 = YG1**3                                                      RAY20810
      YG4 = YG1**4                                                      RAY20820
C****                                                                   RAY20830
      BEF(1) = YG1 * ( (F5-F7)*2./3. - (F6-F8)/12. ) +                  RAY20840
     1         YG3 * ( (F5-F7)/6. - (F6-F8)/12. -                       RAY20850
     2         ( F3 + F11 - F4 - F12 - 2.*F5 + 2.*F7 )/12. )            RAY20860
      BEF(2) = F0 - YG2*( (F1 + F9 + F5 + F7 - 4.*F0) * 2./3. -         RAY20870
     1         ( F2 + F10 + F6 + F8 - 4.*F0 )/24. ) +                   RAY20880
     2         YG4 * (-( F1 + F9 + F5 + F7 - 4.*F0 )/6. +               RAY20890
     3         ( F2 + F10 +      F6 + F8 - 4.*F0 )/24. +                RAY20900
     4         ( F3 + F11 + F4 + F12 - 2.*F1 - 2.*F9 -                  RAY20910
     5         2.*F5 - 2.*F7 + 4.*F0 )/12. )                            RAY20920
      BEF(3) = YG1 * ( (F1 - F9)*2./3. - (F2 - F10)/12. ) +             RAY20930
     1         YG3 * ( (F1 - F9)/6. - (F2 - F10)/12. -                  RAY20940
     2         (F3 + F4 - F11 - F12 - 2.*F1 + 2.*F9)/12. )              RAY20950
      RETURN                                                            RAY20960
      END                                                               RAY20970
      SUBROUTINE BEVC                                                   RAY20980
C****                                                                   RAY20990
C****  CALCULATES B AND E FIELDS                                        RAY21000
C****                                                                   RAY21010
C****                                                                   RAY21020
C****
C**** NFLAG = 0      UNIFORM FIELD MAGNETIC DIPOLE
C****       = 1  NON-UNIFORM FIELD MAGNETIC DIPOLE
      IMPLICIT     REAL*8 (A-H,O-Z)                                     RAY21030
      REAL*8 K,NDX                                                      RAY21040
      COMMON /BLCK10/  BX, BY, BZ, K, TC, DTC                           RAY21050
      COMMON /BLCK11/  EX, EY, EZ, QMC, IVEC                            RAY21060
      COMMON /BLCK71/  CB0,CB1,CB2,CB3,CB4,CB5                          RAY21070
      COMMON /BLCK72/  CE0,CE1,CE2,CE3,CE4,CE5                          RAY21080
      COMMON /BLCK73/  IN,NFLAG                                         RAY21090
      COMMON /BLCK74/  BF,EF,S,DG                                       RAY21100
      COMMON /BLCK77/  RB,NDX
      DIMENSION TC(6),DTC(6),BEF(3)                                     RAY21110
C****                                                                   RAY21120
      GO TO (2,1,2) , IN                                                RAY21130
      PRINT  100,IN                                                     RAY21140
  100 FORMAT (  35H0 ERROR -GO TO -  IN BFUN   IN=     ,I5 )            RAY21150
C****
C**** UNIFORM FIELD REGION
C****
    1 BX = 0.                                                           RAY21160
      BY = BF                                                           RAY21170
      BZ = 0.                                                           RAY21180
      EX = EF                                                           RAY21190
      EY = 0.                                                           RAY21200
      EZ = 0.                                                           RAY21210
      IF( NFLAG .EQ. 0 ) RETURN
      X = TC(1)                                                         RAY09390
      Y = TC(2)                                                         RAY09400
      Z = TC(3)                                                         RAY09410
      DR =X
      RP = X+RB
      IF( RP .LE. 0. ) RP = 1.D-20
      DRR1 = DR/RB                                                      RAY09500
      IF( Y .NE. 0. )  GO TO 14                                         RAY09540
C****
C**** MID-PLANE UNIFORM FIELD REGION
C****
      BY = BF* ( 1. - NDX*DRR1 )
      RETURN                                                            RAY09620
C****
C**** NON MID-PLANE UNIFORM FIELD REGION
C****
   14 YR1 = Y/RB                                                        RAY09630
      YR2 = YR1*YR1                                                     RAY09640
      YR3 = YR2*YR1                                                     RAY09650
      YR4 = YR3*YR1                                                     RAY09660
      RR1 = RB/RP                                                       RAY09670
      RR2 = RR1*RR1                                                     RAY09680
      RR3 = RR2*RR1                                                     RAY09690
      BX  = BF*( -NDX*YR1 - (NDX*RR2 )*YR3/6. )
      BY  = BF* ( 1.-NDX*DRR1+.5*YR2*NDX*RR1 - YR4*NDX*RR3/24. )
      RETURN                                                            RAY21220
C****                                                                   RAY21230
C**** FRINGE FIELD REGIONS:  FIND B AND E FIELDS                        RAY21240
C****                                                                   RAY21250
    2 X = TC(1)                                                         RAY21260
      Y = TC(2)                                                         RAY21270
      Z = TC(3)                                                         RAY21280
      IF ( Y .EQ. 0. ) GO TO 3                                          RAY21290
C****
C**** MAGNETIC: NON-MIDPLANE REGION
C****
      CALL BEY( BEF,Z,X,Y,0 )                                           RAY21300
      BX = BEF(1)                                                       RAY21310
      BY = BEF(2)                                                       RAY21320
      BZ = BEF(3)                                                       RAY21330
      GO TO 4                                                           RAY21340
C****                                                                   RAY21350
C**** MAGNETIC:     MIDPLANE REGION
C****
    3 CONTINUE
      IF( NFLAG .EQ. 0 ) GO TO 6
      SINE = -1.
      IF( IN .EQ. 3 ) SINE=1.
      DR = X*SINE
    6 CALL BEFN(B0,Z,X,Y,DR,0)                                          RAY21360
      BX = 0.                                                           RAY21370
      BY = B0                                                           RAY21380
      BZ = 0.                                                           RAY21390
C****                                                                   RAY21400
C**** NOW FIND E FIELD                                                  RAY21410
C****                                                                   RAY21420
    4 IF ( X .EQ. 0 ) GO TO 5                                           RAY21430
C****
C**** ELECTRIC: NON-MIDPLANE REGION
C****
      CALL BEY( BEF,Z,Y,X,1 )                                           RAY21440
      EX = BEF(2)                                                       RAY21450
      EY = BEF(1)                                                       RAY21460
      EZ = BEF(3)                                                       RAY21470
      RETURN                                                            RAY21480
C****
C**** ELECTRIC:     MIDPLANE REGION
C****
    5 CALL BEFN ( B1,Z,Y,X,0.D0,1 )                                     RAY21490
      EX = B1                                                           RAY21500
      EY = 0.                                                           RAY21510
      EZ = 0.                                                           RAY21520
      RETURN                                                            RAY21530
      END                                                               RAY21540
      SUBROUTINE MULT   ( NO, NP, T, TP ,NUM )                          RAY21550
C****                                                                   RAY21560
C****                                                                   RAY21570
C**** MULTIPOLE     RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIALRAY21580
C**** EQUATIONS OF MOTION.                                              RAY21590
C     T = TIME                                                          RAY21600
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY21610
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY21620
C****                                                                   RAY21630
C****                                                                   RAY21640
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY21650
      CHARACTER*4 ITITLE
      REAL*8  LF, K, L                                                  RAY21660
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY21670
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY21680
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY21690
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY21700
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          RAY21710
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              RAY21720
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY21730
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY21740
      EXTERNAL BMULT                                                    RAY21750
C****                                                                   RAY21770
      LF   = DATA(  1,NO )                                              RAY21780
      DG   = DATA(  2,NO )                                              RAY21790
      A    = DATA( 10,NO )                                              RAY21800
      B    = DATA( 11,NO )                                              RAY21810
      L    = DATA( 12,NO )                                              RAY21820
      W    = DATA( 13,NO )                                              RAY21830
      D    = DATA( 14,NO )                                              RAY21840
      BF   = DATA( 15,NO )                                              RAY21850
      Z1   = DATA( 16,NO )                                              RAY21860
      Z2   = DATA( 17,NO )                                              RAY21870
      C0   = DATA( 20,NO )                                              RAY21880
      C1   = DATA( 21,NO )                                              RAY21890
      C2   = DATA( 22,NO )                                              RAY21900
      C3   = DATA( 23,NO )                                              RAY21910
      C4   = DATA( 24,NO )                                              RAY21920
      C5   = DATA( 25,NO )                                              RAY21930
      C6   = DATA( 26,NO )                                              RAY21940
      C7   = DATA( 27,NO )                                              RAY21950
      C8   = DATA( 28,NO )                                              RAY21960
      DTF = LF/VEL                                                      RAY21970
      BX = 0.                                                           RAY21980
      BY = 0.                                                           RAY21990
      BZ = 0.                                                           RAY22000
      BT = 0.                                                           RAY22010
      S = 0.                                                            RAY22020
C****                                                                   RAY22030
      IF( NP  .GT. 100 ) GO TO 5                                        RAY22040
      PRINT 100, ITITLE(NO)                                             RAY22050
  100 FORMAT(  ' MULTIPOLE  ****  ', A4,'  *************************'/) RAY22060
      PRINT 101                                                         RAY22070
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,RAY22080
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY22090
     2   6HPHI MR , 6X, 1HB             )                               RAY22100
      CALL PRNT2 (TP,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )RAY22110
      PRINT 103                                                         RAY22120
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO CENTERED AXIS SYSTEM ' ) RAY22130
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY22140
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY22150
C****                                                                   RAY22160
    5 TC(1) =  XA                                                       RAY22170
      TC(2) = YA                                                        RAY22180
      TC(3) = ZA - (A+L/2.)                                             RAY22190
      TC(4) =  VXA                                                      RAY22200
      TC(5) =  VYA                                                      RAY22210
      TC(6) =  VZA                                                      RAY22220
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY22230
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY22240
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 RAY22250
C****                                                                   RAY22260
      TDT = ( Z1 - TC(3)  ) /DABS( TC(6) )                              RAY22270
C****                                                                   RAY22280
      TC(1) = TC(1) + TDT * TC(4)                                       RAY22290
      TC(2) = TC(2) + TDT * TC(5)                                       RAY22300
      TC(3) = TC(3) + TDT * TC(6)                                       RAY22310
      T = T + TDT                                                       RAY22320
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY22330
      IF( NP  .LE. 100) PRINT 104                                       RAY22340
  104 FORMAT( 24H0MULTIPOLE FIELD REGION  )                             RAY22350
      CALL FNMIRK( 6, T, DTF ,TC, DTC, DS, ES, BMULT, 0    )            RAY22360
      NSTEP = 0                                                         RAY22370
    6 CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY22380
      DO 7 I = 1, NP                                                    RAY22390
      CALL FNMIRK( 6, T, DTF ,TC, DTC, DS, ES, BMULT, 1    )            RAY22400
      TP = TP + DTF*VEL
      NSTEP = NSTEP + 1                                                 RAY22410
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY22411
      IF( Z2  .LE. TC(3) ) GO TO 8                                      RAY22420
    7 CONTINUE                                                          RAY22430
      GO TO 6                                                           RAY22440
    8 CONTINUE                                                          RAY22450
      XDTF  =-( TC(3) - Z2  ) /DABS( TC(6) )                            RAY22460
      CALL FNMIRK( 6, T,XDTF ,TC, DTC, DS, ES,BMULT,  0    )            RAY22470
      CALL FNMIRK( 6, T,XDTF ,TC, DTC, DS, ES,BMULT,  1    )            RAY22480
      TP = TP + XDTF*VEL
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY22490
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY22500
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY22510
C****                                                                   RAY22520
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY22530
C****                                                                   RAY22540
      TC(3) = TC(3) - (B+L/2.)                                          RAY22550
      IF( NP  .LE. 100) PRINT 109                                       RAY22560
      CALL PRNT2 (TP,S,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY22570
C****                                                                   RAY22580
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY22590
C****                                                                   RAY22600
      TDT = -TC(3) /DABS( TC(6) )                                       RAY22610
      TC(1) = TC(1) + TDT * TC(4)                                       RAY22620
      TC(2) = TC(2) + TDT * TC(5)                                       RAY22630
      TC(3) = TC(3) + TDT * TC(6)                                       RAY22640
      T = T + TDT                                                       RAY22650
      TP = TP + TDT*VEL                                                 RAY22660
      BX = 0.                                                           RAY22670
      BY = 0.                                                           RAY22680
      BZ = 0.                                                           RAY22690
      BT = 0.                                                           RAY22700
      S  = 0.                                                           RAY22710
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY22780
      RETURN                                                            RAY22890
99      CALL PRNT4(NO, IN)                                              RAY22891
        RETURN                                                          RAY22892
      END                                                               RAY22900
      SUBROUTINE BMULT                                                  RAY22910
C****                                                                   RAY22920
C****                                                                   RAY22930
C**** THE RELATIONSHIP BETWEEN B0, ......... B12 AND B(I,J) RELATIVE TO RAY22940
C**** AXES (Z,X) IS GIVEN BY                                            RAY22950
C****                                                                   RAY22960
C****                                                                   RAY22970
C****                                                                   RAY22980
C**** B0  = B( 0, 0 )                                                   RAY22990
C**** B1  = B( 1, 0 )                                                   RAY23000
C**** B2  = B( 2, 0 )                                                   RAY23010
C**** B3  = B( 1, 1 )                                                   RAY23020
C**** B4  = B( 1,-1 )                                                   RAY23030
C**** B5  = B( 0, 1 )                                                   RAY23040
C**** B6  = B( 0, 2 )                                                   RAY23050
C**** B7  = B( 0,-1 )                                                   RAY23060
C**** B8  = B( 0,-2 )                                                   RAY23070
C**** B9  = B(-1, 0 )                                                   RAY23080
C**** B10 = B(-2, 0 )                                                   RAY23090
C**** B11 = B(-1, 1 )                                                   RAY23100
C**** B12 = B(-1,-1 )                                                   RAY23110
C****                                                                   RAY23120
C****                                                                   RAY23130
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY23140
      REAL*8  K, L                                                      RAY23150
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY23160
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          RAY23170
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              RAY23180
      DIMENSION TC(6), DTC(6)                                           RAY23190
      X = TC(1)                                                         RAY23200
      Y = TC(2)                                                         RAY23210
      Z = TC(3)                                                         RAY23220
      CALL MLTT ( B0, Z, X, Y )                                         RAY23230
      CALL MLTT ( B1 , Z + DG, X , Y )                                  RAY23240
      CALL MLTT ( B2 , Z + 2.*DG, X , Y )                               RAY23250
      CALL MLTT ( B3 , Z + DG, X + DG , Y )                             RAY23260
      CALL MLTT ( B4 , Z + DG, X - DG , Y )                             RAY23270
      CALL MLTT ( B5 , Z , X + DG , Y )                                 RAY23280
      CALL MLTT ( B6 , Z , X + 2.*DG , Y )                              RAY23290
      CALL MLTT ( B7 , Z , X - DG , Y )                                 RAY23300
      CALL MLTT ( B8 , Z , X - 2.*DG , Y )                              RAY23310
      CALL MLTT ( B9 , Z - DG, X , Y )                                  RAY23320
      CALL MLTT ( B10, Z - 2.*DG, X , Y )                               RAY23330
      CALL MLTT ( B11, Z - DG, X + DG , Y )                             RAY23340
      CALL MLTT ( B12, Z - DG, X - DG , Y )                             RAY23350
      YG1 = Y/DG                                                        RAY23360
      YG2 = YG1**2                                                      RAY23370
      YG3 = YG1**3                                                      RAY23380
      YG4 = YG1**4                                                      RAY23390
      BX = YG1 * ( (B5-B7)*2./3. - (B6-B8)/12. )  +                     RAY23400
     1     YG3*( (B5-B7)/6. - (B6-B8)/12. -                             RAY23410
     2     (B3 + B11 - B4 - B12 - 2.*B5 + 2.*B7 ) / 12. )               RAY23420
      BY = B0 - YG2*( ( B1 + B9 + B5 + B7 - 4.*B0 ) *2./3. -            RAY23430
     1     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. ) +                     RAY23440
     2     YG4* (-( B1 + B9 + B5 + B7 - 4.*B0 ) / 6. +                  RAY23450
     3     ( B2 + B10 + B6 + B8 - 4.*B0 ) / 24. +                       RAY23460
     4     ( B3 + B11 + B4 + B12 - 2.*B1 - 2.*B9 -                      RAY23470
     5     2.*B5 - 2.*B7 + 4.*B0 ) / 12. )                              RAY23480
      BZ = YG1*( (B1 - B9 ) *2./3. - ( B2 - B10 ) /12. ) +              RAY23490
     1     YG3*( ( B1 - B9 ) / 6. - ( B2 - B10 ) / 12. -                RAY23500
     2     ( B3 + B4 - B11 - B12 - 2.*B1 + 2.*B9 ) / 12.  )             RAY23510
      BT  =DSQRT(BX*BX + BY*BY + BZ*BZ)                                 RAY23520
      RETURN                                                            RAY23530
      END                                                               RAY23540
      SUBROUTINE  MLTT ( BFLD, Z, X, Y )                                RAY23550
C****                                                                   RAY23560
C****                                                                   RAY23570
C****                                                                   RAY23580
C****                                                                   RAY23590
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY23600
      REAL*8  K, L                                                      RAY23610
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY23620
      COMMON  /BLK100/  W, L, D, DG, S, BF, BT                          RAY23630
      COMMON  /BLK101/  C0, C1, C2, C3, C4, C5, C6, C7, C8              RAY23640
      DIMENSION TC(6), DTC(6)                                           RAY23650
      U = 2.*X/W                                                        RAY23660
      S = 2.*Z/L                                                        RAY23670
      DL2 = (L/D)**2                                                    RAY23680
      W1 = C0 + C1*U + C2*U*U + C3*U**3 + C4*U**4 + C5*U**5             RAY23690
      W2 = 1. + C7*( S**4 + DL2*C8*S**8 ) / ( 1. + DL2*C8 )             RAY23700
      BFLD = BF*W1 / W2                                                 RAY23710
      RETURN                                                            RAY23720
      END                                                               RAY23730
      SUBROUTINE COLL ( NO, J, IFLAG  )
C****
C****
C**** TEST AND SET FLAG IF RAY EXCEEDS RECTANGULAR OR ELLIPTICAL
C**** COLLIMATOR CUT-OFF DIMENSIONS
C****
C****
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY00090
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY00110
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL        RAY00130
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY00150
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DIMENSION DATA(75,200),ITITLE(200)
C****
C****
  100 FORMAT( // 5X, 'RAY=', I5, 5X, 'ELEMENT=', I3, 
     1   '   STOPPED - EXCEEDS RECTANGULAR COLLIMATOR DIMENSIONS ' // )
  101 FORMAT( // 5X, 'RAY=', I5, 5X, 'ELEMENT=', I3, 
     1   '   STOPPED - EXCEEDS ELLIPTICAL  COLLIMATOR DIMENSIONS ' // )
C****
C****
      IFLAG = 0
      ICOLL = DATA(1,NO)
      XCEN  = DATA(2,NO)
      YCEN  = DATA(3,NO)
      XMAX  = DATA(4,NO)
      YMAX  = DATA(5,NO)
      IF ( ICOLL .NE. 0 ) GO TO 1
      IF ( (DABS(XA-XCEN) .GT. XMAX) .OR. (DABS(YA-YCEN) .GT. YMAX) )
     1      GO TO 2
      RETURN
    2 PRINT 100, J, NO
      GO TO 3
    1 XC = (XA-XCEN)/XMAX
      YC = (YA-YCEN)/YMAX
      IF ( (XC*XC+YC*YC) .GT. 1. ) GO TO 4
      RETURN
    4 PRINT 101, J, NO
    3 XO(J)  = 1.D10
      YO(J)  = 1.D10
      VXO(J) = 0.
      VYO(J) = 0.
      IFLAG  = 1
      RETURN
      END
      SUBROUTINE  LENS ( NO,  NP,T, TP ,NUM )                           RAY23740
C****                                                                   RAY23750
C****                                                                   RAY23760
C**** THIN LENS ROUTINE                                                 RAY23770
C****                                                                   RAY23780
C****                                                                   RAY23790
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY23800
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY23810
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY23820
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY23830
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY23840
C****                                                                   RAY23860
  100 FORMAT( /  '   THIN LENS     ****  ', A4, '****************',//   RAY23870
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VEL/C' RAY23880
     2   , '    THETA MR      PHI MR'  /  )                             RAY23890
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      RAY23900
C****                                                                   RAY23910
      NUM = NUM+1
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 100, ITITLE(NO)                           RAY23920
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY23930
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY23940
      VZP =  VZA  / VEL                                                 RAY23950
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY23970
      XXA = XA                                                          RAY23980
      YYA = YA                                                          RAY23990
       CS = DATA(9,NO)
       E0 = DATA(10,NO)
       CN = DATA(11,NO)
      IF( E0 .EQ. 0. ) E0 = ENERGY
       FE = (E0/ENERGY)**CN
       TX = DATA(3,NO)*FE
       PY = DATA(7,NO)*FE
       XA =XXA*DATA(1,NO) + VXP*DATA(2,NO)                              RAY24000
      VXP =XXA*TX         + VXP*DATA(4,NO) -                            RAY24010
     1     CS*TX**4         * ( XXA*XXA + YYA*YYA )*XXA/10**9
       YA =YYA*DATA(5,NO) + VYP*DATA(6,NO)                              RAY24020
      VYP =YYA*PY         + VYP*DATA(8,NO) -                            RAY24030
     1     CS*PY**4         * ( XXA*XXA + YYA*YYA )*YYA/10**9
      VXA = VEL*DSIN( VXP/1000.D0 )                                     RAY24040
      VYA = VEL*DSIN( VYP/1000.D0 )                                     RAY24050
      VZA = DSQRT(VEL*VEL -VXA*VXA-VYA*VYA)                             RAY24060
      VZP = VZA/VEL                                                     RAY24070
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24080
      NUM = NUM+1
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TP )
      RETURN                                                            RAY24090
      END                                                               RAY24100
      SUBROUTINE SHROT  ( NO, NP, T, TP ,NUM )                          RAY24110
C****                                                                   RAY24120
C****                                                                   RAY24130
C**** SUBROUTINE DOES TRANSLATIONS FIRST ALONG AXES X, Y, Z IN ORDER,   RAY24140
C**** FOLLOWED BY ROTATIONS ABOUT X, Y, Z   .                           RAY24150
C****                                                                   RAY24160
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY24170
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY24180
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY24190
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY24200
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY24210
C****
C****
  100 FORMAT( / '   TRANSLATE-ROTATE  ****  ', A4,'  ***************'// RAY24320
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VEL/C' RAY24330
     2   , '    THETA MR      PHI MR'  /  )                             RAY24340
  101 FORMAT( '  TRANSLATE  ' )                                         RAY24350
  102 FORMAT( '  ROTATE  '  )                                           RAY24360
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      RAY24370
C****
C****
      NUM = NUM+1
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TP )
      X0 = DATA( 1,NO )                                                 RAY24230
      Y0 = DATA( 2,NO )                                                 RAY24240
      Z0 = DATA( 3,NO )                                                 RAY24250
      CX = DCOS( DATA(4,NO)/57.29578 )                                  RAY24260
      SX = DSIN( DATA(4,NO)/57.29578 )                                  RAY24270
      CY = DCOS( DATA(5,NO)/57.29578 )                                  RAY24280
      SY = DSIN( DATA(5,NO)/57.29578 )                                  RAY24290
      CZ = DCOS( DATA(6,NO)/57.29578 )                                  RAY24300
      SZ = DSIN( DATA(6,NO)/57.29578 )                                  RAY24310
      IF( NP  .LE. 100) PRINT 100                                       RAY24380
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY24390
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY24400
      VZP =  VZA  / VEL                                                 RAY24410
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24420
      IF( (X0 .EQ. 0.) .AND. (Y0 .EQ. 0.) .AND. (Z0 .EQ. 0.) ) GO TO 1  RAY24430
      IF( NP  .LE. 100) PRINT 101                                       RAY24440
      XA = XA-X0                                                        RAY24450
      YA = YA-Y0                                                        RAY24460
      ZA = ZA-Z0                                                        RAY24470
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24480
    1 IF( DATA( 4,NO ) .EQ. 0. ) GO TO 2                                RAY24490
      IF( NP  .LE. 100) PRINT 102                                       RAY24500
      YR =  YA*CX +  ZA*SX                                              RAY24510
      ZR = -YA*SX +  ZA*CX                                              RAY24520
      VYR= VYA*CX + VZA*SX                                              RAY24530
      VZR=-VYA*SX + VZA*CX                                              RAY24540
      YA = YR                                                           RAY24550
      ZA = ZR                                                           RAY24560
      VYA = VYR                                                         RAY24570
      VZA = VZR                                                         RAY24580
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY24590
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY24600
      VZP =  VZA  / VEL                                                 RAY24610
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24620
    2 IF( DATA( 5,NO ) .EQ. 0. ) GO TO 3                                RAY24630
      IF( NP  .LE. 100) PRINT 102                                       RAY24640
      XR = -ZA*SY +  XA*CY                                              RAY24650
      ZR =  ZA*CY +  XA*SY                                              RAY24660
      VXR=-VZA*SY + VXA*CY                                              RAY24670
      VZR= VZA*CY + VXA*SY                                              RAY24680
      XA = XR                                                           RAY24690
      ZA = ZR                                                           RAY24700
      VXA = VXR                                                         RAY24710
      VZA = VZR                                                         RAY24720
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY24730
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY24740
      VZP =  VZA  / VEL                                                 RAY24750
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24760
    3 IF( DATA( 6,NO ) .EQ. 0. ) GO TO 4                                RAY24770
      IF( NP  .LE. 100) PRINT 102                                       RAY24780
      XR =  XA*CZ +  YA*SZ                                              RAY24790
      YR = -XA*SZ +  YA*CZ                                              RAY24800
      VXR= VXA*CZ + VYA*SZ                                              RAY24810
      VYR=-VXA*SZ + VYA*CZ                                              RAY24820
      XA = XR                                                           RAY24830
      YA = YR                                                           RAY24840
      VXA = VXR                                                         RAY24850
      VYA = VYR                                                         RAY24860
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY24870
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY24880
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY24890
      NUM = NUM+1
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TP )
C****
C**** TRANSLATE PARTICLE TO ORIGIN OF COORDINATE SYSTEM
C****
    4 TDT = - ZA / DABS(VZA)                                            RAY25140
      T = T + TDT                                                       RAY25150
      TP = TP + TDT*VEL                                                 RAY25160
      XA = XA + TDT*VXA                                                 RAY25170
      YA = YA + TDT*VYA                                                 RAY25180
      ZA = 0.                                                           RAY25190
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY25200
      RETURN                                                            RAY24900
      END                                                               RAY24910
      SUBROUTINE  DRIFT( NO,  NP,T, TP ,NUM )                           RAY24920
C****                                                                   RAY24930
C****                                                                   RAY24940
C**** Z-AXIS DRIFT ROUTINE                                              RAY24950
C****                                                                   RAY24960
C****                                                                   RAY24970
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY24980
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY24990
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY25000
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY25010
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY25020
C****
C****
  100 FORMAT( /  '   Z-AXIS DRIFT  ****  ', A4, '****************',//   RAY25040
     1'      T CM', 18X, 'X CM', 7X, 'Y CM', 7X, 'Z CM' , '      VEL/C' RAY25050
     2   , '    THETA MR      PHI MR'  /  )                             RAY25060
  103 FORMAT( F10.4, 11X, 3F11.3, F12.5, 2F12.3  )                      RAY25070
C****
C****
      NUM = NUM+1
      NBR = 1
      CALL PLT2 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 100, ITITLE(NO)                           RAY25080
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY25090
      VYP = 1000. *DASIN ( VYA/VEL )                                    RAY25100
      VZP =  VZA  / VEL                                                 RAY25110
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY25130
      TDT =(DATA(1,NO) - ZA) / DABS(VZA)                                RAY25140
      T = T + TDT                                                       RAY25150
      TP = TP + TDT*VEL                                                 RAY25160
      XA = XA + TDT*VXA                                                 RAY25170
      YA = YA + TDT*VYA                                                 RAY25180
      ZA = 0.                                                           RAY25190
      IF( NP  .LE. 100) PRINT 103, TP, XA, YA, ZA, VZP, VXP, VYP        RAY25200
      NUM = NUM+1
      NBR = 2
      CALL PLT2 ( NUM, NO, NBR, TP )
      RETURN                                                            RAY25210
      END                                                               RAY25220
      SUBROUTINE SOLND  ( NO, NP, T, TP ,NUM )                          RAY25230
C****                                                                   RAY25240
C****                                                                   RAY25250
C**** SOLENOID      RAY TRACING BY NUMERICAL INTEGRATION OF DIFFERENTIALRAY25260
C**** EQUATIONS OF MOTION.                                              RAY25270
C     T = TIME                                                          RAY25280
C     TC(1) TO TC(6) =  ( X, Y, Z, VX, VY, VZ )                         RAY25290
C     DTC(1) TO DTC(6) = ( VX, VY, VZ, VXDOT, VYDOT, VZDOT )            RAY25300
C**** BF (POSITIVE) : SOLENOID FIELD IN BEAM DIRECTION                  RAY25310
C**** CBF - USED IN BSOL TO DISTINGUISH BETWEEN COORD. SYSTEMS          RAY25320
C****                                                                   RAY25330
C****                                                                   RAY25340
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY25350
      REAL*8  LF           , K, L                                       RAY25360
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA , ITITLE                                   RAY25370
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY25380
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY25390
      COMMON  /BLCK 7/ NCODE                                            RAY25400
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY25410
      COMMON  /BLCK30/  BF ,      AL, RAD                               RAY25420
      COMMON  /BLCK31/  S, BT                                           RAY25430
      COMMON  /BLCK32/  IN                                              RAY25440
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY25450
      DIMENSION TC(6), DTC(6), DS(6), ES(6)                             RAY25460
      EXTERNAL  BSOL                                                    RAY25470
C****                                                                   RAY25490
C****                                                                   RAY25500
      LF   = DATA(  1,NO )                                              RAY25510
      A    = DATA( 10,NO )                                              RAY25520
      B    = DATA( 11,NO )                                              RAY25530
      L    = DATA( 12,NO )                                              RAY25540
      D    = DATA( 13,NO )                                              RAY25550
      BF   = DATA( 14,NO )                                              RAY25560
      Z11  = DATA( 15,NO )                                              RAY25570
      Z22  = DATA( 16,NO )                                              RAY25580
      DTF1= LF/VEL                                                      RAY25590
      AL  = L/2.                                                        RAY25600
      RAD = D/2.                                                        RAY25610
      BX = 0.                                                           RAY25620
      BY = 0.                                                           RAY25630
      BZ = 0.                                                           RAY25640
      BT = 0.                                                           RAY25650
      S = 0.                                                            RAY25660
C****                                                                   RAY25670
C****                                                                   RAY25680
      IF( NP  .GT. 100 ) GO TO 5                                        RAY25690
  201 FORMAT(  ' SOLENOID    ****  ', A4, '  ***********************'/) RAY25700
      PRINT 201, ITITLE(NO)                                             RAY25710
      PRINT 101                                                         RAY25760
  101 FORMAT( 8H    T CM ,18X, 4HX CM , 7X, 2HBX, 8X, 4HY CM , 7X, 2HBY,RAY25720
     1   8X, 4HZ CM, 7X, 2HBZ, 8X, 6HVEL/C  , 6X, 8HTHETA MR , 5X,      RAY25730
     2   6HPHI MR , 6X, 1HB             )                               RAY25740
      CALL PRNT2 (TP,S,XA   ,YA   ,ZA   ,BX,BY,BZ,BT,VXA  ,VYA  ,VZA   )RAY25750
      PRINT 103                                                         RAY25770
  103 FORMAT(   '0COORDINATE TRANSFORMATION TO CENTERED AXIS SYSTEM ' ) RAY25780
  109 FORMAT(   '0COORDINATE TRANSFORMATION TO D AXIS SYSTEM '       )  RAY25790
C****
C**** TRANSFORM FROM INITIAL ENTRANCE COORDINATES TO VFB COORD.         RAY25800
C****                                                                   RAY25810
    5 TC(1) =  XA                                                       RAY25820
      TC(2) = YA                                                        RAY25830
      TC(3) = ZA-A-AL                                                   RAY25840
      TC(4) =  VXA                                                      RAY25850
      TC(5) = VYA                                                       RAY25860
      TC(6) =  VZA                                                      RAY25870
      R = DSQRT( TC(1)*TC(1) + TC(2)*TC(2) )
      CALL PRNT2 (TP,R,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY25880
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY25890
C**** TRANSLATE PARTICLE TO START OF FIRST FRINGE FIELD                 RAY25900
C****                                                                   RAY25910
      TDT = (-TC(3) -Z11 -AL ) /DABS( TC(6) )                           RAY25920
C****                                                                   RAY25930
      TC(1) = TC(1) + TDT * TC(4)                                       RAY25940
      TC(2) = TC(2) + TDT * TC(5)                                       RAY25950
      TC(3) = TC(3) + TDT * TC(6)                                       RAY25960
      T = T + TDT                                                       RAY25970
      TP = TP + TDT*VEL
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 104                                       RAY25980
  104 FORMAT( 22H0FRINGING FIELD REGION    )                            RAY25990
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BSOL , 0    )            RAY26000
      NSTEP = 0                                                         RAY26010
    6 CONTINUE
      R = DSQRT( TC(1)*TC(1) + TC(2)*TC(2) )
      CALL PRNT2 (TP,R,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY26020
      DO 7 I = 1, NP                                                    RAY26030
      CALL FNMIRK( 6, T, DTF1,TC, DTC, DS, ES, BSOL , 1    )            RAY26040
      TP = TP + DTF1*VEL
      NSTEP = NSTEP + 1                                                 RAY26050
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
        IF (NSTEP  .GT.  200)  GO TO 99                                 RAY26051
      IF( (Z22+AL) .LE. TC(3) ) GO TO 8                                 RAY26060
    7 CONTINUE                                                          RAY26070
      GO TO 6                                                           RAY26080
    8 CONTINUE                                                          RAY26090
      XDTF1 =-( TC(3) -(Z22+AL)  ) / DABS( TC(6) )                      RAY26100
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BSOL ,  0    )            RAY26110
      CALL FNMIRK( 6, T,XDTF1,TC, DTC, DS, ES,BSOL ,  1    )            RAY26120
      TP = TP + XDTF1*VEL
      R = DSQRT( TC(1)*TC(1) + TC(2)*TC(2) )
      CALL PRNT2 (TP,R,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY26130
      NUM = NUM+1
      NBR = 2
      CALL PLT1 ( NUM, NO, NBR, TP )
      IF( NP  .LE. 100) PRINT 105, NSTEP                                RAY26140
  105 FORMAT( 10H   NSTEPS=  ,I5 )                                      RAY26150
C****                                                                   RAY26160
C**** TRANSFORM TO OUTPUT SYSTEM COORD.                                 RAY26170
C****                                                                   RAY26180
      TC(3) = TC(3) - B - AL                                            RAY26190
      IF( NP  .LE. 100) PRINT 109                                       RAY26200
      R = DSQRT( TC(1)*TC(1) + TC(2)*TC(2) )
      CALL PRNT2 (TP,R,TC(1),TC(2),TC(3),BX,BY,BZ,BT,TC(4),TC(5),TC(6) )RAY26210
C****                                                                   RAY26220
C**** TRANSLATE PARTICLE TO OUT SYSTEM COORD.                           RAY26230
C****                                                                   RAY26240
      TDT = -TC(3) /DABS( TC(6) )                                       RAY26250
      TC(1) = TC(1) + TDT * TC(4)                                       RAY26260
      TC(2) = TC(2) + TDT * TC(5)                                       RAY26270
      TC(3) = TC(3) + TDT * TC(6)                                       RAY26280
      T = T + TDT                                                       RAY26290
      TP = TP + TDT*VEL                                                 RAY26300
      BX = 0.                                                           RAY26310
      BY = 0.                                                           RAY26320
      BZ = 0.                                                           RAY26330
      BT = 0.                                                           RAY26340
      S  = 0.                                                           RAY26350
      CALL PRNT6( TP, TC(1), TC(2), TC(3), TC(4), TC(5), TC(6)  )
      NUM = NUM+1
      NBR = 3
      CALL PLT1 ( NUM, NO, NBR, TP )
C****                                                                   RAY26420
      RETURN                                                            RAY26510
99      CALL PRNT4(NO, IN )                                             RAY26511
        RETURN                                                          RAY26512
      END                                                               RAY26520
      SUBROUTINE BSOL                                                   RAY26530
C****                                                                   RAY26540
C****                                                                   RAY26550
C**** ROUTINE VALID FOR FIELDS OUTSIDE CENTRAL ZONE OF ELEMENTAL        RAY26560
C**** SOLENOID                                                          RAY26570
C**** BF    = FIELD AT CENTER OF INFINITE SOLENOID; CURR. DEN. (NI/M)   RAY26580
C**** M.W.GARRETTT  JOURNAL OF APP. PHYS. 34,(1963),P2567               RAY26590
C****                                                                   RAY26600
C****                                                                   RAY26610
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY26620
      REAL*8 K                                                          RAY26630
      DIMENSION  TC(6), DTC(6)                                          RAY26640
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY26650
      COMMON  /BLCK30/  BF ,      AL, RAD                               RAY26660
      COMMON  /BLCK31/  S, BT                                           RAY26670
      COMMON  /BLCK32/  IN                                              RAY26680
C****                                                                   RAY26690
C****                                                                   RAY26700
      DATA PI4/12.566370616D0 /                                         RAY26710
C****                                                                   RAY26720
C****                                                                   RAY26730
C****                                                                   RAY26740
      X = TC(1)                                                         RAY26750
      Y = TC(2)                                                         RAY26760
      Z = TC(3)                                                         RAY26770
      R =DSQRT( X*X + Y*Y )                                             RAY26780
      IF( R  .LT.  (RAD/1.D4)  )  GO TO 5                               RAY26790
      RADR = RAD+R                                                      RAY26800
      AAPR = 4.D0*RAD/RADR                                              RAY26810
      AAMR = (RAD-R)/(2.D0*RAD)                                         RAY26820
      RCSQ = 4.D0*RAD*R/(RADR*RADR)                                     RAY26830
C****                                                                   RAY26840
C**** SOLENOID LEFT  HAND SOURCE                                        RAY26850
C****                                                                   RAY26860
      ZZ = -(AL+Z)                                                      RAY26870
      R1SQ = RADR*RADR  + ZZ*ZZ                                         RAY26880
      R1 = DSQRT(R1SQ)                                                  RAY26890
      RKSQ = 4.D0*RAD*R/R1SQ                                            RAY26900
      CALL FB01AD(RKSQ,       VKS, VES )                                RAY26910
      CALL FB03AD(RCSQ, RKSQ, P )                                       RAY26920
      BZS1 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1                            RAY26930
      BRS1 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)                             RAY26940
C****                                                                   RAY26950
C**** SOLENOID RIGHT HAND SOURCE                                        RAY26960
C****                                                                   RAY26970
      ZZ = AL-Z                                                         RAY26980
      R1SQ = RADR*RADR  + ZZ*ZZ                                         RAY26990
      R1 = DSQRT(R1SQ)                                                  RAY27000
      RKSQ = 4.D0*RAD*R/R1SQ                                            RAY27010
      CALL FB01AD(RKSQ,       VKS, VES )                                RAY27020
      CALL FB03AD(RCSQ, RKSQ, P )                                       RAY27030
      BZS2 = AAPR*ZZ*(VKS+AAMR*(P-VKS) ) /R1                            RAY27040
      BRS2 = R1*(2.D0*(VKS-VES) - RKSQ*VKS)                             RAY27050
      BZ = BF*( BZS2-BZS1 )/PI4                                         RAY27060
      BR = BF*( BRS2-BRS1 )/(R*PI4)                                     RAY27070
      BX = BR * X /R                                                    RAY27080
      BY = BR *  Y/R                                                    RAY27090
      BT =DSQRT( BX**2 + BY**2 + BZ**2 )                                RAY27100
      RETURN                                                            RAY27110
    5 CONTINUE                                                          RAY27120
C****                                                                   RAY27130
C****                                                                   RAY27140
C****                                                                   RAY27150
      COSA = (AL-Z) / DSQRT( RAD*RAD + (AL-Z)**2  )                     RAY27160
      COSB =-(AL+Z) / DSQRT( RAD*RAD + (AL+Z)**2  )                     RAY27170
      BX = 0.                                                           RAY27180
      BY = 0.                                                           RAY27190
      BZ = BF*(COSA-COSB)/2.D0                                          RAY27200
      BT = DABS(BZ)                                                     RAY27210
      RETURN                                                            RAY27220
      END                                                               RAY27230
      SUBROUTINE FB01AD(C,  VK,VE)                                      RAY27240
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY27250
C*IBM REAL*8 XLG/  Z7FFFFFFFFFFFFFFF /                                  RAY27251
      REAL * 8 XLG/'7FFFFFFFFFFFFFFF'X/                                 RAY27260
      D=1D0-C                                                           RAY27270
      IF(D .GT. 0D0)E=-DLOG(D)                                          RAY27280
C**** HARWELL VERSION OF FB01AD                                         RAY27290
      IF(C .GE. 1D0)GO TO 2                                             RAY27300
           VE=E*((((((((((                                              RAY27310
     A     3.18591956555015718D-5*D  +.989833284622538479D-3)*D         RAY27320
     B    +.643214658643830177D-2)*D +.16804023346363385D-1)*D          RAY27330
     C    +.261450147003138789D-1)*D +.334789436657616262D-1)*D         RAY27340
     D    +.427178905473830956D-1)*D +.585936612555314917D-1)*D         RAY27350
     E    +.937499997212031407D-1)*D +.249999999999901772D0)*D)         RAY27360
     F    +(((((((((                                                    RAY27370
     G     .149466217571813268D-3*D  +.246850333046072273D-2)*D         RAY27380
     H    +.863844217360407443D-2)*D+.107706350398664555D-1)*D          RAY27390
     I    +.782040406095955417D-2)*D +.759509342255943228D-2)*D         RAY27400
     J    +.115695957452954022D-1)*D +.218318116761304816D-1)*D         RAY27410
     K    +.568051945675591566D-1)*D +.443147180560889526D0)*D          RAY27420
     L    +1D0                                                          RAY27430
C****                                                                   RAY27440
C**** ROUTINE MODIFIED TO CALCULATE VK AND VE ALWAYS                    RAY27450
C****                                                                   RAY27460
C****                                                                   RAY27470
           VK=E*((((((((((                                              RAY27480
     A     .297002809665556121D-4*D   +.921554634963249846D-3)*D        RAY27490
     B    +.597390429915542916D-2)*D  +.155309416319772039D-1)*D        RAY27500
     C    +.239319133231107901D-1)*D  +.301248490128989303D-1)*D        RAY27510
     D    +.373777397586236041D-1)*D  +.48828041906862398D-1)*D         RAY27520
     E    +.703124997390383521D-1)*D  +.124999999999908081D0)*D         RAY27530
     F    +.5D0)+(((((((((                                              RAY27540
     G     .139308785700664673D-3*D   +.229663489839695869D-2)*D        RAY27550
     H    +.800300398064998537D-2)*D  +.984892932217689377D-2)*D        RAY27560
     I    +.684790928262450512D-2)*D  +.617962744605331761D-2)*D        RAY27570
     J    +.878980187455506468D-2)*D  +.149380135326871652D-1)*D        RAY27580
     K    +.308851462713051899D-1)*D  +.965735902808562554D-1)*D        RAY27590
     L    +1.38629436111989062D0                                        RAY27600
      RETURN                                                            RAY27610
    2 VE=1D0                                                            RAY27620
      VK=XLG                                                            RAY27630
      RETURN                                                            RAY27640
      END                                                               RAY27650
      SUBROUTINE FB02AD(CAYSQ,SINP,COSP,E,F)                            RAY27660
C                                                                       RAY27670
      IMPLICITREAL*8(A-H,O-Z)                                           RAY27680
      PHI=DATAN(SINP/COSP)                                              RAY27690
      IF(CAYSQ*SINP*SINP-0.5D0)1,1,5                                    RAY27700
    1 H=1.0D0                                                           RAY27710
      A=PHI                                                             RAY27720
      N=0                                                               RAY27730
      SIG1=0.D0                                                         RAY27740
      SIG2=0.D0                                                         RAY27750
      SIN2=SINP*SINP                                                    RAY27760
      TERM=SINP*COSP*0.5D0                                              RAY27770
      CRIT=PHI                                                          RAY27780
    2 N=N+1                                                             RAY27790
      RECIP=1.0D0/N                                                     RAY27800
      FACT=(N-.5D0)*RECIP                                               RAY27810
      H1=H                                                              RAY27820
      H=FACT*CAYSQ*H                                                    RAY27830
      A=FACT*A-TERM*RECIP                                               RAY27840
      TERM=TERM*SIN2                                                    RAY27850
      CRIT=CRIT*SIN2                                                    RAY27860
      DEL1=H*A                                                          RAY27870
      DEL2=-.5D0*RECIP*CAYSQ*H1*A                                       RAY27880
      SIG1=SIG1+DEL1                                                    RAY27890
      SIG2=SIG2+DEL2                                                    RAY27900
      IF(DABS(DEL1)-4.0D-16)4,3,3                                       RAY27910
   3  IF(DABS(CRIT)-DABS(A))4,2,2                                       RAY27920
    4 F=PHI+SIG1                                                        RAY27930
      E=PHI+SIG2                                                        RAY27940
      GO TO 8                                                           RAY27950
    5 CFI=1.D0                                                          RAY27960
      CFJ=1.D0                                                          RAY27970
      CFL=0.D0                                                          RAY27980
      CFM=0.D0                                                          RAY27990
      CFN=0.D0                                                          RAY28000
      SIG1=0.D0                                                         RAY28010
      SIG2=0.D0                                                         RAY28020
      SIG3=0.D0                                                         RAY28030
      SIG4=0.D0                                                         RAY28040
      N=0                                                               RAY28050
      FACT1=1.0D0-CAYSQ*SINP*SINP                                       RAY28060
      FACTOR=.5D0*COSP*DSQRT(CAYSQ/FACT1)                               RAY28070
      FACTRO=FACTOR+FACTOR                                              RAY28080
      CAYDSQ=1.0D0-CAYSQ                                                RAY28090
    6 N=N+1                                                             RAY28100
      RECIP=1.0D0/N                                                     RAY28110
      FACTN=RECIP*(N-.5D0)                                              RAY28120
      FACTM=(N+.5D0)/(N+1.0D0)                                          RAY28130
      FACTOR=FACTOR*FACT1                                               RAY28140
      CFI1=CFI                                                          RAY28150
      CFJ1=CFJ                                                          RAY28160
      CFI=CFI*FACTN                                                     RAY28170
      CFJ=CFJ*FACTN*FACTN*CAYDSQ                                        RAY28180
      CFL=CFL+.5D0/(N*(N-.5D0))                                         RAY28190
      CFM=(CFM-FACTOR*RECIP*CFI)*FACTM*FACTM*CAYDSQ                     RAY28200
      CFN=(CFN-FACTOR*RECIP*CFI1)*FACTN*FACTM*CAYDSQ                    RAY28210
      DEL1=CFM-CFJ*CFL                                                  RAY28220
      DEL2=CFN-(FACTN*CFL-.25D0*RECIP*RECIP)*CAYDSQ     *CFJ1           RAY28230
      DEL3=CFJ                                                          RAY28240
      DEL4=FACTM*CFJ                                                    RAY28250
      SIG1=SIG1+DEL1                                                    RAY28260
      SIG2=SIG2+DEL2                                                    RAY28270
      SIG3=SIG3+DEL3                                                    RAY28280
      SIG4=SIG4+DEL4                                                    RAY28290
      IF(DABS (DEL1)-4.0D-16)7,6,6                                      RAY28300
    7 CAYMOD=DSQRT(CAYSQ)                                               RAY28310
      FLOG1=DLOG(4.0D0/(DSQRT(FACT1)+CAYMOD*COSP))                      RAY28320
      T1=(1.0D0+SIG3)*FLOG1+FACTRO*DLOG(.5D0+.5D0*CAYMOD*DABS (SINP))   RAY28330
      T2=(.5D0+SIG4)*CAYDSQ*FLOG1+1.0D0-FACTRO*(1.0D0-CAYMOD*DABS(SINP))RAY28340
      F=T1+SIG1                                                         RAY28350
      E=T2+SIG2                                                         RAY28360
    8 RETURN                                                            RAY28370
      END                                                               RAY28380
      SUBROUTINE FB03AD( GN,CACA,P )                                    RAY28390
C====== 23/03/72 LAST LIBRARY UPDATE                                    RAY28400
      IMPLICITREAL*8(A-H,O-Z)                                           RAY28410
      IF(GN)1,2,2                                                       RAY28420
    1 IF(CACA)3,3,4                                                     RAY28430
    3 P=1.5707963268/DSQRT(1.D0-GN)                                     RAY28440
      RETURN                                                            RAY28450
    4 STH=DSQRT(-GN/(CACA-GN))                                          RAY28460
      CTH=DSQRT(1.D0-STH*STH)                                           RAY28470
      CADA=1.D0-CACA                                                    RAY28480
      CALLFB01AD(CACA,     CAPK,CAPE)                                   RAY28490
      CALLFB02AD(CADA,STH,CTH,E,F)                                      RAY28500
      BR=CAPE*F-CAPK*(F-E)                                              RAY28510
      P=CAPK*CTH*CTH+STH*BR/DSQRT(1.D0-GN)                              RAY28520
      RETURN                                                            RAY28530
    2 IF(GN-CACA)10,30,20                                               RAY28540
   10 STH=DSQRT(GN/CACA)                                                RAY28550
      CTH=DSQRT(1.D0-STH*STH)                                           RAY28560
      CALLFB01AD(CACA,     CAPK,CAPE)                                   RAY28570
      CALLFB02AD(CACA,STH,CTH,E,F)                                      RAY28580
      BR=CAPK*E-CAPE*F                                                  RAY28590
      P=CAPK+BR*STH/(CTH*DSQRT(1.D0-GN))                                RAY28600
      RETURN                                                            RAY28610
   30 CALLFB01AD(CACA,     CAPK,CAPE)                                   RAY28620
      P=CAPE/(1.D0-CACA)                                                RAY28630
      RETURN                                                            RAY28640
   20 CADA=1.D0-CACA                                                    RAY28650
      PI=3.1415926536                                                   RAY28660
      STH=DSQRT((1.D0-GN)/CADA)                                         RAY28670
      CTH=DSQRT(1.D0-STH*STH)                                           RAY28680
      CALLFB01AD(CACA,     CAPK,CAPE)                                   RAY28690
      CALLFB02AD(CADA,STH,CTH,E,F)                                      RAY28700
      BR=PI/2.+CAPK*(F-E)-CAPE*F                                        RAY28710
      P=CAPK+BR*DSQRT(GN)/(CADA*STH*CTH)                                RAY28720
      RETURN                                                            RAY28730
      END                                                               RAY28740
      SUBROUTINE OPTIC( J, JFOCAL, NP, T, TP )                          RAY28750
C****                                                                   RAY28760
C****                                                                   RAY28770
C****                                                                   RAY28780
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY28790
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL        RAY28800
      COMMON  /BLCK 3/  XOR , YOR , ZOR , TH0, PH0, TL1                 RAY28810
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY28820
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA                       RAY28830
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DATA C /2.99792458D10/                                            RAY00270
C****                                                                   RAY28860
C****                                                                   RAY28870
  100 FORMAT( /  ' INTERSECTION POINT IN XZ-PLANE OF CENTRAL RAY AND THIRAY28880
     1S RAY '      )                                                    RAY28890
  101 FORMAT(  ' (IN D AXIS SYSTEM                 )       '         )  RAY28900
  102 FORMAT(  ' (IN OPTIC AXIS SYSTEM             )        '        )  RAY28910
  103 FORMAT( / ' RAY PARAMETERS AT THE FOCAL AXIS SYSTEM  '         )  RAY28920
  104 FORMAT( / ' COORDINATE TRANSFORMATION TO OPTIC AXIS SYSTEM  '  )  RAY28930
  105 FORMAT( / '  *****************************************************RAY28970
     1************************************************************'/  ) RAY28980
C****                                                                   RAY28950
C****                                                                   RAY28960
      IF( NP  .LE. 100) PRINT 105                                       RAY28990
      IF( J  .GT.  2  )  GO TO 19                                       RAY29000
      IF( J  .EQ.  1 )  GO TO 15                                        RAY29010
      IF( J  .EQ. 2)  GO TO 18                                          RAY29020
      CALL EXIT                                                         RAY29030
   15 B1X = XA                                                          RAY29040
      B1Y = YA                                                          RAY29050
      S1X = VXA/VZA                                                     RAY29060
      S1Y = VYA/VZA                                                     RAY29070
      TT = T
      TP1 = TP
      VEL1 = VEL
      VZA1 = VZA
      S1XP = DATAN2( VXA,VZA )                                          RAY29080
      COS1 =DCOS(S1XP)                                                  RAY29090
      SIN1 =DSIN(S1XP)                                                  RAY29100
      ZZZZ = 0.                                                         RAY29110
      TT1 = TT*1.0D+09
      TL1 = TP
        TH0 = 1000. * S1XP                                              RAY29111
        PH0 = 1000. * DASIN (VYA/VEL)                                   RAY29112
      GO TO 17                                                          RAY29120
   18 B2X = XA                                                          RAY29130
      B2Y = YA                                                          RAY29140
      S2X = VXA/VZA                                                     RAY29150
      S2Y = VYA/VZA                                                     RAY29160
C****                                                                   RAY29170
C**** CALCULATE CENTRAL AND PARAXIAL RAY INTERCEPTS IN SYSTEM - D       RAY29180
C****                                                                   RAY29190
      DSX = S1X-S2X                                                     RAY29200
      IF( DSX .EQ. 0. )   DSX = 1.D-30                                  RAY29210
      ZINT =  ( B2X-B1X) /  DSX                                         RAY29220
      XINT = ( B2X*S1X - B1X*S2X ) /  DSX                               RAY29230
      YINT = S2Y*ZINT + B2Y                                             RAY29240
      XOR  = XINT
      YOR  = 0.
      ZOR  = ZINT
      IF( JFOCAL .EQ. 0 ) GO TO 14
      XOR  = B1X
      ZOR  = 0.
   14 CONTINUE
      IF( NP  .GT. 100 ) GO TO 5                                        RAY29330
      PRINT 100                                                         RAY29420
      PRINT 101                                                         RAY29430
      PRINT 114, XINT, YINT, ZINT                                       RAY29440
  114 FORMAT(  14X, 6HXXINT=   F11.4,  3H CM ,  /                       RAY29450
     1         14X, 6HYYINT=   F11.4,  3H CM ,  /                       RAY29460
     2         14X, 6HZZINT=   F11.4,  3H CM ,  /          )            RAY29470
  115 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           RAY29480
     1   F13.5, F13.2, F11.2                   )                        RAY29490
C****                                                                   RAY29500
C**** ALTERATION OF INTERCEPTS TO OPTIC AXIS SYSTEM                     RAY29510
C****                                                                   RAY29520
    5 ZINTZ = ZINT*COS1 + (XINT-B1X) *SIN1                              RAY29530
      XINTX =-ZINT*SIN1 + (XINT-B1X) *COS1                              RAY29540
      ZZZZ = ZINTZ                                                      RAY29550
      IF( JFOCAL  .NE.  0 )  ZZZZ = 0.                                  RAY29560
C****
C**** FLIGHT PATH AND TIME FOR RAY-1 IN FOCAL AXIS SYSTEM
C****
      TT = TT + ZZZZ/DABS(VZA1)
      TT1 = TT*1.0D+09
      TL1 = TP1 + ZZZZ*VEL1/DABS(VZA1)
      IF( NP  .GT. 100 ) GO TO 17                                       RAY29570
      PRINT 102                                                         RAY29580
      PRINT 114, XINTX, YINT, ZINTZ                                     RAY29590
      GO TO 17                                                          RAY29600
C****
C**** GENERAL RAY INTERCEPTS IN D-AXIS SYSTEM
C****
   19 BJX = XA                                                          RAY29610
      BJY = YA                                                          RAY29620
      SJX = VXA/VZA                                                     RAY29630
      SJY = VYA/VZA                                                     RAY29640
      DSX = S1X-SJX                                                     RAY29650
      IF( DSX .EQ. 0. )   DSX = 1.D-30                                  RAY29660
      XINT1 = ( BJX*S1X - B1X*SJX ) /  DSX                              RAY29670
      ZINT1 = ( BJX - B1X ) /  DSX                                      RAY29680
      YINT1 = SJY*ZINT1 + BJY                                           RAY29690
      IF( NP  .GT. 100 ) GO TO 17                                       RAY29740
      PRINT 100                                                         RAY29790
      PRINT 101                                                         RAY29800
      PRINT 114, XINT1, YINT1, ZINT1                                    RAY29810
C****                                                                   RAY29820
C**** TRANSFORM SYSTEM-D TO OPTIC AXIS SYSTEM                           RAY29830
C**** TRANSLATE TO (B1X,0) AND ROTATE BY (S1X,0)                        RAY29840
C****                                                                   RAY29850
   17 IF( JFOCAL .EQ. 2 ) GO TO 13
      XT = XA                                                           RAY29860
      ZT = ZA                                                           RAY29870
      VXT = VXA                                                         RAY29880
      VZT = VZA                                                         RAY29890
      ZA    = ZT*COS1 + ( XT-B1X ) *SIN1                                RAY29900
      XA    =-ZT*SIN1 + ( XT-B1X ) *COS1                                RAY29910
      VZA   = VZT*COS1 + VXT*SIN1                                       RAY29920
      VXA   =-VZT*SIN1 + VXT*COS1                                       RAY29930
   13 CONTINUE
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY29940
      VYP = 1000. * DASIN( VYA/VEL )                                    RAY29950
      VZP = VEL/C                                                       RAY30090
      IF( NP  .GT. 100 ) GO TO 16                                       RAY29980
      PRINT 104                                                         RAY29990
C****                                                                   RAY30000
      PRINT 115, TP, XA,  YA,  ZA,        VZP, VXP, VYP                 RAY30010
   16 TDT = -ZA    /DABS( VZA   )                                       RAY30020
      XA = XA       + TDT * VXA                                         RAY30030
      YA = YA       + TDT * VYA                                         RAY30040
      ZA = ZA       + TDT * VZA                                         RAY30050
      T = T + TDT                                                       RAY30060
      TP = TP + TDT*VEL
      VXP = 1000. *DATAN2( VXA,VZA )                                    RAY30070
      VYP = 1000. * DASIN( VYA/VEL )                                    RAY30080
      VZP = VEL/C                                                       RAY30090
C****
C**** TRANSLATE PARTICLE TO FOCAL AXIS SYSTEM
C****
      XINT2= XA    + ZZZZ* VXA/VZA                                      RAY30110
      YINT2= YA    + ZZZZ* VYA/VZA                                      RAY30120
      ZINT2 = 0.
C****
C****
      TT = T + ZZZZ/DABS(VZA)
      TTJ = TT*1.0D+09
      TLJ = TP + ZZZZ*VEL/DABS(VZA)
C****
C**** PATH LENGTHS AND TIMES RELATIVE TO RAY-1
C****
      TTJ1 = TTJ - TT1
      TLJ1 = TLJ - TL1
C****
C****
      XO(J) = XINT2                                                     RAY30200
      YO(J) = YINT2                                                     RAY30210
      ZO(J) = ZA                                                        RAY30220
      VXO(J) = VXP                                                      RAY30230
      VYO(J) = VYP                                                      RAY30240
      VZO(J) = VZP                                                      RAY30250
C****
C**** SAVE TIME DIFFERENCES IN UNITS OF NS
C****
      RTL(J) = TTJ1
      RLL(J) = TLJ1
C****
C**** SK 12/02/83
C**** GAMMA CORRECTION FOR HIGH ENERGY ELECTRONS
C**** NOT EXACT 
C****
      GAMMA = 1.D0 + ENERGY/(PMASS*931.5016D0)
      IF( GAMMA .LT. 100. ) GAMMA = 1./DSQRT( 1.-VEL*VEL/(C*C) )        RAY38190
C****
C****
      EKE = (GAMMA-1.D0) * (PMASS*931.5016D0)
      EKL(J) = EKE
      IF( NP  .GT. 100 ) RETURN                                         RAY30270
      PRINT 115, TP, XA,  YA,  ZA,        VZP, VXP, VYP                 RAY30140
      PRINT 103                                                         RAY30150
      PRINT 116, XINT2,VXP, YINT2,VYP,ZINT2,TLJ,TLJ1,TTJ,TTJ1,VEL,EKE   RAY30160
  116 FORMAT( / 19X, 'X=', F10.4, ' CM', 5X, 'VX=',F10.4,' MR',    /    RAY30170
     1          19X, 'Y=', F10.4, ' CM', 5X, 'VY=',F10.4,' MR',    /    RAY30180
     2          19X, 'Z=', F10.4, ' CM'          /                      RAY30190
     3          19X, 'L=', F10.4, ' CM', 5X,'DL=',F10.4, ' CM' /
     4          19X, 'T=', F10.4, ' NS', 5X,'DT=',F10.4, ' NS' /
     5          18X,'VF=', 1PD14.4, ' CM/S' /
     6          18X,'KE=',0PF10.4, ' MEV'     )
      IF( JFOCAL  .NE.  0 )  PRINT 99                                   RAY30280
   99 FORMAT( / '   FOCAL POS FIXED BY INPUT DATA = IMAGE DISTANCE  '/ )RAY30290
      RETURN                                                            RAY30300
      END                                                               RAY30310
      SUBROUTINE PRNT( J,NO )                                           RAY30320
C****                                                                   RAY30330
C****                                                                   RAY30340
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY30350
      CHARACTER*4 ITITLE
      COMMON  /BLCK 0/  DATA, ITITLE                                    RAY30360
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP                 RAY30370
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL        RAY30380
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY30390
      COMMON  /BLCK 6/  NP, JFOCAL                                      RAY30400
      DIMENSION DATA(  75,200 ), ITITLE(200)                            RAY30410
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DIMENSION XI(999), YI(999), ZI(999), VXI(999), VYI(999), VZI(999),RAY30430
     1          DELP(999)                                               RAY30440
      CHARACTER*8 LX(15)                                                RAY30450
      CHARACTER*4 LCM                                                   RAY30460
      DATA C /2.99792458D10/
      INTEGER ID2(52), ID3(13), ID4(43), ID5(33),ID6(17),ID7(7),ID8(26),RAY30470
     1        ID9(21)
      DATA ID2 / 11, 19, 29, 41, 51, 12, 20, 30, 42, 52, 13, 21, 31,    RAY30480
     1   43, 53, 14, 22, 32, 44, 54, 15, 25, 33, 45, 55, 16, 26, 34,    RAY30490
     2   46, 56, 17, 27, 35, 47, 57, 18, 28, 36, 48, 58, 37, 49, 59, 38,RAY30500
     3 50,60,39, 61, 40, 62, 63, 64                                  /  RAY30510
      DATA ID3 / 10, 15, 17, 11, 16, 18, 12, 19, 13, 20, 14, 21, 22  /  RAY30520
      DATA ID4 /  7, 20, 28, 34,  8, 21, 29, 35,  9, 22, 30, 36, 10,    RAY30540
     1   23, 31, 37, 11, 24, 32, 38, 12, 25, 33, 39, 13, 26, 40, 46,    RAY30550
     2   16, 27, 41, 47, 17, 42, 48, 18, 43, 49, 19, 44, 50, 45, 51  /  RAY30560
      DATA ID5 / 10, 14, 19, 23, 29, 11, 15, 20, 24, 30, 12, 16, 21,    RAY30570
     1   25, 31, 13, 17, 22, 26, 32, 18, 27, 33, 28, 34, 35, 39, 36,    RAY30580
     2   40, 37, 41, 38, 42  /
      DATA ID6 / 10, 16, 20, 26, 11, 17, 21, 27, 12, 22, 28, 13, 23,    RAY30590
     1   14, 24, 15, 25                                              /  RAY30600
      DATA ID7 / 10, 15, 11, 16, 12, 13, 14                          /  RAY30610
      DATA ID8 / 11, 16, 25, 29, 35, 12, 17, 26, 30, 36, 13, 18, 27,
     1   31, 37, 14, 19, 28, 32, 38, 15, 20, 33, 39, 34, 40          /
      DATA ID9 / 10, 14, 15, 19, 25, 11, 16, 20, 26, 12, 17, 21, 27,
     1   13, 18, 22, 28, 23, 29, 24, 30  /
      DATA LCM / ' CM ' /                                               RAY30620
      DATA LX/ ' ENTR FL','D STEP =',' UNIF FL','D STEP =',             RAY30630
     1         ' EXIT FL','D STEP =',' DIFF/MI','D STEP =',             RAY30640
     2         '        ','   RHO =','        ','  MTYP =',             RAY30650
     3         '   FIELD','  STEP =','  ETYP ='                      /  RAY30660
C****                                                                   RAY30670
C****                                                                   RAY30680
      GO TO ( 1, 2, 3, 1, 1, 1, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),J  RAY30690
      PRINT 109, J                                                      RAY30700
  109 FORMAT(// ' GO TO FELL THROUGH IN ROUTINE PRNT  J= ' ,I5 ///     )RAY30710
      CALL EXIT                                                         RAY30720
C****                                                                   RAY30730
    1 RETURN                                                            RAY30740
C****
C**** COLLIMATOR DATA
C****
  103 FORMAT( // 20X, '*** COLLIMATOR       ***',   A4  /  )            RAY30800
  104 FORMAT(
     1   5X,'ELPS=',  F9.1, 5X,'XCEN=',  F9.4, 5X,'YCEN=',  F9.4,       RAY30820
     2   5X,'XMAX=',  F9.4, 5X,'YMAX=',  F9.4                       )   
   13 PRINT 103, ITITLE(NO)
      PRINT 104,(DATA(I,NO),I=1,5)
      RETURN
C****                                                                   RAY30770
C**** DIPOLE DATA                                                       RAY30780
C****                                                                   RAY30790
  100 FORMAT( // 20X, '*** DIPOLE MAGNET    ***',   A4  /  )            RAY30800
  101 FORMAT(                                                           RAY30810
     1   5X,'  A =',  F9.4, 5X,'NDX =',  F9.4, 5X,'C00 =',  F9.4,       RAY30820
     2   5X,'BR1 =',  F9.4, 5X,'S02 =',1PE12.3,5X,       2A8,0PF8.3,A4,/RAY30830
     3   5X,'  B =',  F9.4, 5X,'BET1=',  F9.4, 5X,'C01 =',  F9.4,       RAY30840
     4   5X,'BR2 =',  F9.4, 5X,'S03 =',1PE12.3,5X,       2A8,0PF8.3,A4,/RAY30850
     5   5X,'  D =',  F9.4, 5X,'GAMA=',  F9.4, 5X,'C02 =',  F9.4,       RAY30860
     6   5X,'XCR1=',  F9.4, 5X,'S04 =',1PE12.3,5X,       2A8,0PF8.3,A4,/RAY30870
     7   5X,'  R =',  F9.4, 5X,'DELT=',  F9.4, 5X,'C03 =',  F9.4,       RAY30880
     8   5X,'XCR2=',  F9.4, 5X,'S05 =',1PE12.3,5X,       2A8,0PF8.3,A4,/RAY30890
     9   5X,' BF =',  F9.4, 5X,'Z11 =',  F9.4, 5X,'C04 =',  F9.4,       RAY30900
     A   5X,'DLS1=',  F9.4, 5X,'S06 =',1PE12.3,5X,       2A8,  I4     ,/RAY30910
     B   5X,'PHI =',0PF9.4, 5X,'Z12 =',  F9.4, 5X,'C05 =',  F9.4,       RAY30920
     C   5X,'DLS2=',  F9.4, 5X,'S07 =',1PE12.3,5X,       2A8,0PF8.3,A4 )RAY30930
  102 FORMAT(                                                           RAY30940
     1   5X,'ALPH=',  F9.4,   5X,'Z21 =',  F9.4,  5X,'C10 =',  F9.4,    RAY30950
     2   5X,'RAP1=',  F9.4,   5X,'S08 ='1PE12.3/, 5X,'BETA=',0PF9.4,    RAY30960
     3   5X,'Z22 =',  F9.4,   5X,'C11 =',  F9.4,  5X,'RAP2=',  F9.4,    RAY30970
     4   5X,'S12 =',1PE12.3/ 43X,'C12 =' 0PF9.4,                        RAY30980
     X                        5X,'WDE =',  F9.4,  5X,'S13 =', 1PE12.3/, RAY30990
     5  43X,'C13 =',0PF9.4,   5X,'WDX =',  F9.4,                        RAY31000
     Y                        5X,'S14 ='1PE12.3,/43X,'C14 =',0PF9.4,    RAY31010
     6  24X,'S15 =',1PE12.3/ 43X,'C15 =' 0PF9.4, 24X,'S16 =', 1PE12.3/, RAY31020
     7  81X,'S17 =',1PE12.3/ 81X,'S18 =' 1PE12.3                     )  RAY31030
C****                                                                   RAY31040
C****                                                                   RAY31050
    2 RHO = 1.D30                                                       RAY31060
      IF( DATA(15,NO) .NE. 0 )   RHO = DSQRT(                           RAY31070
     1 (2.*931.5016*PMASS+ENERGY)*ENERGY)/( (C/1.D10)*DATA(15,NO)*Q0)   RAY31080
      MTYP = DATA(5,NO)                                                 RAY31090
      PRINT 100, ITITLE(NO)                                             RAY31100
      PRINT 101,(DATA(ID2(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY31110
     1          (DATA(ID2(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, RAY31120
     2          (DATA(ID2(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, RAY31130
     3          (DATA(ID2(I),NO),I=16,20),LX( 7),LX( 8),DATA(4,NO),LCM, RAY31140
     4          (DATA(ID2(I),NO),I=21,25),LX(11),LX(12),MTYP      ,     RAY31150
     5          (DATA(ID2(I),NO),I=26,30),LX( 9),LX(10),RHO,       LCM  RAY31160
      PRINT 102,(DATA(ID2(I),NO),I=31,52)                               RAY31170
      RETURN                                                            RAY31180
C****                                                                   RAY31190
C**** EINZEL LENS
C****                                                                   RAY31210
  200 FORMAT( // 20X, '*** EINZEL LENS      ***',   A4  /  )            RAY31220
  120 FORMAT(                                                           RAY31260
     1   5X,'  A =',  F9.4, 5X,' Z1 =',  F9.4, 5X,'C00 =',  F9.4,       RAY31270
     2  19X,                5X, 2A8,0PF8.3,A4,/                         RAY31280
     3   5X,'  B =',  F9.4, 5X,' Z2 =',  F9.4, 5X,'C01 =',  F9.4,       RAY31290
     4  19X,                5X, 2A8,0PF8.3,A4,/                         RAY31300
     5   5X,'  L =',  F9.4,19X,                5X,'C02 =',  F9.4, /     RAY31310
     7   5X,'DIA =',  F9.4,19X,                5X,'C03 =',  F9.4, /     RAY31330
     8   5X,'  V =',  F9.2,                   24X,'C04 =',  F9.4,       RAY31340
     9                     /                  43X,'C05 =',  F9.4 )      RAY31350
C****                                                                   RAY31370
C****                                                                   RAY31380
    3 PRINT 200, ITITLE(NO)                                             RAY31390
      GO TO 21                                                          RAY31400
   21 PRINT 120,(DATA(ID3(I),NO),I= 1,3 ),LX(13),LX(14),DATA(1,NO),LCM, RAY31460
     1          (DATA(ID3(I),NO),I= 4,6 ),LX(11),LX(15),DATA(2,NO),LCM, RAY31470
     2          (DATA(ID3(I),NO),I= 7,13)                               RAY31480
      RETURN                                                            RAY31500
C****                                                                   RAY30770
C**** ELECTROSTATIC DEFLECTOR DATA                                      RAY30780
C****                                                                   RAY30790
  190 FORMAT( // 20X, '*** ELECTROSTATIC DEF.***',   A4  /  )           RAY30800
  191 FORMAT(                                                           RAY30810
     1   5X,'  A =',  F9.4, 5X,'PHI =',  F9.4, 5X,'Z11 =',  F9.4,       RAY30820
     2   5X,'C00 =',  F9.4, 5X,'C10 =',  F9.4, 5X,       2A8,0PF8.3,A4,/RAY30830
     3   5X,'  B =',  F9.4, 5X,'EC2 =',  F9.4, 5X,'Z12 =',  F9.4,       RAY30840
     4   5X,'C01 =',  F9.4, 5X,'C11 =',  F9.4, 5X,       2A8,0PF8.3,A4,/RAY30850
     5   5X,'  D =',  F9.4, 5X,'EC4 =',  F9.4, 5X,'Z21 =',  F9.4,       RAY30860
     6   5X,'C02 =',  F9.4, 5X,'C12 =',  F9.4, 5X,       2A8,0PF8.3,A4,/RAY30870
     7   5X,'  R =',  F9.4, 5X,'WE  =',  F9.4, 5X,'Z22 =',  F9.4,       RAY30880
     8   5X,'C03 =',  F9.4, 5X,'C13 =',  F9.4, 5X,       2A8,0PF8.3,A4,/RAY30890
     9   5X,' EF =',  F9.4, 5X,'WC  =',  F9.4,24X,'C04 =',  F9.4,       RAY30900
     A   5X,'C14 =',  F9.4,                    5X,       2A8,0PF8.3,A4,/RAY30910
     B  62X,'C05 =',0PF9.4, 5X,'C15 =',  F9.4                   )       RAY30920
C****                                                                   RAY31040
C****                                                                   RAY31050
    7 RHO = 1.D30                                                       RAY31060
        EMASS = PMASS * 931.5016
        ETOT = EMASS + ENERGY
        VC2 = (2.*EMASS + ENERGY)*ENERGY / (ETOT*ETOT)
        GAMMA = 1. / DSQRT(1. - VC2)
      IF( DATA(15,NO) .NE. 0 )                                          RAY31070
     1RHO = GAMMA * EMASS * VC2 * 1000. / (DATA(15,NO) * Q0)            RAY31080
      PRINT 190, ITITLE(NO)                                             RAY31100
      PRINT 191,(DATA(ID8(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY31110
     1          (DATA(ID8(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, RAY31120
     2          (DATA(ID8(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, RAY31130
     3          (DATA(ID8(I),NO),I=16,20),LX( 7),LX( 8),DATA(4,NO),LCM, RAY31140
     4          (DATA(ID8(I),NO),I=21,24),LX( 9),LX(10),RHO,LCM   ,     RAY31150
     5          (DATA(ID8(I),NO),I=25,26)                               RAY31160
      RETURN                                                            RAY31180
C****                                                                   RAY31510
C****    VELOCITY SELECTOR DATA                                         RAY31520
C****                                                                   RAY31530
  132 FORMAT( // 20X, '*** VELOCITY SELECTOR***',   A4  /  )            RAY31540
  130 FORMAT(                                                           RAY31550
     1   5X,'  A =',  F9.4, 5X,'Z11 =',  F9.4, 5X,'CB00=',  F9.4,       RAY31560
     2   5X,'CE00=',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY31570
     3   5X,'  B =',  F9.4, 5X,'Z12 =',  F9.4, 5X,'CB01=',  F9.4,       RAY31580
     4   5X,'CE01=',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY31590
     5   5X,'  L =',  F9.4, 5X,'Z21 =',  F9.4, 5X,'CB02=',  F9.4,       RAY31600
     6   5X,'CE02=',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY31610
     7   5X,' BF =',  F9.4, 5X,'Z22 =',  F9.4, 5X,'CB03=',  F9.4,       RAY31620
     8   5X,'CE03=',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY31630
     9   5X,' BE =',  F9.4, 5X,'CB2 =',  F9.4, 5X,'CB04=',  F9.4,       RAY31640
     A   5X,'CE04=',  F9.4, 5X, 2A8,0PF8.3,A4                        )  RAY31650
  131 FORMAT(                                                           RAY31660
     1   5X,' RB =',  F9.4, 5X,'CB4 =',  F9.4, 5X,'CB05=',  F9.4,       RAY31670
     2   5X,'CE05=',  F9.4,/5X,'NDX =',  F9.4, 5X,'CE2 =',  F9.4,       RAY31680
     3   5X,'CB10=',  F9.4, 5X,'CE10=',  F9.4,/5X,' DB =',  F9.4,       RAY31690
     4   5X,'CE4 =',  F9.4, 5X,'CB11=',  F9.4, 5X,'CE11=',  F9.4,/      RAY31700
     5   5X,' DE =',  F9.4,24X,'CB12=',  F9.4, 5X,'CE12=',  F9.4,/      RAY31710
     6   5X,' WB =',  F9.4,24X,'CB13=',  F9.4, 5X,'CE13=',  F9.4,/      RAY31720
     7   5X,' WE =',  F9.4,24X,'CB14=',  F9.4, 5X,'CE14=',  F9.4,/      RAY31730
     8  43X,'CB15=',  F9.4, 5X,'CE15=',  F9.4                        )  RAY31740
C****                                                                   RAY31750
C****                                                                   RAY31760
    8 RHO = 1.D30                                                       RAY31770
      IF( DATA(10,NO)  .NE.  0.   )   RHO = DSQRT(                      RAY31780
     1 (2.*931.5016*PMASS+ENERGY)*ENERGY)/( (C/1.D10)*DATA(10,NO)*Q0)   RAY31790
      PRINT 132,ITITLE(NO)                                              RAY31800
      PRINT 130,(DATA(ID4(I),NO),I= 1,4 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY31810
     1          (DATA(ID4(I),NO),I= 5,8 ),LX( 3),LX( 4),DATA(2,NO),LCM, RAY31820
     2          (DATA(ID4(I),NO),I= 9,12),LX( 5),LX( 6),DATA(3,NO),LCM, RAY31830
     3          (DATA(ID4(I),NO),I=13,16),LX( 7),LX( 8),DATA(4,NO),LCM, RAY31840
     4          (DATA(ID4(I),NO),I=17,20),LX( 9),LX(10),RHO,LCM         RAY31850
      PRINT 131,(DATA(ID4(I),NO),I=21,43)                               RAY31860
      RETURN                                                            RAY31870
C****                                                                   RAY31880
C**** MULTIPOLE (POLES)      DATA                                       RAY31890
C****                                                                   RAY31900
  141 FORMAT( // 20X, '*** MULTIPOLES       ***',   A4  /  )            RAY31910
  140 FORMAT(                                                           RAY31920
     1   5X,'  A =',  F9.4, 3X,'BQUAD =',F10.5,4X,'Z11 =',  F9.4,       RAY31930
     2   5X,'C00 =',  F9.4, 5X,'C10 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31940
     3   5X,'  B =',  F9.4, 3X,'BHEX  =',F10.5,4X,'Z12 =',  F9.4,       RAY31950
     4   5X,'C01 =',  F9.4, 5X,'C11 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31960
     5   5X,'  L =',  F9.4, 3X,'BOCT  =',F10.5,4X,'Z21 =',  F9.4,       RAY31970
     6   5X,'C02 =',  F9.4, 5X,'C12 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31980
     7   5X,'RAD =',  F9.4, 3X,'BDEC  =',F10.5,4X,'Z22 =',  F9.4,       RAY31990
     8   5X,'C03 =',  F9.4, 5X,'C13 =',  F9.4,/                         RAY32000
     9                     22X,'BDDEC =',F10.5,23X,'C04 =', F9.4,       RAY32010
     A   5X,'C14 =',  F9.4/62X,'C05 =',  F9.4, 5X,'C15 =',  F9.4        RAY32020
     B                    /62X,'FRH =',  F9.4, 5X,'DSH =',  F9.4                
     C                    /62X,'FRO =',  F9.4, 5X,'DSO =',  F9.4                
     D                    /62X,'FRD =',  F9.4, 5X,'DSD =',  F9.4                
     E                    /62X,'FRDD=',  F9.4, 5X,'DSDD=',  F9.4     )          
C****                                                                   RAY32030
C****                                                                   RAY32040
    9 PRINT 141, ITITLE(NO)                                             RAY32050
      PRINT 140,(DATA(ID5(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY32060
     1          (DATA(ID5(I),NO),I= 6,10),LX( 3),LX( 4),DATA(2,NO),LCM, RAY32070
     2          (DATA(ID5(I),NO),I=11,15),LX( 5),LX( 6),DATA(3,NO),LCM, RAY32080
     3          (DATA(ID5(I),NO),I=16,33)                               RAY32090
      RETURN                                                            RAY32100
C****                                                                   RAY31880
C**** ACCELERATOR  DATA                                                 RAY31890
C****                                                                   RAY31900
  185 FORMAT( // 20X, '*** ACCELERATOR      ***',   A4  /  )            RAY31910
  186 FORMAT(                                                           RAY31920
     1   5X,'  A =',  F9.4, 3X,'EFLD  =',F10.5,4X,'Z11 =',  F9.4,       RAY31930
     2   5X,'C00 =',  F9.4, 5X,'C10 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31940
     3   5X,'  B =',  F9.4,20X,                4X,'Z12 =',  F9.4,       RAY31950
     4   5X,'C01 =',  F9.4, 5X,'C11 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31960
     5   5X,'  L =',  F9.4,20X,                4X,'Z21 =',  F9.4,       RAY31970
     6   5X,'C02 =',  F9.4, 5X,'C12 =',  F9.4, 8X, 2A8,0PF8.3,A4,/      RAY31980
     7   5X,'RAD =',  F9.4,20X,                4X,'Z22 =',  F9.4,       RAY31990
     8   5X,'C03 =',  F9.4, 5X,'C13 =',  F9.4,/                         RAY32000
     9                     39X,                23X,'C04 =', F9.4,       RAY32010
     A   5X,'C14 =',  F9.4/62X,'C05 =',  F9.4, 5X,'C15 =',  F9.4  )     RAY32020
C****                                                                   RAY32030
C****                                                                   RAY32040
   16 PRINT 185, ITITLE(NO)                                             RAY32050
      PRINT 186,(DATA(ID9(I),NO),I= 1,5 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY32060
     1          (DATA(ID9(I),NO),I= 6,9 ),LX( 3),LX( 4),DATA(2,NO),LCM, RAY32070
     2          (DATA(ID9(I),NO),I=10,13),LX( 5),LX( 6),DATA(3,NO),LCM, RAY32080
     3          (DATA(ID9(I),NO),I=14,21)                               RAY32090
      RETURN                                                            RAY32100
C****                                                                   RAY32110
C**** MULTIPOLE DATA                                                    RAY32120
C****                                                                   RAY32130
  151 FORMAT( // 20X,  '***MULTIPOLE(HE)    ***',   A4  /  )            RAY32140
  150 FORMAT(                                                           RAY32150
     1   5X,'  A =',  F9.4, 5X,' Z1 =',  F9.4, 5X,' C0 =',  F9.4,       RAY32160
     2   5X,' C6 =',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY32170
     3   5X,'  B =',  F9.4, 5X,' Z2 =',  F9.4, 5X,' C1 =',  F9.4,       RAY32180
     4   5X,' C7 =',  F9.4, 5X, 2A8,0PF8.3,A4,/                         RAY32190
     5   5X,'  L =',  F9.4,24X,' C2 =',  F9.4, 5X,' C8 =',  F9.4/       RAY32200
     6   5X,'  W =',  F9.4,24X,' C3 =',  F9.4,/                         RAY32210
     7   5X,'  D =',  F9.4,24X,' C4 =',  F9.4,/                         RAY32220
     8   5X,' BF =',  F9.4,24X,' C5 =',  F9.4                        )  RAY32230
C****                                                                   RAY32240
C****                                                                   RAY32250
   10 PRINT 151, ITITLE(NO)                                             RAY32260
      PRINT 150,(DATA(ID6(I),NO),I= 1,4 ),LX( 1),LX( 2),DATA(1,NO),LCM, RAY32270
     1          (DATA(ID6(I),NO),I= 5,8 ),LX( 7),LX( 8),DATA(2,NO),LCM, RAY32280
     2          (DATA(ID6(I),NO),I= 9,17)                               RAY32290
      RETURN                                                            RAY32300
C****                                                                   RAY32310
C**** TRANSLATE - ROTATE DATA                                           RAY32320
C****                                                                   RAY32330
  170 FORMAT( // 20X, '*** TRANSLATE-ROTATE ***',   A4  /  )            RAY32340
  171 FORMAT(   5X, 5H X0 = ,F9.4,        5X,5H Y0 = ,F9.4,             RAY32350
     1          5X, 5H Z0 = ,F9.4,     / 1X,9HTHETA X = ,F9.4,          RAY32360
     2        1X,9HTHETA Y =,F9.4,       1X,9HTHETA Z = ,F9.4        )  RAY32370
C****                                                                   RAY32380
C****                                                                   RAY32390
   11 PRINT 170, ITITLE(NO)                                             RAY32400
      PRINT 171, ( DATA(I,NO) , I=1,6 )                                 RAY32410
      RETURN                                                            RAY32420
C****                                                                   RAY32430
C**** DRIFT SECTION DATA                                                RAY32440
C****                                                                   RAY32450
   12 PRINT 175, ITITLE(NO)                                             RAY32460
      PRINT 176, ( DATA(I,NO) , I=1,1 )                                 RAY32470
  175 FORMAT( // 20X, '*** DRIFT            ***',   A4  /  )            RAY32480
  176 FORMAT(  19X,  ' Z-DRIFT ='  , F9.4,  ' CM'        )              RAY32490
      RETURN                                                            RAY32500
C****                                                                   RAY32510
C**** SOLENOID DATA                                                     RAY32520
C****                                                                   RAY32530
  161 FORMAT( // 20X, '*** SOLENOID         ***',   A4  /  )            RAY32540
  160 FORMAT(                                                           RAY32550
     1   5X,'  A =',  F9.4, 5X,'Z11 =',  F9.4, 5X,2A8,0PF8.3,A4,/       RAY32560
     2   5X,'  B =',  F9.4, 5X,'Z22 =',  F9.4,/5X,'  L =',  F9.4,/      RAY32570
     3   5X,'DIA =',  F9.4,/5X,' BF =',  F9.4                        )  RAY32580
C****                                                                   RAY32590
C****                                                                   RAY32600
   14 PRINT 161, ITITLE(NO)                                             RAY32610
      PRINT 160,(DATA(ID7(I),NO),I= 1,2 ),LX(13),LX(14),DATA(1,NO),LCM, RAY32620
     1          (DATA(ID7(I),NO),I= 3, 7)                               RAY32630
      RETURN                                                            RAY32640
C****                                                                   RAY32650
C**** LENS DATA                                                         RAY32660
C****                                                                   RAY32670
  180 FORMAT( // 20X, '*** LENS             ***',   A4  /  )            RAY32680
  181 FORMAT(  3X, 7H(X/X) =  ,F9.4,   6H CM/CM                         RAY32690
     1        16X, 7H(X/T) =  ,F9.4,   6H CM/MR  /                      RAY32700
     2         3X, 7H(T/X) =  ,F9.4,   6H MR/CM                         RAY32710
     3        16X, 7H(T/T) =  ,F9.4,   6H MR/MR  /                      RAY32720
     4         3X, 7H(Y/Y) =  ,F9.4,   6H CM/CM                         RAY32730
     5        16X, 7H(Y/P) =  ,F9.4,   6H CM/MR  /                      RAY32740
     6         3X, 7H(P/Y) =  ,F9.4,   6H MR/CM                         RAY32750
     7        16X, 7H(P/P) =  ,F9.4,   6H MR/MR  //                     RAY32760
     8         3X, 7H   CS =  ,F9.4,   6H CM     
     9        16X, 7H   E0 =  ,F9.4,   6H MEV    /
     A         3X, 7H    N =  ,F9.4,   6H        /         )
C****                                                                   RAY32770
C****                                                                   RAY32780
   15 PRINT 180, ITITLE(NO)                                             RAY32790
      PRINT 181, ( DATA(I,NO) , I=1,11 )                                RAY3280
      RETURN                                                            RAY32810
C****                                                                   RAY32820
C****                                                                   RAY32830
      ENTRY PRNT1 ( N )                                                 RAY32840
C****                                                                   RAY32850
C****                                                                   RAY32860
      IF( JFOCAL .EQ. 0 ) PRINT 105
      IF( JFOCAL .EQ. 1 ) PRINT 106
      IF( JFOCAL .EQ. 2 ) PRINT 107
      IF( JFOCAL .GT. 2 ) PRINT 108
      PRINT 110                                                         RAY32870
  105 FORMAT( 1H1, 15X, '****COORDINATES OPTIC AXIS SYSTEM****    
     1  ( Origin at Ray 1-2 Intersection ) '    // )
  106 FORMAT( 1H1, 15X, '****COORDINATES OPTIC AXIS SYSTEM****    
     1  ( Origin at ZD=0.0  ) '    // )
  107 FORMAT( 1H1, 15X, '****COORDINATES D-AXIS SYSTEM****' // )    
  108 FORMAT( 1H1, 15X, '****COORDINATES OPTIC AXIS SYSTEM****' // )    RAY32880
  110 FORMAT(
     1   10X, 'X     THETA     Y      PHI      ZI     DELE'    ,8X,     RAY32890
     2   12HXO        XS  , 10X, 12HYO        YS , 7X, 'L(CM)', 5X,     RAY32900
     3   'T(NS)', 5X, 'KE(MEV)'          /)                             RAY32910
      DO 20  I=1,N                                                      RAY32920
C****
C**** CALCULATE TIME IN (NS)
C****
      TLJ1 = RTL(I)
      PRINT 111,   I, XI(I), VXI(I), YI(I), VYI(I), ZI(I), DELP(I),     RAY32970
     1    XO(I), VXO(I), YO(I), VYO(I), RLL(I), TLJ1, EKL(I)            RAY32980
  111 FORMAT(I5, 4F8.3, 2F8.2, 2X, 2F10.4, 2X, 2F10.4,                  RAY32990
     1       2F10.3, F12.3        /)                                    RAY33000
   20 CONTINUE                                                          RAY33010
      RETURN                                                            RAY33020
C****                                                                   RAY33030
C****                                                                   RAY33040
      ENTRY PRNT2 (TP, S, X, Y, Z, BX, BY, BZ, BT, VX, VY, VZ          )RAY33050
C****                                                                   RAY33060
      IF( NP  .GT. 100 ) RETURN                                         RAY33070
      VXP = 1000. *DATAN2( VX ,VZ  )                                    RAY33080
      VYP = 1000. * DASIN( VY /VEL )                                    RAY33090
      VZP = VEL/C                                                       RAY33100
      PRINT 112,TP,S,X, BX, Y, BY, Z, BZ, VZP, VXP, VYP, BT             RAY33120
  112 FORMAT(2F10.4,     F10.3, F11.4, F10.3, F11.4, F10.3, F11.4,      RAY33130
     1        F13.5, F13.2, F11.2, F10.4         )                      RAY33140
      RETURN                                                            RAY33150
C****                                                                   RAY33160
      ENTRY PRNT3 (TP   ,X,Y,Z,BX,BY,BZ,EX,EY,EZ,VX,VY,VZ)              RAY33170
C****                                                                   RAY33180
  114 FORMAT( 2F9.3, 2F10.4,F9.3, 2F10.4,F9.3, 2F10.4,2F11.3, -9PF9.5 ) RAY33190
C****                                                                   RAY33200
C****                                                                   RAY33210
      IF( NP  .GT. 100 ) RETURN                                         RAY33220
      VXP = 1000. *DATAN2( VX ,VZ  )                                    RAY33230
      VYP = 1000. * DASIN( VY /VEL )                                    RAY33240
      VZP = VEL/C                                                       RAY33250
      PRINT 114, TP   ,X,BX,EX,Y,BY,EY,Z,BZ,EZ,VXP,VYP,VEL              RAY33270
      RETURN                                                            RAY33280
C****                                                                   RAY33281
C****                                                                   RAY33282
C****                                                                   RAY33283
        ENTRY  PRNT4(NO, IN)                                            RAY33284
C****                                                                   RAY33285
115     FORMAT (///, 10X, 'MAXIMUM STEPS EXCEEDED', /10X,               RAY33286
     1   'ELEMENT = ', I4, /10X, 'REGION = ', I4 ///)                   RAY33287
        PRINT 115, NO, IN                                               RAY33288
        RETURN                                                          RAY33290
C****                                                                   RAY33030
C****                                                                   RAY33040
      ENTRY PRNT5 (TP, S, X, Y, Z, EX, EY, EZ, ET, VX, VY, VZ          )RAY33050
C****                                                                   RAY33060
      IF( NP  .GT. 100 ) RETURN                                         RAY33070
      VXP = 1000. *DATAN2( VX ,VZ  )                                    RAY33080
      VYP = 1000. * DASIN( VY /VEL )                                    RAY33090
      VZP = VEL/C                                                       RAY33100
      PRINT 112,TP,S,X, EX, Y, EY, Z, EZ, VZP, VXP, VYP, ET             RAY33120
      RETURN                                                            RAY33150
      ENTRY PRNT6 (TP, X, Y, Z, VX, VY, VZ          )
C****                                                                   RAY33060
      IF( NP  .GT. 100 ) RETURN                                         RAY33070
      VXP = 1000. *DATAN2( VX ,VZ  )                                    RAY33080
      VYP = 1000. * DASIN( VY /VEL )                                    RAY33090
      VZP = VEL/C                                                       RAY33100
      PRINT 116,TP, X, Y, Z, VZP, VXP, VYP
  116 FORMAT( F10.4, 10X, F10.3, 11X, F10.3, 11X, F10.3, 11X,           RAY06140
     1   F13.5, F13.2, F11.2                   )                        RAY06150
C****                                                                   RAY06160
C**** CALCULATE INTERCEPTS IN SYSTEM D                                  RAY06170
C****                                                                   RAY06180
      Z0X = -X/ ( VX / VZ + 1.E-10 )
      Z0Y = -Y/ ( VY / VZ + 1.E-10 )
      PRINT 117, VXF, VYF, Z0X, Z0Y 
  117 FORMAT( / ' INTERSECTIONS WITH VER. AND HOR. PLANES '          ,  RAY06220
     X       /15X, 5H  XP=F10.4, 10H MR    YP= F10.4, 3H MR   /         RAY06230
     1        15X, 5H Z0X=F10.2, 10H CM   Z0Y= F10.2, 3H CM   /        )RAY06240
      RETURN                                                            RAY33150
      END                                                               RAY33291
      SUBROUTINE MATRIX( R, T2  )                                       RAY33300
C****                                                                   RAY33310
C****                                                                   RAY33320
C****                                                                   RAY33330
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY33340
      COMMON  /BLCK 1/  XI, YI, ZI, VXI, VYI, VZI, DELP                 RAY33350
      COMMON  /BLCK 2/  XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL        RAY33360
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DIMENSION XI(999), YI(999), ZI(999), VXI(999), VYI(999), VZI(999),RAY33370
     1          DELP(999)                                               RAY33380
      DIMENSION  R(6,6) , T2(5,6,6), TT(5,6,6)                          RAY33400
      DO 21  I1= 1,6                                                    RAY33410
      DO 21  I2= 1,6                                                    RAY33420
      R(I1,I2) = 0.                                                     RAY33430
      DO 21 I3= 1,5                                                     RAY33440
   21 T2(I3,I1,I2) = 0.                                                 RAY33450
C****                                                                   RAY33460
C****                                                                   RAY33470
C**** CALCULATE COEFFICIENTS                                            RAY33480
C****                                                                   RAY33490
      R(1,1) =  ( XO(3) -  XO(4) ) / ( XI(3) -  XI(4) )                 RAY33500
      R(1,2) =  ( XO(5) -  XO(6) ) / (VXI(5) - VXI(6) )                 RAY33510
      R(1,3) =  ( XO(7) -  XO(8) ) / ( YI(7) -  YI(8) )                 RAY33520
      R(1,4) =  ( XO(9) -  XO(10)) / (VYI(9) - VYI(10))                 RAY33530
      R(1,6) =  ( XO(11)-  XO(12) )/ (DELP(11) - DELP(12) )             RAY33540
      R(2,1) =  (VXO(3) - VXO(4) ) / ( XI(3) -  XI(4) )                 RAY33550
      R(2,2) =  (VXO(5) - VXO(6) ) / (VXI(5) - VXI(6) )                 RAY33560
      R(2,3) =  (VXO(7) - VXO(8) ) / ( YI(7) -  YI(8) )                 RAY33570
      R(2,4) =  (VXO(9) - VXO(10)) / (VYI(9) - VYI(10))                 RAY33580
      R(2,6) =  (VXO(11)- VXO(12) )/ (DELP(11) - DELP(12) )             RAY33590
      R(3,1) =  ( YO(3) -  YO(4) ) / ( XI(3) -  XI(4) )                 RAY33600
      R(3,2) =  ( YO(5) -  YO(6) ) / (VXI(5) - VXI(6) )                 RAY33610
      R(3,3) =  ( YO(7) -  YO(8) ) / ( YI(7) -  YI(8) )                 RAY33620
      R(3,4) =  ( YO(9) -  YO(10)) / (VYI(9) - VYI(10))                 RAY33630
      R(3,6) =  ( YO(11)-  YO(12) )/ (DELP(11) - DELP(12) )             RAY33640
      R(4,1) =  (VYO(3) - VYO(4) ) / ( XI(3) -  XI(4) )                 RAY33650
      R(4,2) =  (VYO(5) - VYO(6) ) / (VXI(5) - VXI(6) )                 RAY33660
      R(4,3) =  (VYO(7) - VYO(8) ) / ( YI(7) -  YI(8) )                 RAY33670
      R(4,4) =  (VYO(9) - VYO(10)) / (VYI(9) - VYI(10))                 RAY33680
      R(4,6) =  (VYO(11)- VYO(12) )/ (DELP(11) - DELP(12) )             RAY33690
      R( 5,5 )  =  1.                                                   RAY33700
      R( 6,6 )  =  1.                                                   RAY33710
      R(5,1) =  (RLL(3) - RLL(4) ) / ( XI(3) -  XI(4) )                 RAY33720
      R(5,2) =  (RLL(5) - RLL(6) ) / (VXI(5) - VXI(6) )                 RAY33730
      R(5,6) =  (RLL(11)- RLL(12) )/ (DELP(11) - DELP(12) )             RAY33740
C****                                                                   RAY33750
C****                                                                   RAY33760
      T2(1,1,1)= ( XO(3) + XO(4) ) /(2.*XI(3)**2 )                      RAY33770
      T2(1,2,2)= ( XO(5) + XO(6) ) /(2.*VXI(5)**2)                      RAY33780
      T2(1,3,3)= ( XO(7) + XO(8) ) /(2.*YI(7)**2 )                      RAY33790
      T2(1,4,4)= ( XO(9) + XO(10) ) /(2.*VYI(9)**2 )                    RAY33800
      T2(1,6,6)= ( XO(11) + XO(12) ) /(2.*DELP(11)**2 )                 RAY33810
      T2(1,1,2)= ( XO(13)+XO(14)-2.*T2(1,1,1)*XI(13)**2-2.*T2(1,2,2)*   RAY33820
     1   VXI(13)**2 ) /(2.*XI(13)*VXI(13) )                             RAY33830
      T2(1,1,6)= ( XO(15) + XO(16) -2.*T2(1,1,1)*XI(15)**2 -            RAY33840
     1  2.*T2(1,6,6)*DELP(15)**2 ) /(2.*XI(15)*DELP(15) )               RAY33850
      T2(1,2,6)= ( XO(17) + XO(18) -2.*T2(1,2,2)*VXI(17)**2 -           RAY33860
     1  2.*T2(1,6,6)*DELP(17)**2 ) /(2.*VXI(17)*DELP(17) )              RAY33870
      T2(1,3,4)= ( XO(19)- XO(20) ) /(2.*YI(19)*VYI(19) )               RAY33880
      T2(2,1,1)= (VXO(3) +VXO(4) ) /(2.*XI(3)**2 )                      RAY33890
      T2(2,2,2)= (VXO(5) +VXO(6) ) /(2.*VXI(5)**2)                      RAY33900
      T2(2,3,3)= (VXO(7) +VXO(8) ) /(2.*YI(7)**2 )                      RAY33910
      T2(2,4,4)= (VXO(9) +VXO(10) ) /(2.*VYI(9)**2 )                    RAY33920
      T2(2,6,6)= (VXO(11) +VXO(12) ) /(2.*DELP(11)**2 )                 RAY33930
      T2(2,1,2)=(VXO(13)+VXO(14)-2.*T2(2,1,1)*XI(13)**2-2.*T2(2,2,2)*   RAY33940
     1   VXI(13)**2 ) /(2.*XI(13)*VXI(13) )                             RAY33950
      T2(2,1,6)= (VXO(15) +VXO(16) -2.*T2(2,1,1)*XI(15)**2 -            RAY33960
     1  2.*T2(2,6,6)*DELP(15)**2 ) /(2.*XI(15)*DELP(15) )               RAY33970
      T2(2,2,6)= (VXO(17) +VXO(18) -2.*T2(2,2,2)*VXI(17)**2 -           RAY33980
     1  2.*T2(2,6,6)*DELP(17)**2 ) /(2.*VXI(17)*DELP(17) )              RAY33990
      T2(2,3,4)= (VXO(19)-VXO(20) ) /(2.*YI(19)*VYI(19) )               RAY34000
      T2(3,1,3)= ( YO(21) - YO(22) ) /(2.*XI(21)*YI(21) )               RAY34010
      T2(3,1,4)= ( YO(23) - YO(24) ) /(2.*XI(23)*VYI(23) )              RAY34020
      T2(3,2,3)= ( YO(25) - YO(26) ) /(2. *VXI(25)*YI(25) )             RAY34030
      T2(3,2,4)= ( YO(27) - YO(28) ) /(2.*VXI(27)*VYI(27) )             RAY34040
      T2(3,3,6)= ( YO(29) - YO(30) ) /(2.*YI(29)*DELP(29) )             RAY34050
      T2(3,4,6)= ( YO(31) - YO(32) ) /(2.*VYI(31)*DELP(31)  )           RAY34060
      T2(4,1,3)= (VYO(21) -VYO(22) ) /(2.*XI(21)*YI(21) )               RAY34070
      T2(4,1,4)= (VYO(23) -VYO(24) ) /(2.*XI(23)*VYI(23) )              RAY34080
      T2(4,2,3)= (VYO(25) -VYO(26) ) /(2. *VXI(25)*YI(25) )             RAY34090
      T2(4,2,4)= (VYO(27) -VYO(28) ) /(2.*VXI(27)*VYI(27) )             RAY34100
      T2(4,3,6)= (VYO(29) -VYO(30) ) /(2.*YI(29)*DELP(29) )             RAY34110
      T2(4,4,6)= (VYO(31) -VYO(32) ) /(2.*VYI(31)*DELP(31)  )           RAY34120
C****                                                                   RAY34130
C**** PATH LENGTH TERMS                                                 RAY34140
C****                                                                   RAY34150
      T2(5,1,1) = ( RLL(3) + RLL(4) - 2*RLL(1) ) /( 2* XI(3)**2 )       RAY34160
      T2(5,2,2) = ( RLL(5) + RLL(6) - 2*RLL(1) ) /( 2*VXI(5)**2 )       RAY34170
      T2(5,3,3) = ( RLL(7) + RLL(8) - 2*RLL(1) ) /( 2* YI(7)**2 )       RAY34180
      T2(5,4,4) = ( RLL(9) + RLL(10)- 2*RLL(1) ) /( 2*VYI(9)**2 )       RAY34190
      T2(5,6,6) = ( RLL(11)+ RLL(12)- 2*RLL(1) ) /( 2*DELP(11)**2 )     RAY34200
      T2(5,1,2) = ( RLL(13)+ RLL(14)- 2*RLL(1) - 2*T2(5,1,1)* XI(13)**2-RAY34210
     1            2*T2(5,2,2)*VXI(13)**2 ) / ( 2* XI(13)*VXI(13) )      RAY34220
      T2(5,1,6) = ( RLL(15)+ RLL(16)- 2*RLL(1) - 2*T2(5,1,1)* XI(15)**2-RAY34230
     1            2*T2(5,6,6)*DELP(15)**2) / ( 2* XI(15)*DELP(15))      RAY34240
      T2(5,2,6) = ( RLL(17)+ RLL(18)- 2*RLL(1) - 2*T2(5,2,2)*VXI(17)**2-RAY34250
     1            2*T2(5,6,6)*DELP(17)**2) / ( 2*VXI(17)*DELP(17))      RAY34260
      T2(5,3,4) = ( RLL(19)- RLL(20)           ) /( 2* YI(19)*VYI(19) ) RAY34270
C****                                                                   RAY34280
C****                                                                   RAY34290
      PRINT 22,  ( ( R(IR, IJ), IJ=1,6), IR=1,6)                        RAY34300
   22 FORMAT(1H1, / 51X, 15H *TRANSFORM* 1  ,  / 6(25X, 6F10.5/)  )     RAY34310
      PRINT 120                                                         RAY34320
  120 FORMAT(   /46X, 25H  *2ND ORDER TRANSFORM*           )            RAY34330
      DO 24 I1= 1,5                                                     RAY34340
      DO 25 I2= 1,6                                                     RAY34350
      PRINT 121, ( I1,I3,I2, T2(I1,I3,I2), I3=1,I2 )                    RAY34360
  121 FORMAT( 6(I4,I2,I1, 1PE11.3)  )                                   RAY34370
   25 CONTINUE                                                          RAY34380
      PRINT 122                                                         RAY34390
  122 FORMAT( 1H  )                                                     RAY34400
   24 CONTINUE                                                          RAY34410
      XTTT=((XO(33)- XO(34) )/2. - R(1,2)*VXI(33) )/VXI(33)**3          RAY34420
      XTPP  = (XO(27) - XO(28) + XO(6) -XO(5))/(2.*VXI(27)*VYI(27)**2)  RAY34430
      XXTT  = (XO(37) - XO(36) + XO(35)-XO(38)- 2.*(XO( 3) - XO( 4) ) )/RAY34440
     1   (4.*XI(35) * VXI(35)**2 )                                      RAY34450
      XXXT  = (XO(35) - XO(37) + XO(36)-XO(38)- 2.*(XO(33) - XO(34) ) )/RAY34460
     1   (4.*XI(35)**2*VXI(35))                                         RAY34470
      XTTD  = (XO(39) - XO(40) + XO(41)-XO(42)- 2.*(XO(11) - XO(12) ) )/RAY34480
     1   (4.*VXI(39)**2*DELP(39))                                       RAY34490
      XTDD  = (XO(39) - XO(41) + XO(40)-XO(42)- 2.*(XO(33) - XO(34) ) )/RAY34500
     1   (4.*VXI(39)*DELP(39)**2)                                       RAY34510
      XXPP  = (XO(23) - XO(24) + XO( 4)-XO( 3))/(2.*XI(23)*VYI(23)**2  )RAY34520
      XPPD  = (XO(31) - XO(32) + XO(12)-XO(11))/(2.*VYI(31)**2*DELP(31))RAY34530
      XTTTT=((XO(33)+XO(34) )/2. - T2(1,2,2)*VXI(33)**2)/ VXI(33)**4    RAY34540
      XTTPP = (XO(27) - XO( 5) + XO(28)-XO( 6) - 2.*XO( 9) ) /          RAY34550
     1   ( 2.*VXI(27)**2*VYI(27)**2 )                                   RAY34560
      XPPDD = (XO(31) - XO(11) + XO(32)-XO(12) - 2.*XO( 9) ) /          RAY34570
     1   ( 2.*VYI(31)**2 * DELP(31)**2 )                                RAY34580
      XPPPP =(XO(43) -T2(1,4,4)*VYI(43)**2) / VYI(43)**4                RAY34590
      ZDDD = ( (RLL(45) - RLL(46) )/2. - R(5,6)*DELP(45) )/DELP(45)**3  RAY34600
      ZDDDD = ( (RLL(45)+RLL(46)-2*RLL(1) )/2. -T2(5,6,6)*DELP(45)**2)/ RAY34610
     1   DELP(45)**4                                                    RAY34620
      XDDD = (( XO(45)- XO(46))/2. - R(1,6)*DELP(45) ) / DELP(45)**3    RAY34630
      XDDDD= (( XO(45)+ XO(46))/2. - T2(1,6,6)*DELP(45)**2 )/DELP(45)**4RAY34640
      TDDD = ((VXO(45)-VXO(46))/2. - R(2,6)*DELP(45) ) / DELP(45)**3    RAY34650
      TDDDD= ((VXO(45)+VXO(46))/2. - T2(2,6,6)*DELP(45)**2 )/DELP(45)**4RAY34660
      PRINT 26, XTTT, XTPP, XXTT, XXXT, XTTD, XTDD, XXPP, XPPD,         RAY34670
     1   XTTTT, XTTPP, XPPDD, XPPPP,                                    RAY34680
     2   XDDD, XDDDD, TDDD, TDDDD,      ZDDD, ZDDDD                     RAY34690
   26 FORMAT('1',/15X, 'X/THETA**3       =' ,1PE11.3   /                RAY34700
     1            15X, 'X/THETA.PHI**2   =' ,1PE11.3   /                RAY34710
     2            15X, 'X/X.THETA**2     =' ,1PE11.3   /                RAY34720
     3            15X, 'X/X**2.THETA     =' ,1PE11.3   /                RAY34730
     4            15X, 'X/THETA**2.DELTA =' ,1PE11.3   /                RAY34740
     5            15X, 'X/THETA.DELTA**2 =' ,1PE11.3   /                RAY34750
     6            15X, 'X/X.PHI**2       =' ,1PE11.3   /                RAY34760
     7            15X, 'X/PHI**2.DELTA   =' ,1PE11.3   //               RAY34770
     8            15X, 'X/THETA**4       =' ,1PE11.3   /                RAY34780
     9            15X, 'X/THETA**2.PHI**2=' ,1PE11.3   /                RAY34790
     A            15X, 'X/PHI**2.DELTA**2=' ,1PE11.3   /                RAY34800
     B            15X, 'X/PHI**4         =' ,1PE11.3   //               RAY34810
     C            15X, 'X/DELTA*3        =' ,1PE11.3   /                RAY34820
     D            15X, 'X/DELTA*4        =' ,1PE11.3   /                RAY34830
     E            15X, 'THETA/DELTA*3    =' ,1PE11.3   /                RAY34840
     F            15X, 'THETA/DELTA*4    =' ,1PE11.3   /                RAY34850
     H            15X, 'Z/DELTA*3        =' ,1PE11.3   /                RAY34860
     I            15X, 'Z/DELTA*4        =' ,1PE11.3   )                RAY34870
      DO 1  I1=1,5                                                      RAY34880
      DO 1  I2=1,6                                                      RAY34890
      DO 1  I3=1,6                                                      RAY34900
    1 TT(I1,I2,I3) = T2(I1,I2,I3)                                       RAY34910
      DO 2 I=1,12                                                       RAY34920
      PSI =  5. * FLOAT(I)                                              RAY34930
      TPSI = .001*DTAN( PSI/57.29578 )                                  RAY34940
      TT(1,1,1) = T2(1,1,1) + R(2,1) * R(1,1) * TPSI                    RAY34950
      TT(1,1,2) = T2(1,1,2) + ( R(2,1)*R(1,2) + R(2,2)*R(1,1) ) * TPSI  RAY34960
      TT(1,2,2) = T2(1,2,2) + R(2,2) * R(1,2) * TPSI                    RAY34970
      TT(1,1,6) = T2(1,1,6) + ( R(2,1)*R(1,6) + R(2,6)*R(1,1) ) * TPSI  RAY34980
      TT(1,2,6) = T2(1,2,6) + ( R(2,2)*R(1,6) + R(2,6)*R(1,2) ) * TPSI  RAY34990
      TT(1,6,6) = T2(1,6,6) + R(2,6) * R(1,6) * TPSI                    RAY35000
      TT(3,1,3) = T2(3,1,3) + R(1,1) * R(4,3) * TPSI                    RAY35010
      TT(3,1,4) = T2(3,1,4) + R(1,1) * R(4,4) * TPSI                    RAY35020
      TT(3,2,3) = T2(3,2,3) + R(1,2) * R(4,3) * TPSI                    RAY35030
      TT(3,2,4) = T2(3,2,4) + R(1,2) * R(4,4) * TPSI                    RAY35040
      TT(3,3,6) = T2(3,3,6) + R(1,6) * R(4,3) * TPSI                    RAY35050
      TT(3,4,6) = T2(3,4,6) + R(1,6) * R(4,4) * TPSI                    RAY35060
      CTTT=XTTT+ ( R(1,2)*T2(2,2,2) + R(2,2)*T2(1,2,2) ) * TPSI         RAY35070
      CTPP=XTPP+ ( R(1,2)*T2(2,4,4) + R(2,2)*T2(1,4,4) ) * TPSI         RAY35080
      CXTT=XXTT+ ( R(1,1)*T2(2,2,2) + R(1,2)*T2(2,1,2) +                RAY35090
     1             R(2,1)*T2(1,2,2) + R(2,2)*T2(1,1,2) ) * TPSI         RAY35100
      CXXT=XXXT+ ( R(1,1)*T2(2,1,2) + R(1,2)*T2(2,1,1) +                RAY35110
     1             R(2,1)*T2(1,1,2) + R(2,2)*T2(1,1,1) ) * TPSI         RAY35120
      CTTD=XTTD+ ( R(1,2)*T2(2,2,6) + R(1,6)*T2(2,2,2) +                RAY35130
     1             R(2,2)*T2(1,2,6) + R(2,6)*T2(1,2,2) ) * TPSI         RAY35140
      CTDD=XTDD+ ( R(1,2)*T2(2,6,6) + R(1,6)*T2(2,2,6) +                RAY35150
     1             R(2,2)*T2(1,6,6) + R(2,6)*T2(1,2,6) ) * TPSI         RAY35160
      CXPP=XXPP+ ( R(1,1)*T2(2,4,4) + R(2,1)*T2(1,4,4) ) * TPSI         RAY35170
      CPPD=XPPD+ ( R(1,6)*T2(2,4,4) + R(2,2)*T2(1,4,4) ) * TPSI         RAY35180
      PRINT 27, PSI                                                     RAY35190
   27 FORMAT(1H1, 35X,'FOCAL PLANE TILT ANGLE= ' ,F07.2,'   DEGREES '  )RAY35200
      PRINT 28,  ( ( R(IR, IJ), IJ=1,6), IR=1,6)                        RAY35210
   28 FORMAT(     / 51X, 15H *TRANSFORM* 1  ,  / 6(25X, 6F10.5/)  )     RAY35220
      PRINT 120                                                         RAY35230
      DO 29 I1= 1,5                                                     RAY35240
      DO 30 I2= 1,6                                                     RAY35250
      PRINT 121, ( I1,I3,I2, TT(I1,I3,I2), I3=1,I2 )                    RAY35260
   30 CONTINUE                                                          RAY35270
      PRINT 122                                                         RAY35280
   29 CONTINUE                                                          RAY35290
      PRINT 26, CTTT, CTPP, CXTT, CXXT, CTTD, CTDD, CXPP, CPPD,         RAY35300
     1   XTTTT, XTTPP, XPPDD, XPPPP,                                    RAY35310
     2   XDDD, XDDDD, TDDD, TDDDD,      ZDDD, ZDDDD                     RAY35320
    2 CONTINUE                                                          RAY35330
      RETURN                                                            RAY35340
      END                                                               RAY35350
      SUBROUTINE MTRX1( M, JEN, NR, ENERGY, JPRT  )                     RAY35360
C****                                                                   RAY35370
C****                                                                   RAY35380
C**** M=0  14 RAYS ARE USED TO EVALUATE THE ABERRATION COEFFICIENTS FOR RAY35390
C**** A POINT SOURCE OBJECT THROUGH 4'TH ORDER                          RAY35400
C**** M=1   6 RAYS ARE USED TO EVALUATE THE ABERRATION COEFFICIENTS FOR RAY35410
C**** A POINT SOURCE OBJECT THROUGH 4'TH ORDER; MIDPLANE ONLY           RAY35420
C****                                                                   RAY35430
C****                                                                   RAY35440
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY35450
      REAL*8 KT, LP                                                     RAY35460
      CHARACTER*8 L(2,50), LX(2,12)                                     RAY35470
      LOGICAL LPLT
      COMMON  /BLCK00/ LPLT
      COMMON  /BLCK 1/ XI, YI, ZI, VXI, VYI, VZI, DELP                  RAY35480
      COMMON  /BLCK 2/ XO, YO, ZO, VXO, VYO, VZO, RTL, RLL, EKL         RAY35490
      COMMON  /BLCK 3/ XOR,  YOR,  ZOR , TH0, PH0, TL1                  RAY35500
      DIMENSION XO(999), YO(999), ZO(999), VXO(999), VYO(999), VZO(999),RAY00200
     1          RTL(999), RLL(999), EKL(999)
      DIMENSION XI(999), YI(999), ZI(999), VXI(999), VYI(999), VZI(999),RAY35510
     1          DELP(999)                                               RAY35520
      DIMENSION CXX(12,10), IX(12), CD(6,4),   LFACT(50), C(50,10)      RAY35540
        DIMENSION DXX(21,10), DXY(21,10)                                RAY35541
      DATA IX/ 1,2,5,7,11,13,19,22,29,32,35,36  /                       RAY35550
      DATA LFACT / 1,0,1,0,2*4,2*3,4,3,2*7,2*6,2*7,2*6,3*10,3*9,        RAY35560
     1   2*10,2*9,3*13,12,4,7,4,7,14*0   /                              RAY35570
      DATA  L  / 'X/TH    ','       =','T/TH    ','       =',           RAY35580
     1           'Y/PH    ','       =','P/PH    ','       =',           RAY35590
     2           'X/TH**2 ','       =','X/PH**2 ','       =',           RAY35600
     3           'T/TH**2 ','       =','T/PH**2 ','       =',           RAY35610
     4           'Y/TH*PH ','       =','P/TH*PH ','       =',           RAY35620
     5           'X/TH**3 ','       =','X/TH*PH*','*2     =',           RAY35630
     6           'T/TH**3 ','       =','T/TH*PH*','*2     =',           RAY35640
     7           'Y/PH**3 ','       =','Y/TH**2*','PH     =',           RAY35650
     8           'P/PH**3 ','       =','P/TH**2*','PH     =',           RAY35660
     9           'X/TH**4 ','       =','X/TH**2*','PH**2  =',           RAY35670
     A           'X/PH**4 ','       =','T/TH**4 ','       =',           RAY35680
     B           'T/TH**2*','PH**2  =','T/PH**4 ','       =',           RAY35690
     C           'Y/TH**3*','PH     =','Y/TH*PH*','*3     =',           RAY35700
     D           'P/TH**3*','PH     =','P/TH*PH*','*3     =',           RAY35710
     E           'X/TH**5 ','       =','X/TH**3*','PH**2  =',           RAY35720
     F           'X/TH*PH*','*4     =','T/TH**5 ','       =',           RAY35730
     G           'X/PH**2(','Trunc.)=','X/TH*PH*','*2(Tr.)=',           RAY35740
     H           'X/T**2 (','Trunc.)=','X/T**3 (','Trunc.)=',           RAY35750
     I           28*'        '   /
      DATA  LX / 'ENERGY(M','EV)    =','XOR (CM)','       =',           RAY35760
     1           'YOR (CM)','       =','ZOR (CM)','       =',           RAY35770
     2           'TH  (MR)','       =','PHI (MR)','       =',           RAY35771
     3           '!XMAX!(C','M)     =','2!YMAX!(','CM)    =',           RAY35780
     4           '!X-WAIST','!(cm)  =','X(X-WAIS','T)     =',           RAY35781
     5           'Z(X-WAIS','T)     =','LENGTH(C','M)     ='  /         RAY35782
C****                                                                   RAY35790
        MM=M                                                            RAY35791
C****                                                                   RAY35800
      I   = JEN                                                         RAY35810
      IF( I   .GT. 10 ) I   = 10                                        RAY35820
C****                                                                   RAY35830
C****                                                                   RAY35840
      XMIN = XO(1)                                                      RAY35850
      XMAX = XO(1)                                                      RAY35860
      YMAX = DABS(YO(1))                                                RAY35870
      DO 4 J=2,NR                                                       RAY35880
      IF( XO(J) .GT. XMAX )  XMAX = XO(J)                               RAY35890
      IF( XO(J) .LT. XMIN )  XMIN = XO(J)                               RAY35900
      IF( DABS(YO(J) ) .GT. YMAX ) YMAX = DABS(YO(J))                   RAY35910
    4 CONTINUE                                                          RAY35920
      CXX(1,I  ) = ENERGY                                               RAY35930
      CXX(2,I  ) = XOR                                                  RAY35940
      CXX(3,I  ) = YOR                                                  RAY35950
      CXX(4,I  ) = ZOR                                                  RAY35960
        CXX(5,I  )=TH0                                                  RAY35961
        CXX(6,I  )=PH0                                                  RAY35962
      CXX(7,I  ) = DABS(XMAX-XMIN)                                      RAY35970
      CXX(8,I  ) = 2.*YMAX                                              RAY35980
C****                                                                   RAYK1500
C****   CALCULATE BEAM WIDTH AT TEN EQUALLY SPACED (5MM)                RAYK1510
C****   DISTANCES EACH SIDE OF ZOR                                      RAYK1520
C****                                                                   RAYK1530
        DO 20 JJ=1,21                                                   RAYK1540
        XMIN = XO(1) + 0.00050 * VXO(1) * (JJ-11)                       RAYK1550
        XMAX = XMIN                                                     RAYK1560
        DO 21 J = 2, 6                                                  RAYK1570
        XJJ = XO(J) + 0.00050 *VXO(J) * (JJ-11)                         RAYK1580
        IF (XJJ.GT.XMAX) XMAX = XJJ                                     RAYK1590
        IF (XJJ.LT.XMIN) XMIN = XJJ                                     RAYK1600
21      CONTINUE                                                        RAYK1610
        DXX(JJ,I) = DABS( XMAX - XMIN)                                  RAYK1620
        DXY(JJ,I) = 0.                                                  RAYK1630
        IF (NR.LE.6) GOTO 20                                            RAYK1640
        DO 22 J=7,NR                                                    RAYK1650
        XJJ = XO(J) + 0.00050* VXO(J) * (JJ-11)                         RAYK1660
        IF ( XJJ.GT.XMAX ) XMAX = XJJ                                   RAYK1670
        IF ( XJJ.LT.XMIN ) XMIN = XJJ                                   RAYK1680
22      CONTINUE                                                        RAYK1690
        DXY(JJ,I) = DABS( XMAX - XMIN)                                  RAYK1700
20        CONTINUE                                                      RAYK1710
C****                                                                   RAYK1720
C****     CALCULATE POSITION OF MINIMUM BEAM WIDTH                      RAYK1730
C****     WITHIN 10.0 CM OF ZOR                                         RAYK1740
        XMX = 1.0D20                                                    RAYK1750
        DO 25  JJ=1, 101                                                RAYK1760
        XMIN = XO(1) + 0.00020 * VXO(1) * (JJ-51)                       RAYK1770
        XMAX = XMIN                                                     RAYK1780
        DO 26  J=2,NR                                                   RAYK1790
        XJJ = XO(J) + 0.00020 * VXO(J) * (JJ-51)                        RAYK1800
        IF ( XJJ.GT.XMAX ) XMAX = XJJ                                   RAYK1810
        IF ( XJJ.LT.XMIN ) XMIN = XJJ                                   RAYK1820
26      CONTINUE                                                        RAYK1830
        DXMAX = DABS( XMAX - XMIN )                                     RAYK1840
        IF ( DXMAX.GE.XMX ) GO TO 25                                    RAYK1850
        XMX = DXMAX                                                     RAYK1860
        ZMX = 0.20 * (JJ - 51)                                          RAYK1870
25      CONTINUE                                                        RAYK1880
        IF ( DABS( ZMX ).GT.9.9 ) ZMX = 1.0D20                          RAYK1890
        CXX( 9, I) = XMX                                                RAYK1900
        CXX(10, I) = .001*TH0*ZMX + XOR                                 RAYK1901
        CXX(11, I) = ZMX+ZOR                                            RAYK1910
        CXX(12, I) = TL1
C****                                                                   RAY35990
C****                                     
      IF( VXI(2) .EQ. 0. )  VXI(2) = 1.D-30
      IF( VXI(3) .EQ. 0. )  VXI(3) = 1.D-30                             RAY36010
      KT = VXI(5)/VXI(3)                                                RAY36020
      DTH = VXI(3)                                                      RAY36030
      TMAX = VXI(5)                                                     RAY36040
      PMAX = VYI(12)
      XT=XO(2)/VXI(2)                                                   RAY36050
      TT=(KT**3*(VXO(3)-VXO(4))- VXO(5)+VXO(6))/(2.* (KT**3-KT)*DTH)    RAY36060
      XTT = ( KT**4*(XO(3) + XO(4)) - (XO(5)+XO(6) )) /                 RAY36070
     1   (2.*(KT**4-KT**2) *DTH*DTH)                                    RAY36080
      TTT = ( KT**4*(VXO(3)+VXO(4)) -(VXO(5)+VXO(6))) /                 RAY36090
     1   (2.*(KT**4-KT**2) *DTH*DTH)                                    RAY36100
      XTTT  = ( KT**5 * ( XO(3) - XO(4) - 2.*XT*DTH ) -                 RAY36110
     1 ( XO(5) - XO(6) -2.*KT*XT*DTH) ) / (2.*(KT**5 - KT**3) *DTH**3 ) RAY36120
      TTTT  = (-KT    * (VXO(3) -VXO(4)) + (VXO(5) -VXO(6) )  ) /       RAY36130
     1   (2.*(KT**3 - KT   ) *DTH**3 )                                  RAY36140
      XTTTT = ( (XO(5)+XO(6))-KT*KT*(XO(3)+XO(4) ) ) /                  RAY36150
     1   (2.*(KT**4 - KT*KT)*DTH**4 )                                   RAY36160
      TTTTT =((VXO(5)+VXO(6))-KT*KT*(VXO(3)+VXO(4))) /                  RAY36170
     1   (2.*(KT**4 - KT*KT)*DTH**4 )                                   RAY36180
      XTTTTT= ( XO(5) - XO(6) - 2.*KT*XT*DTH - KT**3*( XO(3) - XO(4) -  RAY36190
     1   2.*XT*DTH) ) / ( 2.*(KT**5 - KT**3) *DTH**5 )                  RAY36200
      TTTTTT= 0.                                                        RAY36210
C****                                                                   RAY36220
C****                                                                   RAY36230
      C( 1,I)      = XT*10.                                             RAY36240
      C( 2,I)      = TT                                                 RAY36250
      C( 5,I)      = XTT*10.**4                                         RAY36260
      C( 7,I)      = TTT*10.**3                                         RAY36270
      C(11,I)      = XTTT*10.**7                                        RAY36280
      C(13,I)      = TTTT*10.**6                                        RAY36290
      C(19,I)      = XTTTT*10.**10                                      RAY36300
      C(22,I)      = TTTTT*10.**09                                      RAY36310
      C(29,I)      = XTTTTT*10.**13                                     RAY36320
      C(32,I)      = TTTTTT*10.**12                                     RAY36330
      C(35,I)      = (XTT + XTTTT*TMAX*TMAX)*10.**4                     RAY36340
      C(36,I)      = (XTTT+XTTTTT*TMAX*TMAX)*10.**7                     RAY36350
C****                                                                   RAY36360
C****                                                                   RAY36370
      IF( M .NE. 0 ) GO TO 1                                            RAY36380
      LP = VYI(12)/VYI(7)                                               RAY36390
      DPH = VYI(7)                                                      RAY36400
      XPP   = (LP**4*XO(7) - XO(12)) /((LP**4 - LP*LP)*DPH*DPH )        RAY36410
      TPP   = (LP**4*VXO(7)-VXO(12)) /((LP**4 - LP*LP)*DPH*DPH )        RAY36420
      XPPPP = (XO(12)-LP*LP*XO(7) ) /((LP**4-LP*LP)*DPH**4)             RAY36430
      TPPPP =(VXO(12)-LP*LP*VXO(7)) /((LP**4-LP*LP)*DPH**4)             RAY36440
      XTPP  = (LP**4*( XO(8) - XO(9)) - ( XO(13) - XO(14)) - (LP**4-1.)*RAY36450
     1   ( XO(3) - XO(4)) -(( XO(10) - XO(11)) - KT*( XO(8) - XO(9) ) - RAY36460
     2   ( XO(5) - XO(6) ) + KT*( XO(3) - XO(4) ) ) *                   RAY36470
     3   ( ( LP**4 - LP*LP) / (KT**3-KT) ))/(2.*(LP**4-LP*LP)*          RAY36480
     4   DTH*DPH*DPH )                                                  RAY36490
      TTPP  = (LP**4*(VXO(8) -VXO(9)) - (VXO(13) -VXO(14)) - (LP**4-1.)*RAY36500
     1   (VXO(3) -VXO(4)) -((VXO(10) -VXO(11)) - KT*(VXO(8) -VXO(9) ) - RAY3650A
     2   (VXO(5) -VXO(6) ) + KT*(VXO(3) -VXO(4) ) ) *                   RAY3650B
     3   ( ( LP**4 - LP*LP) / (KT**3-KT) ))/(2.*(LP**4-LP*LP)*          RAY3650C
     4   DTH*DPH*DPH )                                                  RAY3650D
      XTTPP = ( ( XO(8)+XO(9) ) - ( XO(3)+XO(4) ) - 2.*XO(7)) /         RAY36510
     1   (2.*DTH*DTH*DPH*DPH)                                           RAY36520
      TTTPP = ( (VXO(8)+VXO(9)) - (VXO(3)+VXO(4)) -2.*VXO(7)) /         RAY36530
     1   (2.*DTH*DTH*DPH*DPH)                                           RAY36540
      YP    = ( LP**3 * YO(7) - YO(12) ) / ( (LP**3 - LP)*DPH )         RAY36550
      PP    = ( LP**3 *VYO(7) -VYO(12) ) / ( (LP**3 - LP)*DPH )         RAY36560
      YPPP  = (YO(12) - LP*YO(7)) /((LP**3-LP)*DPH**3 )                 RAY36570
      PPPP  =(VYO(12) -LP*VYO(7)) /((LP**3-LP)*DPH**3 )                 RAY36580
      YTTP  = ( YO(8) + YO(9) - 2.*YO(7) ) / (2.*DTH*DTH*DPH )          RAY36590
      PTTP  = (VYO(8) +VYO(9) - 2.*VYO(7)) / (2.*DTH*DTH*DPH )          RAY36600
      YTPPP = ( YO(13) - LP*YO(8) - YO(12) + LP*YO(7) ) /               RAY36610
     1   ((LP**3 - LP)*DTH*DPH**3 )                                     RAY36620
      PTPPP = (VYO(13) - LP*VYO(8)-VYO(12) + LP*VYO(7)) /               RAY36630
     1   ((LP**3 - LP)*DTH*DPH**3 )                                     RAY36640
      YTTTP = ( YO(10) - YO(11) -KT*(YO(8)-YO(9) ) ) /                  RAY36650
     1   (2.*(KT**3-KT) * DTH**3*DPH )                                  RAY36660
      PTTTP = (VYO(10) -VYO(11) -KT*(VYO(8)-VYO(9))) /                  RAY36670
     1   (2.*(KT**3-KT) * DTH**3*DPH )                                  RAY36680
      YTP   = ( (YO(10)-YO(11) -KT**3*(YO(8)-YO(9) ) ) /(2.*(KT-KT**3))-RAY36690
     1   YTPPP*DTH*DPH**3 ) /(DTH*DPH)                                  RAY36700
      PTP   = ((VYO(10)-VYO(11)-KT**3*(VYO(8)-VYO(9))) /(2.*(KT-KT**3))-RAY36710
     1   PTPPP*DTH*DPH**3 ) /(DTH*DPH)                                  RAY36720
      XTTTPP= ( XO(10) - XO(11) - KT*( XO(8) - XO(9)) - ( XO(5) - XO(6))RAY36730
     1   +KT*( XO(3) - XO(4) ) ) / (2.*(KT**3-KT) * DTH**3*DPH*DPH )    RAY36740
      TTTTPP= 0.                                                        RAY36750
      XTPPPP= ( XO(13) - XO(14) - LP*LP*( XO(8) - XO(9)) + (LP*LP-1.) * RAY36760
     1   ( XO(3) - XO(4) ) ) / (2.*(LP**4-LP*LP) * DTH*DPH**4 )         RAY36770
      TTPPPP= (VXO(13) -VXO(14) - LP*LP*(VXO(8) -VXO(9)) + (LP*LP-1.) * RAY36780
     1   (VXO(3) -VXO(4) ) ) / (2.*(LP**4-LP*LP) * DTH*DPH**4 )         RAY3678A
      C( 3,I)      = YP*10.                                             RAY36790
      C( 4,I)      = PP                                                 RAY36800
      C( 6,I)      = XPP*10.**4                                         RAY36810
      C( 8,I)      = TPP*10.**3                                         RAY36820
      C( 9,I)      = YTP*10.**4                                         RAY36830
      C(10,I)      = PTP*10.**3                                         RAY36840
      C(12,I)      = XTPP*10.**7                                        RAY36850
      C(14,I)      = TTPP*10.**6                                        RAY36860
      C(15,I)      = YPPP*10.**7                                        RAY36870
      C(16,I)      = YTTP*10.**7                                        RAY36880
      C(17,I)      = PPPP*10.**6                                        RAY36890
      C(18,I)      = PTTP*10.**6                                        RAY36900
      C(20,I)      = XTTPP*10.**10                                      RAY36910
      C(21,I)      = XPPPP*10.**10                                      RAY36920
      C(23,I)      = TTTPP*10.**09                                      RAY36930
      C(24,I)      = TPPPP*10.**09                                      RAY36940
      C(25,I)      = YTTTP*10.**10                                      RAY36950
      C(26,I)      = YTPPP*10.**10                                      RAY36960
      C(27,I)      = PTTTP*10.**09                                      RAY36970
      C(28,I)      = PTPPP*10.**09                                      RAY36980
      C(30,I)      = XTTTPP*10.**13                                     RAY36990
      C(31,I)      = XTPPPP*10.**13                                     RAY37000
      C(33,I)      = (XPP + XPPPP*PMAX*PMAX)*10.**4                     RAY37010
      C(34,I)      = (XTPP + XTTTPP*TMAX*TMAX +XTPPPP*PMAX*PMAX)*10.**7 RAY37020
C****                                                                   RAY37030
C****                                                                   RAY37040
   13 FORMAT( 2I5 )
   14 FORMAT(   )                                                       RAY37050
   15 FORMAT( //  , 8( 15X, 2A8,  F9.4 /  ) /,3( 15X, 2A8, F8.3/))      RAY37060
   16 FORMAT(    15X, 2A8, 1PE12.3, 0PF15.4   )                         RAY37070
      IF( JPRT .EQ. 3 ) GO TO 23
      PRINT 15,( ( LX(J,K),J=1,2),  CXX(K,I), K=1,12)                   RAY37080
      DO 2 JJ=1,36                                                      RAY37090
      COEF = C(JJ,I)/ 10.**LFACT(JJ)                                    RAY37100
      IF( (JJ.EQ. 5).OR.(JJ.EQ. 11).OR.(JJ.EQ.19).OR.(JJ.EQ.29))PRINT 14RAY37110
    2 PRINT 16, (L(J,JJ), J=1,2), COEF, C(JJ,I)                         RAY37120
      GO TO 23                                                          RAY37130
C****                                                                   RAY37140
C****                                                                   RAY37150
    1 CONTINUE                                                          RAY37160
      IF( JPRT .EQ. 3 ) GO TO 23
      PRINT 15,( ( LX(J,K),J=1,2),  CXX(K,I), K=1,12)                   RAY37170
      DO 3 JJ=1,12                                                      RAY37180
      K = IX(JJ)                                                        RAY37190
      COEF = C(K,I)/10.**LFACT(K)                                       RAY37200
    3 PRINT 16, ( L(J,K),J=1,2), COEF, C(K,I)                           RAY37210
C****                                                                   RAYK2000
C****   PRINT OUT BEAM WIDTH                                            RAYK2010
C****                                                                   RAYK2020
23      CONTINUE                                                        RAYK2030
      IF( JPRT .EQ.3 ) RETURN
        PRINT 29                                                        RAYK2040
        DO 24 JJ=1, 21                                                  RAYK2050
        DZ = 0.50 * (JJ-11)                                             RAYK2060
        PRINT 30, DZ, DXX(JJ,I), DXY(JJ,I)                              RAYK2070
   24   CONTINUE                                                        RAYK2080
   29   FORMAT ('1', 3X, 'IMAGE SIZE !XMAX!(CM)', //2X, 'DZ (CM)',      RAYK2090
     1  2X, '  1-6  ', 2X, '  1-NR')                                    RAYK2100
   30   FORMAT (F8.2, 2F9.3)                                            RAYK2110
        RETURN                                                          RAY37220
C****                                                                   RAY37230
C****                                                                   RAY37240
      ENTRY MPRNT( NEN )                                                RAY37250
C****                                                                   RAY37260
      IF( LPLT) WRITE(2,13) NEN, MM
   18 FORMAT(   4X, 2A8, 10F11.3      )                                 RAY37270
      IF( NEN .GT. 10 )  NEN = 10                                       RAY37280
      PRINT 14                                                          RAY37290
      DO 8 K=1,8                                                        RAY37300
      IF( LPLT ) WRITE(2,18)(LX(J,K),J=1,2),(CXX(K,I),I=1,NEN)
    8 PRINT 18,   ( LX(J,K),J=1,2),(CXX(K,I),I=1,NEN)                   RAY37310
      PRINT 14                                                          RAY37320
C****                                                                   RAY37330
      IF(MM .NE. 0 )  GO TO 5                                           RAY37340
      DO 7 K=1,36                                                       RAY37350
      IF( (K .EQ. 5).OR.(K .EQ. 11).OR.(K .EQ.19).OR.(K .EQ.29))PRINT 14RAY37360
      IF( LPLT ) WRITE(2,18) (L(J,K),J=1,2),(C(K,I),I=1,NEN )
    7 PRINT 18,   ( L(J,K),J=1,2),(C(K,I),I=1,NEN )                     RAY37370
      GO TO 19                                                          RAY37380
    5 DO 6 JJ=1,12                                                      RAY37390
      K = IX(JJ)                                                        RAY37400
      IF( LPLT ) WRITE(2,18) ( L(J,K),J=1,2), ( C(K,I), I=1,NEN)
    6 PRINT 18, ( L(J,K),J=1,2),(C(K,I), I=1,NEN )                      RAY37410
C****                                                                   RAY37420
C**** CHROMATIC ABERRATION COEFFICIENTS                                 RAY37430
C**** CALCULATED ONLY FOR CASE OF NEN= 5 ENERGIES                       RAY37440
C****                                                                   RAY37450
   19 CONTINUE                                                          RAY37460
      IF( NEN .NE. 5 ) RETURN                                           RAY37470
      DEL = CXX(1,4)/CXX(1,3) - 1.                                      RAY37480
      DO 9 I=1,6                                                        RAY37490
      IF( I .EQ. 1 ) K=2                                                RAY37500
      IF( I .EQ. 2 ) GO TO 10                                           RAY37510
      IF( I .EQ. 3 ) K=5                                                RAY37520
      IF( I .EQ. 4 ) K=11                                               RAY37530
      IF( I .EQ. 5 ) K=19                                               RAY37540
      IF( I .EQ. 6 ) K=29                                               RAY37550
      IF( I .GT. 2 ) GO TO 11                                           RAY37560
      X1 =(CXX(K,1) - CXX(K,3))/100.                                    RAY37570
      X2 =(CXX(K,2) - CXX(K,3))/100.                                    RAY37580
      X4 =(CXX(K,4) - CXX(K,3))/100.                                    RAY37590
      X5 =(CXX(K,5) - CXX(K,3))/100.                                    RAY37600
      GO TO 12                                                          RAY37610
   11 X1 = C(K,1) - C(K,3)                                              RAY37620
      X2 = C(K,2) - C(K,3)                                              RAY37630
      X4 = C(K,4) - C(K,3)                                              RAY37640
      X5 = C(K,5) - C(K,3)                                              RAY37650
   12 CD(I,1) = (8. *(X4-X2) - (X5-X1) )/(12.  *DEL)                    RAY37660
      CD(I,2) = (16.* (X4+X2) - (X5+X1) )/(24.  *DEL*DEL)               RAY37670
      CD(I,3) = ( (X5-X1) - 2.*(X4-X2) )/(12.  *DEL**3)                 RAY37680
      CD(I,4) = ( (X5+X1) - 4.*(X4+X2) )/(24.  *DEL**4)                 RAY37690
      GO TO 9                                                           RAY37700
   10 Z1 =(CXX(4,1) - CXX(4,3))/100.                                    RAY37710
      Z2 =(CXX(4,2) - CXX(4,3))/100.                                    RAY37720
      Z4 =(CXX(4,4) - CXX(4,3))/100.                                    RAY37730
      Z5 =(CXX(4,5) - CXX(4,3))/100.                                    RAY37740
      TPSI = (8.* (Z4-Z2) - (Z5-Z1) ) / (8.* (X4-X2) - (X5-X1) )        RAY37750
      PSI = 57.29578D0 * DATAN(TPSI)                                    RAY37760
      DZ1 = Z1 - X1*TPSI                                                RAY37770
      DZ2 = Z2 - X2*TPSI                                                RAY37780
      DZ4 = Z4 - X4*TPSI                                                RAY37790
      DZ5 = Z5 - X5*TPSI                                                RAY37800
      CD(I,1) = -C(2,3)*( 8.*(DZ4-DZ2) - (DZ5-DZ1) )/(12.  *DEL)        RAY37810
      CD(I,2) = -C(2,3)*( 16.*(DZ4+DZ2) - (DZ5+DZ1) )/(24.  *DEL*DEL)   RAY37820
      CD(I,3) = -C(2,3)*( (DZ5-DZ1) - 2.*(DZ4-DZ2) )/(12.  *DEL**3)     RAY37830
      CD(I,4) = -C(2,3)*( (DZ5+DZ1) - 4.*(DZ4+DZ2) )/(24.  *DEL**4)     RAY37840
    9 CONTINUE                                                          RAY37850
      PRINT 14                                                          RAY37860
      PRINT 17, PSI, (I,I=1,4), ( (CD(K,I),I=1,4), K=1,6 )              RAY37870
      IF( LPLT ) WRITE(2,17) PSI, (I,I=1,4), ( ( CD(K,I),I=1,4), K=1,6 )
   17 FORMAT(4X,'PSI            =', F11.3,/4X,'N              =',4(I7,  RAY37880
     1  4X),/4X,'X/D**N         =',4F11.3,/4X,'X/T*D**N       =',4F11.3,RAY37890
     2      /4X,'X/T**2*D**N    =',4X,1P4E11.3,                         RAY37900
     3      /4X,'X/T**3*D**N    =',4X,1P4E11.3,                         RAY37910
     4      /4X,'X/T**4*D**N    =',4X,1P4E11.3,                         RAY37920
     5      /4X,'X/T**5*D**N    =',4X,1P4E11.3             )            RAY37930
      RETURN                                                            RAY37940
      END                                                               RAY37950
      SUBROUTINE  DERIV( BFUN )                                         RAY37960
C****                                                                   RAY37970
C****                                                                   RAY37980
C****                                                                   RAY37990
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY38000
      REAL*8  K                                                         RAY38010
      COMMON  /BLCK 4/  ENERGY, VEL, PMASS, Q0                          RAY38020
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC                          RAY38030
      COMMON  /BLCK11/  EX, EY, EZ, QMC, IVEC                           RAY38040
      DIMENSION TC(6), DTC(6)                                           RAY38050
      DATA  C /2.99792458D10 /                                          RAY38060
C****                                                                   RAY38070
C****                                                                   RAY38080
      CALL BFUN                                                         RAY38090
      DTC(1) = TC(4)                                                    RAY38100
      DTC(2) = TC(5)                                                    RAY38110
      DTC(3) = TC(6)                                                    RAY38120
      IF( IVEC .NE. 0 )  GO TO 4                                        RAY38130
      DTC(4) = K * ( TC(5) * BZ - TC(6) * BY )                          RAY38140
      DTC(5) = K * ( TC(6) * BX - TC(4) * BZ )                          RAY38150
      DTC(6) = K * ( TC(4) * BY - TC(5) * BX )                          RAY38160
      RETURN                                                            RAY38170
    4 VEL = DSQRT( TC(4)**2 + TC(5)**2 + TC(6)**2 )                     RAY38180
C****
C**** SK 12/02/83
C**** GAMMA CORRECTION FOR HIGH ENERGY ELECTRONS
C**** NOT EXACT 
C****
      GAMMA = 1.D0 + ENERGY/(PMASS*931.5016D0)
      IF( GAMMA .LT. 100. ) GAMMA = 1./DSQRT( 1.-VEL*VEL/(C*C) )        RAY38190
C****
C****
      K = 1./(QMC*GAMMA)                                                RAY38200
      AK = K/(8.98755D13)                                               RAY38210
      ETERM = (EX*TC(4)+EY*TC(5)+EZ*TC(6) )*AK                          RAY38220
      DTC(4) = K*( TC(5)*BZ - TC(6)*BY + EX*1.D7 ) - TC(4)*ETERM        RAY38230
      DTC(5) = K*( TC(6)*BX - TC(4)*BZ + EY*1.D7 ) - TC(5)*ETERM        RAY38240
      DTC(6) = K*( TC(4)*BY - TC(5)*BX + EZ*1.D7 ) - TC(6)*ETERM        RAY38250
      RETURN                                                            RAY38260
      END                                                               RAY38270
      SUBROUTINE FNMIRK(N,X,H,Y,DY,D,E,BFUN,  NDEX)                     RAY38280
      IMPLICIT REAL*8(A-H,O-Z)                                          RAY38290
      EXTERNAL BFUN                                                     RAY38300
      DIMENSION Y(1),DY(1),D(1),E(1)                                    RAY38310
      IF( NDEX.NE.0) GO TO 20                                           RAY38320
      DO 10 I=1,N                                                       RAY38330
      D(I)=Y(I)                                                         RAY38340
 10   CONTINUE                                                          RAY38350
      CALL DERIV ( BFUN )                                               RAY38360
      RETURN                                                            RAY38380
 20   HALFH=0.5*H                                                       RAY38370
      DO 30 I=1,N                                                       RAY38390
      T=HALFH*DY(I)                                                     RAY38400
      Y(I)=D(I)+T                                                       RAY38410
      E(I)=T                                                            RAY38420
 30   CONTINUE                                                          RAY38430
      XZERO=X                                                           RAY38440
      X=X+HALFH                                                         RAY38450
      CALL DERIV ( BFUN )                                               RAY38460
      DO 40 I=1,N                                                       RAY38470
      T=HALFH*DY(I)                                                     RAY38480
      Y(I)=D(I)+T                                                       RAY38490
      E(I)=E(I)+2.0*T                                                   RAY38500
 40   CONTINUE                                                          RAY38510
      CALL DERIV ( BFUN )                                               RAY38520
      DO 50 I=1,N                                                       RAY38530
      T=H*DY(I)                                                         RAY38540
      Y(I)=D(I)+T                                                       RAY38550
      E(I)=E(I)+T                                                       RAY38560
 50   CONTINUE                                                          RAY38570
      X=XZERO+H                                                         RAY38580
      CALL DERIV ( BFUN )                                               RAY38590
      DO 60 I=1,N                                                       RAY38600
      Y(I)=D(I)+(E(I)+HALFH*DY(I))*.333333333                           RAY38610
      D(I)=Y(I)                                                         RAY38620
 60   CONTINUE                                                          RAY38630
      CALL DERIV ( BFUN )                                               RAY38640
      RETURN                                                            RAY38650
      END                                                               RAY38660
      SUBROUTINE PLTOUT ( JEN, J, NUM )
C****
C****
C**** THIS ROUTINE STORES STEP-BY-STEP POSITION INFORMATION FOR EACH
C**** RAY FOR USE BY PLOTTING ROUTINES.
C****
C****
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 K
      LOGICAL LPLT
      COMMON  /BLCK00/ LPLT
      COMMON  /BLCK 5/  XA, YA, ZA, VXA, VYA, VZA
      COMMON  /BLCK10/  BX, BY, BZ, K, TC, DTC
      DIMENSION TC(6), DTC(6)
      DIMENSION GRAPH(4,512), ICOR(512,2)
C****
C****
C****
      IF( NUM .GT. 512 ) NUM = 512
      WRITE (1)   JEN, J, NUM
      WRITE (1)   ( GRAPH(1,IK),IK=1,NUM), ( GRAPH(2,IK),IK=1,NUM),
     1            ( GRAPH(3,IK),IK=1,NUM), ( GRAPH(4,IK),IK=1,NUM)
      WRITE (1)   (  ICOR(IK,1),IK=1,NUM), (  ICOR(IK,2),IK=1,NUM)
      RETURN
C****
C****
      ENTRY   PLT1( NUM, NO, NBR, TP )
C****
C****
      IF( .NOT. LPLT ) RETURN
      IF( NUM .GT. 512 ) RETURN
      GRAPH( 1,NUM) = TC(1)
      GRAPH( 2,NUM) = TC(2)
      GRAPH( 3,NUM) = TC(3)
      GRAPH( 4,NUM) = TP
      ICOR ( NUM,1) = NO
      ICOR ( NUM,2) = NBR
      RETURN
C****
C****
      ENTRY   PLT2( NUM, NO, NBR, TP )
C****
C****
      IF( .NOT. LPLT ) RETURN
      IF( NUM .GT. 512 ) RETURN
      GRAPH( 1,NUM) = XA
      GRAPH( 2,NUM) = YA
      GRAPH( 3,NUM) = ZA
      GRAPH( 4,NUM) = TP
      ICOR ( NUM,1) = NO
      ICOR ( NUM,2) = NBR
      RETURN
      END
C*IBM FUNCTION DASIN(X)
C**** 
C**** ROUTINE TO PASS CALL TO IBM DOUBLE PRECISION ARC-SINE
C****
C*IBM IMPLICIT REAL*8(A-H,O-Z)
C*IBM DASIN = DARSIN(X)
C*IBM RETURN
C*IBM END

       INTEGER*4 FUNCTION ITCPU( )
C****
C****
C**** SYSTEM SERVICES ROUTINE IS USED TO OBTAIN CPU TIME USED
C**** TIME IS MEASURED IN UNITS OF 10msec TICKS
C****
C****
C      EXTERNAL JPI$_CPUTIM
C      INTEGER*4 SYS$GETGPI
C      INTEGER*4 ICPU
C      INTEGER*4 ITEMLIST(4)
C      INTEGER*2 ITEMLST(8)
C      EQUIVALENCE( ITEMLIST(1), ITEMLST(1) )
C****
C****
C      ITEMLST(1)  = 4
C      ITEMLST(2)  = %LOC(JPI$_CPUTIM)
C      ITEMLST(7)  = 4
C      ITEMLST(8)  = 0
C      ITEMLIST(2) = %LOC(ICPU)
C      ITEMLIST(3) = 0
C****
C****
C      CALL SYS$GETJPI( , , , ITEMLIST, , , )
       ITCPU = 0
       RETURN
       END
