C*******************************************************************************
C                        ZONE SELECTION SUBROUTINE
C*******************************************************************************
C
      SUBROUTINE MITRAY_ZONE (ZB,ZC,Z11,Z12,Z21,Z22,IZONE)
C
C     This subroutine determines which zone of the magnet we are in
C     (entrance fringe field, uniform region, exit fringe field, or 
c     overlapping entrance/exit fringe fields).
C
C     INPUT (REAL*8)
C     --------------
C     ZB, ZC        z-coordinate in the B and C axis systems
C     Z11, Z12      boundaries of entrance fringe field in B axis system
C     Z21, Z22      boundaries of exit fringe field in C axis system
c     
C     OUTPUT (INTEGER*4)
C     ------------------
C     IZONE         = 0 for entrance/exit far field
C                   = 1 for pure entrance fringe field (no overlap with 
c                       exit fringe field)
C                   = 2 for interior ("uniform field") region
C                   = 3 for pure exit fringe field (no overlap with
C                       entrance fringe field
C                   = 4 for case where the entrance and exit fringe fields
C                       overlap
C                   = -1  error
C
C
      IMPLICIT none
C
      REAL*8 ZB,ZC,Z11,Z12,Z21,Z22
      INTEGER*4 IZONE
      LOGICAL LENTR,LEXIT
C
      include 'gcflag.inc'          !geant
      include 'gcunit.inc'          !geant
C
      include 'mitray_diag.inc'     !local
      include 'diagnostic.inc'      !local
C
C     DEFAULT VALUE OF -1 FOR IZONE
      IZONE=-1
C
C----------------------------------------------------------------------------
C
C     Now figure out which zone we are in, from the B and C coordinates
c
C     Check for entrance/exit far zone
      IF(ZB.GT.Z11 .OR. ZC.GT.Z22)THEN
          IZONE=0
          IF(LDIAG)THEN
              WRITE(lout,*)'FAR ENTRANCE/EXIT REGION'
          ENDIF
          RETURN
      ENDIF
C
C     Check for uniform field zone
      IF(ZB.LE.Z12 .AND. ZC.LE.Z21)THEN
          IZONE=2
          IF(LDIAG)THEN
              WRITE(lout,*)'UNIFORM FIELD REGION'
          ENDIF
          RETURN      
      ENDIF
C
C     Check for entrance or exit fringe field
      IF(ZB.LE.Z11 .AND. ZB.GT.Z12)THEN
          LENTR=.TRUE.
          IZONE=1
      ELSE
          LENTR=.FALSE.
      ENDIF
      IF(ZC.LE.Z22 .AND. ZC.GT.Z21)THEN
          LEXIT=.TRUE.
          IZONE=3
      ELSE
          LEXIT=.FALSE.
      ENDIF
C      
C     For the special case of a very short magnet, where the entrance
c     and exit fringe fields overlap, and there is NO uniform field
c     region, both LENTR and LEXIT will be true from the above tests.
c     We signal this by setting IZONE=4
c
      IF(LENTR .AND. LEXIT)THEN
          IZONE=4
      ENDIF
C
C     LDIAG=.TRUE. means we want diagnostic printout at each step
C
      IF(IZONE.EQ.1)THEN
          IF(LDIAG)THEN
            WRITE(lout,*)'ENTRANCE FRINGE FIELD REGION'
          ENDIF
      ELSE IF(IZONE.EQ.3)THEN
          IF(LDIAG)THEN
            WRITE(lout,*)'EXIT FRINGE FIELD REGION'
          ENDIF
      ELSE IF(IZONE.EQ.4)THEN
          IF(LDIAG)THEN
            WRITE(lout,200)
200         FORMAT(' OVERLAPPING ENTRANCE & EXIT FRINGE FIELDS', 
     1      ' (SHORT MAGNET)'/
     2      ' TOTAL FIELD = ENTRANCE + EXIT - UNIFORM FIELD'//)
          ENDIF
      ELSE IF(IZONE.EQ.-1)THEN
          WRITE(lout,210)ZB,ZC,Z11,Z12,Z21,Z22
210       FORMAT(' **ERROR** IN GU_MITRAY_ZONE' /
     1    '           ERROR DETERMINING WHICH ZONE WE ARE IN'/
     2    '           ZB=',F12.4,'    ZC=',F12.4/
     3    '           Z11=',F12.4,'   Z12=',F12.4,'  Z21=',F12.4,
     4                'Z22=',F12.4)
          jstop  = 1
          ieotri = 1
      ENDIF
      RETURN
      END
