      SUBROUTINE HNOISE
C***********************************************************
C! Generate a lookup table (NOICOD) for noisy Hcal         *
C! towers.  See HNOISY for definition of NOICOD.           *
C! NNOISE is filled with the number of noise objects       *
C! found in the event.                                     *
C! This routine should be called once per event.           *
C! The tower information is taken from the HSDA bank.      *
C!                                                         *
C! Author: S.Wasserbaech        061289                     *
C!                                                         *
C?
C!======================================================================
#ifndef DOC
#include "hnoicd.h"
#include "bcs.h"
#include "hsdajj.h"
      INTEGER NGPBA(192), NGPEC(120)
      INTEGER NOIBA(192), NOIEC(120)
      DATA N1MIN,N2MIN,N3MIN,N4MIN,N5MIN /30,18,12,15,10/
#include "bmacro.h"
C
      NNOISE = 0
C
C     Fill the noicod array with -1:
      CALL UFILL(NOICOD,1,5952,-1)
C
C     Look for HSDA bank:
      KHSDA = NLINK('HSDA',0)
      IF (KHSDA .EQ. 0) GO TO 1000
C
C     The HNGR bank contains one word per Hcal tower.
C     This word is given by
C     ITABL(KHNGR,ITH,IPH) = 1000*(endcap group #) + (barrel group #)
C     The group numbers refer to hardware groupings of the
C     towers relevant for noise identification.
C     The barrel group # is between 1 and 192 (or zero for
C     non-barrel towers).
C     The endcap group # is between 1 and 120 (or zero for
C     non-endcap towers).
C     Look for HNGR bank:
      KHNGR = NLINK('HNGR',0)
      IF (KHNGR .EQ. 0) THEN
        WRITE (6,'(A)') ' HNOISE> HNGR bank not found.'
        STOP
      ENDIF
C
C     Clear the counters and noise flags:
      CALL UZERO(NGPBA,1,192)
      CALL UZERO(NGPEC,1,120)
      CALL UZERO(NOIBA,1,192)
      CALL UZERO(NOIEC,1,120)
C
C     Now loop over the hit towers.
C     Count the hit towers in each group.
C     This code correctly handles multiple hits in a tower.
      DO 100 I=1,LROWS(KHSDA)
        IND0 = KROW(KHSDA,I)
        ITH = IW(IND0+JHSDTI)
        IPH = IW(IND0+JHSDPI)
        IF (NOICOD(ITH,IPH) .EQ. -1) THEN
          NOICOD(ITH,IPH) = 0
          IGR = ITABL(KHNGR,ITH,IPH)
          IGPBA = MOD(IGR,1000)
          IGPEC = IGR/1000
          IF (IGPBA .GT. 0) NGPBA(IGPBA) = NGPBA(IGPBA) + 1
          IF (IGPEC .GT. 0) NGPEC(IGPEC) = NGPEC(IGPEC) + 1
        ENDIF
  100 CONTINUE
C
C     Loop through all groups and identify noise objects.
C
C     For the barrel we consider the groups four at a time.
C     The noise objects are defined as follows:
C       Type 1: total # of hit towers in the four groups .ge. N1MIN;
C       Type 2: total # of hit towers in the first and
C                            fourth groups .ge. N2MIN;
C       Type 3: # of hit towers in the first group OR
C                            the fourth group .ge. N3MIN.
C     If the Type 1 condition is not satisfied, the Type 2
C     condition is checked.  If the Type 2 condition is not
C     satisfied, the Type 3 conditions are checked.
C     If a noise object of Type 1 or 2 is found, all four groups
C     are flagged.  Groups with Type 3 noise are flagged
C     individually.
C
C     For the endcaps we consider the groups ten at a time.
C     The noise objects are defined as follows:
C       Type 4: total # of hit towers in one of the pairs
C                 of groups (2+3, 4+5, 6+7, or 8+9) .ge. N4MIN;
C       Type 5: # of hit towers in any single group .ge. N5MIN.
C     The Type 4 condition is checked for the four pairs of
C     groups.  The Type 5 condition is checked for each of the
C     ten groups which are not involved in a Type 4 object.
C     If a noise object of Type 4 is found, the two involved
C     groups are flagged.  Groups with Type 5 noise are flagged
C     individually.
C
C     The arrays NOIBA and NOIEC receive the type number of
C     the noise in each group, or zero if no noise is found.
C
C     OK, here we go.  First the barrel:
C
      DO 200 IGP=1,192,4
        IG1 = IGP
        IG2 = IGP + 1
        IG3 = IGP + 2
        IG4 = IGP + 3
C
        N1234 = NGPBA(IG1) + NGPBA(IG2) + NGPBA(IG3) + NGPBA(IG4)
        N14 = NGPBA(IG1) + NGPBA(IG4)
        N1 = NGPBA(IG1)
        N4 = NGPBA(IG4)
C
        IF (N1234 .GE. N1MIN) THEN
C         Type 1 object found!
          NNOISE = NNOISE + 1
          NOIBA(IG1) = 1
          NOIBA(IG2) = 1
          NOIBA(IG3) = 1
          NOIBA(IG4) = 1
        ELSEIF (N14 .GE. N2MIN) THEN
C         Type 2 object found!
          NNOISE = NNOISE + 1
          NOIBA(IG1) = 2
          NOIBA(IG2) = 2
          NOIBA(IG3) = 2
          NOIBA(IG4) = 2
        ELSE
          IF (N1 .GE. N3MIN) THEN
C           Type 3 object found!
            NNOISE = NNOISE + 1
            NOIBA(IG1) = 3
          ENDIF
          IF (N4 .GE. N3MIN) THEN
C           Type 3 object found!
            NNOISE = NNOISE + 1
            NOIBA(IG4) = 3
          ENDIF
        ENDIF
  200 CONTINUE
C
C     Now the endcaps:
      DO 300 IGP=1,120,10
C
C       Loop over the four pairs of groups for Type 4:
        DO 210 IG=IGP+1,IGP+7,2
          IG1 = IG
          IG2 = IG + 1
          N12 = NGPEC(IG1) + NGPEC(IG2)
          IF (N12 .GE. N4MIN) THEN
C           Type 4 object found!
            NNOISE = NNOISE + 1
            NOIEC(IG1) = 4
            NOIEC(IG2) = 4
          ENDIF
  210   CONTINUE
C
C       Loop over all ten groups for Type 5, but skip the
C       ones with Type 4 already flagged:
        DO 220 IG=IGP,IGP+9
          IF ((NOIEC(IG) .EQ. 0) .AND. (NGPEC(IG) .GE. N5MIN)) THEN
C           Type 5 object found!
            NNOISE = NNOISE + 1
            NOIEC(IG) = 5
          ENDIF
  220   CONTINUE
  300 CONTINUE
C
C     Loop over the hit towers again.
C     Classify the towers in NOICOD.
      DO 310 I=1,LROWS(KHSDA)
        IND0 = KROW(KHSDA,I)
        ITH = IW(IND0+JHSDTI)
        IPH = IW(IND0+JHSDPI)
        IGR = ITABL(KHNGR,ITH,IPH)
        IGPBA = MOD(IGR,1000)
        IGPEC = IGR/1000
        IF (IGPBA .GT. 0) THEN
          IF (NOIBA(IGPBA) .GT. 0)
     >      NOICOD(ITH,IPH) = IBSET(NOICOD(ITH,IPH),NOIBA(IGPBA)-1)
        ENDIF
        IF (IGPEC .GT. 0) THEN
          IF (NOIEC(IGPEC) .GT. 0)
     >      NOICOD(ITH,IPH) = IBSET(NOICOD(ITH,IPH),NOIEC(IGPEC)-1)
        ENDIF
  310 CONTINUE
C
 1000 CONTINUE
      RETURN
      END
#endif
