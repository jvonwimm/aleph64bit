      SUBROUTINE QEXTRA(PIN,ICH,XTRAP,IFIRS)
CKEY  FILL / INTERNAL
C ------------------------------------------------------------------------------
C! Extrapolates tracks in Ecal
C  Called from FIXRTRL
C      Author  : M.N. Minard                 10/11/93
C
C Input : PIN  (7)       = x.y.z.px,py,pz,p of track to extrapolate
C         ICH            = Track charge
C Output: XTRAP(3,3)     = x,y,z coordinates of extrapolated point
C                          in each stack
C         IFIRS          = 1 if track extrapolate S1 , 2 in S2
C ------------------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      DIMENSION XTRAP(3,3),XDER(3,3)
      DIMENSION EVECT(7),EVECE(7),PLAN(4),XX(3)
      DIMENSION ITOW (3),PIN(7),IFIR(3)
      DIMENSION RECAL(3), ZECAL(3)
      DIMENSION RHCAL(2), ZHCAL(2)
      INTEGER   EFNDSC , EFNDMD
      EXTERNAL  EFNDSC , EFNDMD
      DATA RECAL / 190.7, 204.1, 219.5/
      DATA ZECAL / 259.0, 273.7, 290.1/
#include "bmacro.h"
C ------------------------------------------------------------------------------
C-    Initialisation counters & arrays
C
      CALL VZERO ( XTRAP,9  )
      CALL VZERO ( XDER ,9  )
      CALL VZERO ( IFIR ,3  )
C
C-    Point to the track
C
      CALL UCOPY (PIN,EVECT(1),7)
C
C-    Extrapolation in electromagnetic calorimeter
C
      K = 1
 30   CONTINUE
C
      CH = ICH
      RHO = RECAL(K)
      ZED = ZECAL(K)
      CALL AUHCYL(RHO,ZED,QMFLD,CH,EVECT,EVECE,IH)
C
      IF ( IH.EQ.0 ) THEN
C-    Not valid extrapolation
            GO TO 50
      ENDIF

C
C-    Get subcomponent and module
C
      CALL UCOPY(EVECE(1),XX,3)
      KSC = EFNDSC(XX)
      IMD = EFNDMD(KSC,XX)
C
C-    Get equation of plane in the stack
C
      CALL EPLMST('ALEPH',KSC,IMD,K,PLAN)
C
C-    Find intersection with plane
C
      CALL QFNDIN(EVECE,PLAN,IERR)
      IF(IERR.EQ.1) GO TO 10
      IFIR (K) = IFIR (K) + 1
C
C-    Store stack extrapolation
C
C-    Extrapolated point
        CALL UCOPY (EVECE(1),XTRAP(1,K),3)
C-    Direction cosines
        CALL UCOPY (EVECE(4),XDER (1,K),3)
C
C-    Calculate new impact
C
        CALL UCOPY ( EVECE(1),EVECT(1),6 )
C
C
C-    Prepare next point
C
      IF ( K.NE.3) THEN
         K = K+1
         GO TO 30
      ENDIF
 50   CONTINUE

C
 10   CONTINUE
      IFIRS = 0
      IF ( IFIR(1).EQ.1) IFIRS = 1
      IF (IFIR(2).EQ.1) IFIRS = 2
      IFIRT = 0
      DO 11 I=1,3
       IFIRT = IFIR(I)+IFIRT
 11   CONTINUE
      IF ( IFIRS.EQ.0.AND.IFIRT.EQ.1) IFIRS =0
      RETURN
      END
#endif
