      SUBROUTINE E4COSZ(E4CLU,P4,TETIFL,XE4100,CORZ)
C===========================================================
C
C!    Routine to calculate zero suppression for e-id
C                                 Author : M.N Minard
C                                 Date   : 22/10/91
C     E4CLU     Input : Energy in 4 central tower
C     TETIFL    Input : Theta index of cluster
C     XE4100    Output: Expected fraction in 4 tower
C     CORZ      Output: Correction fator for zer suppression
C=============================================================
#ifndef DOC
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
      INTEGER ALGTDB, GTSTUP
      EXTERNAL ALGTDB, GTSTUP
      EXTERNAL NAMIND
      DIMENSION CELZE(6,8)
      CHARACTER DET*2,LIST*4
#include "e4par2.h"
      DATA DET    / 'EC' /
      DATA IROLD  / 0 /
      DATA NAELZE / 0 /
#include "bmacro.h"
C
C-    Initialise name bank at the beginning
C
      IF ( NAELZE.EQ.0 ) THEN
        NAELZE = NAMIND('ELZE')
        LIST = 'ELZE'
#include "gtdbbk.h"
        KELZE = IW(NAELZE)
        IF ( KELZE.NE.0 ) THEN
          NELZE = LROWS(KELZE)
          DO 5 IELZE= 1,NELZE
            JELZE = KROW(KELZE,IELZE)
            CALL UCOPY(RW(JELZE+1),CELZE(1,IELZE),6)
    5     CONTINUE
        ELSE
          IGAL = IW(NAMIND('AJOB'))
          IT =1
          IF( IGAL.NE.0) IT =2
          CALL UCOPY(CDEFZ(1,1,IT),CELZE(1,1),48)
        ENDIF
      ENDIF
C
C-    If bank exist use present code otherwise uses previous
C     corrections
C
      CORZ = 1.
      XE4100 = 1.
      IR = 8
      TET100 = TETIFL
      IF ( TETIFL.GT.IBARMD) TET100 = 229-TETIFL
      DO 10 IELZE = 1, 8
        IF(TET100.GE.CELZE(1,IELZE).AND.TET100.LT.CELZE(2,IELZE)
     &    ) IR = IELZE
   10 CONTINUE
      IF ( TETIFL.GE.IBARMN.AND.TETIFL.LT.IBARMX) THEN
        XE4100 = CELZE(3,IR)
        CORZ=CELZE(5,IR)+P4*CELZE(6,IR)
        IF(CORZ.LT.0)CORZ=0.
        CORZ = 1.+(CORZ/E4CLU)
      ELSE
        GG100 = CELZE(4,IR)
        XE4100 = CELZE(3,IR)
        CORZ=CELZE(5,IR)+P4*CELZE(6,IR)
        IF(CORZ.LT.0)CORZ=0.
        CORZ = 1.+(CORZ/E4CLU)
        LONTET = XLON1+XLON2*TET100+XLON3*TET100**2
        SURTET = SUR1*GG100*LONTET
        XE4100 = XE4100 + ALP100*(SURTET-SUR100)
      ENDIF
      RETURN
      END
#endif
