*DK sairun
      SUBROUTINE SAIRUN
C-----------------------------------------------------------------------
C!    initialize common variables and Banks for the SATR
C                                H.Burkhardt October 1986
C                        modified for Data Base access October 87
C     Modified H. Meinhard 29-May-1990: Get alignment constants from
C                                       LALI and LSLO banks
C - modified by : B.Bloch-Devaux - 911015
C                 introduce SETUP dependence
C---------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "satrco.h"
#include "sanamc.h"
#include "alcons.h"
#include "iocom.h"
#include "jqcom.h"
      INTEGER  ALGTDB,GTSTUP
      EXTERNAL ALGTDB,GTSTUP
      EXTERNAL NAMIND
#include "bmacro.h"
C----------------------------------------
C     initialize variables for the SATR
C     only if SATR requested in the geometry before year 92
      ISAST = GTSTUP('PM',1)
      IF (ISAST.GE.1) GO TO  900
C     get SATR constants from the data base
C
      IND=ALGTDB(LRDBIO,'LALILSLOSATRSLAY',IRUNJO)
      IF(IND.EQ.0) THEN
        CALL ALTELL ('SAIRUN: no geometry banks from DBASE',0,'STOP')
        GOTO 900
      ENDIF
C     link to SATR bank
      KSATR=IW(NAMIND('SATR'))
      IF(KSATR.EQ.0) THEN
        WRITE(LOUTIO,'(/1X,''+++SAIRUN no SATR bank found'')')
        GOTO 900
      ENDIF
      KSA=KSATR+LMHLEN
C     link to SLAY bank
      KSLAY=IW(NAMIND('SLAY'))
      IF(KSLAY.EQ.0) THEN
        WRITE(LOUTIO,'(/1X,''+++SAIRUN no SLAY bank found'')')
        GOTO 900
      ENDIF
      NROW=LROWS(KSLAY)
      NCOL=LCOLS(KSLAY)
C     fill constants in common SATRCO
      DO 210 I=1,NROW
        PHIBSA(I)=RTABL(KSLAY,I,5)
  210 CONTINUE
      NLAYSA=IW(KSA+6)
      NSECSA=IW(KSA+7)
      NBRASA=IW(KSA+8)
      RMINSA=RW(KSA+11)
      RMAXSA=RW(KSA+12)
      BRTHSA=RW(KSA+13)
      DEADSA=RW(KSA+29)
      DGASSA=RW(KSA+31)
      NCRASA=IW(KSA+35)
      NCARSA=IW(KSA+36)
      NTDCSA=IW(KSA+37)
      NSIDSA=2
C     init counter variables
      NHITSA=0
      NLOSSA=0
      XHITSA=0.
      XLOSSA=0.
      XWIRSA=0.
      XEVTSA=0.
      DO 10 I1=1,9
        NHLASA(I1)=0
  10  CONTINUE
      DO 20 I2=1,9
        NHLDSA(I2)=0
  20  CONTINUE
C
C  Refer to output banks  only if SET SATR required
C
      IF (IDETJO(6).NE.0) THEN
          NASAHT=NAMIND('SAHT')
          NASADI=NAMIND('SADI')
          CALL BKFMT('SAHT','2I,(I)')
          CALL BKFMT('SADI','2I,(I)')
      ENDIF
 900  CONTINUE
      END
#endif
