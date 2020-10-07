      REAL FUNCTION QSTRU(ITK,I)
C***********************************************************************
C Find matching criteria of Ith alpha EF object matching input MC track
C or vice versa.
C If QSRTU returns >  0.,        it is energy shared between cluster and FKIN
C If QSRTU returns <  0., > -1., it is angle match for HCAL/SICAL and FKIN
C If QSRTU returns < -1.,        it is shared hits for charged track and FKIN
C track.
C If the input or returned EF object is KEFOTY 5,6 or 8, the match is angle.
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         O4/O3/97 PBT Hadronic objects are matched on angle (until PHMH bank
C                      restored to POTs).
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER II,JJ,KK,JT
      INTEGER IMCEF,IEFMC
#include "qmacro.h"
C***********************************************************************
      QSTRU = 0.
      IF( ITK.GE.KFEFT .AND. ITK.LE.KLEFT ) THEN
        JT = IEFMC(ITK)
        IF( JT.GT.0 .AND. JT.LE.MXEF .AND. I.LE.MXMTCH )
     +    QSTRU = SEFMC( JT, I )
      ELSE IF( ITK.GE.KFMCT .AND. ITK.LE.KLMCT ) THEN
        JT = IMCEF(ITK)
        IF( JT.GT.0 .AND. JT.LE.MXMC .AND. I.LE.MXMTCH )
     +    QSTRU = SMCEF( JT, I )
      ENDIF
C
 999  RETURN
      END
#endif
