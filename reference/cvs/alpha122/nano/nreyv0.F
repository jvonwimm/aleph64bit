      SUBROUTINE NREYV0(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create YV0V bank from NDV0 bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created YV0V from NDV0
C!                                       1=can't find NDV0
C!                                       2=no space to book YV0V
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank YV0V from the NDV0 bank. As some
C?   quality cuts are made before filling the NDV0 bank while the NANO
C?   production some V0s avaliable on the DST or MINI are missing.
C?   Although no error matrices are available.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndv0jj.h"
#include "ndtkjj.h"
      INTEGER NLINK,NVERS,NAMIND
      INTEGER INDNV0,INDYV0,IGARB,KOYV0V,KRNDV0,INOV0,INOCH,IRET
      INTEGER IRONTK,INDNTK,KRNDTK,IROWN,IROWY
      INTEGER I,J,ITK1,ITK2
      INTEGER MASK8
      REAL    GP,GPHI,GTHETA
      DATA MASK8/255/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDV0
C
      IRET=3
      INDNV0=IW(NAMIND('NDV0'))
      IF(INDNV0.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDV0=INDNV0
      KCNDV0=IW(KONDV0+1)
      KRNDV0=IW(KONDV0+2)
C
C!..LINK TO BANK NDTK
C
      IRET=3
      INDNTK=IW(NAMIND('NDTK'))
      IF(INDNTK.EQ.0)THEN
        IRET=5
        RETURN
      ENDIF
      KONDTK=INDNTK
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
C
C!..CREATE BANK YV0V
C
      CALL AUBOS('YV0V',0,LMHLEN+LYV0VA*KRNDV0,INDYV0,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NRECYV ### No space to book YVV0')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('YV0V','2I,(2I,24F,1I,30F)')
      NAYV0V=NAMIND('YV0V')
      KOYV0V=INDYV0
      IW(KOYV0V+1)=LYV0VA
      IW(KOYV0V+2)=KRNDV0
C
C!..FILL BANK YV0V
C
      IROWN=KONDV0+LMHLEN
      IROWY=KOYV0V+LMHLEN
      DO 100 I=1,KRNDV0
C!..TRACKS
        ITK1=IAND(ISHFT(IW(IROWN+JNDVHY),-16),MASK8)
        ITK2=IAND(ISHFT(IW(IROWN+JNDVHY),-24),MASK8)
        DO 110 J=1,KRNDTK
          IRONTK=KONDTK+LMHLEN+(J-1)*LNDTKA
          IF (IAND(IW(IRONTK+JNDTTA),MASK8).EQ.ITK1) THEN
            IW(IROWY+JYV0K1)=J
          ENDIF
          IF (IAND(IW(IRONTK+JNDTTA),MASK8).EQ.ITK2) THEN
            IW(IROWY+JYV0K2)=J
          ENDIF
  110   CONTINUE
C!..VERTEX
        RW(IROWY+JYV0VX)=FLOAT(IW(IROWN+JNDVVX)) /   10000.0
        RW(IROWY+JYV0VY)=FLOAT(IW(IROWN+JNDVVY)) /   10000.0
        RW(IROWY+JYV0VZ)=FLOAT(IW(IROWN+JNDVVZ)) /   10000.0
C!..V0-MOMENTA
        GP    = FLOAT(IW(IROWN+JNDVVM)) / 10000000.0
        GPHI  = FLOAT(IW(IROWN+JNDVPH)) / 10000.0
        GTHETA= FLOAT(IW(IROWN+JNDVTH)) / 10000.0
        RW(IROWY+JYV0PX)=GP*SIN(GTHETA)*COS(GPHI)
        RW(IROWY+JYV0PY)=GP*SIN(GTHETA)*SIN(GPHI)
        RW(IROWY+JYV0PZ)=GP*COS(GTHETA)
C!..CHI2
        RW(IROWY+JYV0C2)=FLOAT(IW(IROWN+JNDVC2)) / 100.0
C!..MOMENTUM OF POSITIVE PARTICLE
        GP    = FLOAT(IW(IROWN+JNDVPP  )) / 10000000.0
        GTHETA= FLOAT(IW(IROWN+JNDVPP+1)) / 10000.0
        GPHI  = FLOAT(IW(IROWN+JNDVPP+2)) / 10000.0
        RW(IROWY+JYV0P1  )=GP*SIN(GTHETA)*COS(GPHI)
        RW(IROWY+JYV0P1+1)=GP*SIN(GTHETA)*SIN(GPHI)
        RW(IROWY+JYV0P1+2)=GP*COS(GTHETA)
C!..MOMENTUM OF NEGATIVE PARTICLE
        RW(IROWY+JYV0P2  )=RW(IROWY+JYV0PX)-RW(IROWY+JYV0P1  )
        RW(IROWY+JYV0P2+1)=RW(IROWY+JYV0PY)-RW(IROWY+JYV0P1+1)
        RW(IROWY+JYV0P2+2)=RW(IROWY+JYV0PZ)-RW(IROWY+JYV0P1+2)
C!..ICODE
        IW(IROWY+JYV0IC)=IAND(ISHFT(IW(IROWN+JNDVHY),-8),MASK8)-32
        IROWN=IROWN+LNDV0A
        IROWY=IROWY+LYV0VA
  100 CONTINUE
      CALL BLIST(IW,'S+','YV0V')
      IRET=0
  999 RETURN
      END
#endif
