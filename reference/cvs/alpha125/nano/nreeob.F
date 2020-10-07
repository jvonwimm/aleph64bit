      SUBROUTINE NREEOB(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create EFOL bank from NDOB bank (and a pseudo NDTK
C!                  in case of MC events to have matching info)
C!
C!   Author   :- Gerrit Graefe         11-FEB-1995
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created EFOL from NDOB
C!                                       1=can't find NDOB
C!                                       2=no space to book EFOL
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the EFOL bank from the NDOB bank. If the
C?   event is a MC event and no NDTK bank is given it will be created
C?   with momenta taken from NDOB and set to a non-zero value to avoid
C?   problems with divisions. The main purpose is to have easy access
C?   to the matching information between MC - reco and MC - truth tracks.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE M4,M8
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndobjj.h"
#include "ndtkjj.h"
#include "ndmsjj.h"
      INTEGER NLINK,NVERS,NAMIND,NERR1
      INTEGER I,J,INDOB,INDEFO,IGARB,IROWN,IROWE,KRNDOB,
     &        KCEFOL,IRET,KOEFOL,ITK,IROWT,KRNDTK,INDTK,INDMS,INDNT
      INTEGER KRNDMS,ITK1,ITK2
      INTEGER M4,M8
      DATA    M4,M8/15,255/
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDOB
C
      IRET=3
      INDOB=IW(NAMIND('NDOB'))
      IF(INDOB.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDOB=INDOB
      KCNDOB=IW(KONDOB+1)
      KRNDOB=IW(KONDOB+2)
C
C!..CREATE BANK EFOL
C
      CALL AUBOS('EFOL',3,LMHLEN+LEFOLA*KRNDOB,INDEFO,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREEOB ### No space to book EFOL')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('EFOL','2I,(5F,6I)')
      NAEFOL=NAMIND('EFOL')
      KOEFOL=INDEFO
      IW(KOEFOL+1)=LEFOLA
      IW(KOEFOL+2)=KRNDOB
C
C!..FILL BANK EFOL
C
      ITK1=0
      DO 100 I=1,KRNDOB
        IROWN=KONDOB+LMHLEN+(I-1)*LNDOBA
        IROWE=KOEFOL+LMHLEN+(I-1)*LEFOLA
C..MOMENTA
        RW(IROWE+JEFOPX)=FLOAT(IW(IROWN+JNDOPX)) / 10000000.0
        RW(IROWE+JEFOPY)=FLOAT(IW(IROWN+JNDOPY)) / 10000000.0
        RW(IROWE+JEFOPZ)=FLOAT(IW(IROWN+JNDOPZ)) / 10000000.0
        RW(IROWE+JEFOEW)=FLOAT(IW(IROWN+JNDOEN)) / 10000000.0
C..TYPE
        IW(IROWE+JEFOTY)=IAND(IW(IROWN+JNDOTY),M4)
C..TRACK
        IW(IROWE+JEFOLT)=IAND(ISHFT(IW(IROWN+JNDOTY),-12),M8)
        IF(IW(IROWE+JEFOLT).GT.ITK1)ITK1=IW(IROWE+JEFOLT)
C..JET
        IW(IROWE+JEFOLJ)=IAND(ISHFT(IW(IROWN+JNDOTY), -4),M8)
C..SET OTHERS TO ZERO
        IW(IROWE+JEFOLH)=0
        IW(IROWE+JEFOLC)=0
        IW(IROWE+JEFOLE)=0
        RW(IROWE+JEFOWE)=0.0
  100 CONTINUE
      CALL BLIST(IW,'S+','EFOL')
C
C!..IF MC - EVENT THEN LOOK FOR NDTK BANK:
C
      INDNT=IW(NAMIND('NDNT'))
      INDMS=IW(NAMIND('NDMS'))
      IF(INDNT.NE.0.OR.INDMS.NE.0)THEN
C
C!..TRY TO FIND OUT NUMBER OF ROWS IN ORIGINAL NDTK BANK:
C
        KONDMS=INDMS
        KCNDMS=IW(KONDMS+1)
        KRNDMS=IW(KONDMS+2)
        ITK=0
        ITK2=0
        DO 160 I=1,KRNDMS
          IF(IAND(ISHFT(IW(INDMS+LMHLEN+(I-1)*KCNDMS+JNDMHI),-10),M8).
     &      GT.ITK2)ITK2=IAND(ISHFT(IW(INDMS+LMHLEN+(I-1)*KCNDMS+JNDMHI
     &      ),-10),M8)
  160   CONTINUE
        IF(ITK1.GT.ITK2)THEN
          ITK=ITK1
        ELSE
          ITK=ITK2
        ENDIF
        INDTK=0
        INDTK=IW(NAMIND('NDTK'))
        IF(INDTK.EQ.0)THEN
          IF(ITK.EQ.0)THEN
            IRET=4
            GOTO 999
          ENDIF
C
C!..IF NO NDTK BANK AVAILABLE, BOOK ONE
C
          CALL AUBOS('NDTK',1,LMHLEN+LNDTKA*ITK,INDTK,IGARB)
          IF(IGARB.EQ.2)THEN
             CALL QWMESE('### NREEOB ### No space to book NDTK')
            IRET=2
            RETURN
          ENDIF
          CALL BKFMT('NDTK','2I,(6I)')
          KONDTK=INDTK
          IW(KONDTK+1)=LNDTKA
          IW(KONDTK+2)=ITK
          IROWT=KONDTK
          DO 150 I=1,ITK*LNDTKA
            IW(IROWT+LMHLEN+I)=0
  150     CONTINUE
          DO 120 I=1,KRNDOB
            IROWN=KONDOB+LMHLEN+(I-1)*LNDOBA
            IF(IAND(ISHFT(IW(IROWN+JNDOTY),-12),M8).EQ.0)GOTO 120
            IROWT=KONDTK+LMHLEN+(IAND(ISHFT(IW(IROWN+JNDOTY),-12),M8)-1)
     &           *LNDTKA
C..MOMENTA
            IW(IROWT+JNDTPX)=IW(IROWN+JNDOPX)
            IW(IROWT+JNDTPY)=IW(IROWN+JNDOPY)
            IW(IROWT+JNDTPZ)=IW(IROWN+JNDOPZ)
            IW(IROWT+JNDTDZ)=0
            IW(IROWT+JNDTTA)=IAND(ISHFT(IW(IROWN+JNDOTY),-20),M8)
            IW(IROWT+JNDTTQ)=0
  120     CONTINUE
C..LOOK IF SOME TRACKS ARE NOT FILLED:
          DO 130 I=1,ITK
            IROWT=KONDTK+LMHLEN+(I-1)*LNDTKA
            IF(IW(IROWT+JNDTPX).EQ.0.AND.IW(IROWT+JNDTPY).EQ.0.AND.
     &         IW(IROWT+JNDTPZ).EQ.0)THEN
              IW(IROWT+JNDTPX)=1
              IW(IROWT+JNDTPY)=1
              IW(IROWT+JNDTPZ)=1
            ENDIF
  130     CONTINUE
          CALL BLIST(IW,'S+','NDTK')
        ENDIF
      ENDIF
      IRET=0
  999 RETURN
      END
#endif
