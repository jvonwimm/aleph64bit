      SUBROUTINE NRELEP(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create PDLT+PMLT bank from NDCL bank
C!
C!   Author   :- Gerrit Graefe         10-FEB-1995
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created PDLT from NDCL
C!                                       1=can't find NDCL
C!                                       2=no space to book PDLT
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the banks PDLT (and PMLT in case of Monte Carlo
C?   from the NDCL bank.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      SAVE M2,M3,M4,M8,NERR1
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndcljj.h"
#include "ndtkjj.h"
      INTEGER NLINK,NVERS,NAMIND,NERR1
      INTEGER I,J,INDCL,INDPML,INDPDL,IGARB,IROWN,IROWPD,IROWPM,KRNDCL,
     &        KOPDLT,KOPMLT,IRET,INDTK,IROWT,IIASL,KOIASL,KCIASL
      INTEGER IPARTY,IREGCO,ITKPOI
      INTEGER M2,M3,M4,M8
      DATA    M2,M3,M4,M8/3,7,15,255/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDCL
C
      IRET=3
      INDCL=IW(NAMIND('NDCL'))
      IF(INDCL.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDCL=INDCL
      KCNDCL=IW(KONDCL+1)
      KRNDCL=IW(KONDCL+2)
C
C!..LINK TO BANK NDTK
C
      INDTK=IW(NAMIND('NDTK'))
      IF(INDTK.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      IROWT=INDTK+LMHLEN
C
C!..LINK TO BANK IASL IF NECCESSARY
C
      IF(XMCEV)THEN
        IIASL=IW(NAMIND('IASL'))
        IF(IIASL.EQ.0)THEN
          IF(NERR1.LT.10)THEN
            NERR1=NERR1+1
            CALL QWMESE
     &         ('+++ NRELEP +++ Bank IASL missing. Can`t link to FKIN')
          ENDIF
        ELSE
          KOIASL=IIASL
          KCIASL=IW(KOIASL+1)
        ENDIF
      ENDIF
C
C!..CREATE BANK PDLT
C
      CALL AUBOS('PDLT',0,LMHLEN+LPDLTA*KRNDCL,INDPDL,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NRELEP ### No space to book PDLT')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('PDLT','2I,(2I,2F,2I)')
      NAPDLT=NAMIND('PDLT')
      KOPDLT=INDPDL
      IW(KOPDLT+1)=LPDLTA
      IW(KOPDLT+2)=KRNDCL
C
C!..AND BANK PMLT IF MC- EVENT
C
      IF(XMCEV)THEN
        CALL AUBOS('PMLT',0,LMHLEN+LPMLTA*KRNDCL,INDPML,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL QWMESE('### NRELEP ### No space to book PMLT')
          IRET=2
          RETURN
        ENDIF
        CALL BKFMT('PMLT','2I,(8I)')
        NAPMLT=NAMIND('PMLT')
        KOPMLT=INDPML
        IW(KOPMLT+1)=LPMLTA
        IW(KOPMLT+2)=KRNDCL
      ENDIF
C
C!..FILL BANK PDLT
C
      DO 100 I=1,KRNDCL
        IROWN=KONDCL+LMHLEN+(I-1)*LNDCLA
        IROWPD=KOPDLT+LMHLEN+(I-1)*LPDLTA
        IROWPM=KOPMLT+LMHLEN+(I-1)*LPMLTA
C..
        IPARTY=IAND(IW(IROWN+JNDCTA),M2)
        IREGCO=IAND(ISHFT(IW(IROWN+JNDCTA),-2),M2)*10
        IF(IPARTY.EQ.0)THEN
          IPARTY=2
        ELSEIF(IPARTY.EQ.1)THEN
          IPARTY=3
        ELSEIF(IPARTY.EQ.2)THEN
          IPARTY=5
        ELSEIF(IPARTY.EQ.3)THEN
          IPARTY=6
        ENDIF
C..PARTICLE CODE
        IW(IROWPD+JPDLPA)=IPARTY+IREGCO
C..TRANSVERSE MOMENTA
        RW(IROWPD+JPDLPI)=FLOAT(IW(IROWN+JNDCPI)) / 10000000.0
        RW(IROWPD+JPDLPE)=FLOAT(IW(IROWN+JNDCPE)) / 10000000.0
C..VALIDITY
        IW(IROWPD+JPDLVP)=IAND(ISHFT(IW(IROWN+JNDCTA),-4),M2)*10
C..TRACK
        IW(IROWPD+JPDLFR)=IAND(ISHFT(IW(IROWN+JNDCTA),-6),M8)
C..JET
        ITKPOI=IROWT+(IW(IROWPD+JPDLFR)-1)*LNDTKA+JNDCTA
        IW(IROWPD+JPDLJT)=IAND(ISHFT(IW(ITKPOI),-11),M4)
C
C!..FILL BANK PMLT
C
        IF(XMCEV)THEN
C..QUARK FLAVOUR
          IW(IROWPM+JPMLFL)=IAND(ISHFT(IW(IROWN+JNDCTA),-14),M3)
C..POPPING FLAG
          IW(IROWPM+JPMLPO)=IAND(ISHFT(IW(IROWN+JNDCTA),-17),M3)
C..ICHAIN
          IW(IROWPM+JPMLCH)=IAND(ISHFT(IW(IROWN+JNDCTA),-20),M2)+1
C..SPLIT
          IW(IROWPM+JPMLSP)=IAND(ISHFT(IW(IROWN+JNDCTA),-22),M4)
C..LEPID
          IW(IROWPM+JPMLLE)=IAND(ISHFT(IW(IROWN+JNDCTA),-26),M3)
C..IBMES
          IW(IROWPM+JPMLME)=IAND(ISHFT(IW(IROWN+JNDCTA),-29),M3)-2
C..FRFT TRACK
          IW(IROWPM+JPMLFR)=IAND(ISHFT(IW(IROWN+JNDCTA),-6),M8)
C..KINE TRACK
          IF(IIASL.NE.0)THEN
            IW(IROWPM+JPMLKT)=IW(KOIASL+LMHLEN+(IW(IROWPM+JPMLFR)-1)
     &        *KCIASL+1)
          ENDIF
        ENDIF
  100 CONTINUE
      CALL BLIST(IW,'S+','PDLT')
      IF(XMCEV)CALL BLIST(IW,'S+','PMLT')
      IRET=0
  999 RETURN
      END
#endif
