      SUBROUTINE NREPGP(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create PGPC bank from NDPH bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!   Modified :- Gerrit Graefe         27-FEB-1995  If photons were
C!                                                  taken from PGAC,
C!                                                  create PGAC
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created PGPC from NDPH
C!                                       1=can't find NDPH
C!                                       2=no space to book PGPC
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank PGPC from the NDPH bank. Only few
C?   numbers from PGPC are stored on NDPH so the recreated PGPC contains
C?   not much more than the momenta.
C?
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndphjj.h"
#include "nehejj.h"
      INTEGER NLINK,NVERS,NAMIND,NAVERS
      INTEGER I,J,INDPH,INDPGP,IGARB,IROWN,IROWP,KRNDPH,KOPGPC,IRET,
     &        INEHE,IBK,KOPGAC
      INTEGER IGOR,ILEN,IGEC,IGPC,IGTC,IGQU,IGPE
      REAL    GP(3)
      INTEGER MASK2,MASK8
      DATA MASK2,MASK8/3,255/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDPH
C
      IRET=3
      INDPH=IW(NAMIND('NDPH'))
      IF(INDPH.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDPH=INDPH
      KCNDPH=IW(KONDPH+1)
      KRNDPH=IW(KONDPH+2)
C
C!..LINK TO BANK NEHE
C
      CALL GETNAV(NAVERS)
      IF(NAVERS.GT.115)THEN
        INEHE=IW(NAMIND('NEHE'))
        IF(INEHE.EQ.0)THEN
          CALL QWMESE(
     &      '### NRECPG ### Can`t find NEHE. Will create PGAC bank !')
          IBK=1
        ELSE
          IBK=IAND(ISHFT(IW(INEHE+LMHLEN+JNEHPF),-30),1)
        ENDIF
      ELSE
        IBK=0
      ENDIF
C
C!..FILL POINTERS ACCORDING TO BANK
C
      IF(IBK.EQ.0)THEN
C
C!..CREATE BANK PGPC
C
        CALL AUBOS('PGPC',0,LMHLEN+LPGPCA*KRNDPH,INDPGP,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL QWMESE('### NRECPG ### No space to book PGPC')
          IRET=2
          RETURN
        ENDIF
        CALL BKFMT('PGPC','2I,(8F,1I,8F,1I)')
        NAPGPC=NAMIND('PGPC')
        KOPGPC=INDPGP
        IW(KOPGPC+1)=LPGPCA
        IW(KOPGPC+2)=KRNDPH
        CALL BLIST(IW,'S+','PGPC')
        IGEC=JPGPEC
        IGTC=JPGPTC
        IGPC=JPGPPC
        IGPE=JPGPPE
        IGQU=JPGPQU
        IGOR=KOPGPC
        ILEN=LPGPCA
      ELSE
C
C!..CREATE BANK PGAC
C
        CALL AUBOS('PGAC',0,LMHLEN+LPGACA*KRNDPH,INDPGP,IGARB)
        IF(IGARB.EQ.2)THEN
          CALL QWMESE('### NRECPG ### No space to book PGAC')
          IRET=2
          RETURN
        ENDIF
        CALL BKFMT('PGAC','2I,(7F,2I,13F,3I)')
        NAPGAC=NAMIND('PGAC')
        KOPGAC=INDPGP
        IW(KOPGAC+1)=LPGACA
        IW(KOPGAC+2)=KRNDPH
        CALL BLIST(IW,'S+','PGAC')
        IGEC=JPGAEC
        IGTC=JPGATC
        IGPC=JPGAPC
        IGPE=JPGAPE
        IGQU=JPGAQU
        IGOR=KOPGAC
        ILEN=LPGACA
      ENDIF
C
C!..FILL BANK PG.C
C
      DO 100 I=1,KRNDPH
        IROWN=KONDPH+LMHLEN+(I-1)*LNDPHA
        IROWP=IGOR+LMHLEN+(I-1)*ILEN
C!..MOMENTA
        GP(1)=FLOAT(IW(IROWN+JNDPPX)) / 10000000.0
        GP(2)=FLOAT(IW(IROWN+JNDPPY)) / 10000000.0
        GP(3)=FLOAT(IW(IROWN+JNDPPZ)) / 10000000.0
        RW(IROWP+IGEC)=SQRT(GP(1)*GP(1)+GP(2)*GP(2)+GP(3)*GP(3))
        RW(IROWP+IGTC)=ACOS(GP(3) / RW(IROWP+IGEC))
        RW(IROWP+IGPC)=ATAN2(GP(2),GP(1)) + (QQPI-SIGN(QQPI,GP(1)))
C!..POINTER TO PECO BANK
C!..AS PECO WILL BE NOT AVALIABLE ON A DST MADE FROM A NANO THE FOLLOWING
C!..LINE IS COMMENTED OUT.
        IW(IROWP+IGPE)=IAND(IW(IROWN+JNDPPA),MASK8)
C!..CRACK
        IF(IAND(ISHFT(IW(IROWN+JNDPPA), -8),MASK2).EQ.0) THEN
          IW(IROWP+IGQU)=1
        ELSE
          IW(IROWP+IGQU)=0
        ENDIF
  100 CONTINUE
      IRET=0
  999 RETURN
      END
#endif
