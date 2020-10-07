      SUBROUTINE EFOLMK  (IFIT ,ESUMW , IER  )
C=======================================================================
C
C!    Built EFOL bank for E-Flow results
C!    Fill input common to ALTHRU for jet
C-    IFIT Input =1 Calo , 2 =E-Flow
C-    ESUMW Input Wire energy
C-    IER   Output error code =1 no jet to preform
C-                            -1 Jet array too small
C-                            -2 EFOL bank not lifted
C-    Bank created EFOL
C-    Bank used CHRL
C-
C-     Author  : M.N. Minard                 04/01/91
C?
C!======================================================================
#ifndef DOC
#include "jarray.h"
#include "efoljj.h"
#include "pjetef.h"
#include "bcs.h"
      DIMENSION INEL (12) , PVEC (4)
      DATA INEL / 1 , 1 , 0 , 0 , 0, 0, 0, 0,2,2,3,3/
#include "bmacro.h"

C
C-    Lift array to maximum value
C
      ETRACK = 0.
      IER = 0
      IF ( IFIT.EQ.2 ) THEN
      KEFOL = NDROP('EFOL',1)
      NLEN = NINDMX*LEFOLA+LMHLEN
      CALL AUBOS ('EFOL',1,NLEN,KEFOL,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 900
      CALL BLIST(IW,'E+','EFOL')
      IW(KEFOL+LMHCOL)  = LEFOLA
      IW(KEFOL+LMHROW) = 0
      ENDIF
      NEFOL = 0
      IFOL = 0
      DO 40 IELEM = 1,NINDMX
      IARRAY (2,IELEM) = 0
      IEL = IARRAY(3,IELEM)
      ITYPC = MOD (IARRAY(1,IELEM),100)
      ITYP  = IARRAY(1,IELEM)/100
C-    Extract theta dependent weights
C
      CALL WCAL(ITYP,XARRAY(1,IELEM),WEIGT,IFIT)
      IF(WEIGT.LT.0.0001) GO TO 40
      CALL UCOPY(XARRAY(1,IELEM),PVEC(1),4)
      IF (ITYP .GT .12 )   GO TO 40
C
C-     Search position in EFOL bank
C
C
      IF ( IFIT.EQ.2) THEN
        IFOL  = 0
        IF ( NEFOL.NE.0 ) THEN
          DO 25 IEFOL = 1,NEFOL
          IF (ITABL(KEFOL,IEFOL,JEFOTY).EQ.ITYPC) THEN
            ITI = ITABL(KEFOL,IEFOL,JEFOLJ)
            IF(INEL(ITI).EQ.INEL(ITYP)) THEN
              IF(ITABL(KEFOL,IEFOL,JEFOLE+INEL(ITYP)).EQ.IEL) THEN
                IFOL = IEFOL
              ENDIF
            ENDIF
          ENDIF
  25      CONTINUE
        ENDIF
        IF ( IFOL .EQ.0 ) THEN
          NEFOL = NEFOL + 1
          IFOL = NEFOL
          IW(KROW(KEFOL,IFOL)+JEFOTY ) = ITYPC
          IW(KROW(KEFOL,IFOL)+JEFOLJ ) = ITYP
          IW(KROW(KEFOL,IFOL)+JEFOLE+INEL(ITYP))=IEL
        ENDIF
        JEFOL = KROW(KEFOL,IFOL)
      ENDIF
C
C-       Store vector in EFOL bank
C
      IF (IFIT.EQ.2) THEN
        RW(JEFOL+JEFOPX) = RW(JEFOL+JEFOPX)+PVEC(1)*WEIGT
        RW(JEFOL+JEFOPY) = RW(JEFOL+JEFOPY)+PVEC(2)*WEIGT
        RW(JEFOL+JEFOPZ) = RW(JEFOL+JEFOPZ)+PVEC(3)*WEIGT
        RW(JEFOL+JEFOEW) = RW(JEFOL+JEFOEW)+PVEC(4)*WEIGT
        RW(JEFOL+JEFOWE) = RW(JEFOL+JEFOWE)+PVEC(4)
      ENDIF
      IF(ITYP.EQ.1.OR.ITYP.EQ.2) ETRACK = ETRACK + PVEC(4)
C
C-    Calculate total energy for each fit
C
      EWEI = PVEC(4)*WEIGT
      VRECNF(4,IFIT) = VRECNF(4,IFIT)+EWEI
      VRECNF(5,IFIT) = VRECNF(5,IFIT)
     &                  + EWEI*(SQRT(PVEC(1)**2+PVEC(2)**2)/PVEC(4))
C
      DO 35 I1 = 1,3
      VRECNF(I1,IFIT) = VRECNF(I1,IFIT)+PVEC(I1)*WEIGT
 35   CONTINUE
 40   CONTINUE
      IF (IFIT.EQ.2) THEN
C
C-    Close EFOL  bankd
C
        IW(KEFOL+LMHROW) = NEFOL
        CALL AUBOS('EFOL',1,LMHLEN+NEFOL*LEFOLA,KEFOL,IGARB)
        KCHRL = IW(NAMIND('CHRL'))
        NCHRL = 0
        IF ( KCHRL.NE.0) NCHRL = LROWS(KCHRL)
C
C-    Now update # pointers & prepare Jet buffer
C
        DO 70 IEFOL = 1, NEFOL
        JEFOL = KROW(KEFOL,IEFOL)
        WEIG = RW(JEFOL+JEFOEW)/RW(JEFOL+JEFOWE)
        RW(JEFOL+JEFOWE) = WEIG
        IW(JEFOL+JEFOLJ) = 0
C
C-    Now update calobject for all
C
        DO 60 IL=1,3
        IF ( ITABL(KEFOL,IEFOL,JEFOTY+IL).NE.0) THEN
          IELEM = ITABL(KEFOL,IEFOL,JEFOTY+IL)
          DO 55 ICHRL = 1, NCHRL
          IF(ITABL(KCHRL,ICHRL,1+IL).EQ.IELEM)THEN
            IW(KROW(KEFOL,IEFOL)+JEFOLC)=ITABL(KCHRL,ICHRL,1)
            IF ( IL.EQ.1.AND.ITABL(KEFOL,IEFOL,JEFOTY).EQ.1) THEN
C
C-            Search for corresponding electron track
C
              CALL EFTIEL(ITABL(KEFOL,IEFOL,JEFOLE),ITRAK)
              IW(KROW(KEFOL,IEFOL)+JEFOLT) = ITRAK
            ENDIF
          ENDIF
 55       CONTINUE
        ENDIF
 60     CONTINUE
 70     CONTINUE
      ENDIF
      IF ( VRECNF(4,IFIT).GT.200.AND.ESUMW.LT.0.1*VRECNF(4,IFIT))
     &  GO TO 980
C
C-    Fill jet Array
C
      WI = 1.
      IRANG = 0
      DO 80  IEFOL = 1, NEFOL
      JEFOL = KROW(KEFOL,IEFOL)
      CALL UCOPY (RW(JEFOL+JEFOPX) , PVEC(1), 4)
      CALL PVFILL (IFIT,PVEC(1),WI,IRANG,ISTOP)
      IW(JEFOL+JEFOLJ) = IRANG
      IF (ISTOP.NE.0) GO TO 999
 80   CONTINUE
      RETURN
 999  CALL RERROR ('EFOLMK',1,'ARRAY JETSET TOO SMALL')
      IER = -1
      RETURN
 900  CALL RERROR ('EFOLMK',1,' NOT ENOUGH SPACE FOR EFOL BANK')
      IER = -2
      RETURN
 980  IER = 1
      RETURN
      END
#endif
