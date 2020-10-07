      SUBROUTINE MUIDO(IER)
C----------------------------------------------------------------------
C
CKEY MUONID MUON IDENTIFICATION / INTERNAL
C
C!  - Official ALEPH Muon ID routine
C!    Brings together HCAL and MUON chamber information
C!    Treats shadowing
C!
C!    Author    - G.Taylor              15-MAY-1992
C!
C!    Outputs :  MUID bos bank
C!              IER /I = Error flag
C!                     = 0 ok
C!                     = 1,2 garbage collection
C!
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hmadjj.h"
#include "mcadjj.h"
#include "muidjj.h"
      LOGICAL MSOLMC
      DATA NHMAD,NMCAD,NFRFT,NHROA,NMHIT,NMTHR/6*0/
#include "bmacro.h"
C----------------------------------------------------------------------
      IF(NHMAD.EQ.0) THEN
        NHMAD = NAMIND('HMAD')
        NMCAD = NAMIND('MCAD')
        NFRFT = NAMIND('FRFT')
        NHROA = NAMIND('HROA')
        NMHIT = NAMIND('MHIT')
        NMTHR = NAMIND('MTHR')
        CALL BKFMT('MUID','2I,(I,2F,2I)')
        CALL BKFMT('D4CD','I')
      ENDIF
      IER = 0
C
C Make official ID for all tracks
C
      KHMAD = IW(NHMAD)
      KMCAD = IW(NMCAD)
      IF(KHMAD.LE.0.AND.KMCAD.EQ.0) THEN
       NMU=0
      ELSE
       IF(KHMAD.GT.0) NMU=LROWS(KHMAD)
       IF(KMCAD.GT.0) NMU=LROWS(KMCAD)
       IF(KHMAD.GT.0.AND.KMCAD.GT.0) NMU=LROWS(KHMAD)+LROWS(KMCAD)
      ENDIF
C
      CALL AUBOS('MUID',0,NMU*LMUIDA+LMHLEN,KMUID,IGARB)
      IF(IGARB.EQ.2) THEN
        IER=1
        RETURN
      ELSE IF(IGARB.EQ.1) THEN
        KHMAD = IW(NHMAD)
        KMCAD = IW(NMCAD)
      ENDIF
C
      IW(KMUID+LMHCOL)=LMUIDA
      IW(KMUID+LMHROW)=NMU
      IF(NMU.EQ.0) RETURN
C
      KMTHR = IW(NMTHR)
      KMHIT = IW(NMHIT)
      KFRFT = IW(NFRFT)
C
      NMUON=0
      DO 40 KTK = 1,LROWS(KFRFT)
        IBE = 0
        IBT = 0
        MULT = 0
        INHMAD=0
        IF (KHMAD.LE.0) GOTO 11
        DO 10 IMAD = 1,LROWS(KHMAD)
          IF (KTK.EQ.ITABL(KHMAD,IMAD,JHMATN)) THEN
            INHMAD=1
            IBE = ITABL(KHMAD,IMAD,JHMAIE)
            IBT = ITABL(KHMAD,IMAD,JHMAIT)
            MULT = ITABL(KHMAD,IMAD,JHMAMH)
            GOTO 11
          ENDIF
   10   CONTINUE
   11   CONTINUE
C
        IM1 = 0
        IM2 = 0
        RAPP=-1.
        DIST=-1.
        ANG=-1.
        INMHIT=0
        IF (KMCAD.LE.0) GOTO 21
        DO 20 ICAD = 1,LROWS(KMCAD)
          IF (KTK.EQ.ITABL(KMCAD,ICAD,JMCATN)) THEN
            INMHIT=1
            IM1 = ITABL(KMCAD,ICAD,JMCANH)
            IM2 = ITABL(KMCAD,ICAD,JMCANH+1)
            R1=1000.
            R2=1000.
            D1=1000.
            D2=1000.
            IF (IM1.GT.0.AND.RTABL(KMCAD,ICAD,JMCADC).GT..0001)THEN
              D1=RTABL(KMCAD,ICAD,JMCADH)
              R1=RTABL(KMCAD,ICAD,JMCADH)/RTABL(KMCAD,ICAD,JMCADC)
            ENDIF
            IF (IM2.GT.0.AND.RTABL(KMCAD,ICAD,JMCADC+1).GT..0001)THEN
              D2=RTABL(KMCAD,ICAD,JMCADH+1)
              R2=RTABL(KMCAD,ICAD,JMCADH+1)/RTABL(KMCAD,ICAD,JMCADC+1)
            ENDIF
            IF(IM1.GT.0.AND.IM2.GT.0.AND.RTABL(KMCAD,ICAD,JMCAAC).GT.
     &           0.00001)
     &           ANG=RTABL(KMCAD,ICAD,JMCAAM)/RTABL(KMCAD,ICAD,JMCAAC)
            IF(MIN(R1,R2).LT.999.) RAPP=MIN(R1,R2)
            IF(MIN(D1,D2).LT.999.) DIST=MIN(D1,D2)
            GOTO 21
          ENDIF
   20   CONTINUE
   21   CONTINUE
C
        NT = 0
        NL10 = 0
        IBEF = 0
        NH03 = 0
        DO 30 LAY = 22,0,-1
          IBEF = IBITS(IBE,LAY,1) + IBEF
          IBTF = IBITS(IBT,LAY,1)
          IF (IBTF.NE.0) THEN
            IF (IBEF.LE.3) NH03 = NH03 + 1
            IF (IBEF.LE.10) NL10 = NL10 + 1
            NT = NT + 1
          ENDIF
   30   CONTINUE
        IF(NL10.GT.10) NL10=10
        IF(NH03.GT.3) NH03=3
C
C Simple IDF HCAL, MUON, and .AND.
C
        IDF = 0
        CALL MUIDF(IBEF,NT,IM1,IM2,NL10,NH03,MULT,0,RAPP,ANG,IDF)
        IF((INHMAD.EQ.1.OR.INMHIT.EQ.1).AND.IDF.NE.0) THEN
          NMUON=NMUON+1
          IW(KMUID+LMHLEN+LMUIDA*(NMUON-1)+JMUIIF)=IDF
          RW(KMUID+LMHLEN+LMUIDA*(NMUON-1)+JMUISR)=HGSUDN(KTK)
          RW(KMUID+LMHLEN+LMUIDA*(NMUON-1)+JMUIDM)=DIST
          IW(KMUID+LMHLEN+LMUIDA*(NMUON-1)+JMUIST)=0
          IW(KMUID+LMHLEN+LMUIDA*(NMUON-1)+JMUITN)=KTK
        ENDIF
   40 CONTINUE
C
      CALL AUBOS('MUID',0,NMUON*LMUIDA+LMHLEN,KMUID,IGARB)
      IF(IGARB.NE.0) THEN
        IER=2
        RETURN
      ENDIF
C
      IW(KMUID+LMHCOL)=LMUIDA
      IW(KMUID+LMHROW)=NMUON
C
C Check this track for shadowing
C
      DO 60 INMU1=1,LROWS(KMUID)-1
        ID1= IW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUIIF)
        IT1= IW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUITN)
        SUDN1=RW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUISR)
        DIST1=RW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUIDM)
        IF(ID1.LE.0) GOTO 60
        DO 61 INMU2=INMU1+1,LROWS(KMUID)
          ID2= IW(KMUID+LMHLEN+LMUIDA*(INMU2-1)+JMUIIF)
          IT2= IW(KMUID+LMHLEN+LMUIDA*(INMU2-1)+JMUITN)
          SUDN2=RW(KMUID+LMHLEN+LMUIDA*(INMU2-1)+JMUISR)
          DIST2=RW(KMUID+LMHLEN+LMUIDA*(INMU2-1)+JMUIDM)
          IF(ID2.LE.0) GOTO 61
          ID1= IW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUIIF)
          IF(ID1.LE.0) GOTO 61
          IF (MSHAD(IT1,IT2).EQ.1) THEN
C look for muon chamber hits in each layer for each track
            IW(KMUID+LMHLEN+LMUIDA*(INMU1-1)+JMUIST)=IT2
            IW(KMUID+LMHLEN+LMUIDA*(INMU2-1)+JMUIST)=IT1
            IM1=0
            IM2=0
            IM3=0
            IM4=0
            DO 63 ICAD = 1,LROWS(KMCAD)
              IF (IT1.EQ.ITABL(KMCAD,ICAD,JMCATN)) THEN
                IM1 = ITABL(KMCAD,ICAD,JMCANH)
                IM2 = ITABL(KMCAD,ICAD,JMCANH+1)
              ENDIF
              IF (IT2.EQ.ITABL(KMCAD,ICAD,JMCATN)) THEN
                IM3 = ITABL(KMCAD,ICAD,JMCANH)
                IM4 = ITABL(KMCAD,ICAD,JMCANH+1)
              ENDIF
   63       CONTINUE
            IDF1=0
            IF(MOD(ABS(ID1),10).EQ.1.OR.
     &         MOD(ABS(ID1),10).EQ.3.OR.
     &         MOD(ABS(ID1),10).EQ.4) IDF1=1
            IF(IM1.NE.0) IDF1=IDF1+2
            IF(IM2.NE.0) IDF1=IDF1+2
            IDF2=0
            IF(MOD(ABS(ID2),10).EQ.1.OR.
     &         MOD(ABS(ID2),10).EQ.3.OR.
     &         MOD(ABS(ID2),10).EQ.4) IDF2=1
            IF(IM3.NE.0) IDF2=IDF2+2
            IF(IM4.NE.0) IDF2=IDF2+2
C
C Solve Shadow ambiguity
C
            CALL MSOLSH(IT1,IDF1,DIST1,SUDN1,D,IT2,IDF2,DIST2,SUDN2,D,
     &        IGT)
            IF(IGT.EQ.IT1) THEN
              IBTRAC=IT2
              INDB=INMU2
            ELSE
              IBTRAC=IT1
              INDB=INMU1
            ENDIF
            IF(.NOT.MSOLMC(IBTRAC))THEN
              IW(KMUID+LMHLEN+LMUIDA*(INDB-1)+JMUIIF) =
     &                -IW(KMUID+LMHLEN+LMUIDA*(INDB-1)+JMUIIF)
              GOTO 61
            ENDIF
          ENDIF
   61   CONTINUE
   60 CONTINUE
C
      RETURN
      END
#endif
