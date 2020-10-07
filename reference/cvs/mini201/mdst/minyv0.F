      SUBROUTINE MINYV0
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill YV0V bank from DTRA.
C
C     Author: Stephen Haywood      03-Apr-90
C
C     V0 daughters are identified from DTRA and the YV0V bank is
C     reconstructed by calling YMFMIN.
C     This requires FRFT be rebuilt from DTRA.
C     DVER is required to determine number of main vertices.
C     DVRS is required to determine bank number for YV0V.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "dverjj.h"
#include "dtrajj.h"
#include "dvrsjj.h"
#include "yv0vjj.h"
#include "frftjj.h"
#include "minfac.h"
C
      DIMENSION VERT(3),PV0(3),PX(2),VERM(3,3),PXMS(2,2),PV1(3),PV2(3)
      DOUBLE PRECISION PVMAT(3,3),CPV(6,6)
C
#include "bmacro.h"
C
      KDTRA = NLINK('DTRA',100)
      IF (KDTRA.LE.0) THEN
         CALL MINUPD('DTRA')
         KDTRA = NLINK('DTRA',100)
         IF (KDTRA.LE.0) RETURN
      ENDIF
      NDTRA = LROWS(KDTRA)
      IF (NDTRA.LE.0) RETURN
C
      KDVER = NLINK('DVER',100)
      IF (KDVER.LE.0) THEN
         CALL MINUPD('DVER')
         KDVER = NLINK('DVER',100)
      ENDIF
      NDVER = 0
      IF (KDVER.GT.0) NDVER = LROWS(KDVER)
C
C++   Determine how many main vertices there are.
C
      NPYER = 0
      DO 10 I=1,NDVER
   10 IF(ITABL(KDVER,I,JDVEMV).EQ.1) NPYER = NPYER + 1
C
C++   Determine how many vertices there are by finding vertex bit-map
C++   with the highest bit set (with the largest numerical value).
C
      MXPAT = 0
      DO 20 I=1,NDTRA
         IPAT = ITABL(KDTRA,I,JDTRVB)
         IF (IPAT.GT.MXPAT) MXPAT = IPAT
   20 CONTINUE
C
      NVER = 0
      DO 30 I=1,32
   30 IF (JBIT(MXPAT,I).EQ.1) NVER = I
C
      NYV0V = NVER - NPYER
      IF (NYV0V.LE.0) RETURN
C
C++   Determine bank number of YV0V from DVRS.
C
      KDVRS = NLINK('DVRS',0)
      IF (KDVRS.GT.0) THEN
         NR = ITABL(KDVRS,1,JDVRV0)
      ELSE
         NR = 0
      ENDIF
C
C++   Create YV0V bank.
C
      LEN = LMHLEN + LYV0VA * NYV0V
      CALL AUBOS('YV0V',NR,LEN, KYV0V,IGARB)
      CALL BLIST(IW,'S+','YV0V')
      IF(IGARB.GE.2) THEN
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KDTRA = NLINK('DTRA',100)
      ENDIF
      IW(KYV0V+LMHCOL) = LYV0VA
      IW(KYV0V+LMHROW) = NYV0V
C
C++   Now fill links to the daughter tracks from bit pattern in DTRA.
C
      MAPMN = 2**NPYER
      DO 200 I=1,NDTRA
         MAPVB = ITABL(KDTRA,I,JDTRVB)
         IF(MAPVB.GT.0. .AND. MAPVB.LT.MAPMN) GOTO 200
         ICHRG = ITABL(KDTRA,I,JDTRCH)
         DO 250 IVER=NPYER+1,NVER
            MATCH = JBIT(MAPVB,IVER)
            IF(MATCH.NE.1) GOTO 250
            IYV0V = IVER - NPYER
            IF(ICHRG.GT.0) THEN
               IW(KROW(KYV0V,IYV0V)+JYV0K1) = I
            ELSE
               IW(KROW(KYV0V,IYV0V)+JYV0K2) = I
            ENDIF
  250    CONTINUE
  200 CONTINUE
C
C++   Check that each V0 has two daughters - if not, remove the V0.
C++   This is necessary because firstly the V0 is almost useless
C++   without the daughters; and secondly, certain routines which
C++   rely on the daughter tracks will crash in their absence.
C++   On the Cray, missing daughters can be a result of the decompress-
C++   ion code zeroing the vertex bit map when there are 32 vertices.
C
      NODAU = 0
      DO I=1,NYV0V
         K1 = ITABL(KYV0V,I,JYV0K1)
         K2 = ITABL(KYV0V,I,JYV0K2)
         IF (K1.LE.0 .OR. K2.LE.0) NODAU = NODAU + 1
      ENDDO
C
      IF (NODAU.NE.0) THEN
         IYV0V = 0
         DO 260 I=1,NYV0V
            K1 = ITABL(KYV0V,I,JYV0K1)
            K2 = ITABL(KYV0V,I,JYV0K2)
            IF (K1.LE.0 .OR. K2.LE.0) GOTO 260
            IYV0V = IYV0V + 1
            CALL UCOPY(IW(KROW(KYV0V,I)+1),IW(KROW(KYV0V,IYV0V)+1),2)
  260    CONTINUE
         NYV0V = IYV0V
         LEN = LMHLEN + LYV0VA * NYV0V
         CALL AUBOS('YV0V',NR,LEN, KYV0V,IGARB)
         IW(KYV0V+LMHROW) = NYV0V
      ENDIF
C
C++   Get link to FRFT - without FRFT, cannot do more.
C
      KFRFT = IW(NAMIND('FRFT'))
      IF (KFRFT.LE.0) RETURN
C
C++   Fill V0 quantities by calling YMFMIN.
C
      DO 300 I=1,NYV0V
C
C++      First step: find daughters and check different angles,
C++      otherwise can cause problems for YMFMIN.
C++      If necessary supply small shift to angles.
C
         K1 = ITABL(KYV0V,I,JYV0K1)
         K2 = ITABL(KYV0V,I,JYV0K2)
         IF (RTABL(KFRFT,K1,JFRFTL).EQ.RTABL(KFRFT,K2,JFRFTL)) THEN
            TANL = RTABL(KFRFT,K1,JFRFTL)
            RW(KROW(KFRFT,K1)+JFRFTL) = TANL + (1.+TANL**2) * 0.1/AFACTM
         ENDIF
         IF (RTABL(KFRFT,K1,JFRFP0).EQ.RTABL(KFRFT,K2,JFRFP0)) THEN
            PHI = RTABL(KFRFT,K1,JFRFP0)
            RW(KROW(KFRFT,K1)+JFRFP0) = PHI + 0.1/AFACTM
         ENDIF
C
         CALL YMFMIN(K1,K2, IV0NU,VERT,VERM,PV0,PV1,PV2,PVMAT,CPV,PX,
     &     PXMS,CHI2,NDF,CHIT,PSII,PSIJ,IER)
         CALL UCOPY(VERT,RW(KROW(KYV0V,I)+JYV0VX),3)
         RW(KROW(KYV0V,I)+JYV0VM+0) = VERM(1,1)
         RW(KROW(KYV0V,I)+JYV0VM+1) = VERM(1,2)
         RW(KROW(KYV0V,I)+JYV0VM+2) = VERM(2,2)
         RW(KROW(KYV0V,I)+JYV0VM+3) = VERM(1,3)
         RW(KROW(KYV0V,I)+JYV0VM+4) = VERM(2,3)
         RW(KROW(KYV0V,I)+JYV0VM+5) = VERM(3,3)
         CALL UCOPY(PV0,RW(KROW(KYV0V,I)+JYV0PX),3)
         RW(KROW(KYV0V,I)+JYV0PM+0) = SNGL(PVMAT(1,1))
         RW(KROW(KYV0V,I)+JYV0PM+1) = SNGL(PVMAT(1,2))
         RW(KROW(KYV0V,I)+JYV0PM+2) = SNGL(PVMAT(2,2))
         RW(KROW(KYV0V,I)+JYV0PM+3) = SNGL(PVMAT(1,3))
         RW(KROW(KYV0V,I)+JYV0PM+4) = SNGL(PVMAT(2,3))
         RW(KROW(KYV0V,I)+JYV0PM+5) = SNGL(PVMAT(3,3))
         RW(KROW(KYV0V,I)+JYV0X1) = PX(1)
         RW(KROW(KYV0V,I)+JYV0X2) = PX(2)
         RW(KROW(KYV0V,I)+JYV0XM+0) = PXMS(1,1)
         RW(KROW(KYV0V,I)+JYV0XM+1) = PXMS(1,2)
         RW(KROW(KYV0V,I)+JYV0XM+2) = PXMS(2,2)
         RW(KROW(KYV0V,I)+JYV0C2) = CHI2
         IW(KROW(KYV0V,I)+JYV0IC) = NDF
         CALL UCOPY(PV1,RW(KROW(KYV0V,I)+JYV0P1),3)
         CALL UCOPY(PV2,RW(KROW(KYV0V,I)+JYV0P2),3)
         L = 0
         DO 310 J=1,6
         DO 320 K=1,J
            RW(KROW(KYV0V,I)+JYV0EP+L) = SNGL(CPV(K,J))
            L = L + 1
  320   CONTINUE
  310   CONTINUE
        RW(KROW(KYV0V,I)+JYV0DM) = CHIT
        RW(KROW(KYV0V,I)+JYV0S1) = PSII
        RW(KROW(KYV0V,I)+JYV0S2) = PSIJ
  300 CONTINUE
C
      RETURN
      END
#endif
