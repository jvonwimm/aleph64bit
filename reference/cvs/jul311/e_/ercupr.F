      SUBROUTINE ERCUPR
C-----------------------------------------------------------------------
C! Correct ETDI  energies for bug in gains related to missing wires
CKEY  ECAL  CLEAN  /  JULIA  USER
C                            B.Bloch-Devaux    25/10/89
C  Structure : SUBROUTINE
C              External references:NAMIND,WBANK,BKFRW,WDROP(BOS77)
C                                  UCOPY(CERNLIB)
C                                  EITHR (This lib)
C              Comdecks references:BCS,BMACRO
C  Input : none
C  Output : none
C  Banks  : input  -  ETDI
C           output - JDETDI (work bank dropped at the end)
C  Action : removes from ETDI  addresses from storeys below
C          threshold after gain correction
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON /RLOCAL/ JDETDI
      INTEGER EITHR
      EXTERNAL EITHR
      DIMENSION TH(3)
      COMMON/EFA/FACTOR(128,384,3),ICOR
#include "bcs.h"
C  Thresholds  on barrel stacks in Mev
      DATA TH / 20.,28.,44./
      DATA IFIR/0/
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF (ICOR.EQ.0) RETURN
C
      IF (IFIR.EQ.0) THEN
         IFIR = 1
         JDETDI = 0
         NAETDI = NAMIND('ETDI')
         ITB = 114
         IPB = 192
C   Get Barrel on line thresholds for 3 stacks (Mev)
         DO 10 IS = 1,3
 10      TH(IS)=EITHR(ITB,IPB,IS)*1.E-03
      ENDIF
C
      JETDI = IW(NAETDI)
      IF ( JETDI.GT.0) THEN
        NETDI = LROWS(JETDI)
        IF ( NETDI.GT.0) THEN
C   Create JDETDI work bank
          CALL WBANK (IW,JDETDI,LCOLS(JETDI)*LROWS(JETDI)+LMHLEN,*999)
          IW(JDETDI+LMHCOL) = LCOLS(JETDI)
C
C-    Loop on ETDI
C
      DO 100 IETDI = 1,NETDI
        IADDR = ITABL (JETDI,IETDI,1)
        ITET = IBITS (IADDR,16,8)
        IPHI = IBITS (IADDR,2,9)
        KETDI = KROW(JETDI,IETDI)
        IF (ITET.LE.50 .OR. ITET.GT.178) GO TO 122
          IOK = 0
          DO 101 I = 1,3
            ISTK = I
            IEN = ITABL(JETDI,IETDI,1+ISTK)
            ESRAW = FLOAT(IEN) * 1.E-03
            ESCOR = ESRAW*FACTOR(ITET-50,IPHI,ISTK)
            IF (ESCOR.GE.TH(I)) IOK = 1
            IW(KETDI+1+ISTK) = IFIX(ESCOR*1000.)
 101      CONTINUE
          IF (IOK.NE.1) GO TO 100
C          add that line to JDETDI bank
 122        KWORK = KNEXT(JDETDI)
            CALL UCOPY(IW(KETDI+1),IW(KWORK+1),LCOLS(JETDI))
            IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
100   CONTINUE
C   compress JDETDI and copy it to ETDI, then drop JDETDI
      IF (LROWS(JDETDI).LT.LROWS(JETDI)) THEN
         LEN = LROWS(JDETDI)*LCOLS(JDETDI)+LMHLEN
         CALL WBANK (IW,JDETDI,LEN,*997)
         CALL BKFRW (IW,'ETDI',0,IW,JDETDI,*997)
      ENDIF
 997  CALL WDROP (IW,JDETDI)
      ENDIF
      ENDIF
C
999   CONTINUE
C
      RETURN
      END
#endif
