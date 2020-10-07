      SUBROUTINE ESWPST
C-----------------------------------------------------------------------
C! Correct ETDI  energies fors swapped stack cables in Barrel mod 6
C  Valid only for real data ( run # > 2000)
CKEY  ECAL  CLEAN  /  JULIA  USER
C                            B.Bloch-Devaux    5/02/90
C  Structure : SUBROUTINE
C              External references:NAMIND,WBANK,BKFRW,WDROP(BOS77)
C                                  UCOPY,VZERO(CERNLIB)
C                                  EITHR (This lib)
C              Comdecks references:BCS,BMACRO
C  Input : none
C  Output : none
C  Banks  : input  -  ETDI
C           output - JDETDI (work bank dropped at the end)
C  Action : swap back stacks which were so and update zero suppression
C           in Barrel #6 , for iphi=161 to 176
C                          stack 1 : itetha 131 swapped with itheta 133
C                                    itheta 132 swapped with itheta 134
C                          stack 2 : itheta 123 swapped with itheta 125
C                                    itheta 124 swapped with itheta 126
C-----------------------------------------------------------------------
#ifndef DOC
      COMMON /ESWLOC/ JDETDI
      INTEGER EITHR
      EXTERNAL EITHR
      DIMENSION TH(3)
      DIMENSION ITEMP(8,16),INDEX(8,16)
#include "bcs.h"
#include "rcurnt.h"
C  Thresholds  on barrel stacks in Kev
      DATA TH / 20000.,28000.,44000./
      DATA IFIR/0/
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF ( FMCRUN ) RETURN
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
C   Create JDETDI work bank with safety for missing towers
          LR = LROWS(JETDI)+4*16
          CALL WBANK( IW,JDETDI,LCOLS(JETDI)*LR+LMHLEN,*999)
          IW(JDETDI+LMHCOL) = LCOLS(JETDI)
          CALL VZERO(ITEMP,128)
          CALL VZERO(INDEX,128)
          NSWP = 0
C
C-    Loop on ETDI
C
      DO 100 IETDI = 1,NETDI
        IADDR = ITABL (JETDI,IETDI,1)
        ITET = IBITS (IADDR,16,8)
        IPHI = IBITS (IADDR,2,9)
        KETDI = KROW(JETDI,IETDI)
        IF (IPHI.GE.161 .AND. IPHI.LE.176) THEN
           IF (ITET.GE.123 .AND. ITET.LE.126) THEN
            ITEMP(ITET-122,IPHI-160) = ITABL(JETDI,IETDI,3)
            INDEX(ITET-122,IPHI-160) = IETDI
           ELSE IF ( ITET.GE.131 .AND.ITET.LE.134) THEN
            ITEMP(ITET-126,IPHI-160) = ITABL(JETDI,IETDI,2)
            INDEX(ITET-126,IPHI-160) = IETDI
           ELSE
            GO TO 122
           ENDIF
           NSWP = NSWP+1
           GO TO 100
       ENDIF
C          add that line to JDETDI bank
 122        KWORK = KNEXT(JDETDI)
            CALL UCOPY(IW(KETDI+1),IW(KWORK+1),LCOLS(JETDI))
            IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
100   CONTINUE
      IF (NSWP.EQ.0) GO TO 997
C   Now look at possibly swapped stacks
      DO 200 J=1,16
         DO 201 II=1,4
            I=II
            IS = 2
            IF(I.GE.3) THEN
              I=II+2
              IS = 1
            ENDIF
               IF (INDEX(I,J)*INDEX(I+2,J) .GT.0) THEN
C  Both here
                   K1 = KROW(JETDI,INDEX(I,J))
                   K2 = KROW(JETDI,INDEX(I+2,J))
                   IW(K1+IS+1) = ITEMP(I+2,J)
                   IW(K2+IS+1) = ITEMP(I,J)
C Check if each tower has at least one storey above on-line threshold
                   IOK1 = 0
                   IOK2 = 0
                   DO 202 K=1,3
                      IF (IW(K1+1+K).GE.TH(K)) IOK1=1
                      IF (IW(K2+1+K).GE.TH(K)) IOK2=1
 202               CONTINUE
                   IF ( IOK1.EQ.1) THEN
C          add both lines to JDETDI bank
                    KWORK = KNEXT(JDETDI)
                    CALL UCOPY(IW(K1+1),IW(KWORK+1),LCOLS(JETDI))
                    IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
                   ENDIF
                   IF ( IOK2.EQ.1) THEN
                    KWORK = KNEXT(JDETDI)
                    CALL UCOPY(IW(K2+1),IW(KWORK+1),LCOLS(JETDI))
                    IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
                   ENDIF
                ELSE IF (INDEX(I,J)+INDEX(I+2,J).GT.0) THEN
C   Only one channel is here
                   K12 = KROW(JETDI,INDEX(I,J)+INDEX(I+2,J))
                   IW(K12+IS+1) = 0
C Is tower still above threshold?
                    IF ( IW(K12+2).GE.TH(1) .OR. IW(K12+3).GE.TH(2) .OR.
     $              IW(K12+4).GE.TH(3)) THEN
                      KWORK = KNEXT(JDETDI)
                      CALL UCOPY(IW(K12+1),IW(KWORK+1),LCOLS(JETDI))
                      IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
                    ENDIF
C Is tower created with single stack above threshol?
                    IF ( ITEMP(I+2,J)+ITEMP(I,J) .GE.TH(IS) ) THEN
                      ITET = IBITS(IW(K12+1),16,8)
                      KWORK = KNEXT(JDETDI)
                      IF ( I.LE.2) THEN
                     IW(KWORK+1) = ISHFT(J+160,2)+ISHFT(246+2*I-ITET,16)
                      ELSE IF (I.GE.5) THEN
                     IW(KWORK+1) = ISHFT(J+160,2)+ISHFT(254+2*I-ITET,16)
                       ENDIF
                      IW(KWORK+1+IS) = ITEMP(I+2,J)+ITEMP(I,J)
                      IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
                    ENDIF
                  ENDIF
  201    CONTINUE
  200 CONTINUE
C   compress JDETDI and copy it to ETDI, then drop JDETDI
         LEN = LROWS(JDETDI)*LCOLS(JDETDI)+LMHLEN
         CALL WBANK (IW,JDETDI,LEN,*997)
         CALL BKFRW (IW,'ETDI',0,IW,JDETDI,*997)
 997  CALL WDROP (IW,JDETDI)
      ENDIF
      ENDIF
C
999   CONTINUE
C
      RETURN
      END
#endif
