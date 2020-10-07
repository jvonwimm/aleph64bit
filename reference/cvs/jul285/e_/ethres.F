      SUBROUTINE ETHRES
C------------------------------------------------------------
C
C! Cleans ETDI,EWDI banks if Module Energy from wires is below threshold
CKEY ECAL CLEAN  / USER
C  B. Bloch -Devaux  October 25,1989
C  Structure : SUBROUTINE
C              External references:NAMIND,WBANK,WDROP,BKFRW(BOS77)
C                                  UCOPY(CERNLIB)
C                                  EMDTOW(ALEPHLIB)
C              Comdecks references:BCS,BMACRO
C  Input : none
C  Output : none
C  Banks  : input  - EWDI, ETDI
C                    EWTH (data cards)
C           output - JDETDI (work bank dropped at the end)
C                  - JDEWDI (work bank dropped at the end)
C  Action : removes from ETDI and EWDI addresses in modules below thresh
C           except for random triggers ( no pad suppression )
C-----------------------------------------------------------------------
#ifndef DOC
      DIMENSION LTHR(36),DWTHR(36)
      LOGICAL LTHR
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL
      COMMON /LOCAL/ JDETDI,JDEWDI
#include "bcs.h"
#include "rcurnt.h"
      DATA IFIR/0/
#include "bmacro.h"
C ---------------------------------------------------------------------
      IF (IFIR.EQ.0) THEN
         IFIR = 1
         JDETDI = 0
         JDEWDI = 0
         NETDI = NAMIND ('ETDI')
         NEWDI = NAMIND ('EWDI')
         NEVEH = NAMIND ('EVEH')
      NAECDF = NAMIND('ECDF')
C     set threshold: look at EWTH data card to reset the threshold
         RWTHR = 50000.
         JETHR = IW(NAMIND('EWTH'))
         IF (JETHR.NE.0) THEN
            IF (IW(JETHR).GE.1) RWTHR = REAL(IW(JETHR+1))
         ENDIF
      ENDIF
C   in any case do not apply suppression to random triggers
C
      CALL RTRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSATR,FAIL)
      IF (TRAND) RETURN
      JEWDI = IW(NEWDI)
      IF (JEWDI.LE.0) GOTO 999
      IF (LROWS(JEWDI).EQ.0) GOTO 999
C
         DO 2 II= 1,36
      DWTHR(II) = 0.
 2       LTHR(II) = .TRUE.
C
C-   Look for eventual shift of wires
C
      KECDF = IW(NAECDF)
      NECDF = 0
      IF ( KECDF.NE.0) NECDF = LROWS(KECDF)
      DO IL=1,NECDF
        IMODU = ITABL(KECDF,IL,1)
        DWTHR(IMODU) = RTABL(KECDF,IL,2)
      ENDDO
C   Create JDEWDI work bank
      CALL WBANK (IW,JDEWDI,LCOLS(JEWDI)*LROWS(JEWDI)+LMHLEN,*999)
      IW(JDEWDI+LMHCOL) = LCOLS(JEWDI)
C
      DO 11 I = 1,LROWS(JEWDI)
        IMODU = ITABL(JEWDI,I,1)
        IF (IMODU.GE.1 .AND. IMODU.LE.36) THEN
          RWSUM = 0.
          DO 12 J = 1,45
            RWSUM = RWSUM+REAL(ITABL(JEWDI,I,J+1))
 12       CONTINUE
      IF ( RWSUM.GE.RWTHR+DWTHR(IMODU)) LTHR(IMODU) = .FALSE.
        ENDIF
 11   CONTINUE
C  There are planes below threshold
      DO 10 I = 1,LROWS(JEWDI)
C      Get plane address
        IAD = ITABL(JEWDI,I,1)
C      Reset module if below threshold
        IF ( .NOT.LTHR(IAD)) THEN
C          add that line to JDEWDI bank
            KEWDI = KROW(JEWDI,I)
            KWORK = KNEXT(JDEWDI)
            CALL UCOPY(IW(KEWDI+1),IW(KWORK+1),LCOLS(JEWDI))
            IW(JDEWDI+LMHROW) = IW(JDEWDI+LMHROW)+1
        ENDIF
  10  CONTINUE
C
C   compress JDEWDI and copy it to EWDI, then drop JDEWDI
      IF (LROWS(JDEWDI).LT.LROWS(JEWDI)) THEN
         LEN = LROWS(JDEWDI)*LCOLS(JDEWDI)+LMHLEN
         CALL WBANK (IW,JDEWDI,LEN,*998)
         CALL BKFRW (IW,'EWDI',0,IW,JDEWDI,*998)
      ENDIF
 998  CALL WDROP (IW,JDEWDI)
      JETDI = IW(NETDI)
      IF (JETDI.LE.0) GO TO 999
      IF (LROWS(JETDI).EQ.0) GO TO 999
C   Create JDETDI work bank
      CALL WBANK (IW,JDETDI,LCOLS(JETDI)*LROWS(JETDI)+LMHLEN,*999)
      IW(JDETDI+LMHCOL) = LCOLS(JETDI)
C  There are towers and noisy ones
      DO 20 I = 1,LROWS(JETDI)
C      Get tower address
        IAD = ITABL(JETDI,I,1)
        ITET = IBITS(IAD,16,8)
        IPHI = IBITS(IAD,2,9)
C   Protect against bad raw data - suppress illegal adresses
C
             IF(ITET.GT.228.OR.IPHI.GT.384
     +        .OR.ITET.LT.1.OR.IPHI.LT.1)THEN
                 IERR=1
                 CALL RERROR('ETHRES',IERR,'Illegal ECAL tower adr.')
             ELSE
        CALL EMDTOW(ITET,IPHI,ISC,IMD,IRG)
        IMOD = IMD + 12 * (ISC-1)
C    Reset channel if inside module where Wire energy below threshold
        IF ( .NOT.LTHR(IMOD)) THEN
C          add that line to JDETDI bank
            KETDI = KROW(JETDI,I)
            KWORK = KNEXT(JDETDI)
            CALL UCOPY(IW(KETDI+1),IW(KWORK+1),LCOLS(JETDI))
            IW(JDETDI+LMHROW) = IW(JDETDI+LMHROW)+1
        ENDIF
       ENDIF
  20  CONTINUE
C
C   compress JDETDI and copy it to ETDI, then drop JDETDI
      IF (LROWS(JDETDI).LT.LROWS(JETDI)) THEN
         LEN = LROWS(JDETDI)*LCOLS(JDETDI)+LMHLEN
         CALL WBANK (IW,JDETDI,LEN,*997)
         CALL BKFRW (IW,'ETDI',0,IW,JDETDI,*997)
      ENDIF
 997  CALL WDROP (IW,JDETDI)
C
 999  RETURN
      END
#endif
