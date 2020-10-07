      SUBROUTINE GIMEDK(CNAME,CGIME,GTYPE,GMODE,IRC)
C-----------------------------------------------------------------------
C! LOOK FOR GIME + DROP UNUSED DISKS
CKEY ALREAD GIME DROP UNUSED / INTERNAL
C - M.TALBY - 900119
C             900212 - use QDFREE avalaible on all IBMs
C                      introduce (QUIET option
C - Modifid by  M.TALBY - 900505
C
C     INPUT:    CNAME      CARD NAME
C               CGIME      GIME DISK
C               GTYPE      FILE TYPE
C               GMODE      FILE MODE
C
C     OUTPUT    IRC        = 0  CARD FOUND GIME SUCCESSFUL
C                          =-1  GIME NOT SUCCESSFUL
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#if defined(IBM)
#include "alrcom.h"
      DIMENSION NRF(4)
      CHARACTER*4 CARIN(MINPUT),CAROUT(MOUTPT)
      CHARACTER*1 DKMOD(26)
      CHARACTER*7 OPT
      CHARACTER*20 CPRER(4),CPREW(5),CDROP,CWORK
      CHARACTER*(*) CNAME, CGIME, GTYPE, GMODE
      INTEGER ACARD0
      LOGICAL FIRST
      DATA OPT/' (QUIET'/
      DATA DKMOD/'Z','Y','X','W','V','U','T','S','R','Q','P','O','N'
     &          ,'M','L','K','J','I','H','G','F','E','D','C','B','A'/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      IRC = 0
      CDROP = ' '
      CWORK = ' '
C --
C   1st entry : some initialization
C --
      IF(FIRST) THEN
        FIRST = .FALSE.
        LW = 0
        NRO = -1
        IST1 = 1
        IST2 = 3
        DO 4 I=1,5
           DKMODW(I) = '    '
           CPREW(I)  = '    '
           IF(I.GT.4) GOTO 4
           DKMODR(I) = '    '
           CPRER(I)  = '    '
           NRF(I) = -1
 4      CONTINUE
C - get ALEPH data card names
        CALL AIOCAR (CARIN,MINPUT,CAROUT,MOUTPT)
C - Look for free access modes
        IFM  = 0
        IFMR = 0
        IFMW = 0
        DO 10 J = 1,26
          CALL VMCMS('EXEC QDFREE ' //' '//DKMOD(J),I)
          IF(I.EQ.0) GOTO 10
          IFM = IFM+1
          IF(IFM.GT.3 .AND. IFM.LE.7) THEN
            IFMR = IFMR+1
            DKMODR(IFMR) = DKMOD(J)
          ELSEIF(IFM.GT.7 .AND. IFM.LE.12) THEN
            IFMW = IFMW+1
            DKMODW(IFMW) = DKMOD(J)
            IF(IFMW.EQ.5) GOTO 11
          ENDIF
   10   CONTINUE
      ENDIF

C
C - next entry
C
C - keep in CWORK :"GIME tid addr" without bank
   11 LENG = LNBLNK(CGIME)
      CWORK(1:4) = 'GIME'
      IB = INDEX(CGIME(1:LENG),'GIME')
      IB = IB+3
      LENW = 4
      NW = 0
      IMO = 0
 1    IB = IB+1
      IF (IB.GT.LENG) GOTO 3
      IF (CGIME(IB:IB).EQ.' ') GOTO 1
C     the 1st non blank char. indicates the beginning of a word
      ICG = IB
 2    IB = IB+1
      IF (IB.LE.LENG .AND. CGIME(IB:IB).NE.' ') GOTO 2
C     then the 1st blank char. indicates the end of a word
      NW = NW+1
      IF (NW.LE.2) THEN
C     concatenate the 2 ist words to "GIME" without blank
         CWORK = CWORK(1:LENW)//CGIME(ICG:IB-1)
         LENW = LNBLNK(CWORK)
      ELSEIF (NW.EQ.3) THEN
C     keep the position of the mode if given : it is the 3rd word
         IMO = ICG
      ENDIF
      IF (NW.LT.3) GOTO 1
 3    CONTINUE
C - look for position of filemode in GMODE
      LGM = LNBLNK(GMODE)
      IF (LGM.EQ.0) LGM = 2
C
C - READ or WRITE mode
C
      DO 14 L = 1,MOUTPT
         IF(CNAME.EQ.CAROUT(L)) GOTO 16
 14   CONTINUE
      GOTO 20
C --
C   GIME DISK IN WRITE ACCESS MODE  ================================
C --
 16   CONTINUE
      NAMI = ACARD0(NRF(L))
      DO 15 IN = 1,IFMW
         IF (CWORK.EQ.CPREW(IN)) THEN
             GMODE(LGM:LGM) = DKMODW(IN)
             GOTO 99
         ENDIF
   15 CONTINUE
      IF(L.EQ.LW .AND. NRF(L).EQ.NRO) THEN
        IF(LW.LT.5) LW = LW+1
      ELSE
        LW = L
        NRO = NRF(L)
      ENDIF
C
C - drop DKMODW(LW) if already used
      IF (CPREW(LW)(1:1).NE.' ') THEN
         CDROP = 'EXEC DROP '//DKMODW(LW)
         CALL VMCMS(CDROP,K)
      ENDIF
C --
C  introduce true filemode in CGIME and GMODE and call VMCMS
C --
      IF(IMO.NE.0) THEN
         CGIME(IMO:IMO) = DKMODW(LW)
      ELSE
         CGIME = CGIME(1:LENG)//' '//DKMODW(LW)
      ENDIF
      GMODE(LGM:LGM) = DKMODW(LW)
      LENG = LNBLNK(CGIME)
      CALL VMCMS ('EXEC ' //CGIME(1:LENG), I)
      CPREW(LW) = CWORK
      GOTO 100
C --
C   GIME DISK IN READ ACCESS MODE    ================================
C   SKIP THE GIME IF IT HAS BEEN ALREADY EXECUTED
C
 20   CONTINUE
C
C - set GMODE to "*" by default
      GMODE(LGM:LGM) = '*'
C
C - look for option: (opt in CGIME
      IOPT = INDEX(CGIME,'(')
      IF(IOPT.NE.0) OPT = CGIME(IOPT:LENG)
C --
      IFTYP = INDEX(GTYPE,'EDIR')
C
C -- 1st entry : NEXT=0
      IF(IFTYP.NE.0 .AND. CPRER(IST1)(1:1).EQ.' ') THEN
C     if it is the first EDIR store the GIME in CPRER(1)
C     then execute the GIME with the first mode DKMODR(1)
        DO 25 JD = 1,2
          IF(CPRER(JD).EQ.CWORK(1:LENW)) GOTO 99
 25     CONTINUE
        NEXT = 0
        IMR = IST1
        IF(IST1.LT.2) IST1= IST1+1
C
      ELSEIF(IFTYP.EQ.0 .AND. CPRER(IST2)(1:1).EQ.' ') THEN
C     if it is the 1st or 2nd non EDIR check that the same
C     GIME has not yet been done. If yes then RETURN.
C     if not then store the GIME in CPRER(IMR)
C     IMR = 3 OR 4
C     THEN EXECUTE THE GIME WITH THE 3ND OR 4RD MODE DKMODR(IMR)
        DO 30 JD = 3,IFMR
          IF(CPRER(JD).EQ.CWORK(1:LENW)) GOTO 99
 30     CONTINUE
        NEXT = 0
        IMR = IST2
        IF(IST2.LT.4) IST2 = IST2+1
C
C -- next entry : NEXT=1
      ELSEIF (IFTYP.NE.0) THEN
C     if it is a next EDIR check that the same GIME has not yet been
C     done. If yes then RETURN.
C     if not then drop the disk linked in DKMODR(1)
C                 store the GIME in CPRER(1)
C                 execute the GIME with DKMODR(1)
        DO 35 I = 1,2
          IF(CPRER(I).EQ.CWORK(1:LENW)) GOTO 99
   35   CONTINUE
        NEXT = 1
        IST1 = 3-IST1
        IMR  = IST1
C
      ELSE
C     if it is a next non EDIR check that the same GIME has not yet been
C     done. If yes then RETURN.
C     if not then drop the disk linked in DKMODR(IMR)
C                 store the GIME in CPRER(IMR)
C                 execute the GIME with DKMODR(IMR)
C     IMR = 3 OR 4
        DO 40 JD = 3,IFMR
          IF(CPRER(JD).EQ.CWORK(1:LENW)) GOTO 99
 40     CONTINUE
        NEXT = 1
        IST2 = 7 - IST2
        IMR = IST2
      ENDIF
C
C - execute DROP and GIME
C
      IF (NEXT.EQ.1) THEN
         CDROP = 'EXEC DROP '//DKMODR(IMR)//OPT
         CALL VMCMS(CDROP,K)
      ENDIF
      CPRER(IMR) = CWORK(1:LENW)
      IF(IMO.NE.0) THEN
         CGIME(IMO:IMO) = DKMODR(IMR)
      ELSE
         CGIME = CGIME(1:LENG)//' '//DKMODR(IMR)
      ENDIF
      GMODE(LGM:LGM) = DKMODR(IMR)
      LENG = LNBLNK(CGIME)
      CGIME = CGIME(1:LENG)//OPT
      LENG = LNBLNK(CGIME)
      CALL VMCMS('EXEC ' //CGIME(1:LENG),I)
C ======================================================================
C
 100  CONTINUE
      IF (I .NE. 0 .AND. I .NE. 4 )  IRC = -1
C --
   99 RETURN
#endif
      END
#endif
