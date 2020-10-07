      SUBROUTINE EPADCL
C------------------------------------------------------------
C
C! Cleans ETDI bank using information from background LCAL triggers
CKEY ECAL CLEAN  / JULIA   USER
C  B. Bloch -Devaux  December ,1989
C  Structure : SUBROUTINE
C           External references:NAMIND,WBANK,BKTOW,BKFRW,WDROP(BOS77)
C                               UCOPY,VZERO (CERNLIB)
C                               EMDTOW      (ALEPHLIB)
C                               RTRIGF,ENOISE,EITHR,ENSTAT(This Library)
C           Comdecks references:BCS,BMACRO,ECCAJJ,RCURNT
C  Input : none
C  Output : none
C  Banks  : input  - ETDI
C
C           output - JDETDI(10) work bank for ETDI from LCA triggers
C                    JNETDI     (work bank dropped at the end)
C                    ETDI after clean up and correction for absolute
C                         calibration from ECCA bank
C  Action : builds list of noisy or shifted storeys from LCAL triggers
C           and update ETDI bank accordingly
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "eccajj.h"
#include "rcurnt.h"
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL
#include "etdipt.h"
      DIMENSION IFL(3),IMEA(3)
      CHARACTER*80 MSG
      INTEGER EITHR
      EXTERNAL EITHR
      DATA NR,NCUR / 0,0 /
      DATA IFIR/0/
#include "bmacro.h"
C ---------------------------------------------------------------------
C
C
C - Determine bunch number
C
      CALL RQBUNC(IBUN,INBU,NWAG,IQUA)
      IBUNCH = 1
      IF ( IBUN.GT.0) IBUNCH = IBUN
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        CALL VZERO(JDETDI ,NEV)
        JNETDI = 0
        JNWORK = 0
        JFWORK = 0
        NAETDI = NAMIND ('ETDI')
        NAECCA = NAMIND ('ECCA')
C     take care of prescan in FALCON
        LE=0
        K=0
        DO 50 J=1,NEV
          IND=NLINK('EPRS',J)
          IF(IND.GT.0.AND.IW(IND).GT.0)THEN
            K=K+1
            CALL BKTOW(IW,'EPRS',J,IW,JDETDI(K),*51)
            LE=LE+IW(JDETDI(K))
            NR=NR+1
            NCUR=NCUR+1
          ENDIF
   50   CONTINUE
   51   CONTINUE
      ENDIF
C
C?   If Run NOT Initialized RETURN
C?   this can happen is slowc control record occur before run initializa
C
      IF (.NOT.INIRUC) GOTO 999
C
C   Look at trigger bits
      CALL RTRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSATR,FAIL)
C   Look at ETDI data
      JETDI = IW(NAETDI)
      IF (JETDI.GT.0) THEN
        IF(IW(JETDI).EQ.0) GO TO 999
        IF (LROWS(JETDI).EQ.0) GO TO 999
        INEW = 0
        NLEN = IW(JETDI)
C       keep only LCAL-ONLY triggers
        IF (TLCAL.AND..NOT.TPHYS.AND..NOT.TRAND.AND..NOT.FAIL) THEN
          NCUR = NCUR+1
C       Look  if we already have NEV banks
          IF (NR.EQ.NEV) THEN
            NR = NR - 1
C         Drop oldest bank
            IF ( NCUR.GT.10) NCUR = 1
            CALL WDROP(IW,JDETDI(NCUR))
          ENDIF
          CALL WBANK(IW,JDETDI(NCUR),NLEN,*999)
          CALL BKTOW(IW,'ETDI',0,IW,JDETDI(NCUR),*999)
          NR = NR+1
          INEW = 1
        ENDIF
C         Compute averages and sigmas if NEV events stored
        IF (NR.EQ.NEV) THEN
          IF ( INEW .EQ. 1) CALL ENOISE
          CALL WBANK(IW,JNETDI,NLEN,*999)
          IW(JNETDI+LMHCOL) = LCOLS(JETDI)
        ENDIF
        JECCA = IW(NAECCA)
        DO 10 I = 1,LROWS(JETDI)
C             Get tower address
          IAD = ITABL(JETDI,I,1)
          ITET = IBITS(IAD,16,8)
          IPHI = IBITS(IAD,2,9)
          IBAD = IBITS(IAD,0,2)+IBITS(IAD,12,4)+IBITS(IAD,24,8)
C   Protect against bad raw data - suppress illegal adresses
C
          IF(ITET.GT.228.OR.IPHI.GT.384
     +        .OR.ITET.LT.1.OR.IPHI.LT.1.OR.IBAD.NE.0)THEN
            IERR=-1
            CALL RERROR('EPADCL',IERR,'Illegal ECAL tower adr.')
          ELSE
C   Get module number for this tower
            CALL EMDTOW(ITET,IPHI,ISC,IMD,IRG)
            IMOD = IMD + 12 * (ISC-1)
C
C Protect against bad data in MonteCarlo
            IF( IMOD .LT. 1 .OR. IMOD .GT. 36 ) THEN
              IERR=-1
              CALL RERROR('EPADCL',IERR,'Illegal ECAL module no.')
              GOTO 10
            ENDIF
C
            IMODB = IMOD + (IBUNCH-1) * 36
            IF (JECCA.LE.0) THEN
              CAL = 1.
            ELSE
              CAL = RTABL(JECCA,IMODB,JECCCP)
            ENDIF
            IF(NR.EQ.NEV) CALL ENSTAT(IAD,IFL,IMEA)
            NST = 0
            DO 11 IST = 1,3
              IF ( NR.EQ.NEV ) THEN
C     Reset channel if noisy ,shift if offset and count how many storeys
C             are still above threshold EITHR in this tower
                IF ( IFL(IST).EQ.1) IW(KROW(JETDI,I)+1+IST) =0
                IF ( IFL(IST).EQ.2) IW(KROW(JETDI,I)+1+IST) =
     $             IW(KROW(JETDI,I)+1+IST)- IMEA(IST)
                IF (ITABL(JETDI,I,IST+1).GT.EITHR(ITET,IPHI,IST))
     $             NST = NST+1
              ENDIF
C    apply absolute calibration to the 3 stacks
              IW(KROW(JETDI,I)+IST+1)=NINT(CAL*ITABL(JETDI,I,IST+1))
   11       CONTINUE
            IF ( NR.EQ.NEV .AND. NST.GT.0) THEN
C        add that line to JNETDI bank
              KETDI = KROW(JETDI,I)
              KWORK = KNEXT(JNETDI)
              CALL UCOPY(IW(KETDI+1),IW(KWORK+1),LCOLS(JETDI))
              IW(JNETDI+LMHROW) = IW(JNETDI+LMHROW)+1
            ENDIF
          ENDIF
   10   CONTINUE
C
C       compress JNETDI and copy it to ETDI, then drop JNETDI
        IF (NR.EQ.NEV .AND. LROWS(JNETDI).LT.LROWS(JETDI)) THEN
          LEN = LROWS(JNETDI)*LCOLS(JNETDI)+LMHLEN
          CALL WBANK(IW,JNETDI,LEN,*998)
          CALL BKFRW (IW,'ETDI',0,IW,JNETDI,*998)
        ENDIF
  998   CALL WDROP (IW,JNETDI)
      ENDIF
C
  999 RETURN
      END
#endif
