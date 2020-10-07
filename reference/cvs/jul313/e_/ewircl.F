      SUBROUTINE EWIRCL
C------------------------------------------------------------
C
C! Cleans EWDI using info on known bugs and apply absolute calibration
CKEY ECAL CLEAN WIRE  / JULIA  USER
C  B. Bloch -Devaux  December ,1989
C  Structure : SUBROUTINE
C             External references:NAMIND,WBANK,BKTOW,BKFRW,WDROP(BOS77)
C                                 UCOPY,VZERO (CERNLIB)
C                                 WIRJUL            (This Library)
C             Comdecks references:BCS,BMACRO,RCURNT,ECCAJJ
C  Input : none
C  Output : none
C  Banks  : input  - EWDI,EWHE,ECCA
C
C           output - JDEWDI (work bank dropped at the end)
C                    EWDI and EWHE after clean up and absolute
C                        calibration from ECCA bank
C  Action : corrects wire energies and pastis infos for known bugs in
C           in particular run ranges.
C           Apply absolute calibration to wire info (EWDI,EWHE)
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "eccajj.h"
      DIMENSION FAC(3),ITEMP(8),KPIED(36)
      DATA KPIED/48,54,46,37,38,56,47,73,42,70,57,15,
     &           55,55,51,-25,-21,50,-43,-33,-41,-24,56,57,
     &           39,36,40,25,72,95,29,32,43,31,32,10/
      DATA FAC / 1.901,1.932,1.901 /
      DATA ITEMP/ 8*0/
#include "bmacro.h"
C ---------------------------------------------------------------------
      NAEWDI=NAMIND('EWDI')
      NAEWHE=NAMIND('EWHE')
      NAECCA=NAMIND('ECCA')
      IMDSUM = 0
      CALL RQBUNC(IBU,INBU,NWAG,IQUA)
      IBUNCH = 1
      IF ( IBU.GT.0) IBUNCH = IBU
      JEWDI=IW(NAEWDI)
      JEWHE=IW(NAEWHE)
      JECCA = IW(NAECCA)
      NEWHE=0
      NEWDI=0
C
C?  Check EWHE consistency
C
      IF(JEWHE.GT.0)THEN
        IF(LROWS(JEWHE)*LCOLS(JEWHE)+LMHLEN.NE.IW(JEWHE)) THEN
          WRITE(IW(6),100) IW(JEWHE),LCOLS(JEWHE),LROWS(JEWHE)
  100     FORMAT ( 1X,'+++++WARNING EWIRCL : WRONG HEADER EWHE '
     $,  'LENGTH IS ',I10,'FOR #COL,#ROWS ',2I5)
          IF (IW(JEWHE).EQ.74.AND. IW(JEWHE+LMHCOL).EQ.2) THEN
            CALL RERROR('EWIRCL',1,' EWHE wrong mini header -FIXED ')
            IW(JEWHE+LMHROW) = 36
          ELSE
            CALL RERROR('EWIRCL',-2,' EWHE wrong mini header -DROPPED')
            CALL BDROP(IW,'EWHE')
            JEWHE=0
          ENDIF
        ENDIF
        NEWHE=LROWS(JEWHE)
        DO 40 I=1,NEWHE
          JMOD=ITABL(JEWHE,I,2)
          IF(JMOD.LT.1.OR.JMOD.GT.36)THEN
            CALL RERROR('EWIRCL',-3,' EWHE bad module nr.')
            JEWHE=0
            NEWHE=0
            GOTO 41
          ENDIF
   40   CONTINUE
   41   CONTINUE
      ENDIF
C
C?  Check EWDI consistency
C
      IF(JEWDI.GT.0)THEN
        IF(LROWS(JEWDI)*LCOLS(JEWDI)+LMHLEN.NE.IW(JEWDI)) THEN
          WRITE(IW(6),101) IW(JEWDI),LCOLS(JEWDI),LROWS(JEWDI)
  101     FORMAT ( 1X,'+++++WARNING EWIRCL : WRONG HEADER EWDI '
     $,  'LENGTH IS ',I10,'FOR #COL,#ROWS ',2I5)
          IF (IW(JEWDI+LMHCOL).EQ.54) THEN
            CALL RERROR('EWIRCL',4,' EWDI wrong mini header -FIXED')
            IW(JEWDI+LMHROW) = (IW(JEWDI)-2)/54
          ELSE
            CALL RERROR('EWIRCL',-5,' EWDI wrong mini header -DROPPED')
            CALL BDROP(IW,'EWDI')
            JEWDI=0
          ENDIF
        ENDIF
        NEWDI=LROWS(JEWDI)
        DO 50 I=1,NEWDI
          JMOD=ITABL(JEWDI,I,1)
          IF(JMOD.LT.1.OR.JMOD.GT.36)THEN
            CALL RERROR('EWIRCL',-6,' EWDI bad module nr.')
            CALL BDROP(IW,'EWDI')
            JEWDI=0
            NEWDI=0
            GOTO 51
          ENDIF
   50   CONTINUE
   51   CONTINUE
      ENDIF
C
C?   Correct for various bugs and apply calibration
C
      IF (JECCA.LE.0) CAL = 1.
      IF ( JEWDI.GT.0) THEN
        NEWDI=LROWS(JEWDI)
        IF (NEWDI.GT.0) THEN
C  Correct so-called Pastis bug
          IF ( IRUNRC.GE.4001 .AND. IRUNRC.LE.4103 ) CALL WIRJUL
C
          IF ( IRUNRC.GE.4001 .AND. IRUNRC.LE.4107 ) THEN
C Plane 33 was wrongly considered as belonging to stack 3 for that perio
            IMDSUM = 1
            DO 1 I=1,NEWDI
              IMOD = ITABL(JEWDI,I,1)
              KEWDI = KROW(JEWDI,I)
              COR = FAC ((IMOD-1)/12 + 1)
              ECOR = ITABL(JEWDI,I,33+1)/COR
              IW(KEWDI+33+1) = NINT(ECOR)
    1       CONTINUE
          ENDIF
          IF ( IRUNRC.GE.4984 .AND. IRUNRC.LE.5059 ) THEN
C  Correct Pedestal of PASTIS data in EWDI banks  if necessary
            DO 11 I=1,NEWDI
              III=ITABL(JEWDI,I,1)
              KLINE=KROW(JEWDI,I)
              IMO1=0
              DO 15 K=47,54
                IMOMO=MOD(IW(KLINE+K),8)
                IF(IMOMO.NE.0)IMO1=1
   15         CONTINUE
              DO 16 K=47,54
C Correct ROC pedestal for Gain 8 if necessary
                IF ( IMO1.EQ.1)    IW(KLINE+K)=IW(KLINE+K)+
     $                NINT(1000.*FLOAT(KPIED(III))/45.)
   16         CONTINUE
   11       CONTINUE
          ENDIF
          IF ( IRUNRC.GE.4983 .AND. IRUNRC.LE.5300 ) THEN
C Plane 1  was wrong for that period , we have to reset it
            IMDSUM = 1
            DO 2 I=1,NEWDI
              KEWDI = KROW(JEWDI,I)
              IW(KEWDI+1+1) = 0
    2       CONTINUE
          ENDIF
          IF ( IRUNRC.GE.5333 .AND. IRUNRC.LE.5535 ) THEN
C Mod 25/26 Pastis data were swapped, swap them back if both present
C Reset the single one if only one here as data is meaningless
            I25 = 0
            I26 = 0
            DO 5 I=1,NEWDI
              KEWDI = KROW(JEWDI,I)
              IMODU = ITABL(JEWDI,I,1)
              IF (IMODU.EQ.25) THEN
                CALL UCOPY (IW(KEWDI+47),ITEMP(1),8)
                I25 = I
              ELSE IF (IMODU.EQ.26 ) THEN
                I26 = I
              ENDIF
    5       CONTINUE
            IF (I25*I26 .GT.0) THEN
              K25 = KROW(JEWDI,I25)
              K26 = KROW(JEWDI,I26)
              CALL UCOPY (IW(K26+47),IW(K25+47),8)
              CALL UCOPY (ITEMP(1),IW(K26+47),8)
              CALL VZERO (ITEMP,8)
            ELSE IF (I25*I26 .EQ. 0 .AND. I25+I26 .NE.0) THEN
              KI = KROW(JEWDI,I25+I26)
              CALL VZERO(IW(KI+47),8)
            ENDIF
          ENDIF
          IF (IMDSUM.EQ.1) THEN
C   If something was corrected in the wire energies, update the digital
C   sum in EWHE bank.
            IF ( JEWHE.GT.0) THEN
              NEWHE=LROWS(JEWHE)
              IF (NEWHE.GT.0) THEN
                JMI = 1
                DO 7 I = 1,NEWDI
                  IMOD = ITABL(JEWDI,I,1)
                  ISUM = 0
                  DO 8 IPL = 1,45
    8             ISUM = ISUM+ITABL(JEWDI,I,IPL+1)
                  DO 9 J = JMI,NEWHE
                    JMOD = ITABL(JEWHE,J,2)
                    IF (JMOD.EQ.IMOD) THEN
                      IW(KROW(JEWHE,J)+1) = ISUM
                      GO TO 7
                    ENDIF
                    JMI = JMI + 1
    9             CONTINUE
    7           CONTINUE
              ENDIF
            ENDIF
          ENDIF
C
C?      Finally apply cailbration to EWHI
C
          CAL = 1.
          DO 4 I = 1,NEWDI
            IMOD = ITABL(JEWDI,I,1)
            IMODB = (IBUNCH-1)*36 + IMOD
            IF ( JECCA.GT.0)   CAL = RTABL(JECCA,IMODB,JECCCW)
C  apply absolute calibration to the 45 planes
            DO 6 IPL = 1,45
              IW(KROW(JEWDI,I)+IPL+1)=NINT(IW(KROW(JEWDI,I)+IPL+1)*CAL)
    6       CONTINUE
    4     CONTINUE
        ENDIF
      ENDIF
C
C?   Apply calibration to EWHE
C
      IF ( JEWHE.GT.0) THEN
        NEWHE=LROWS(JEWHE)
        IF (NEWHE.GT.0) THEN
          CAL = 1.
          DO 3 I=1,NEWHE
            IMODU = ITABL(JEWHE,I,2)
            KEWHE = KROW(JEWHE,I)
C Wrong wire pedestals in EWHE for that period, correct it
            IF ( IRUNRC.GE.4001 .AND. IRUNRC.LE.4983 ) THEN
              IW(KEWHE+1) = IW(KEWHE+1) - KPIED(IMODU)*1000
            ENDIF
            IMODB = (IBUNCH-1)*36 + IMODU
            IF(JECCA.GT.0) CAL = RTABL(JECCA,IMODB,JECCCW)
            IW(KEWHE+1)=NINT(IW(KEWHE+1)*CAL)
    3     CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
#endif
