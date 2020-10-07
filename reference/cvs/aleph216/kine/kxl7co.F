      SUBROUTINE KXL7CO(LUPAR)
C -----------------------------------------------------------------
C - Modified for Jetset 7.3 T.Medcalf - 900802
C                           B.Bloch   - 900926
C
C! Set LUND parameters by data cards
CKEY KINE KINGAL LUND7 DECAY  /  USER INTERNAL
C  Every LUND parameter is a BOS data card keyword,the index of the
C  parameter is the bank number.
C
C  the list of keywords with their format is given below:
C
C 'MSTU'(I),'PARU'(F),'MSTJ'(I),'PARJ'(F),
C 'KCH1'(I),'KCH2'(I),'KCH3'(I),'PMA1'(F),'PMA2'(F),'PMA3'(F),
C 'PMA4'(F),'PARF'(F),
C 'MDC1'(I),'MDC2'(I),'MDC3'(I),'MDM1'(I),'MDM2'(I),'BRAT'(F),
C 'KFD1'(I),'KFD2'(I),'KFD3'(I),'KFD4'(I),'KFD5'(I)
C
C
C    KEY  i  /  ival     ====>  KEY(i)=ival
C    RKEY i  /  value    ====>  RKEY(i)=value
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXL7CO
C              External References: NAMIND/BKFMT/BLIST(BOS77)
C                                   KXL7BR (this Lib)
C              Comdecks referenced: BCS,LUNDCOM
C
C - usage    : CALL KXL7CO(LUPAR)
C - input    : LUPAR=No. of read data cards
CTM
C   Note that, if a particle mass (PMA1), width (PMA2) or life-time (PMA
C  is modified, the PART bank entry is changed accordingly.
C
#ifndef DOC
      SAVE
#include "bcs.h"
#include "lun7com.h"
C
      PARAMETER (LKEYS=23)
      CHARACTER*4 KEY(LKEYS),CHAINT
      CHARACTER*1 FMT(LKEYS)

      DATA KEY / 'MSTU','PARU','MSTJ','PARJ',
     &           'KCH1','KCH2','KCH3','PMA1',
     &           'PMA2','PMA3','PMA4','PARF',
     &           'MDC1','MDC2','MDC3','MDM1',
     &           'MDM2','BRAT','KFD1','KFD2',
     +           'KFD3','KFD4','KFD5'/
      DATA FMT /'I','F','I','F',
     &          'I','I','I','F',
     &          'F','F','F','F',
     &          'I','I','I','I',
     &          'I','F','I','I',
     &          'I','I','I'/
      DATA NAPAR/0/
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
      LUPAR=0
      DO 50 I=1,LKEYS
         NAMI=NAMIND(KEY(I))
         IF (IW(NAMI).EQ.0) GOTO 50
         KIND=NAMI+1
   15    KIND=IW(KIND-1)
         IF (KIND.EQ.0) GOTO 49
         LUPAR = LUPAR+1
         J = IW(KIND-2)
         GOTO (21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
     +                                    37,38,39,40,41,42,43) I
   21    MSTU(J) = IW(KIND+1)
       GOTO 15
   22    PARU(J) = RW(KIND+1)
       GOTO 15
   23    MSTJ(J) = IW(KIND+1)
       GOTO 15
   24    PARJ(J) = RW(KIND+1)
       GOTO 15
   25    KC = LUCOMP(J)
         KCHG(KC,1) = IW(KIND+1)
       GOTO 15
   26    KC = LUCOMP(J)
         KCHG(KC,2) = IW(KIND+1)
       GOTO 15
   27    KC = LUCOMP(J)
         KCHG(KC,3) = IW(KIND+1)
       GOTO  15
   28    KC = LUCOMP(J)
         PMAS(KC,1) = RW(KIND+1)
         IOF = 6
       GOTO 115
   29    KC = LUCOMP(J)
         PMAS(KC,2) = RW(KIND+1)
         IOF = 9
       GOTO 115
   30    KC = LUCOMP(J)
         PMAS(KC,3) = RW(KIND+1)
       GOTO 15
   31    KC = LUCOMP(J)
         PMAS(KC,4) = RW(KIND+1)/3.33E-12
         IOF = 8
       GOTO 115
   32    PARF(J) = RW(KIND+1)
       GOTO 15
   33    KC = LUCOMP(J)
         MDCY(KC,1) = IW(KIND+1)
       GOTO 15
   34    KC = LUCOMP(J)
         MDCY(KC,2) = IW(KIND+1)
       GOTO 15
   35    KC = LUCOMP(J)
         MDCY(KC,3) = IW(KIND+1)
       GOTO 15
   36    MDME(J,1) = IW(KIND+1)
       GOTO 15
   37    MDME(J,2) = IW(KIND+1)
       GOTO 15
   38    BRAT(J) = RW(KIND+1)
       GOTO 15
   39    KFDP(J,1) = IW(KIND+1)
       GOTO 15
   40    KFDP(J,2) = IW(KIND+1)
       GOTO 15
   41    KFDP(J,3) = IW(KIND+1)
       GOTO 15
   42    KFDP(J,4) = IW(KIND+1)
       GOTO 15
   43    KFDP(J,5) = IW(KIND+1)
       GOTO 15
   49    CONTINUE
         CALL BKFMT (KEY(I),FMT(I))
         CALL BLIST (IW,'C+',KEY(I))
       GOTO 50
  115 CONTINUE
      IPART = KGPART(J)
      JPART = IW(NAPAR)
      IF (IPART.GT.0) THEN
        RW(JPART+LMHLEN+(IPART-1)*IW(JPART+1)+IOF)= RW(KIND+1)
        IANTI = IW(JPART+LMHLEN+(IPART-1)*IW(JPART+1)+10)
        IF (IANTI.NE.IPART) RW(JPART+LMHLEN+(IANTI-1)*IW(JPART+1)+IOF)=
     $                      RW(KIND+1)
      ENDIF
      GOTO 15
   50 CONTINUE
C
C      Look for more modifications of decay parameters
C
      CALL KXL7BR
      RETURN
      END
#endif
