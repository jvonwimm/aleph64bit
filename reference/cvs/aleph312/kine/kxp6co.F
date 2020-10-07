      SUBROUTINE KXP6CO(LUPAR)
C -----------------------------------------------------------------
C - Modified for Pythia 6.1
C                           B.Bloch   - 980112
C
C! Set LUND parameters by data cards
CKEY KINE KINGAL LUND7 DECAY  /  USER INTERNAL
C  Every PYTHIA parameter is a BOS data card keyword,the index of
C  the parameter is the bank number.
C
C  the list of keywords with their format is given below:
C
C 'MSTU'(I),'PARU'(F),'MSTJ'(I),'PARJ'(F),
C 'KCH1'(I),'KCH2'(I),'KCH3'(I),'KCH4'(I),
C 'PMA1'(F),'PMA2'(F),'PMA3'(F),'PMA4'(F),
C 'PARF'(F),'CHA1'(I),'CHA2'(I),
C 'MDC1'(I),'MDC2'(I),'MDC3'(I),'MDM1'(I),'MDM2'(I),'BRAT'(F),
C 'KFD1'(I),'KFD2'(I),'KFD3'(I),'KFD4'(I),'KFD5'(I),
C 'MSEL'(I),'MSUB'(I),'CKIN'(F),'MSTP'(I),'PARP'(F),
C 'MSTI'(I),'PARI'(F),'IMSS'(I),'RMSS'(F)
C
C
C    KEY  i  /  ival     ====>  KEY(i)=ival
C    RKEY i  /  value    ====>  RKEY(i)=value
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXP6CO
C              External References: NAMIND/BKFMT/BLIST(BOS77)
C                                   KXP6BR (this Lib)
C              Comdecks referenced: BCS,LUNDCOM
C
C - usage    : CALL KXP6CO(LUPAR)
C - input    : LUPAR=No. of read data cards
C
C  Note that, if a particle mass(PMA1), width(PMA2) or life-time(PMA4)
C  is modified, the PART bank entry is changed accordingly.
C
#ifndef DOC
#include "pyt6com.h"
#include "bcs.h"
      PARAMETER (LKEYS=35)
      CHARACTER*4 KEY(LKEYS),CHAINT
      CHARACTER*1 FMT(LKEYS)
      DATA KEY / 'MSTU','PARU','MSTJ','PARJ',
     &           'KCH1','KCH2','KCH3','KCH4',
     &           'PMA1','PMA2','PMA3','PMA4',
     &           'PARF',       'CHA1','CHA2',
     &           'MDC1','MDC2','MDC3','MDM1',
     &           'MDM2','BRAT','KFD1','KFD2',
     +           'KFD3','KFD4','KFD5','MSEL',
     $           'MSUB','CKIN','MSTP','PARP',
     &           'MSTI','PARI','IMSS','RMSS'/
      DATA FMT /'I','F','I','F',
     &          'I','I','I','I',
     &          'F','F','F','F',
     &          'I',    'I','I',
     &          'I','F','I','I',
     &          'I','I','I','I',
     &          'I','I','I','I',
     &          'I','F','I','I',
     &          'I','F','I','F'/
      DATA NAPAR/0/
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
      LUPAR=0
      iut = iw(6)
      WRITE(iut,1)
 1    FORMAT(//
     .20X,' YOU ARE RUNNING PYTHIA 6.1 INSIDE THE                 '//
     .20X,'     K I N G A L  - PACKAGE                            '//
     .20X,'      for commentS  SEND MAIL TO :                     '/
     .20X,'         Brigitte.BLOCH@cern.ch                        '//
     .20X,'                                                       '/
     .15X,'-------------------------------------------------------'///
     .20X,' You set up the following PYTHIA parameters            '//)
 2    FORMAT(20X,' KEY : ',A4,'  =  ',I8)
 3    FORMAT(20X,' KEY : ',A4,'  =  ',F8.4)
 4    FORMAT(20X,' KEY : ',A4,'(',I3,')  =  ',I8)
 5    FORMAT(20X,' KEY : ',A4,'(',I8,')  =  ',E10.4)
 6    FORMAT(20X,' KEY : ',A4,'(',I3,')  =  ',A4)
      DO 150 I=1,LKEYS
         NAMI=NAMIND(KEY(I))
         IF (IW(NAMI).EQ.0) GOTO 150
         KIND=NAMI+1
   15    KIND=IW(KIND-1)
         IF (KIND.EQ.0) GOTO 149
         LUPAR = LUPAR+1
         J = IW(KIND-2)
         GOTO (21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
     +37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55) I
   21    MSTU(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MSTU(J)
       GOTO 15
   22    PARU(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PARU(J)
       GOTO 15
   23    MSTJ(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MSTJ(J)
       GOTO 15
   24    PARJ(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PARJ(J)
       GOTO 15
   25    KC = PYCOMP(J)
         KCHG(KC,1) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KCHG(KC,1)
       GOTO 15
   26    KC = PYCOMP(J)
         KCHG(KC,2) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KCHG(KC,2)
       GOTO 15
   27    KC = PYCOMP(J)
         KCHG(KC,3) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KCHG(KC,3)
       GOTO  15
   28    KC = PYCOMP(J)
         KCHG(KC,4) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KCHG(KC,4)
       GOTO  15
   29    KC = PYCOMP(J)
         PMAS(KC,1) = RW(KIND+1)
         IOF = 6
         WRITE(iut,5)KEY(I),J,PMAS(KC,1)
       GOTO 115
   30    KC = PYCOMP(J)
         PMAS(KC,2) = RW(KIND+1)
         IOF = 9
         WRITE(iut,5)KEY(I),J,PMAS(KC,2)
       GOTO 115
   31    KC = PYCOMP(J)
         PMAS(KC,3) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PMAS(KC,3)
       GOTO 15
   32    KC = PYCOMP(J)
         PMAS(KC,4) = RW(KIND+1)/3.33E-12
         IOF = 8
         WRITE(iut,5)KEY(I),J,PMAS(KC,4)
       GOTO 115
   33    PARF(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PARF(j)
       GOTO 15
   34    KC = PYCOMP(J)
         CHAF(KC,1) = CHAINT(IW(KIND+1))
         WRITE(iut,6)KEY(I),J,CHAF(KC,1)
       GOTO 15
   35    KC = PYCOMP(J)
         CHAF(KC,2) = CHAINT(IW(KIND+1))
         WRITE(iut,6)KEY(I),J,CHAF(KC,2)
       GOTO 15
   36    KC = PYCOMP(J)
         MDCY(KC,1) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MDCY(KC,1)
       GOTO 15
   37    KC = PYCOMP(J)
         MDCY(KC,2) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MDCY(KC,2)
       GOTO 15
   38    KC = PYCOMP(J)
         MDCY(KC,3) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MDCY(KC,3)
       GOTO 15
   39    MDME(J,1) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MDCY(J,1)
       GOTO 15
   40    MDME(J,2) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MDCY(J,2)
       GOTO 15
   41    BRAT(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,BRAT(j)
       GOTO 15
   42    KFDP(J,1) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KFDP(j,1)
       GOTO 15
   43    KFDP(J,2) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KFDP(j,2)
       GOTO 15
   44    KFDP(J,3) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KFDP(j,3)
       GOTO 15
   45    KFDP(J,4) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KFDP(j,4)
       GOTO 15
   46    KFDP(J,5) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,KFDP(j,5)
       GOTO 15
   47    MSEL = IW(KIND+1)
         WRITE(iut,2)KEY(I),MSEL
       GOTO 15
   48    MSUB(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MSUB(J)
       GOTO 15
   49    CKIN(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,CKIN(J)
       GOTO 15
   50    MSTP(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MSTP(J)
       GOTO 15
   51    PARP(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PARP(J)
       GOTO 15
   52    MSTI(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,MSTI(J)
       GOTO 15
   53    PARI(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,PARI(J)
       GOTO 15
   54    IMSS(J) = IW(KIND+1)
         WRITE(iut,4)KEY(I),J,IMSS(J)
       GOTO 15
   55    RMSS(J) = RW(KIND+1)
         WRITE(iut,5)KEY(I),J,RMSS(J)
       GOTO 15
 149  CONTINUE
         CALL BKFMT (KEY(I),FMT(I))
         CALL BLIST (IW,'C+',KEY(I))
       GOTO 150
 115  CONTINUE
      IPART = KGPART(J)
      JPART = IW(NAPAR)
      IF (IPART.GT.0) THEN
        RW(JPART+LMHLEN+(IPART-1)*IW(JPART+1)+IOF)= RW(KIND+1)
        IANTI = IW(JPART+LMHLEN+(IPART-1)*IW(JPART+1)+10)
        IF (IANTI.NE.IPART) RW(JPART+LMHLEN+(IANTI-1)*IW(JPART+1)+IOF)=
     $                      RW(KIND+1)
      ENDIF
      GOTO 15
 150  CONTINUE
      WRITE(iut,7)
 7    FORMAT(/,/,15X,
     .'--------------------------------------------------------'//)

C
C      Look for more modifications of decay parameters
C
      CALL KXP6BR
      RETURN
      END
#endif
