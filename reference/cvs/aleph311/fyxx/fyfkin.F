      SUBROUTINE FYFKIN(IBFUL)
C---------------------------------------------------------------
CKEY FYXX  /INTERNAL
C! Fill FKIN and FVER monte carlo banks from KINE and VERT banks
C! Fill FPOL and FZFR monte carlo banks from KPOL and KZFR banks
C! Fill FSHO monte carlo bank from KSHO bank
C - J. Hilgart 05.07.87             B.Bloch 10/10/90,13/03/98
C - Input banks   : INDKIN, INDVER, JDKNFO, JDKOFN, JDVOUT
C - Output banks  : FKIN, FVER, FPOI ,FPOL,FZFR,FSHO
C - Output argument : IBFUL = -1 if not enough space to book a bank
C Called by FYKINE
C Calls:  AUBOS                                    from  ALEPHLIB.HLB
#ifndef DOC
      SAVE
#include "bcs.h"
#include "kgjjpar.h"
#include "fxjjpar.h"
#include "fyrela.h"
#include "kzfrjj.h"
#include "fzfrjj.h"
#include "fshojj.h"
#include "kshojj.h"
C
      INTEGER FYNHIS
      DATA IONC /0/
#include "bmacro.h"
#include "kmacro.h"
C ---------------------------------------------------------------------
C Initializations:
      IF (IONC.EQ.0) THEN
         NFKIN = NAMIND('FKIN')
         NFVER = NAMIND('FVER')
         NFPOI = NAMIND('FPOI')
         NFPOL = NAMIND('FPOL')
         NFZFR = NAMIND('FZFR')
         NFSHO = NAMIND('FSHO')
         NKSHO = NAMIND('KSHO')
         NKZFR = NAMIND('KZFR')
         NKPOL = NAMIND('KPOL')
         NKVOL = NAMIND('KVOL')
         NKHIS = NAMIND('KHIS')
         IONC = 1
      ENDIF
      IBFUL = 0
C
C Create FKIN, FVER, FPOI banks
      LE = LMHLEN + LFKINA*LTKNUM
      CALL AUBOS('FKIN',0,LE,JFKIN,IGAR1)
      IF (JFKIN.EQ.0) GOTO 990
      IW(JFKIN+LMHCOL) = LFKINA
      IW(JFKIN+LMHROW) = LTKNUM
C
      LE = LMHLEN + LFVERA*LVXNUM
      CALL AUBOS('FVER',0,LE,JFVER,IGAR2)
      IF (JFVER.EQ.0) GOTO 990
      IW(JFVER+LMHCOL) = LFVERA
      IW(JFVER+LMHROW) = LVXNUM
C
      LE = LMHLEN + LFPOIA*IW(INDKIN)
      CALL AUBOS('FPOI',0,LE,JFPOI,IGAR4)
      IF (JFPOI.EQ.0) GOTO 990
      IW(JFPOI+1) = LFPOIA
      IW(JFPOI+2) = IW(INDKIN)
C
      IF (IGAR1+IGAR2+IGAR4 .GT. 0) THEN
         JFKIN = IW(NFKIN)
         JFVER = IW(NFVER)
         CALL BWIND (IW,'KINE',IW(INDKIN),INDKIN)
         CALL BWIND (IW,'VERT',IW(INDVER),INDVER)
         IF (INDKIN.EQ.0 .OR. INDVER.EQ.0) GOTO 990
      ENDIF
C
C - Fill FVER bank
C
      JKVOL = IW(NKVOL)
      DO 10 IVO =1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 10
         JVER = IW(INDVER+IVO)
         MOTHO = INPTRK(JVER)
         IF (MOTHO .EQ. 0) THEN
            MOTH = 0
         ELSE
            MOTH = ITABL(JDKNFO,MOTHO,1)
         ENDIF
         NOUT  = ITABL (JDVOUT,IVN,2)
         IFOUT = ITABL (JDVOUT,IVN,1)
         KFVER = KROW (JFVER,IVN)
         RW(KFVER+JFVEVX) = RW(KPARVK(JVER)+1)
         RW(KFVER+JFVEVY) = RW(KPARVK(JVER)+2)
         RW(KFVER+JFVEVZ) = RW(KPARVK(JVER)+3)
         RW(KFVER+JFVETO) = TOFLIT(JVER)
         IW(KFVER+JFVEIP) = MOTH
         IW(KFVER+JFVEIS) = IFOUT
         IW(KFVER+JFVENS) = NOUT
         IF (JKVOL .EQ. 0) THEN
            IW(KFVER+JFVEVN)=INTCHA ('    ')
            IW(KFVER+JFVEVM)=INTCHA ('    ')
         ELSE
            IW(KFVER+JFVEVN) = ITABL(JKVOL,IVO,JKVOVN)
            IF (LCOLS(JKVOL).GE.JKVOVM) THEN
               IW(KFVER+JFVEVM) = ITABL(JKVOL,IVO,JKVOVM)
            ENDIF
         ENDIF
  10  CONTINUE
C
C - Fill FKIN bank
C
      JKHIS = IW(NKHIS)
      KFKIN = JFKIN + LMHLEN
      DO 20 ITN = 1,LTKNUM
         ITO = ITABL (JDKOFN,ITN,1)
         JKIN = IW(INDKIN+ITO)
         RW(KFKIN+JFKIPX) = RW(KPARVK(JKIN)+1)
         RW(KFKIN+JFKIPY) = RW(KPARVK(JKIN)+2)
         RW(KFKIN+JFKIPZ) = RW(KPARVK(JKIN)+3)
         RW(KFKIN+JFKIMA) = PMASVK(JKIN)
         IW(KFKIN+JFKIPA) = KINTYP(JKIN)
         INPV = ITABL (JDVNFO,INPVRT(JKIN),1)
         IW(KFKIN+JFKIOV) = INPV
         IF (NOFVK(JKIN) .GT. 1) THEN
            DO 19 IV =2,NOFVK(JKIN)
               IVSO = IW(KLISVK(JKIN)+IV)
               IVSN = ITABL(JDVNFO,IVSO,1)
               IF(IVSN .NE. 0) IW(KFKIN+JFKIEV) = IVSN
 19         CONTINUE
         ENDIF
         IF (JKHIS.GT.0 .AND. ITO .LE. KINTRK) THEN
            IOHIS = ITABL(JKHIS,ITO,1)
            IF (IOHIS .NE. 0) IW(KFKIN+JFKIHC) = FYNHIS (IOHIS)
         ENDIF
  20  KFKIN = KFKIN + LCOLS(JFKIN)
C
C - Fill FPOI bank
C  Tracks which have been dropped are related to their first
C  first direct predecessor, in its new ordering in FKIN of course,
C  which has been kept.
C
      DO 40 ID = 1, IW(INDKIN)
         INWR = ITABL (JDKNFO,ID,1)
         IF (INWR .EQ. 0) THEN
C        Track was dropped. Go up the tree until a non-dropped parent
C        is found
            IW(KROW(JFPOI,ID)+JFPOIS) = 1
            IOLD = ID
 41         IOLD = MOTHVK(NLINK('KINE',IOLD))
            INWR = ITABL (JDKNFO,IOLD,1)
            IF (INWR .EQ. 0) GOTO 41
         ENDIF
         IW(KROW(JFPOI,ID)+JFPOIP) = INWR
 40   CONTINUE
C
C - create FSHO by swapping KSHO and FSHO, then update the track #
C
      JKSHO = IW(NKSHO)
      IF (JSHOL .GT. 0) THEN
         CALL BSWAP (IW,'KSHO','FSHO')
         JFSHO = IW(NFSHO)
         DO 49 I = 1,LROWS(JFSHO)
            ITO = ITABL (JFSHO,I,JFSHKI)
            IF (ITO .GT. 0) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               IW(KROW(JFSHO,I)+JFSHKI) = ITN
            ENDIF
            ITO = ITABL (JFSHO,I,JFSHK1)
            IF (ITO .GT. 0) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               IW(KROW(JFSHO,I)+JFSHK1) = ITN
            ENDIF
            ITO = ITABL (JFSHO,I,JFSHK2)
            IF (ITO .GT. 0) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               IW(KROW(JFSHO,I)+JFSHK2) = ITN
            ENDIF
 49      CONTINUE
      ENDIF
C
C - create FPOL by swapping KPOL and FPOL, then update the track #
C
      JKPOL = IW(NKPOL)
      IF (JKPOL .GT. 0) THEN
         CALL BSWAP (IW,'KPOL','FPOL')
         JFPOL = IW(NFPOL)
         DO 50 I = 1,LROWS(JFPOL)
            ITO = ITABL (JFPOL,I,JFPOKI)
            IF (ITO .GT. 0) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               IW(KROW(JFPOL,I)+JFPOKI) = ITN
            ENDIF
 50      CONTINUE
      ENDIF
C
C - create FZFR , then fill the fragmentation for row # (FKIN row #)
C
      JKZFR = IW(NKZFR)
      IF (JKZFR .GT. 0) THEN
         LE = LMHLEN + LFZFRA*LROWS(JKZFR)
         CALL AUBOS ('FZFR',0,LE,JFZFR,IGAR)
         IF (JFZFR .EQ. 0) GOTO 990
         JFZFR = IW(NFZFR)
         IW(JFZFR+LMHCOL)=LFZFRA
         IW(JFZFR+LMHROW)=LROWS(JKZFR)
         DO 51 I = 1,LROWS(JFZFR)
            ITO = I
            ZFR = RTABL(JKZFR,I,JKZFEF)
            IF (ZFR .GT. 0.) THEN
               ITN = ITABL (JDKNFO,ITO,1)
               RW(KROW(JFZFR,ITN)+JFZFZF) = ZFR
            ENDIF
 51      CONTINUE
      ENDIF
C
C normal, healthy return
#if defined(FYDEB)
      CALL PRFKIN
#endif
      RETURN
C sick return: not enough space to book bank...
 990  CONTINUE
      IBFUL = -1
      END
#endif
