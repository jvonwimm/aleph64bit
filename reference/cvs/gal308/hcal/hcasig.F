*DK hcasig
      SUBROUTINE HCASIG
C --------------------------------------------
C
C! -   Create analog signal
C!
C!     Author G. Zito  - 86/05/21
C!     Modified by G.Catanesi 87/10/27
C!     Modified by F.Ranjard  89/02/10
C!     Modified by A.Venturi & A.Sciaba` 98/7/1
C!             increase the size of HTHT
C!
C!     Input bank: JDHCSE
C!     Output banks:JDHCHI,HWHT,HTHT       'E' list
C!
C!    -Called by ASASIG
C!       -Calls : HCGEST, HCCRDI, HCINDU, HCCRTO, HCCRPL
C!           HCHISE,HCHIST,HCSTAT          from this .HLB
C!           SORTIQ                        from CERNLIB
C!
C -------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "bcs.h"
#include "iocom.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "hchijj.h"
#include "hcthjj.h"
#include "hthtjj.h"
#include "hwhtjj.h"
#include "hphtjj.h"
#include "hlwdjj.h"
#include "hccoun.h"
#include "bmacro.h"
C --------------------------------------------------------
C        Do some statistics and print track elements
C
      IF (JDHCSE.EQ.0) RETURN
      NHCSE = LROWS (JDHCSE)
      IF (NHCSE .EQ. 0) GOTO 30
      KHCSE = JDHCSE + LMHLEN
C - order HCSE in increasing order of tube address
      CALL SORTIQ(IW(KHCSE+1),LHCSEA,NHCSE,5)
C
      IF(FHCDEB)THEN
C
         WRITE(LOUTIO,500)NHCSE
         DO 10 I = 1, NHCSE
            WRITE(LOUTIO,510)I,(IW(KHCSE+II),II=1,5),(RW(KHCSE+II),II
     +      =6,11) ,IW(KHCSE+12)
            KHCSE = KHCSE + LHCSEA
   10    CONTINUE
C
         IF (FHCDB2 .AND. JDHCTH.GT.0) THEN
            CALL SORTIQ(IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH),LROWS(JDHCTH),
     &                  1)
         ENDIF
C
      ENDIF
C
C
C      Generate streamers:  JDHCSE -> JDHCHI (1-4)
C
      CALL WBANK(IW,JDHCHI,NHCSE*LHCHIA+LMHLEN,*20)
      IW(JDHCHI-3) = INTCHA ('HCHI')
      IW(JDHCHI+1) = LHCHIA
      IW(JDHCHI+2) = 0
      CALL HCGEST
C
C     Create digital pattern: JDHCHI -> HWHT
C     and fill word(14) of HCSE with the HWHT row #
C
      CALL ALBOS ('HWHT',0,NHCSE*LHWHTA+LMHLEN,JHWHT,IGARB)
      IW(JHWHT+1) = LHWHTA
      IW(JHWHT+2) = 0
      CALL BLIST(IW,'E+','HWHT')
      CALL HCCRDI
C
C     Remember the hit position along the tube
C     JDHCHI + HCSE -> HLWD
      NHCHI = LROWS(JDHCHI)
      CALL ALBOS ('HLWD',0,NHCHI*LHLWDA+LMHLEN,JHLWD,IGARB)
      IW(JHLWD+1) = LHLWDA
      IW(JHLWD+2) = 0
      CALL BLIST(IW,'E+','HLWD')
      CALL HCHLWD
C
C     Take in account the induction effect on digital pattern
C
      IF(ICHCJO(4).EQ.0)THEN
         CALL HCINDU
         IF(FHCDEB)THEN
            JHWHT = IW(NAHWHT)
            NHWHT = LROWS (JHWHT)
            KHWHT = JHWHT + LMHLEN
            WRITE(LOUTIO,520)NHWHT
            WRITE(LOUTIO,530) (IW(KHWHT+I), I=1,NHWHT*LHWHTA,LHWHTA)
         ENDIF
      ENDIF
C
C
C     Create tower storey pattern: JDHCSE + JDHCHI -> 'HTHT'
C     and fill HCSE word(15) with the HTHT row #
C
      CALL ALBOS ('HTHT',0,20*NHCSE*LHTHTA+LMHLEN,JHTHT,IGARB)
      IW(JHTHT+1) = LHTHTA
      IW(JHTHT+2) = 0
      CALL BLIST(IW,'E+','HTHT')
      CALL HCCRTO
C
C     Create planes signals: JDHCSE + JDHCHI -> 'HPHT'
C
      CALL ALBOS ('HPHT',0,NHCSE*LHPHTA+LMHLEN,JHPHT,IGARB)
      IW(JHPHT+LMHCOL) = LHPHTA
      IW(JHPHT+LMHROW) = 0
      CALL BLIST(IW,'E+','HPHT')
      CALL HCCRPL
C
C     order HTHT in increasing order of  storey address
C     modify HCSE accordingly
C
      CALL HCHISE
C
C     Do some statistics on analog signals
C
      CALL HCSTAT
C
C - if GALEPH flag write history banks
      IF (FGALJO) THEN
C
C - create history banks 'HWTD' and 'HTTD' which give the correlation
C   between track# and fired tubes / fired storeys.
C
         CALL HCHIST ('HWTD',4,NAHWTD,12,14)
         CALL HCHIS1 ('HTTD',4,NAHTTD,12,15)
      ENDIF
C
C - compress output banks
      CALL AUBPRS ('HTHTHWHTHWTDHTTDHPHT')
C
      GOTO 30
C
C - no enough space to book working bank
C
   20 CONTINUE
      CALL WDROP (IW,JDHCSE)
      CALL WDROP (IW,JDHCHI)
      CALL ALTELL('HCASIG: not enough space to book work bank',1,'NEXT')
C
   30 CONTINUE
C       Drop here temporary banks
C
      CALL WDROP (IW,JDHCSE)
      CALL WDROP(IW,JDHCHI)
C
      RETURN
  500 FORMAT (/1X,'+++HCASIG+++ HCSE  McHcTubeSegments ',I5/ 1X,
     +'seg#  Tu#  La#  Mo#  Po#  Tub.add.    XTube     YTube  ',
     +' Proj.on.wi  XAleph    YAleph    ZAleph  Trk#')
  510 FORMAT (5I5,I10,6F10.2,I5)
  520 FORMAT (1X,'+++ HCASIG +++ HWHT with induction effect ',I5)
  530 FORMAT(1X,10I10)
      END
#endif
