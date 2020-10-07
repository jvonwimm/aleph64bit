      SUBROUTINE TPCRUN
C
C-------------------------------------------------------------------
C! Run summary from TPC reconstruction
C!
C!    Author:  W. Wiedenmann  14-07-93 store summary information
C!                                     in JTRE bank
C!   Modified :- E. Lancon             21-JUL-1993
C!              Add dE/dx summary
C!   Modified :- E. Lancon             26-AUG-1993
C!              restrict the fit to the FWHM interval
C!             - F.Ranjard             10-MAY-1994
C!              fill JHDX bank with DEDX histograms
C!   Modified :- I. Tomalin            15-AUG-1995
C!              call TVDVEL
C!------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
#include "tflags.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "jhdxjj.h"
#include "rparac.h"
#include "jtrejj.h"
#include "rcurnt.h"
C
      CHARACTER*80 HTIT
C
      PARAMETER( NP =  3)
      DIMENSION PAR(NP), STEP(NP), PMIN(NP), PMAX(NP), SIGPAR(NP)
C
      DIMENSION      LWTKTM(LTSECT),LPTKTM(LTSECT),
     &               LMISTM(LTSECT),LWSYTM(LTSECT),LPSYTM(LTSECT),
     &               LBADTM(LTSECT),LDRPTM(LTSECT),LHORTM(LTSECT),
     &               LROWTM(LTSECT),LBDHTM(LTSECT),LTPDTM(LTSECT),
     &               LCPDTM(LTSECT)

      DATA           LWTKTM/LTSECT*0/,LPTKTM/LTSECT*0/,
     &               LMISTM/LTSECT*0/,LWSYTM/LTSECT*0/,LPSYTM/LTSECT*0/,
     &               LBADTM/LTSECT*0/,LDRPTM/LTSECT*0/,LHORTM/LTSECT*0/,
     &               LROWTM/LTSECT*0/,LBDHTM/LTSECT*0/,LTPDTM/LTSECT*0/,
     &               LCPDTM/LTSECT*0/

C
C--------------------------------------------------------------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
C Calculate TPC drift velocity using VDET.
      CALL TVDVEL
C
      IF (FMCRUN) GOTO 999
C
      WRITE(LOUTRL,20)
   20 FORMAT(//1X,T20,'Run Summary for TPC Reconstruction')
      WRITE(LOUTRL,21)
   21 FORMAT(1X,T20,'--- ------- --- --- --------------'/)
C
C++   Fill Monitoring bank
C
      CALL BDROP (IW,'JTRE')
      CALL AUBOS('JTRE',IRUNRC,LJTREA*LTSECT+LMHLEN,KJTRE,IER)
      IF (IER.EQ.2) GOTO 777
      IW(KJTRE+LMHCOL) = LJTREA
      IW(KJTRE+LMHROW) = LTSECT
      DO 105 I=1,LTSECT
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRWT)= NWTKTM(I)-LWTKTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRPT)= NPTKTM(I)-LPTKTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRMD)= NMISTM(I)-LMISTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRTW)= NWSYTM(I)-LWSYTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRTP)= NPSYTM(I)-LPSYTM(I)
C
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRDF)= NDRPTM(I)-LDRPTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRHR)= NHORTM(I)-LHORTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRRR)= NROWTM(I)-LROWTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRBH)= NBDHTM(I)-LBDHTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRTT)= NTPDTM(I)-LTPDTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRDP)= NCPDTM(I)-LCPDTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRBC)= NBADTM(I)-LBADTM(I)
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRFA)= 0
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRFB)= 0
         IW(KJTRE+LMHLEN+(I-1)*IW(KJTRE+1)+JJTRFC)= 0
C.....   Store counters for the next run
         LWTKTM(I)=NWTKTM(I)
         LPTKTM(I)=NPTKTM(I)
         LMISTM(I)=NMISTM(I)
         LWSYTM(I)=NWSYTM(I)
         LPSYTM(I)=NPSYTM(I)
         LDRPTM(I)=NDRPTM(I)
         LHORTM(I)=NHORTM(I)
         LROWTM(I)=NROWTM(I)
         LBDHTM(I)=NBDHTM(I)
         LTPDTM(I)=NTPDTM(I)
         LCPDTM(I)=NCPDTM(I)
         LBADTM(I)=NBADTM(I)
  105 CONTINUE
C
      WRITE(LOUTRL,22)
   22 FORMAT(/1X,T20,'TPC Summary stored in JTRE Bank   ')
C
C?   Fill dE/dx monitoring bank
C
      IF (FREPRO) GOTO 999
C
      CALL BDROP (IW,'JTDXJHDX')
      CALL AUBOS('JHDX',IRUNRC,LJHDXA*(LTSECT+1)+LMHLEN,KJHDX,IER)
      IF (IER.EQ.2) GOTO 888
      CALL BKFMT('JHDX','2I,(F)')
C
      IW(KJHDX+LMHCOL) = LJHDXA
      IW(KJHDX+LMHROW) = LTSECT+1
C
      IOFF = JULTP*1000 + 100
      DO I=1,LTSECT+1
        ID = IOFF + I
        JJHDX = KROW(KJHDX,I)
        CALL HUNPAK (ID,RW(JJHDX+JJHDBI), '  ', 1)
      ENDDO
C
C
      WRITE(LOUTRL,23)
   23 FORMAT(/1X,T20,'dE/dx monitoring stored in JHDX Bank   ')
      GOTO 999
C
  777 CALL RERROR ('TPCRUN', 1, 'UNABLE TO CREATE JTRE')
      GOTO 999
C
  888 CALL RERROR ('TPCRUN', 2, 'UNABLE TO CREATE JHDX')
      GOTO 999
C
  999 RETURN
      END
#endif
