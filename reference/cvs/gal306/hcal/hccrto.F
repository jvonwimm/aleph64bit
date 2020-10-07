*DK hccrto
      SUBROUTINE HCCRTO
C --------------------------------------------
C
C!   Builds the tower hit bank
C
C!     Author      :G.Zito      86/05/21
C!     Modified    :G.Catanesi  86/12/03
C!                 :G.Catanesi  87/10/21
C!                 :G.Catanesi  88/08/20
C!
C!     Input bank  : JDHCSE   McHcTrachSegment
C!                   JDHCHI   McHcHits
C!     Output bank : 'HTHT'   McHcStoreys
C!
C!         Called by: HCASIG
C!        -Calls : HFNDTR, HCSTEN     from this .HLB
C!                 HFNDTW             from Alephlib
C------------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "hchijj.h"
#include "hthtjj.h"
      DIMENSION ANGL(2), ITOW(3), ITRI(2)
      CHARACTER*6 SYSRF
C
#include "bmacro.h"
C
C       Loop on hits
C
      NHCHI = LROWS (JDHCHI)
      IF(NHCHI.EQ.0) RETURN
      KHCHI = JDHCHI + LMHLEN
      JHTHT = IW(NAHTHT)
      NHTHT = 0
      KHTHT = JHTHT + LMHLEN
C
C       ANGL  = phi, theta
C       ITOW  = nphi, ntheta, nstac
C       ITRI  = nphi trigger, ntheta trigger
C
      DO 50 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         KHCSE = KROW (JDHCSE,NSEG)
         IPOR = ITABL (JDHCSE,NSEG,4)
         ILAY = ITABL (JDHCSE,NSEG,2)
C
C find tower adress i,j,k
C
         CALL HFNDTW(RW(KHCSE+9),SYSRF,ITOW(2),ITOW(1),ITOW(3), ISUBC,
     +   IMOD,IPL,IDEAD)
C
         IF(IDEAD.GT.0) GOTO 50
C
         CALL HFNDTR(RW(KHCSE+9),IPOR,ITOW,ITRI)
C
         NTOW = ITOW(1)*10000 + ITOW(2)*100 + ITOW(3)
C
C - IF it is not the 1st entry in 'HTHT' THEN
C      IF this tower# has already been entered into 'HTHT' THEN
C         sum up energy into this tower
C         jump to the next tower
C   ELSE  check dead regions if not Hcdetail
C         fill tower bank
C   ENDIF
C
         IF (NHTHT.EQ.0) GOTO 30
         DO 10 JROW =1,NHTHT
            IF (NTOW .EQ. ITABL(JHTHT,JROW,1)) GOTO 20
   10    CONTINUE
         GOTO 30
C        the tower# has already been entered
   20    EDEP = HCSTEN(IW(KHCHI+4))
         KJROW = KROW(JHTHT,JROW)
         RW(KJROW+4) = RW(KJROW+4) + EDEP
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + EDEP
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) =RW(KHCTH+5)+EDEP
         ENDIF
         IW(KHCSE+JHCSFS) = JROW
         GOTO 50
C
   30    CONTINUE
C
C - Fill 'HTHT' with new tower
         NHTHT = NHTHT + 1
         IW(KHTHT+1) = NTOW
         IW(KHTHT+2) = ITRI(1)
         IW(KHTHT+3) = ITRI(2)
         RW(KHTHT+4) = HCSTEN(IW(KHCHI+4))
C
         IF(FHCDB2)THEN
            HCEPOR(IPOR) = HCEPOR(IPOR) + RW(KHTHT+4)
            IROW = LOCTAB (IW(JDHCTH+LMHLEN+1),LCOLS(JDHCTH) ,LROWS
     +      (JDHCTH),1,IW(KHCSE+12))
            KHCTH = KROW(JDHCTH,IROW)
            RW(KHCTH+5) = RW(KHCTH+5) + RW(KHTHT+4)
         ENDIF
         KHTHT = KHTHT + LHTHTA
C
   40    IW(KHCSE+JHCSFS) = NHTHT
   50 KHCHI = KHCHI + LHCHIA
      IW(JHTHT+LMHROW) = NHTHT
C
      IF (FHCDEB) THEN
         ETOT = 0.
         KHTHT = JHTHT + LMHLEN
         DO 60 I = 1,NHTHT
            ETOT = ETOT + RW(KHTHT+4)
 60      KHTHT = KHTHT + LHTHTA
         KHTH1 = JHTHT + LMHLEN
         WRITE (LOUTIO,500) LROWS(JHTHT),ETOT
         WRITE (LOUTIO,510) ((IW(K+I),I=1,3),RW(K+4),K=KHTH1, KHTH1
     +   +NHTHT*LHTHTA-1,LHTHTA)
      ENDIF
      RETURN

 500  FORMAT (/1X,'+++HCCRTO+++ McHcStoreys - ',5X,I3,' storeys , ',
     &        F5.2,' GeV deposited energy'/
     +3(4X,'PhThSt  Pt  Tt' ,'   Energy', 6X))
  510 FORMAT (3(I10,I4,I4,F10.5,5X))
      END
#endif
