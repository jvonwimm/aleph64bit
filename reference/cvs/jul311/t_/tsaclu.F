      SUBROUTINE TSACLU
C---------------------------------------------------------------
C! Find theta-phi pad clusters
C!
C!    Author:    M. Mermikides  890904
C!
C!    Description
C!    ===========
C?    1.  Use a simple clustering algorithm to find pad hits in adjacent
C?        bins of the histogram stored in the JTPHIS work bank.
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpcojj.h"
#include "tbcojj.h"
#include "tsawrk.h"
C
      LOGICAL LFRST
      SAVE NTPCO,NTBCO
C
      DATA LTPHC/4/
C
      DATA LFRST/.TRUE./
C
#include "bmacro.h"
C
      IF (LFRST) THEN
         LF RST=.FALSE.
         NTPCO=NAMIND('TPCO')
         NTBCO=NAMIND('TBCO')
      ENDIF
C
      IF( LROWS(JTHPHI).LE.1) GO TO 999
      NBTHE = IW(JTPHIS+LMHCOL)
      NBPHI = IW(JTPHIS+LMHROW)
      NCLUS = 0
C
C                      <=================== Main loop of cluster finder
    1 MXNCL = 0
C  Find most populated bin among unused entries
      DO 10 I=1, LROWS(JTHPHI)
         IF(ITABL(JTHPHI,I,4).GT.100) GO TO 10
         ITHE = ITABL(JTHPHI,I,1)
         IPHI = ITABL(JTHPHI,I,2)
         IND = KROW(JTPHIS,IPHI)+ITHE
         IF (IW(IND).GT.MXNCL) THEN
            MXNCL = IW(IND)
            IMAX = I
         ENDIF
   10 CONTINUE
C
C   If max. not found, it means that we have exhausted list.
C
      IF (MXNCL.EQ.0) GO TO 100
C
C  Look at central and adjacent histogram bins to accumulate cluster
C
      ITHE = ITABL(JTHPHI,IMAX,1)
      IPHI = ITABL(JTHPHI,IMAX,2)
      NCLUS = NCLUS + 1
C
      DO 20 IP = -1,1
         IPH = IPHI + IP
         IF (IPH.LE.0) IPH = NBPHI
         IF (IPH.GT.NBPHI) IPH = 1
         DO 30 IT = -1,1
            ITH = ITHE + IT
            IF (ITH.LE.0) ITH = NBTHE
            IF (ITH.GT.NBTHE) ITH = 1
            IND = KROW(JTPHIS,IPH)+ITH
            IF (IW(IND).LE.0) GO TO 30
C Now mark entries belonging to cluster
            DO 40 IC=1, LROWS(JTHPHI)
               IF(ITH.NE.ITABL(JTHPHI,IC,1)) GO TO 40
               IF(IPH.NE.ITABL(JTHPHI,IC,2)) GO TO 40
               IROW = ITABL(JTHPHI,IC,4)
               IW(KROW(JTHPHI,IC)+4) = NCLUS*100 + IROW
   40       CONTINUE
C Set contents of histogram bin negative so it doesn't get used again
            IW(IND) = -IW(IND)
   30    CONTINUE
   20 CONTINUE
C
      GO TO 1
C                      ==========>      Get next cluster
C
C Now sort JTHPHI in increasing cluster/padrow number
C
  100 CALL SORTIQ(IW(JTHPHI+LMHLEN+1),LCOLS(JTHPHI),LROWS(JTHPHI),4)
C
C  Fill cluster ref. bank
C
      CALL WBANK(IW,JTPHCL,LMHLEN+NCLUS*LTPHC,*800)
      IW(JTPHCL+LMHCOL) = LTPHC
      IW(JTPHCL+LMHROW) = 0
      IW(JTPHCL-3) = INTCHA('JTP3')
C  Find cluster centroid
      KTPCO = IW(NTPCO)
      KTBCO = IW(NTBCO)
      THEAV = 0.
      DPAV = 0.
      NCOOR = 0
      IOFF = 0
      ICLAS = ITABL(JTHPHI,1,4)/100
      IC = ITABL(JTHPHI,1,3)
      IF (IC.GT.0) THEN
         PHI1 = RTABL(KTPCO,IC,JTPCPH)
      ELSE
         PHI1 = RTABL(KTBCO,-IC,JTBCPH)
      ENDIF
      DO 60 I=1,LROWS(JTHPHI)
         ICLU = ITABL(JTHPHI,I,4)/100
         IC = ITABL(JTHPHI,I,3)
         IF (IC.GT.0) THEN
            PHI = RTABL(KTPCO,IC,JTPCPH)
            THE = ATAN(RTABL(KTPCO,IC,JTPCRV)/RTABL(KTPCO,IC,JTPCZV))
         ELSE
            PHI = RTABL(KTBCO,-IC,JTBCPH)
            THE = ATAN(RTABL(KTBCO,-IC,JTBCRV)/RTABL(KTBCO,-IC,JTBCZV))
         ENDIF
         IF (THE.LT.0.0) THE = THE + PI
C
C  Find mean theta, phi. Take care of wrap-around in phi by averageing
C  deviation from 1st phi position. Theta should be free from wrap-
C  around problems because of the acceptance of the TPC.
C
         IF(ICLU.EQ.ICLAS) THEN
            THEAV = THEAV + THE
            DPAV = DPAV + TRPHDI(PHI,PHI1)
            NCOOR = NCOOR + 1
         ELSE
C  Break in cluster number found
            RW(KNEXT(JTPHCL) + 1) = THEAV/FLOAT(NCOOR)
            PHIAV = DPAV/FLOAT(NCOOR) + PHI1
            IF (PHIAV.GT.TWOPI) PHIAV = PHIAV - TWOPI
            IF (PHIAV.LT.0.)    PHIAV = PHIAV + TWOPI
            RW(KNEXT(JTPHCL) + 2) = PHIAV
            IW(KNEXT(JTPHCL) + 3) = NCOOR
            IW(KNEXT(JTPHCL) + 4) = IOFF
            IW(JTPHCL + LMHROW) = IW(JTPHCL + LMHROW) + 1
            IOFF = IOFF + NCOOR
            NCOOR = 1
            PHI1 = PHI
            DPAV = 0.
            ICLAS = ICLU
            THEAV = THE
         ENDIF
C
   60 CONTINUE
C  Don't forget last cluster
      RW(KNEXT(JTPHCL) + 1) =THEAV/FLOAT(NCOOR)
      PHIAV = DPAV/FLOAT(NCOOR) + PHI1
      IF (PHIAV.GT.TWOPI) PHIAV = PHIAV - TWOPI
      IF (PHIAV.LT.0.)    PHIAV = PHIAV + TWOPI
      RW(KNEXT(JTPHCL) + 2) = PHIAV
      IW(KNEXT(JTPHCL) + 3) = NCOOR
      IW(KNEXT(JTPHCL) + 4) = IOFF
      IW(JTPHCL + LMHROW) = IW(JTPHCL + LMHROW) + 1
C
      GO TO 999
  800 CALL RERROR('TSACLU',4,'Error booking/extending work bank')
  999 RETURN
      END
#endif
