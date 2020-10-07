      SUBROUTINE MPREDA
C--------------------------------------------------------------------
C!  prepare muon data
C!
C!   F.Bossi/G.Capon/D.Kuhn             861107
C!
C!   Modified    by   R.Xu              871029
C!
C!   Input:  - Banks MUDI    : raw data from mu-chamber
C!                             (Barrel, Middle-angle and Endcaps)
C! Output:   bank MHIT
C!
C! variables used :
C!  ISUBC      : subcomp # (1=endcaps,2=midangle,3=barrel)
C!  NSLOT      : slot #    (1,24 barrel; 25,62 midangle; 63,78 endcap)
C!  LAYER      : streamer tubes layer in a slot =1,2
C!  XX,YY      : Coordinates of strips clusters in local module system
C!  RHO,THETA,PHI: Rho, Theta and Phi of hit in ALEPH - system
C!
C!
C!   Description
C!   ===========
C!   Digitisings of strip clusters from hits in Mu-chambers are
C!   converted to polar coordinates in the ALEPH-system
C!
C!
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "rflags.h"
#include "mdebug.h"
#include "rlunit.h"
#include "munamc.h"
        PARAMETER (NHITMX=500)
C        put the above value of Nhitmx in Rerror message below !!
#include "mhitjj.h"
#include "mujosu.h"
#include "bmacro.h"
C--------------------------------------------------------------------
C--------------------------------------------------------------------
        CALL BLIST(IW,'E+','MHITMCADMTHR')
C
      IHIST=JHISRF(JULMU)
      JMUDI=IW(NAMUDI)
      IF(JMUDI.EQ.0) THEN
         IF(IMPRIN.GE.1)WRITE(LDEBRL,*)'  NO MUON HITS IN THIS EVENT'
         RETURN
      ENDIF
      NCL=IW(JMUDI+2)
      NNMUDI=NNMUDI+1
      NCLTOT=NCLTOT+NCL
      IF(NCL.GT.NCLMAX)NCLMAX=NCL
      LMUDI=IW(JMUDI+1)
      NMHIT=0
C
C?          create MHIT bank
C
      LNHIT =  2+ NHITMX*LMHITA
      CALL AUBOS('MHIT',0,LNHIT,JMHIT,IGARB)
      IF(IGARB.EQ.1)JMUDI=IW(NAMUDI)
      IF (IGARB.EQ.2) THEN
         CALL RERROR('MPREDA',1,'no space for MHIT bank')
         RETURN
      ENDIF
      KMHIT = JMHIT + 2
C
C             check if data card MUEX exists on input file
C             if it exists, the external layer of muon chambers
C             will not be processed
C
      JMUEX=NLINK('MUEX',0)
C
C           loop on x and y strips
C
      DO 20 J=1,NCL-1
         NSLOT=ITABL(JMUDI,J,1)
         IF (JMUEX.GT.0) THEN
            IF(NSLOT.GE.13 .AND. NSLOT.LE.24) GO TO 20
            IF(NSLOT.GE.35 .AND. NSLOT.LE.43) GO TO 20
            IF(NSLOT.GE.54 .AND. NSLOT.LE.62) GO TO 20
            IF(NSLOT.GE.67 .AND. NSLOT.LE.70) GO TO 20
            IF(NSLOT.GE.75 .AND. NSLOT.LE.78) GO TO 20
         ENDIF
         ISUBC=3
         IF(NSLOT.GE.25)ISUBC=2
         IF(NSLOT.GE.63)ISUBC=1
         IF(NSLOT.GE.79)ISUBC=3
C
C?      JPL ... number of strip plane : 1(X),2(Y),3(Y),4(X)
C?      JSTRP=  strip #     JCLUS = cluster width
C?      LAYER=1 for first x-y layer pair, =2 for the second
C?      ISUBC=subcomp # =1(endcaps),2(middle-angle),3(barrel)
C
         JPL=ITABL(JMUDI,J,2)
         JSTRP=ITABL(JMUDI,J,4)
         JCLUS=ITABL(JMUDI,J,3)
         LAYR2=JPL/3+1
C
         DO 10 I=J+1,NCL
            NSLO2=ITABL(JMUDI,I,1)
         IF (JMUEX.GT.0) THEN
            IF(NSLO2.GE.13 .AND. NSLO2.LE.24) GO TO 10
            IF(NSLO2.GE.35 .AND. NSLO2.LE.43) GO TO 10
            IF(NSLO2.GE.54 .AND. NSLO2.LE.62) GO TO 10
            IF(NSLO2.GE.67 .AND. NSLO2.LE.70) GO TO 10
            IF(NSLO2.GE.75 .AND. NSLO2.LE.78) GO TO 10
         ENDIF
C
C?     associate x and y strip to form a muon hit if :
C?       1) same slot  :  (NSLOT=NSLO2)
C?       2) same  layer  :  (LAYR2=LAYER)
C?       3) orthogonal views : (IPL.NE.JPL)
C?
            IF(NSLO2.NE.NSLOT) GO TO 10
            IPL=ITABL(JMUDI,I,2)
            LAYER=IPL/3+1
            IF(LAYER.NE.LAYR2) GO TO 10
            IF(IPL.EQ.JPL) GO TO 10
C
            ISTRP=ITABL(JMUDI,I,4)
            ICLUS=ITABL(JMUDI,I,3)
            NMHIT=NMHIT+1
C
C?             convert strip numbers to local coordinates
C
        IF(IPL.EQ.2.OR.IPL.EQ.3)CALL MXYLOC(ISUBC,LAYER,JSTRP,JCLUS,
     +  ISTRP,ICLUS,XX,YY)
        IF(IPL.EQ.1.OR.IPL.EQ.4)CALL MXYLOC(ISUBC,LAYER,ISTRP,ICLUS,
     +  JSTRP,JCLUS,XX,YY)
C
C?             convert local coordinates to Aleph polar coordinates
C
         IF((ISUBC.EQ.3).AND.(NSLOT.LE.24)) THEN
            CALL MBAPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,
     +      THETA, PHI)
         ENDIF
            IF(ISUBC.EQ.2)CALL MMAPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,
     +      PHI)
            IF(ISUBC.EQ.1)CALL MENPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,THETA,
     +      PHI)
            IF(NSLOT.GE.79)CALL MBSPOL(NSLOT,ISUBC,LAYER,XX,YY,RHO,
     +      THETA, PHI)
C
C?             fill bank MHIT / fill histos
C
            IW(KMHIT + JMHISC) = ISUBC
            IW(KMHIT + JMHISN) = NSLOT
            IW(KMHIT + JMHILN) = LAYER
            RW(KMHIT + JMHIXL) = XX
            RW(KMHIT + JMHIYL) = YY
            RW(KMHIT + JMHIRH) = RHO
            RW(KMHIT + JMHITH) = THETA
            RW(KMHIT + JMHIPH) = PHI
            KMHIT=KMHIT+LMHITA
            IF (IHIST.GT.0) THEN
               CALL HFILL(8001,THETA,0.,1.)
               CALL HFILL(8002,PHI,0.,1.)
               FNSLO=NSLOT
               CALL HFILL(8003,FNSLO,0.,1.)
            ENDIF
        IF (NMHIT .GE. NHITMX) THEN
            CALL RERROR('MPREDA',2,'more than 500 muon hits')
            GO TO 30
        ENDIF
C
 10      CONTINUE
 20   CONTINUE
C
 30   CONTINUE
      FNMHI=NMHIT
      IF (IHIST.GT.0) CALL HFILL(8004,FNMHI,0.,1.)
      IF(IMPRIN.GE.1)WRITE(LDEBRL,71)NMHIT
 71   FORMAT(1X,I3,' MUON HITS IN THIS EVENT')
C
C?           adjust length of bank MHIT
C
      IF (NMHIT.EQ.0) THEN
         IND=NDROP('MHIT',0)
      ELSE
         IW(JMHIT + 1) = LMHITA
         IW(JMHIT + 2) = NMHIT
         LNHIT = 2 + NMHIT*LMHITA
         CALL AUBOS('MHIT',0,LNHIT,JMHIT,IGARB)
      IF (IGARB.EQ.2) CALL RERROR('HPREDA',3,'no update of MHIT bank')
         IF(IMPRIN.GE.3) CALL PRTABL('MHIT',0)
      ENDIF
C
C?         compare with MonteCarlo true data
C
      IF(NMHIT.GT.0.AND.JHISRF(JULMU).GE.2) CALL MUCOMP
C
      RETURN
      END
#endif
