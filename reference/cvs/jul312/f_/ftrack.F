        SUBROUTINE FTRACK
C-------------------------------------------------------------------
C
C!  extrapolate TPC track through Ecal, Coil, Hcal
C!
C!  G.Capon                              28-Aug-87
C!
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "hcalg1.h"
#include "alcons.h"
#include "rparac.h"
#include "rflags.h"
#include "trexjj.h"
#include "frftjj.h"
#include "rconds.h"
#include "yknkjj.h"
      PARAMETER (NSTEP=26)
C                        parametrisation of dE/dx in Ecal and Coil
      PARAMETER (AL1=.172E-3,AL2=-8.2,AL3=1.14)
      PARAMETER (PB1=.166E-3,PB2=-6.1,PB3=4.12)
      PARAMETER (WMU=.10566, WMU2=.011164)
      DIMENSION ATFRA(9,NSTEP+4)
      DIMENSION DVE(7),DVF(7),ATIN(8),VV0(6)
      DIMENSION SCOO(NSTEP+4),STEP(NSTEP+4),TH2PR(NSTEP+4),R2MS(NSTEP+4)
      DIMENSION RADL(3)
      DATA RADL /2.4,8.05,2.45/
      DATA R1ECA,Z1ECA,R2ECA,Z2ECA,R1COI,R2COI
     +   / 185. , 245., 234., 297., 248.,264.7/
      DATA PCUT/1./
      DATA INIJO,NITER/0,4/
#include "bmacro.h"
C
      IF(INIJO.EQ.0) THEN
         INIJO=1
         IBON=1
         IF(FIELRC .LT. 0.1) IBON=0
         CALL BLIST(IW,'T+','TREX')
C          Test presence of Hcal geometry banks :
C          if only TPC+ECAL processing is required one must call
C          Hiniru to have Hcal geometry (G.Capon - Feb 1990)
         JHBGE=IW(NAMIND('HBGE'))
         IF (JHBGE.EQ.0) CALL HINIRU
         NFRFT=NAMIND('FRFT')
         NYKNK=NAMIND('YKNK')
      ENDIF
      JFRFT=IW(NFRFT)
      IF(JFRFT.EQ.0) GO TO 99
      NTRKS=LROWS(JFRFT)
      JYKNK=IW(NYKNK)
C
C?             loop on TPC tracks
C
      DO 10 ITRAC = 1,NTRKS
C
C?             don't extrapolate tracks coming into a kink
C
         IF (JYKNK.NE.0) THEN
            DO IYKNK = 1, LROWS(JYKNK)
                IF (ITRAC.EQ.ITABL(JYKNK,IYKNK,JYKNIT)) GO TO 10
            ENDDO
         ENDIF
C
         NVETT=0
         CURV =RTABL(JFRFT,ITRAC,JFRFIR)
         TADIP=RTABL(JFRFT,ITRAC,JFRFTL)
         PHI  =RTABL(JFRFT,ITRAC,JFRFP0)
         DW   =RTABL(JFRFT,ITRAC,JFRFD0)
         ZW   =RTABL(JFRFT,ITRAC,JFRFZ0)
         VV0(1)=CURV
         VV0(2)=TADIP
         VV0(3)=PHI
         VV0(4)=DW
         VV0(5)=ZW
         VV0(6)=RTABL(JFRFT,ITRAC,JFRFAL)
C     get DW, PHI after mul. scatt. in ITC-TPC wall  !!
         CALL UNEWDP(VV0,DW,PHI)
C
         RHO=1./ABS(CURV)
         PT=FIELRC*RHO*CLGHT/100000.
         THETA = ATAN2 (1.,TADIP)
         STHET = SIN(THETA)
         PPINI  = PT/STHET
C
C?             select penetrating particles
C?             note that we have different p-cuts in FTRACK and MUASS
C
         IF (PPINI . LT. PCUT) GO TO 10

         CTHET = COS(THETA)
         CPHI  = COS(PHI)
         SPHI  = SIN(PHI)
C
         DVF(1) =+DW*SPHI
         DVF(2) =-DW*CPHI
         DVF(3) = ZW
         DVF(4) = STHET*CPHI
         DVF(5) = STHET*SPHI
         DVF(6) = CTHET
         DVF(7) = PPINI
         Q     =-SIGN (1.,CURV)
C
C             track extrapol up to Ecal entrance
C
         CALL AUHCYL(R1ECA,Z1ECA,FIELRC,Q,DVF,DVE,ICODE)
         IF(ICODE.EQ.0) GO TO 10
C-      Initialize ATFRA
         DO 100 I1 =  1,  9
           DO 110 I2 =  1,  NSTEP+4
             ATFRA (I1,I2) = 0.
  110      CONTINUE
  100    CONTINUE
         NVETT=NVETT+1
         DVE(7)=DVF(7)
         ATFRA(8,NVETT)=1500.
         CALL UCOPY(DVE,ATFRA(1,NVETT),6)
         DO 21 K=4,6
 21      ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVE(7)
C
C      track extrap through Ecal in NITER = 4 steps
C
         ENE=SQRT(DVF(7)*DVF(7)+WMU2)
         DO 25 NIT=1,NITER
            R2=R1ECA+(R2ECA-R1ECA)*FLOAT(NIT)/FLOAT(NITER)
            Z2=Z1ECA+(Z2ECA-Z1ECA)*FLOAT(NIT)/FLOAT(NITER)
            CALL AUHCYL(R2,Z2,FIELRC,Q,DVE,DVF,ICODE)
            IF(ICODE.EQ.0) GO TO 10
            DIST=VDIST(DVE,DVF,3)
            BE=DVF(7)/ENE
            GA=ENE/WMU
            DEDX=DIST*PB1*(ALOG(GA*BE)-BE*BE-PB2)*PB3/BE/BE
            ENE=ENE-DEDX
            IF(ENE .LT. 1.5*WMU) GO TO 51
            DVF(7)=SQRT(ENE*ENE-WMU2)
            IF(NIT.LT.NITER) CALL UCOPY(DVF,DVE,7)
 25      CONTINUE
         NVETT=NVETT+1
         ATFRA(8,NVETT)=0.
         CALL UCOPY(DVF,ATFRA(1,NVETT),6)
         DO 22 K=4,6
 22      ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVF(7)
C
C             track extrapol through coil
C             approx Zcoil=Zhcal !!, R2coil=rmax-field !!
C
         CALL AUHCYL(R1COI,HZEMIN,FIELRC,Q,DVF,DVE,ICODE)
         IF(ICODE.EQ.1) THEN
            NVETT=NVETT+1
            DVE(7)=DVF(7)
            ATFRA(8,NVETT)=2500.
            CALL UCOPY(DVE,ATFRA(1,NVETT),6)
            DO 23 K=4,6
 23         ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVE(7)
C
            CALL AUHCYL(R2COI,HZEMIN,FIELRC,Q,DVE,DVF,ICOD4)
            NVETT=NVETT+1
            ATFRA(8,NVETT)=0.
            DIST=VDIST(DVE,DVF,3)
C                   correct DIST for real coil thickness
            DIST=DIST*44./16.7
            BE=DVF(7)/ENE
            GA=ENE/WMU
            DEDX=DIST*AL1*(ALOG(GA*BE)-BE*BE-AL2)*AL3/BE/BE
            ENE=ENE-DEDX
            IF(ENE .LT. 1.5*WMU) GO TO 51
            DVF(7)=SQRT(ENE*ENE-WMU2)
            CALL UCOPY(DVF,ATFRA(1,NVETT),6)
            DO 24 K=4,6
 24         ATFRA(K,NVETT)=ATFRA(K,NVETT)*DVF(7)
         ELSE
            CALL UCOPY(DVE,ATIN,6)
            GO TO 40
         ENDIF
C
C              extrapolate track up to Hcal first plane
C
         IF(ICOD4.EQ.1)THEN
            PHI1=PIBY12
            CALL AULPOL(HXBMIN,HZEMIN,PHI1,DVF,ATIN,ICODE)
            IF(ICODE.EQ.0) GO TO 10
         ELSE
            CALL UCOPY(DVF,ATIN,6)
         ENDIF
C
C               follow track in Hcal (with mag field)
C
 40      CONTINUE
         ATIN(4)=ATIN(4)*DVF(7)
         ATIN(5)=ATIN(5)*DVF(7)
         ATIN(6)=ATIN(6)*DVF(7)
         ATIN(8)=Q
         CALL HTRACK(IBON,1,ATIN,IFAIL,NVETT,ATFRA)
C
C          create TREX bank (bank NR = ITRAC )
C
 51      CONTINUE
         LEN=2+LTREXA*NVETT
         CALL AUBOS('TREX',ITRAC,LEN,JTREX,IGARB)
         IF(IGARB.EQ.2) THEN
            CALL RERROR('FTRACK',1,'no space for TREX bank')
            GO TO 99
         ENDIF
         IW(JTREX+1)=LTREXA
         IW(JTREX+2)=NVETT
         KTREX=JTREX+2
C
C          fill TREX bank
C
         DO 50 N=1,NVETT
            CALL UCOPY(ATFRA(1,N),RW(KTREX+1),6)
            IW(KTREX+JTRERG)=NINT(ATFRA(8,N))
            IW(KTREX+JTRELA)=NINT(ATFRA(9,N))
            KTREX=KTREX+LTREXA
 50      CONTINUE
C
         SCOO(1)=0.
         CALL VZERO(R2MS,NSTEP+4)
         CALL VZERO (TH2PR,NSTEP+4)
C
C          compute steps and partial lengths of trajectory
C
         DO 60 N=1,NVETT-1
            DX=RTABL(JTREX,N+1,JTREX3)-RTABL(JTREX,N,JTREX3)
            DY=RTABL(JTREX,N+1,JTREX3+1)-RTABL(JTREX,N,JTREX3+1)
            DZ=RTABL(JTREX,N+1,JTREX3+2)-RTABL(JTREX,N,JTREX3+2)
            STEP(N)=SQRT(DX*DX+DY*DY+DZ*DZ)
            SCOO(N+1)=SCOO(N)+STEP(N)
            NMED=ITABL(JTREX,N,JTRERG)/1000
            IF(NMED.GT.0 .AND. NMED.LE.3) THEN
               PX=RTABL(JTREX,N,JTREP3)
               PY=RTABL(JTREX,N,JTREP3+1)
               PZ=RTABL(JTREX,N,JTREP3+2)
               PTOT2=PX*PX+PY*PY+PZ*PZ
               ETOT2=PTOT2+WMU2
               PIBE2=PTOT2*(PTOT2/ETOT2)
               CORR=1.
               IF(STEP(N).GT.0)
     &              CORR=(1.+LOG10(STEP(N)/RADL(NMED))/9.)**2
               TH2PR(N)=225.0E-6/PIBE2*STEP(N)/RADL(NMED)*CORR
            ENDIF
C
C                          compute average mult. scatt. radius
C
            DO 70 I=1,N
                DVOL=SCOO(N+1)-SCOO(I+1)
                R2MS(N+1)=R2MS(N+1) + TH2PR(I)*(STEP(I)*STEP(I)/3.+
     +               STEP(I)*DVOL+DVOL*DVOL)
 70         CONTINUE
C
            JROW=KROW(JTREX,N+1)
            RW(JROW+JTRERM)=SQRT(R2MS(N+1))
 60      CONTINUE
C
C              for first row put IFAIL in place of m.s. radius !!
C
         JROW=KROW(JTREX,1)
         RW(JROW+JTRERM)=IFAIL
C
C?             ...and now the next track
C
 10   CONTINUE
C
 99   RETURN
C
      END
#endif
