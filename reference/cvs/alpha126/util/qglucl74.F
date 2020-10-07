      SUBROUTINE QGLUCL74(MINJET,DMAX1,DMAX2,MULSYM
     &                 ,NJET,TGEN,DMIN)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C   Author   : J. von Wimmersperg       26-AUG-2019
C
C   Description
C   ===========
C!   Evaluate jet multiplicities based on the LUCLUS algorithm (JETSET 7.4)
C
C input :
C         MINJET min. # of jets to be reconstructed (usually=1)
C         DMAX1 max. distance to form starting clusters(usually=0.25GeV)
C         DMAX2  "      "     to join 2 clusters       (usually=2.5 GeV)
C         MULSYM = 1 symmetric distance criterion + reassignment (usual)
C                = 2 symmetric distance criterion no reassignment
C                = 3 Mass distance meas., fixed max cluster mass (usual)
C                = 4 Mass distance meas., scaled (Evis) max cluster mass
C                = 5 Durham distance measure
C         *** note only MULSYM = 1 has reassignment procedure
C-----------------------------------------------------------------------------
C         also NOTE convenient way to force to NJETS by setting DMAX2=-NJETS 
C-----------------------------------------------------------------------------
C         KTBI            # of particles           (from QCTBUF)
C         QTBIX,Y,Z,E(i)  four momentum (i=1,KTBI) ( "     "   )
C output: NJET number of reconstructed jets
C              = -1 if not enough particles
C              = -2 not enough working space (MXWORK)
C              = -3 failed to reconstruct as requested
C         TGEN generalized thrust = -1 if failed
C         DMIN minimum distance between 2 jets = -1 if failed
C         QTBOX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)(in QCTBUF)
C         KTBOF(i)        jet # of particle i     (i=1,KTBI)( "   "   )
C======================================================================
#ifndef DOC
      SAVE CONVRG
C
#include "qctbuf.h"
      DIMENSION P(KTBIMX+2*KTBOMX,5)
      DIMENSION K(KTBIMX+2*KTBOMX,5)
      DIMENSION V(KTBIMX+2*KTBOMX,5)
C
      REAL PSS,PS(5)
      DATA CONVRG /0.0001/
C ---------------------------------------------------------------
C
C...Functions: distance measure in pT, (pseudo)mass or Durham pT. 
      R2T(I1,I2)=(P(I1,5)*P(I2,5)-P(I1,1)*P(I2,1)-P(I1,2)*P(I2,2)- 
     &P(I1,3)*P(I2,3))*2.*P(I1,5)*P(I2,5)/(0.0001+P(I1,5)+P(I2,5))**2 
      R2M(I1,I2)=2.*P(I1,4)*P(I2,4)*(1.-(P(I1,1)*P(I2,1)+P(I1,2)* 
     &P(I2,2)+P(I1,3)*P(I2,3))/(P(I1,5)*P(I2,5))) 
      R2D(I1,I2)=2.*MIN(P(I1,4),P(I2,4))**2*(1.-(P(I1,1)*P(I2,1)+
     &P(I1,2)*P(I2,2)+P(I1,3)*P(I2,3))/(P(I1,5)*P(I2,5))) 
*
*
      MSTU47 = MINJET
      MSTU46 = MULSYM
      PARU43 = DMAX1
      PARU44 = DMAX2
      PARU45 = DMAX2
      PARU48 = CONVRG
* handle jet forcing option                                        
      if(DMAX2.lt.0) then
       MSTU47=-int(DMAX2)
       PARU44=1000.
      endif
*
C
      N = KTBI
C
C...momenta and sum of momenta for particles

      NP=0
      DO J=1,5
       PS(J)=0.
      ENDDO
      PSS=0.
      DO 100 I=1,N
      KTBOF(I) = 0
      P(I,1) = QTBIX(I)
      P(I,2) = QTBIY(I)
      P(I,3) = QTBIZ(I)
      P(I,4) = QTBIE(I)
      P(I,5) = SQRT(MAX(P(I,4)**2-P(I,1)**2-P(I,2)**2-P(I,3)**2,0.))
  100 CONTINUE

C...Find which particles are to be considered in cluster search.
      DO 140 I=1,N
      IF(N+2*NP.GE.KTBIMX+KTBOMX-5) THEN
        WRITE (6,*)'(QGLUCL74:) Error, insufficient working space'
        NJET = -2
        GOTO 999
      ENDIF

C...Take copy of these particles, with space left for jets later on. 
      NP=NP+1 
      K(N+NP,3)=I 
      DO 120 J=1,5 
      P(N+NP,J)=P(I,J) 
  120 CONTINUE 
*JVW      IF(MSTU(42).EQ.0) P(N+NP,5)=0. 
*JVW      IF(MSTU(42).EQ.1.AND.K(I,2).NE.22) P(N+NP,5)=PMAS(101,1) 
      P(N+NP,4)=SQRT(P(N+NP,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2) 
      P(N+NP,5)=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2) 
      DO 130 J=1,4 
      PS(J)=PS(J)+P(N+NP,J) 
  130 CONTINUE 
      PSS=PSS+P(N+NP,5) 
  140 CONTINUE 
      DO 160 I=N+1,N+NP 
      K(I+NP,3)=K(I,3) 
      DO 150 J=1,5 
      P(I+NP,J)=P(I,J) 
  150 CONTINUE 
  160 CONTINUE 
      PS(5)=SQRT(MAX(0.,PS(4)**2-PS(1)**2-PS(2)**2-PS(3)**2)) 
 
C...Very low multiplicities not considered. 
      IF(NP.LT.MSTU47) THEN 
        WRITE (6,*)'(QGLUCL74:) too few particles for analysis'
        NJET=-1
        GOTO 999 
      ENDIF 
 
C...Find precluster configuration. If too few jets, make harder cuts. 
      NLOOP=0 
      IF(MSTU46.LE.3.OR.MSTU46.EQ.5) THEN 
        R2ACC=PARU44**2 
      ELSE 
        R2ACC=PARU45*PS(5)**2 
      ENDIF 
      RINIT=1.25*PARU43 
      IF(NP.LE.MSTU47+2) RINIT=0. 
  170 RINIT=0.8*RINIT 
      NPRE=0 
      NREM=NP 
      DO 180 I=N+NP+1,N+2*NP 
      K(I,4)=0 
  180 CONTINUE

C...Sum up small momentum region. Jet if enough absolute momentum. 
      IF(MSTU46.LE.2) THEN 
        DO 190 J=1,4 
        P(N+1,J)=0. 
  190   CONTINUE 
        DO 210 I=N+NP+1,N+2*NP 
        IF(P(I,5).GT.2.*RINIT) GOTO 210 
        NREM=NREM-1 
        K(I,4)=1 
        DO 200 J=1,4 
        P(N+1,J)=P(N+1,J)+P(I,J) 
  200   CONTINUE 
  210   CONTINUE 
        P(N+1,5)=SQRT(P(N+1,1)**2+P(N+1,2)**2+P(N+1,3)**2) 
        IF(P(N+1,5).GT.2.*RINIT) NPRE=1 
        IF(RINIT.GE.0.2*PARU43.AND.NPRE+NREM.LT.MSTU47) GOTO 170 
        IF(NREM.EQ.0) GOTO 170 
      ENDIF

C...Find fastest remaining particle. 
  220 NPRE=NPRE+1 
      PMAX=0. 
      DO 230 I=N+NP+1,N+2*NP 
      IF(K(I,4).NE.0.OR.P(I,5).LE.PMAX) GOTO 230 
      IMAX=I 
      PMAX=P(I,5) 
  230 CONTINUE 
      DO 240 J=1,5 
      P(N+NPRE,J)=P(IMAX,J) 
  240 CONTINUE 
      NREM=NREM-1 
      K(IMAX,4)=NPRE 

C...Sum up precluster around it according to pT separation. 
      IF(MSTU46.LE.2) THEN 
        DO 260 I=N+NP+1,N+2*NP 
        IF(K(I,4).NE.0) GOTO 260 
        R2=R2T(I,IMAX) 
        IF(R2.GT.RINIT**2) GOTO 260 
        NREM=NREM-1 
        K(I,4)=NPRE 
        DO 250 J=1,4 
        P(N+NPRE,J)=P(N+NPRE,J)+P(I,J) 
  250   CONTINUE 
  260   CONTINUE 
        P(N+NPRE,5)=SQRT(P(N+NPRE,1)**2+P(N+NPRE,2)**2+P(N+NPRE,3)**2) 

C...Sum up precluster around it according to mass or 
C...Durham pT separation. 
      ELSE 
  270   IMIN=0 
        R2MIN=RINIT**2 
        DO 280 I=N+NP+1,N+2*NP 
        IF(K(I,4).NE.0) GOTO 280
        IF(MSTU46.LE.4) THEN 
          R2=R2M(I,N+NPRE) 
        ELSE
          R2=R2D(I,N+NPRE) 
        ENDIF
        IF(R2.GE.R2MIN) GOTO 280 
        IMIN=I 
        R2MIN=R2 
  280   CONTINUE 
        IF(IMIN.NE.0) THEN 
          DO 290 J=1,4 
          P(N+NPRE,J)=P(N+NPRE,J)+P(IMIN,J) 
  290     CONTINUE 
          P(N+NPRE,5)=SQRT(P(N+NPRE,1)**2+P(N+NPRE,2)**2+P(N+NPRE,3)**2) 
          NREM=NREM-1 
          K(IMIN,4)=NPRE 
          GOTO 270 
        ENDIF 
      ENDIF
 
C...Check if more preclusters to be found. Start over if too few. 
      IF(RINIT.GE.0.2*PARU43.AND.NPRE+NREM.LT.MSTU47) GOTO 170 
      IF(NREM.GT.0) GOTO 220 
      NJET=NPRE 
 
C...Reassign all particles to nearest jet. Sum up new jet momenta. 
  300 TSAV=0. 
      PSJT=0. 
  310 IF(MSTU46.LE.1) THEN 
        DO 330 I=N+1,N+NJET 
        DO 320 J=1,4 
        V(I,J)=0. 
  320   CONTINUE 
  330 CONTINUE 
        DO 360 I=N+NP+1,N+2*NP 
        R2MIN=PSS**2 
        DO 340 IJET=N+1,N+NJET 
        IF(P(IJET,5).LT.RINIT) GOTO 340 
        R2=R2T(I,IJET) 
        IF(R2.GE.R2MIN) GOTO 340 
        IMIN=IJET 
        R2MIN=R2 
  340   CONTINUE 
        K(I,4)=IMIN-N 
        DO 350 J=1,4 
        V(IMIN,J)=V(IMIN,J)+P(I,J) 
  350   CONTINUE 
  360   CONTINUE 
        PSJT=0. 
        DO 380 I=N+1,N+NJET 
        DO 370 J=1,4 
        P(I,J)=V(I,J) 
  370   CONTINUE 
        P(I,5)=SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2) 
        PSJT=PSJT+P(I,5) 
  380   CONTINUE 
      ENDIF 

C...Find two closest jets. 
      R2MIN=2.*MAX(R2ACC,PS(5)**2) 
      DO 400 ITRY1=N+1,N+NJET-1 
      DO 390 ITRY2=ITRY1+1,N+NJET 
      IF(MSTU46.LE.2) THEN 
        R2=R2T(ITRY1,ITRY2) 
      ELSEIF(MSTU46.LE.4) THEN
        R2=R2M(ITRY1,ITRY2)
      ELSE
        R2=R2D(ITRY1,ITRY2)
      ENDIF 
      IF(R2.GE.R2MIN) GOTO 390 
      IMIN1=ITRY1 
      IMIN2=ITRY2 
      R2MIN=R2 
  390 CONTINUE 
  400 CONTINUE 
 
C...If allowed, join two closest jets and start over. 
      IF(NJET.GT.MSTU47.AND.R2MIN.LT.R2ACC) THEN 
        IREC=MIN(IMIN1,IMIN2) 
        IDEL=MAX(IMIN1,IMIN2) 
        DO 410 J=1,4 
        P(IREC,J)=P(IMIN1,J)+P(IMIN2,J) 
  410   CONTINUE 
        P(IREC,5)=SQRT(P(IREC,1)**2+P(IREC,2)**2+P(IREC,3)**2) 
        DO 430 I=IDEL+1,N+NJET 
        DO 420 J=1,5 
        P(I-1,J)=P(I,J) 
  420   CONTINUE 
  430 CONTINUE 
        IF(MSTU46.GE.2) THEN 
          DO 440 I=N+NP+1,N+2*NP 
          IORI=N+K(I,4) 
          IF(IORI.EQ.IDEL) K(I,4)=IREC-N 
          IF(IORI.GT.IDEL) K(I,4)=K(I,4)-1 
  440     CONTINUE 
        ENDIF 
        NJET=NJET-1 
        GOTO 300

C...Divide up broad jet if empty cluster in list of final ones. 
      ELSEIF(NJET.EQ.MSTU47.AND.MSTU46.LE.1.AND.NLOOP.LE.2) THEN 
        DO 450 I=N+1,N+NJET 
        K(I,5)=0 
  450   CONTINUE 
        DO 460 I=N+NP+1,N+2*NP 
        K(N+K(I,4),5)=K(N+K(I,4),5)+1 
  460   CONTINUE 
        IEMP=0 
        DO 470 I=N+1,N+NJET 
        IF(K(I,5).EQ.0) IEMP=I 
  470   CONTINUE 
        IF(IEMP.NE.0) THEN 
          NLOOP=NLOOP+1 
          ISPL=0 
          R2MAX=0. 
          DO 480 I=N+NP+1,N+2*NP 
          IF(K(N+K(I,4),5).LE.1.OR.P(I,5).LT.RINIT) GOTO 480 
          IJET=N+K(I,4) 
          R2=R2T(I,IJET) 
          IF(R2.LE.R2MAX) GOTO 480 
          ISPL=I 
          R2MAX=R2 
  480     CONTINUE 
          IF(ISPL.NE.0) THEN 
            IJET=N+K(ISPL,4) 
            DO 490 J=1,4 
            P(IEMP,J)=P(ISPL,J) 
            P(IJET,J)=P(IJET,J)-P(ISPL,J) 
  490       CONTINUE 
            P(IEMP,5)=P(ISPL,5) 
            P(IJET,5)=SQRT(P(IJET,1)**2+P(IJET,2)**2+P(IJET,3)**2) 
            IF(NLOOP.LE.2) GOTO 300 
          ENDIF 
        ENDIF 
      ENDIF 
 
C...If generalized thrust has not yet converged, continue iteration. 
      IF(MSTU46.LE.1.AND.NLOOP.LE.2.AND.PSJT/PSS.GT.TSAV+PARU48) 
     &THEN 
        TSAV=PSJT/PSS 
        GOTO 310 
      ENDIF 
 
C...Reorder jets according to energy. 
      DO 510 I=N+1,N+NJET 
      DO 500 J=1,5 
      V(I,J)=P(I,J) 
  500 CONTINUE 
  510 CONTINUE 
      DO 540 INEW=N+1,N+NJET 
      PEMAX=0. 
      DO 520 ITRY=N+1,N+NJET 
      IF(V(ITRY,4).LE.PEMAX) GOTO 520 
      IMAX=ITRY 
      PEMAX=V(ITRY,4) 
  520 CONTINUE 
      K(INEW,1)=31 
      K(INEW,2)=97 
      K(INEW,3)=INEW-N 
      K(INEW,4)=0 
      DO 530 J=1,5 
      P(INEW,J)=V(IMAX,J) 
  530 CONTINUE 
      V(IMAX,4)=-1. 
      K(IMAX,5)=INEW 
  540 CONTINUE 
 
C...Clean up particle-jet assignments and jet information. 
      DO 550 I=N+NP+1,N+2*NP 
      IORI=K(N+K(I,4),5) 
      K(I,4)=IORI-N 
      IF(K(K(I,3),1).NE.3) K(K(I,3),4)=IORI-N 
      K(IORI,4)=K(IORI,4)+1 
  550 CONTINUE 
      IEMP=0 
      PSJT=0. 
      DO 570 I=N+1,N+NJET 
      K(I,5)=0 
      PSJT=PSJT+P(I,5) 
      P(I,5)=SQRT(MAX(P(I,4)**2-P(I,5)**2,0.)) 
      DO 560 J=1,5 
      V(I,J)=0. 
  560 CONTINUE 
      IF(K(I,4).EQ.0) IEMP=I
* JVW fill jet buffer
      QTBOX(I-N) = P(I,1)
      QTBOY(I-N) = P(I,2)
      QTBOZ(I-N) = P(I,3)
      QTBOE(I-N) = P(I,4) 
*
  570 CONTINUE 

      KTBOF(I) = K(I,4)
 
C...Select storing option. Output variables. Check for failure. 
      MSTU61=N+1 
      MSTU62=NP 
      MSTU63=NPRE 
      PARU61=PS(5) 
      TGEN=PSJT/PSS 
      DMIN=SQRT(R2MIN) 
      IF(NJET.LE.1) DMIN=0. 
      IF(IEMP.NE.0) THEN 
        WRITE (6,*)'(QGLUCL74:) failed to reconstruct as requested'
        NJET=-3
        TGEN=-1.
        DMIN=-1.
      ENDIF 
* JVW fill jet # of each particle
      DO I=1,N
       KTBOF(I) = K(I,4)
      ENDDO
*
  999 RETURN
      END
#endif
