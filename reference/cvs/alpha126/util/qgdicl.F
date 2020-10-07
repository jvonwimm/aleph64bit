      SUBROUTINE QGDICL(NJETS,PTCUT,PTNEXT,NJMAX,NJMIN,MR,MD)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C   Author   : J. von Wimmersperg       28-JUL-2015
C
C   Description
C   ===========
C!   Evaluate jet multiplicities using the DICLUS algorithm
C
C  input:
C         PTCUT: Maximum invariant pt allowed when                   
C                clusterning three particles into two
C                if PTCUT=-N then force to N jets
C         MR=0 : particles are assigned to nearest jet but 
C                jets 4 vectors untouched
C         MR=1 : particles are assigned to nearest jet and 
C                jets are reclustered (gives better performance!)
C         MD=MODE: (2 seems to give best performance)
C                Recoil Strategy: 0 => Both final jets               
C                receives transverse recoil, 1=> Only                
C                closest jet receives recoil. 2 => Same as 1         
C                but use invariant mt instead of invariant pt        
C                as measure. -1 => same as 1 but join the            
C                selected cluster with nearest cluster with          
C                nearest cluster just adding four momenta.           
C                            -2 => as -1 but with mt measure. 
C output: NJETS= number of reconstructed jets
C              < 0 error
C         QTBOX,Y,Z,E(j)  four  momentum of jet j (j=1,NJET)(in QCTBUF)
C         KTBOF(i)        jet # of particle i     (i=1,KTBI)( "   "   )
C======================================================================
#ifndef DOC
C
#include "qctbuf.h"
      REAL*8 PP(5,KTBIMX),PJ(5,KTBIMX)
      REAL*8 PT,PTNXT
      INTEGER NJID(1000)
      REAL*8 cosang,cang
C ---------------------------------------------------------------
C
C
      icnt=0
      ireclus=0
      NJREQ=0
      if(NJMAX.gt.KTBOMX) NJMAX=KTBOMX
      if(NJMIN.lt.2) NJMIN=2
c handle forcing of jets case (NJREQ.ne.0)
      IF(PTCUT.LT.0) THEN
       NJREQ=-int(PTCUT)
       PTCUT=1.5
c make sure clustering limit is within sensible bounds
       IF(NJREQ.LT.NJMIN) NJREQ=NJMIN
       IF(NJREQ.GT.KTBI) NJREQ=KTBI
       IF(NJREQ.GT.NJMAX) NJREQ=NJMAX
      ENDIF
      NP = KTBI
      PT=dble(PTCUT)
C
C...fill 4-momentum P with the 4-momenta of all the particles in the event 
C...from the qctbuf

      DO I=1,NP
       KTBOF(I) = 0
       PP(1,I) = dble(QTBIX(I))
       PP(2,I) = dble(QTBIY(I))
       PP(3,I) = dble(QTBIZ(I))
       PP(4,I) = dble(QTBIE(I))
       PP(5,I)=DSQRT(MAX(0.D0,
     &           PP(4,I)**2-PP(3,I)**2-PP(2,I)**2-PP(1,I)**2))
      ENDDO

C analyze event up to a resolution of NJREQ jets
* determine if we want reclustering or not
* MY=0  => no assignment of particles to jets, never do this!
* MY=1  => particles are assigned to nearest jet but jets 4 vectors untouched
* MY=NP => particles are assigned to nearest jet and jets are reclustered
      MY=1
      if(MR.eq.0) then
       MY=1
      elseif(MR.eq.1) then
       MY=NP
      endif
 100  continue
      icnt=icnt+1
* remember NJREQ=0 means we are not forcing number of jets
      if(NJREQ.gt.0) then
       if(icnt.gt.100) then
        write(6,*)'icnt gt 100, diclus quit',np
        goto 999
       endif
       if(icnt.gt.25.and.NJETS-NJREQ.eq.1) then
        ireclus=1
        goto 777
       endif
      endif
*
      NJID(1)=MY
      CALL DICLUS(MD,5,NP,PP,0,0,PT,PTNXT,NJMAX,NJMIN,NJ,NJID,PJ,IER)
      if(IER.ne.0) then
       NJETS=-IER
       write(6,*)'qgdicl error 1',NJETS
       goto 999
      endif
      PTNEXT=real(PTNXT)
      NJETS=NJ
c      if(icnt.gt.15) then
c       write(6,*)'DICLUS problems icnt,pt,njets,np,mc=',
c     &                            icnt,pt,njets,np,mc
c      endif
      if(njets.gt.njreq.and.njreq.gt.0) then
c       if(icnt.gt.50) then
c        pt=pt*1.
c       else
        pt=ptnxt+0.000001d0
c       endif
       goto 100
      endif
      if(njets.lt.njreq) then
       pt=dble(PT*0.4)
       goto 100
      endif
      IF(NJETS.gt.KTBOMX) THEN
* too many jets
       NJETS=-3
       write(6,*)'qgdicl error 4',NJETS
       goto 999
      ENDIF
 777  continue

      if(ireclus.eq.1) then
c       write(6,*)'problems clustering to',njreq,'jets, mc=',mc
c       write(6,*)'we have following jets:'
       ikill=0
       do i=1,njets
c        write(6,*)'jet',i,PJ(1,I),PJ(2,I),PJ(3,I),PJ(4,I)
* if a jet is 0 energy (not sure why it happens) then kill it add it to ia later
        if(PJ(4,I).lt.1.e-10) then
         ikill=i
        else
         ia=i
        endif
       enddo
c       write(6,*)'particle assignments, number =',np
c       DO I=1,NP
c        write(6,*)i,njid(i)
c       ENDDO
*** problems: njets is njreq+1 -> force into njreq jets according to angle
       if(ikill.eq.0) then
        cosang=-2.D0
        do i=1,njreq
         do j=i+1,njets
          cang=(PJ(1,I)*PJ(1,J)+PJ(2,I)*PJ(2,J)+PJ(3,I)*PJ(3,J))/
     &         sqrt(PJ(1,I)**2+PJ(2,I)**2+PJ(3,I)**2)/
     &         sqrt(PJ(1,J)**2+PJ(2,J)**2+PJ(3,J)**2)
          if(cang.gt.cosang) then
           cosang=cang
           ia=i
           ib=j
          endif
         enddo
        enddo
c        write(6,*)'ang (deg) between jets',ia,'and',ib,'being joined',
c     &            acos(cosang)*180/3.1415927
       else
        ib=ikill
       endif
* add jets ia and ib, we will delete ib afterwards
       do i=1,4
        PJ(i,ia)=PJ(i,ia)+PJ(i,ib)
        PJ(i,ib)=0.
       enddo
       DO I=1,NP
        if(NJID(I).eq.ib) NJID(I)=ia
       ENDDO
       if(ib.ne.njets) then
* move jet njets to ib position
        do i=1,4
         PJ(i,ib)=PJ(i,njets)
         PJ(i,njets)=0.
        enddo
        DO I=1,NP
         if(NJID(I).eq.njets) NJID(I)=ib
        ENDDO
       endif
c       write(6,*)'reclustering done'
c       do i=1,njreq
c        write(6,*)'jet',i,PJ(1,I),PJ(2,I),PJ(3,I),PJ(4,I)
c       enddo
c       write(6,*)'particle assignments, number =',np
c       DO I=1,NP
c        write(6,*)i,njid(i)
c       ENDDO
      endif
*** 
      DO I=1,NP
       KTBOF(I)=NJID(I)
      ENDDO
      if(njreq.eq.0) then
       mjets=njets
      else
       mjets=njreq
      endif
      DO I=1,mjets
       QTBOX(I)=real(PJ(1,I))
       QTBOY(I)=real(PJ(2,I))
       QTBOZ(I)=real(PJ(3,I))
       QTBOE(I)=real(PJ(4,I))
      ENDDO

  999 RETURN
      END
#endif
C***********************************************************************
C $Id: diclus.f,v 3.8 2000/03/08 21:25:42 leif Exp $
      SUBROUTINE DICLUS(MODE,NDIM,NP,PP,IC1,IC2,
     $                  PTCUT,PTNEXT,NJMAX,NJMIN,NJ,IJ,PJ,IERR)
C
C Main interface to the Dipole Clustering algorithm (aka ARCLUS).
C
C Inputs:
C        INTEGER MODE       Recoil Strategy: 0 => Both final jets
C                           receives transverse recoil, 1=> Only
C                           closest jet receives recoil. 2 => Same as 1
C                           but use invariant mt instead of invariant pt
C                           as measure. -1 => same as 1 but join the
C                           selected cluster with nearest cluster with
C                           nearest cluster just adding four momenta.
C                           -2 => as -1 but with mt measure.
C        INTEGER NDIM       Dimension of PP and PJ vectors (PP(NDIM,*)
C        INTEGER NP         Number of entries in PP vector
C        DOUBLE PP(NDIM,*)  Vector of 3- or 4- momenta of particles
C                           to be clustered
C        INTEGER IC1,IC2    IC1/10 is the index of a pseudoparticle in PP
C                           which is not allowed to be clustered in
C                           DICLUS. If MOD(IC1,10) > 0 the direction of
C                           the pseudoparticle must not change. If
C                           MOD(IC1,10) > 1 the magnitude of the pseudo
C                           particle must not change. IC2 means the same
C                           for a second pseudo particle.
C        DOUBLE PTCUT       Maximum invariant pt allowed when
C                           clusterning three particles into two
C        INTEGER NJMAX      Maximum number of jets allowed
C        INTEGER NJMIN      ABS(NJMIN) is the minimum number of jets
C                           allowed. If less than 0, continue from
C                           previous clustering
C        INTEGER IJ(*)      If IJ(1) is larger than 0 then all particles
C                           in PP are assigned to a jet and the jet number
C                           is stored in the IJ vector. If IJ(1)=NP, then
C                           each jets is redefined as the sum of the
C                           momenta of the particles assigned to it.
C
C Outputs:
C        DOUBLE PTNEXT      Minimum invariant pt among the remaining
C                           jets
C        INTEGER NJ         Number of jets produced
C        INTEGER IJ(*)      The number of the nearest jet for each entry
C                           In the PP vector. Will only be given if
C                           IJ(1)>0 when called, otherwise IJ is left
C                           untouched.
C        DOUBLE PJ(NDIM,*)  Vector of 3- or 4- momenta of jets produced
C        INTEGER IERR       If <> 0 an error occurred during clustering
C
C
      IMPLICIT NONE
C Subroutine arguments
      INTEGER MODE,NDIM,NP,IC1,IC2,NJMAX,NJMIN,NJ,IERR
      DOUBLE PRECISION PP(NDIM,NP),PJ(NDIM,NJMAX),PTCUT,PTNEXT
C Other variables
      INTEGER NPMAX,I,J,I1,I2,I3,J1,J2,J3,NJC,MAXI,IJ(NP),I1C,I3C
* jvw
      integer mj
* jvw
      PARAMETER (NPMAX=1000)
      DOUBLE PRECISION P(6,NPMAX),MAXE,PT2M(NPMAX),PT2,PT2MIN,PT2MI2
      DOUBLE PRECISION D11,D12,D13,D14,D15,D16
      DOUBLE PRECISION D21,D22,D23,D24,D25,D26
      DOUBLE PRECISION D31,D32,D33,D34,D35,D36
      DOUBLE PRECISION D1D2,D2D3,D1D3,DR12,DR23
      DOUBLE PRECISION DIPT2I,DINVM2
      INTEGER ISLEFT(NPMAX),MIN1(NPMAX),MIN3(NPMAX),MIN1I2,MIN3I2
      LOGICAL TED(NPMAX),TED1,REDO,NEWJET
      SAVE ISLEFT,MIN1,MIN3,TED,P,PT2M,NJC
C End of declarations
C
C Check sanity of input
      IERR=-1
      IF (NDIM.LT.3) RETURN
      IF (NP.LE.0.OR.NP.GT.NPMAX) RETURN
      IF (NJMAX.LT.2) RETURN
      IF (ABS(NJMIN).GT.NJMAX) RETURN
      IERR=1
C
C Initialize:
      IF (NJMIN.GE.0) THEN      
        NJC=NP
        DO 100 I=1,NP
          DO 110 J=1,MIN(NDIM,5)
            P(J,I)=PP(J,I)
 110      CONTINUE
          IF (NDIM.LT.4) THEN
            P(4,I)=SQRT(P(3,I)**2+P(2,I)**2+P(1,I)**2)
            P(5,I)=0.0D0
          ELSEIF (NDIM.LT.5) THEN
            P(5,I)=SQRT(MAX(0.0D0,
     $           P(4,I)**2-P(3,I)**2-P(2,I)**2-P(1,I)**2))
          ENDIF
          P(6,I)=P(5,I)**2

          ISLEFT(I)=1
          IF (I.EQ.IC1/10) ISLEFT(I)=2+MOD(IC1,10)
          IF (I.EQ.IC2/10) ISLEFT(I)=2+MOD(IC2,10)
          TED(I)=.TRUE.
          MIN1(I)=0
          MIN3(I)=0
 100    CONTINUE
      ELSE
C
C Continue from previous run
        DO 120 I=1,NP
          ISLEFT(I)=-ISLEFT(I)
 120    CONTINUE
      ENDIF
C
C Start main loop
 200  J1=0
      J2=0
      J3=0
      PT2MIN=1.0D20
C
C Loop through all remaining particles
      DO 210 I2=1,NP
        IF (ISLEFT(I2).EQ.1) THEN
          PT2MI2=PT2M(I2)
          MIN1I2=MIN1(I2)
          MIN3I2=MIN3(I2)
          D21=P(1,I2)
          D22=P(2,I2)
          D23=P(3,I2)
          D24=P(4,I2)
          D25=P(5,I2)
          D26=P(6,I2)
          REDO=TED(I2).OR.MIN1(I2).EQ.0.OR.MIN3(I2).EQ.0
          IF (MIN1I2.NE.0) THEN
            IF (ISLEFT(MIN1I2).EQ.0) REDO=.TRUE.
            IF (TED(MIN1I2)) REDO=.TRUE.
          ENDIF
          IF (MIN3I2.NE.0) THEN
            IF (ISLEFT(MIN3I2).EQ.0) REDO=.TRUE.
            IF (TED(MIN3I2)) REDO=.TRUE.
          ENDIF
          IF (REDO) PT2MI2=1.0D20
C For each jet find pair of jets w.r.t. which pt is minimum
C Only check pairs where one or both jets have changed.
          DO 220 I1=1,NP-1
            IF (ISLEFT(I1).NE.0.AND.I1.NE.I2) THEN
              D11=P(1,I1)
              D12=P(2,I1)
              D13=P(3,I1)
              D14=P(4,I1)
              D15=P(5,I1)
              D16=P(6,I1)
              D1D2=MAX(D14*D24-D13*D23-D12*D22-D11*D21,0.0D0)
              IF (ABS(MODE).GE.2) THEN
                DR12=MAX(2.0D0*D1D2+D16+D26,0.0D0)
              ELSE
                DR12=4.0D0*MAX(D1D2-D15*D25,0.0D0)
              ENDIF
              TED1=TED(I1)
              DO 230 I3=I1+1,NP
                IF (ISLEFT(I3).NE.0.AND.I3.NE.I2.AND.
     $               (REDO.OR.TED(I3).OR.TED1)) THEN 
                  D31=P(1,I3)
                  D32=P(2,I3)
                  D33=P(3,I3)
                  D34=P(4,I3)
                  D35=P(5,I3)
                  D36=P(6,I3)
                  D2D3=MAX(D24*D34-D23*D33-D22*D32-D21*D31,0.0D0)
                  IF (ABS(MODE).GE.2) THEN
                    DR23=MAX(2.0D0*D2D3+D26+D36,0.0D0)
                  ELSE
                    DR23=MAX(D2D3-D25*D35,0.0D0)
                  ENDIF
                  D1D3=MAX(D14*D34-D13*D33-D12*D32-D11*D31,0.0D0)
                  PT2=DR12*DR23/(D16+D26+D36+2.0D0*(D1D2+D2D3+D1D3))
                  IF (ABS(MODE).GT.2) PT2=PT2-D26
                  IF (PT2.LT.PT2MI2) THEN
                    PT2MI2=PT2
                    MIN1I2=I1
                    MIN3I2=I3
                  ENDIF
                ENDIF
 230          CONTINUE
            ENDIF
 220      CONTINUE
C
          PT2M(I2)=PT2MI2
          MIN1(I2)=MIN1I2
          MIN3(I2)=MIN3I2
          IF (PT2MI2.LT.PT2MIN) THEN
            PT2MIN=PT2MI2
            J1=MIN1I2
            J2=I2
            J3=MIN3I2
          ENDIF
        ENDIF
 210  CONTINUE
C
C If no pt was found below ptcut then copy remaining jets to ouput
C vector and exit.
      IF (J1.EQ.0.OR.NJC.LE.MAX(ABS(NJMIN),2).OR.
     $     PT2MIN.GT.PTCUT**2) THEN
        PTNEXT=SQRT(PT2MIN)
        IF (NJC.GT.NJMAX) RETURN
        IERR=0
        DO 300 NJ=1,NJC
          MAXE=0.0D0
          MAXI=0
          DO 310 I=1,NP
            IF (ISLEFT(I).GT.0) THEN
              IF (P(4,I).GT.MAXE) THEN
                MAXE=P(4,I)
                MAXI=I
              ENDIF
            ENDIF
 310      CONTINUE
          DO 320 J=1,MIN(NDIM,5)
            PJ(J,NJ)=P(J,MAXI)
 320      CONTINUE
          ISLEFT(MAXI)=-ISLEFT(MAXI)
 300    CONTINUE
        NJ=NJC
        IF (IJ(1).LE.0) RETURN
        NEWJET=(IJ(1).EQ.NP)
        DO 400 I=1,NP
          I1C=0
          I3C=0
          PT2MI2=1.0E20
          DO 410 I1=1,NJ-1
            DO 420 I3=I1+1,NJ
              PT2=DIPT2I(PJ(1,I1),PP(1,I),PJ(1,I3),NDIM,MODE)
              IF (PT2.GT.PT2MI2) GOTO 420
              I1C=I1
              I3C=I3
              PT2MI2=PT2
 420        CONTINUE
 410      CONTINUE
          IJ(I)=0
          IF (I1C.LE.0) GOTO 400
          IF (DINVM2(PJ(1,I1C),PP(1,I),NDIM,MODE).LT.
     $        DINVM2(PJ(1,I3C),PP(1,I),NDIM,MODE)) THEN
            IJ(I)=I1C
          ELSE
            IJ(I)=I3C
          ENDIF
 400    CONTINUE
          
        IF (.NOT.NEWJET) RETURN
        DO 440 I=1,NJ
          DO 450 J=1,NDIM
            PJ(J,I)=0.0D0
 450      CONTINUE
 440    CONTINUE
        DO 460 I=1,NP
          DO 470 J=1,NDIM
            PJ(J,IJ(I))=PJ(J,IJ(I))+PP(J,I)
 470      CONTINUE
 460    CONTINUE
* jvw
        mj=0
        do i=1,nj
c         write(6,*)'jets',pj(4,i)
         if(pj(4,i).gt.1.e-10) mj=mj+1
        enddo
        if(mj.lt.nj) then
c         write(6,*)'zeroed jets detected nj mj',nj,mj
         call sortjet(nj,np,pj,ij,ndim,njmax)
         nj=mj
        endif
c        do i=1,np
c         write(6,*)'sort add particle',i,pp(4,i),'to jet',ij(i)
c        enddo
* jvw
        RETURN
      ENDIF
C
C Cluster the chosen jet into the two jets w.r.t. which its pt is
C minimum
      DO 500 I=1,NP
        TED(I)=.FALSE.
 500  CONTINUE
      CALL DIJOIN(MODE,P(1,J1),P(1,J2),P(1,J3),ISLEFT(J1),ISLEFT(J3))
      
      ISLEFT(J2)=0
      IF (ISLEFT(J1).LT.4) TED(J1)=.TRUE.
      IF (ISLEFT(J3).LT.4) TED(J3)=.TRUE.
      NJC=NJC-1
C
C Loop back
      GOTO 200
C
      END
C
C***********************************************************************
      DOUBLE PRECISION FUNCTION DIPT2I(P1,P2,P3,NDIM,MODE)
C
      IMPLICIT NONE
C
      INTEGER NDIM,MODE
      DOUBLE PRECISION P1(NDIM),P2(NDIM),P3(NDIM),
     $     D14,D15,D24,D25,D34,D35,DS12,DS23,DS123
C
      IF(NDIM.GT.3) THEN
        D14=P1(4)
        D24=P2(4)
        D34=P3(4)
      ELSE
        D14=SQRT(P1(3)**2+P1(2)**2+P1(1)**2)
        D24=SQRT(P2(3)**2+P2(2)**2+P2(1)**2)
        D34=SQRT(P3(3)**2+P3(2)**2+P3(1)**2)
      ENDIF
      DS12=MAX((D14+D24)**2-(P1(3)+P2(3))**2-
     $         (P1(2)+P2(2))**2-(P1(1)+P2(1))**2,0.0D0)
      DS23=MAX((D34+D24)**2-(P3(3)+P2(3))**2-
     $         (P3(2)+P2(2))**2-(P3(1)+P2(1))**2,0.0D0)
      DS123=MAX((D14+D24+D34)**2-(P1(3)+P2(3)+P3(3))**2-
     $          (P1(2)+P2(2)+P3(2))**2-(P1(1)+P2(1)+P3(1))**2,0.0D0)
      IF (ABS(MODE).GE.2.OR.NDIM.LT.4) THEN
        DIPT2I=DS12*DS23/DS123
        IF (ABS(MODE).GT.2)
     $       DIPT2I=DIPT2I-(P2(4)**2-P2(3)**2-P2(2)**2-P2(1)**2)
      ELSE
        IF(NDIM.GT.4) THEN
          D15=P1(5)
          D25=P2(5)
          D35=P3(5)
        ELSE
          D15=SQRT(MAX(0.0D0,P1(4)**2-P1(3)**2-P1(2)**2-P1(1)**2))
          D25=SQRT(MAX(0.0D0,P2(4)**2-P2(3)**2-P2(2)**2-P2(1)**2))
          D35=SQRT(MAX(0.0D0,P3(4)**2-P3(3)**2-P3(2)**2-P3(1)**2))
        ENDIF
        DIPT2I=MAX(DS12-(D15+D25)**2,0.0D0)*
     $         MAX(DS23-(D35+D25)**2,0.0D0)/DS123
      ENDIF
C
      RETURN
C
      END
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION DINVM2(P1,P2,NDIM,MODE)
C
      IMPLICIT NONE
C
      INTEGER NDIM,MODE
      DOUBLE PRECISION P1(NDIM),P2(NDIM),D14,D15,D24,D25
C
      IF(NDIM.GT.3) THEN
        D14=P1(4)
        D24=P2(4)
      ELSE
        D14=SQRT(P1(3)**2+P1(2)**2+P1(1)**2)
        D24=SQRT(P2(3)**2+P2(2)**2+P2(1)**2)
      ENDIF
      DINVM2=MAX((D14+D24)**2-(P1(3)+P2(3))**2-
     $           (P1(2)+P2(2))**2-(P1(1)+P2(1))**2,0.0D0)
      IF (ABS(MODE).LT.2.AND.NDIM.GT.3) THEN
        IF(NDIM.GT.4) THEN
          D15=P1(5)
          D25=P2(5)
        ELSE
          D15=SQRT(MAX(0.0D0,P1(4)**2-P1(3)**2-P1(2)**2-P1(1)**2))
          D25=SQRT(MAX(0.0D0,P2(4)**2-P2(3)**2-P2(2)**2-P2(1)**2))
        ENDIF
        DINVM2=DINVM2-(D15+D25)**2
      ENDIF
C
      RETURN
C
      END
C
C***********************************************************************
C***********************************************************************
C $Id: dijoin.f,v 3.4 1998/05/22 09:34:11 leif Exp $
      SUBROUTINE DIJOIN(MODE,P1,P2,P3,IL1,IL3)
C
C Cluster three jets into two
C
C Inputs:
C         INTEGER MODE             Recoil Strategy
C         DOUBLE P1(6),P2(6),P3(6) Initial momenta
C         INTEGER IL1,IL3          Pseudo particle information for
C                                  momenta 1 and 3
C Outputs:
C         DOUBLE P1(6),P3(6)       Momenta of jets 1 and 3 after
C                                  clustering
C
      IMPLICIT NONE
C Subroutine arguments
      INTEGER MODE,IL1,IL3
      DOUBLE PRECISION P1(6),P2(6),P3(6)
C Other variables
      DOUBLE PRECISION DE,DB(3),P1S(6),P3S(6),PHI,PHI2,THE,PSI,BET,
     $     S12,S23,DINVM2
      INTEGER J
C End of declarations
C
C Especially simple for MODE < 0

      IF (MODE.LT.0) THEN
        S12=DINVM2(P1(1),P2(1),6,MODE)
        S23=DINVM2(P3(1),P2(1),6,MODE)
        IF (S12.LT.S23) THEN
          DO 10 J=1,4
            P1(J)=P1(J)+P2(J)
 10       CONTINUE
          P1(5)=SQRT(S12)
          P1(6)=S12
        ELSE
          DO 20 J=1,4
            P3(J)=P3(J)+P2(J)
 20       CONTINUE
          P3(5)=SQRT(S23)
          P3(6)=S23
        ENDIF
        RETURN
      ENDIF

C Deal with special cases for pseudoparticles
      IF (IL1.EQ.4.AND.IL3.EQ.4) RETURN
      IF (IL1.EQ.4) THEN
        DO 100 J=1,6
          P1S(J)=P1(J)
 100    CONTINUE
      ENDIF
      IF (IL3.EQ.4) THEN
        DO 110 J=1,6
          P3S(J)=P3(J)
 110    CONTINUE
      ENDIF
      IF (IL1.GE.3.AND.IL3.GE.3) THEN
        DE=P1(4)+P3(4)
        DO 200 J=1,3
          DB(J)=-(P1(J)+P3(J))/DE
 200    CONTINUE
      ELSE
        DE=P1(4)+P2(4)+P3(4)
        DO 210 J=1,3
          DB(J)=-(P1(J)+P2(J)+P3(J))/DE
 210    CONTINUE
      ENDIF
C
C Boost particles to CMS
      IF (DB(1)**2+DB(2)**2+DB(3)**2.GE.1.0D0) THEN
        DO 220 J=1,4
          P1(J)=P1(J)+P2(J)*0.5D0
          P3(J)=P3(J)+P2(J)*0.5D0
 220    CONTINUE
        P1(5)=0.0D0
        P1(6)=0.0D0
        P3(5)=0.0D0
        P3(6)=0.0D0
        RETURN
      ENDIF
      CALL DIBOOS(DB,P1)
      CALL DIBOOS(DB,P2)
      CALL DIBOOS(DB,P3)
C
C Rotate according to recoil strategy
      DE=(P1(4)+P2(4)+P3(4))*0.5D0
      PHI=0.0D0
      IF (P1(2).NE.0.0D0.OR.P1(1).NE.0.0D0) PHI=ATAN2(P1(2),P1(1))
      CALL DIRPHI(-PHI,P1)
      CALL DIRPHI(-PHI,P3)
      THE=0.0D0
      IF (P1(3).NE.0.0D0.OR.P1(1).NE.0.0D0) THE=ATAN2(P1(1),P1(3))
      CALL DIRTHE(-THE,P1)
      CALL DIRTHE(-THE,P3)
C
      IF (IL1.LT.3.OR.IL3.LT.3) THEN
        PHI2=0.0D0
        IF (P3(2).NE.0.0D0.OR.P3(1).NE.0.0D0) PHI2=ATAN2(P3(2),P3(1))
        CALL DIRPHI(-PHI2,P1)
        CALL DIRPHI(-PHI2,P3)
        IF (IL1.GT.1.OR.(ABS(MODE).GE.1.AND.P1(4).GE.P3(4))) THEN
          PSI=0.0D0
        ELSE
          BET=-ATAN2(P3(1),-P3(3))
          IF (IL3.GT.1.OR.(ABS(MODE).GE.1.AND.P1(4).LT.P3(4))) THEN
            PSI=BET
          ELSE
            PSI=BET*(P3(4)**2)/(P1(4)**2+P3(4)**2)
          ENDIF
        ENDIF
      ELSE
        PHI2=0.0D0
        PSI=0.0D0
      ENDIF
C
C Set new momenta of remaining jets
      P1(1)=0.0D0
      P1(2)=0.0D0
      P1(3)=DE
      P1(4)=DE
      P1(5)=0.0D0
      P1(6)=0.0D0
      P3(1)=0.0D0
      P3(2)=0.0D0
      P3(3)=-DE
      P3(4)=DE
      P3(5)=0.0D0
      P3(6)=0.0D0
C
C Boost back
      DO 300 J=1,3
        DB(J)=-DB(J)
 300  CONTINUE
      CALL DIRTHE(PSI,P1)
      CALL DIRTHE(PSI,P3)
      CALL DIRPHI(PHI2,P1)
      CALL DIRPHI(PHI2,P3)
      CALL DIRTHE(THE,P1)
      CALL DIRTHE(THE,P3)
      CALL DIRPHI(PHI,P1)
      CALL DIRPHI(PHI,P3)
      CALL DIBOOS(DB,P1)
      CALL DIBOOS(DB,P3)
C
C Reset jets to original values for special pseudoparticles
      IF (IL1.EQ.4) THEN
        DO 400 J=1,6
          P1(J)=P1S(J)
 400    CONTINUE
      ENDIF
      IF (IL3.EQ.4) THEN
        DO 410 J=1,6
          P3(J)=P3S(J)
 410    CONTINUE
      ENDIF
C
      RETURN
C
      END
C
C***********************************************************************
C***********************************************************************
C $Id: dirphi.f,v 3.1 1996/03/08 09:56:05 leif Exp $
      SUBROUTINE DIRPHI(PHI,P)
C
C Rotate around z-axis
C
C Inputs:
C         DOUBLE PHI          Rotation angle
C         DOUBLE P(3)         Vector to rotate
C
C Outputs:
C         DOUBLE P(3)         Rotated vector
C
      IMPLICIT NONE
C Subroutine arguments
      DOUBLE PRECISION PHI,P(3)
C Other variables
      DOUBLE PRECISION CPHI,SPHI,PX,PY
C End of declarations
C
      IF (ABS(PHI).LE.1.0D-20) RETURN
C
      SPHI=SIN(PHI)
      CPHI=COS(PHI)
      PX=P(1)
      PY=P(2)
      P(1)=CPHI*PX-SPHI*PY
      P(2)=SPHI*PX+CPHI*PY
C
      RETURN
C
      END
C
C***********************************************************************
C***********************************************************************
C $Id: dirthe.f,v 3.1 1996/03/08 09:56:18 leif Exp $
      SUBROUTINE DIRTHE(THE,P)
C
C Rotate around y-axis
C
C Inputs:
C         DOUBLE THE          Rotation angle
C         DOUBLE P(3)         Vector to rotate
C
C Outputs:
C         DOUBLE P(3)         Rotated vector
C
      IMPLICIT NONE
C Subroutine arguments
      DOUBLE PRECISION THE,P(3)
C Other variables
      DOUBLE PRECISION CTHE,STHE,PX,PZ
C End of declarations
C
      IF (ABS(THE).LE.1.0D-20) RETURN
C
      STHE=SIN(THE)
      CTHE=COS(THE)
      PX=P(1)
      PZ=P(3)
      P(3)=CTHE*PZ-STHE*PX
      P(1)=STHE*PZ+CTHE*PX
C
      RETURN
C
      END
C
C***********************************************************************
C***********************************************************************
C $Id: diboos.f,v 3.1 1996/03/08 09:55:46 leif Exp $
      SUBROUTINE DIBOOS(DB,P)
C
C Boost
C
C Inputs:
C         DOUBLE DB(3)             Boost vector
C         DOUBLE P(4)              4-Vector to boost
C
C Outputs:
C         DOUBLE P(4)              Boosted 4-vector
C
      IMPLICIT NONE
C Subroutine arguments
      DOUBLE PRECISION DB(3),P(4)
C Other variables
      DOUBLE PRECISION DBT2,DGA,DBP,DGABP
C End of declarations
C
      DBT2=DB(1)**2+DB(2)**2+DB(3)**2
      IF (DBT2.LE.1.0D-20) RETURN
      DGA=1.0D0/SQRT(1.0D0-DBT2)
      DBP=P(1)*DB(1)+P(2)*DB(2)+P(3)*DB(3)
      DGABP=DGA*(DGA*DBP/(1.0D0+DGA)+P(4))
      P(1)=P(1)+DGABP*DB(1)
      P(2)=P(2)+DGABP*DB(2)
      P(3)=P(3)+DGABP*DB(3)
      P(4)=DGA*(P(4)+DBP)
C
      RETURN
C
      END
C
C***********************************************************************
      subroutine sortjet(nj,np,pj,ij,ndim,njmax)
* j.von wimmersperg 2020
      double precision PJOLD(ndim,njmax),PJ(ndim,njmax)      
      integer ijold(np),ij(np),ied(nj),ied2(nj)
      real ene(nj)
*
      do i=1,np
        ijold(i)=ij(i)
      enddo
      do j=1,nj
       do k=1,ndim
        PJOLD(k,j)=PJ(k,j)
       enddo
       ene(j)=real(PJ(4,j))
      enddo
      call sortzv(ene,ied,nj,1,1,0)
      do j=1,nj
c       write(6,*)'j ene(j) ied(j) ene(ied(j))',j,ene(j),ied(j),
c     &                                               ene(ied(j))
       do k=1,ndim
        PJ(k,j)=PJOLD(k,ied(j))
        ied2(ied(j))=j
       enddo
      enddo
      do i=1,np
c       write(6,*)'sortjet particle',i,'was jet',ijold(i),'now',
c     &                                       ied2(ijold(i))
       ij(i)=ied2(ijold(i))
      enddo
      end
