      SUBROUTINE QGPTCL (YVALUT,THRUST,NJTLIM,YJTLIM,EVIS)
CKEY JETS /INTERNAL
C----------------------------------------------------------------------
C!  - PTCLUS Jet ALgorithm (see ALEPH  89-150)
C!
C!   Author   :- J.M.SCARR
C------CLUSTER (JET) ALGORITHM FOR HIGH ENERGY FINAL STATES.
C------SCARR JM. 89.II.13 DOX#89.3.                     LST CHG 89.II.28
C!
C!   Inputs:
C!
C!      NJTLIM  =  LIMIT MINIMUM NUMBER OF CLUSTERS.
C!                 (ALWAYS TEST  KTBO)
C!      YJTLIM  =  UPPER LIMIT OF  SEMS  OF 2 CLUSTERS MERGED.
C!
C!
C!   Outputs:
C!        -     KTBO  =  NUMBER OF CLUSTERS FOUND.(in QCTBUF.INC)
C!        -   THRUST  =  GENERALISED THRUST.
C!      YVALUT(1) =   SEMS  OF LAST MERGED CLUSTERS.
C!      YVALUT(2) =   LOWEST  SEMS  OF TWO CLUSTERS.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C!      YVALUT(3)=   FRACTIONAL ENERGY REASSIGNED IN LAST MERGE.
C!                 SEMS = SCALED EFFECTIV MASS SQUARED, M*M/S.
C!======================================================================
#ifndef DOC
      SAVE LBEGIN
#include "qcdesh.h"
#include "qctbuf.h"
C     KTBOMX is a parameter which is maximum number jets
      INTEGER TOPEN, PARTIC
      REAL EVIS
C
*      NOTE.      SEMS (SCALED EFFECTIVE MASS SQUARED), Y = MASS**2/S.
*      ====     IF LSCALE FALSE, YJTLIM,YVALUT ARE CORRESPONDING MASSES.
*                 (CURRENTLY, PSEUDO-MASSES USED)
*
*               IF LPRINT TRUE, DIAGNOSTIC PRINT IS OBTAINED.
*------
      LOGICAL   LBEGIN,LPRINT,LSCALE, LREASS
      PARAMETER (PPSQLM=0.15,LPRINT=.FALSE.,LSCALE=.TRUE. )
      DATA       LBEGIN /.TRUE./
C      SAVE       HWSQLM
C     limit on iterations
      PARAMETER (NITLIM = 5)
C
C     these arrays hold the jets internal to the program
      INTEGER MXTRKS
      PARAMETER ( MXTRKS = 200)
      REAL PJETX(MXTRKS), PJETY(MXTRKS)
      REAL PJETZ(MXTRKS), PJETE(MXTRKS)
C            X-MOMENTUM     Y-MOMENTUM     Z-MOMENTUM     ENERGY
      INTEGER KJETNU (MXTRKS)
C     KJETNU(I) is the number of tracks in jet I
C     KTBIMX is a parameter which is maximum number of input tracks
      REAL     PM(MXTRKS), PP(MXTRKS), YVALUT(3)
      INTEGER  IORDER(MXTRKS)
C
C======================================================
C
      IF(KTBI.GT.KTBIMX) THEN
        KTBO = -1
C       QGPTCLUS -INPUT TRACKS EXCEEDS MAX NUMBER ALLOWED
        GOTO 999
      ENDIF
C
      IF (KTBI .LT. 2) THEN
        KTBO = -1
C       QJPTCLS - ZERO OR ONE TRACKS INPUT
        GOTO 999
      ENDIF
C
C     check the integrity of the input data
      DO 10 ICOUNT = 1, KTBI
        IF( (QTBIX(ICOUNT) .EQ. 0.0) .AND.
     &       (QTBIY(ICOUNT) .EQ. 0.0) .AND.
     &       (QTBIZ(ICOUNT) .EQ. 0.0)   ) THEN
          CALL QWMESE('_QGPTCL_ Track with zero momentum found ')
          KTBO = -1
          GOTO 999
        ENDIF
   10 CONTINUE
C
      IF(LBEGIN) THEN
        IF(LSCALE) THEN
          WRITE (KUPRNT,4) PPSQLM,YJTLIM,NJTLIM
    4     FORMAT(//,10X,'USING PTCLUS: PPSQLM,YJTLIM,NJTLIM =',
     &       2F8.3,I5,//)
        ELSE
          WRITE (KUPRNT,5) PPSQLM,YJTLIM,NJTLIM
    5     FORMAT(//,10X,'USING PTCLUS: PPSQLM,WJTLIM,NJTLIM =',
     &       2F8.3,I5,//)
          HWSQLM = YJTLIM*YJTLIM*0.5
        ENDIF
        LBEGIN = .FALSE.
      ENDIF
C
      LNJ=0
C------STEP 1.  FORM INITIAL CLUSTERS ABOUT MOST ENERGETIC PARTICLES.---
C     sort in energy order
      IORDER(1)=1
      DO 12  I=2,KTBI
        IORDER(I)=I
        J=I
        DO  13 JJ=1,I-1
          K1=J-1
          IF ( QTBIE(IORDER(J)).LE.QTBIE(IORDER(K1))) GO TO 12
            ISAVE=IORDER(K1)
            IORDER(K1)=IORDER(J)
         IORDER(J)=ISAVE
   13    J=J-1
   12 CONTINUE
C----------------------------
C
C     start cluster about TOPEN, the most energetic particle
      TOPEN=IORDER(1)
      PJETX(1) = QTBIX(TOPEN)
      PJETY(1) = QTBIY(TOPEN)
      PJETZ(1) = QTBIZ(TOPEN)
      PJETE(1) = QTBIE(TOPEN)
      KJETNU(1)=1
      KTBOF(TOPEN)=1
      PP(1)= QTBIX(TOPEN)*QTBIX(TOPEN)
     &       + QTBIY(TOPEN)*QTBIY(TOPEN)
     &       + QTBIZ(TOPEN)*QTBIZ(TOPEN)
      PM(1)=SQRT(PP(1))
      NJ=1
      ENYTOT = QTBIE(TOPEN)
C
C     now loop over all particles
C
      DO  20 I=2, KTBI
        PARTIC = IORDER(I)
        ENYTOT = QTBIE(PARTIC)+ENYTOT
        PP(I)=QTBIX(PARTIC)*QTBIX(PARTIC)
     &         + QTBIY(PARTIC)*QTBIY(PARTIC)
     &         + QTBIZ(PARTIC)*QTBIZ(PARTIC)
        PM(I)=SQRT(PP(I))
        PERPMN = 1.E30
        DO  21 JET = 1, NJ
          PDOTP =  QTBIX(PARTIC)* PJETX(JET)
     &           + QTBIY(PARTIC)* PJETY(JET)
     &           + QTBIZ(PARTIC)* PJETZ(JET)
          IF(PDOTP .GT. 0.0) THEN
            PERPSQ=(PP(JET)*PP(I)-PDOTP*PDOTP)/
     &             ( (PM(JET)+PM(I))*(PM(JET)+PM(I)) )
            IF(PERPSQ.LT.PERPMN) THEN
              PERPMN=PERPSQ
              JMIN=JET
            ENDIF
          ENDIF
   21   CONTINUE
C
C
        IF (PERPMN .LT. PPSQLM) THEN
C         if perpendicular momentum is less than the limit
C         combine track into existing cluster
          PJETX(JMIN) = PJETX(JMIN) + QTBIX(PARTIC)
          PJETY(JMIN) = PJETY(JMIN) + QTBIY(PARTIC)
          PJETZ(JMIN) = PJETZ(JMIN) + QTBIZ(PARTIC)
          PJETE(JMIN) = PJETE(JMIN) + QTBIE(PARTIC)
          KJETNU(JMIN) = KJETNU(JMIN) + 1
          KTBOF(PARTIC)=JMIN
          PP(JMIN) = PJETX(JMIN)*PJETX(JMIN)
     &               + PJETY(JMIN)*PJETY(JMIN)
     &               + PJETZ(JMIN)*PJETZ(JMIN)
          PM(JMIN) = SQRT(PP(JMIN))
        ELSE
C         form a new cluster
          NJ = NJ+1
C
          PJETX(NJ) =QTBIX(PARTIC)
          PJETY(NJ) =QTBIY(PARTIC)
          PJETZ(NJ) =QTBIZ(PARTIC)
          PJETE(NJ) =QTBIE(PARTIC)
          KJETNU (NJ)=1
          KTBOF(PARTIC)=NJ
          PP(NJ)=PP(I)
          PM(NJ)=PM(I)
        ENDIF
   20 CONTINUE
C
C     end loop over particles
C
C     check here on total energy is OK
C     and also on a negative EVIS
      IF ( (ENYTOT .LE. 0.0) .OR. (EVIS .LT. 0.0) ) THEN
        CALL QWMESE('_QGPTCL_ Warning: Negative total energy ')
      ENDIF
C
      IF (LSCALE) THEN
        IF ( EVIS .EQ. 0.0) THEN
          HWSQLM = YJTLIM*ENYTOT*ENYTOT*0.5
        ELSE
          HWSQLM = YJTLIM*EVIS*EVIS*0.5
        ENDIF
      ELSE
        HWSQLM = YJTLIM*YJTLIM*0.5
      ENDIF
C
C------------------------------------------------------------------
C------STEP 2.  MERGE INITIAL CLUSTERS DOWN TO  NJTLIM. ----------------
  100 CONTINUE
      PERPMN = 1.E30
      DO 30  I = 2,NJ
        DO 31 J = 1,I-1
          PDOTP=   PJETX(I) *PJETX(J)
     &           + PJETY(I) *PJETY(J)
     &           + PJETZ(I) *PJETZ(J)
          IF(PDOTP.GT.0.0) THEN
C------     USING PSEUDO-MASS = E1*E2*(1.-COSINE)  SEEMS BEST.
            PERPSQ=(-PDOTP/(PM(I)*PM(J))+1.) * PJETE(I) * PJETE(J)
            IF(PERPSQ.LT.PERPMN) THEN
              PERPMN=PERPSQ
              JM = J
              IM = I
            ENDIF
          ENDIF
   31   CONTINUE
   30 CONTINUE
C
C     it this the finish of the algorithm ?
      IF( (PERPMN .GT. HWSQLM) .OR. (NJ .LE. NJTLIM)) GO TO 200
      PERPML=PERPMN
C     we havent finished yet. Merge the two clusters separated by
C     the least perpendicular momentum
C     merge cluster IM into cluster JM
      PJETX(JM) = PJETX(JM) + PJETX(IM)
      PJETY(JM) = PJETY(JM) + PJETY(IM)
      PJETZ(JM) = PJETZ(JM) + PJETZ(IM)
      PJETE(JM) = PJETE(JM) + PJETE(IM)
      KJETNU(JM) = KJETNU(JM) + KJETNU(IM)
      PM(JM)=SQRT( PJETX(JM)*PJETX(JM)
     &         + PJETY(JM)*PJETY(JM)
     &         + PJETZ(JM)*PJETZ(JM)       )
C
C------THIS LOOP PREVENTS UNNECESSARY ITERATIONS.
      DO 40  I = 1, KTBI
        IF(KTBOF(I) .EQ. IM) KTBOF(I) = JM
        IF(KTBOF(I) .GT. IM) KTBOF(I) = KTBOF(I)-1
   40 CONTINUE
C
C     reduce the number of jets by one
      NJ=NJ-1
C
      DO 50  ICOUNT = IM,NJ
        PJETX(ICOUNT) = PJETX(ICOUNT +1)
        PJETY(ICOUNT) = PJETY(ICOUNT +1)
        PJETZ(ICOUNT) = PJETZ(ICOUNT +1)
        PJETE(ICOUNT) = PJETE(ICOUNT +1)
        KJETNU(ICOUNT) = KJETNU(ICOUNT +1)
        PM(ICOUNT) = PM(ICOUNT +1)
   50 CONTINUE
C------STEP 3.  REASSIGN PARTICLES TO CLUSTERS.-------------------------
      EMOVED = 0.0
      NITERS = 0
  125 CONTINUE
      LREASS = .FALSE.
C
C     loop through the particles I
      DO 60  I=1, KTBI
        PLNGMX = -1.E30
C       loop through the jets
        DO 61 JET = 1,NJ
          PLONG=(QTBIX(I)* PJETX(JET)+QTBIY(I) *PJETY(JET)
     &       + QTBIZ(I) *PJETZ(JET)) /PM(JET)
          IF(PLONG.GT.PLNGMX) THEN
            PLNGMX=PLONG
            JM=JET
          ENDIF
   61   CONTINUE
        IF (KTBOF(I) .NE. JM) THEN
          KTBOF(I)=JM
          LREASS = .TRUE.
          EMOVED = QTBIE(I) +EMOVED
        ENDIF
   60 CONTINUE
C
C------REFORM CLUSTERS.
C      if a reassignment has happened then...
      IF (LREASS) THEN
        NITERS = NITERS + 1
        JET = 0
        DO 300  J1 = 1,NJ
          JET=JET+1
          PJETX(JET) = 0.0
          PJETY(JET) = 0.0
          PJETZ(JET) = 0.0
          PJETE(JET) = 0.0
          KJETNU(JET)= 0
C         loop over particles I
          DO 70  I = 1, KTBI
            IF(KTBOF(I) .EQ. JET) THEN
              PJETX(JET) = PJETX(JET) + QTBIX(I)
              PJETY(JET) = PJETY(JET) + QTBIY(I)
              PJETZ(JET) = PJETZ(JET) + QTBIZ(I)
              PJETE(JET) = PJETE(JET) + QTBIE(I)
              KJETNU(JET) = KJETNU(JET) + 1
            ENDIF
   70     CONTINUE
C
          IF( KJETNU(JET) .EQ. 0) THEN
C------      REASSIGNMENT HAS ELIMINATED A CLUSTER.
            DO 80  I=1, KTBI
              IF(KTBOF(I) .GT. JET) KTBOF(I)=KTBOF(I)-1
   80       CONTINUE
            JET=JET-1
          ELSE
            PM(JET)=SQRT(PJETX(JET)*PJETX(JET)
     &             + PJETY(JET)*PJETY(JET)
     &             + PJETZ(JET)*PJETZ(JET)    )
          ENDIF
  300   CONTINUE
        NJ=JET
      ENDIF
C
C------IF A CLUSTER HAS SPLIT, ITERATE TRACK ALLOCATION TO CLUSTERS.
      IF ( LREASS .AND. (NITERS .LT. NITLIM) ) GO TO 125
      IF (NJ .GE. 2) GO TO 100
C------TERMINATION.
  200 CONTINUE
C
C     finish of program. Prepare the output data
C
C     now put number of found jets into KTBO
      KTBO = NJ
C
C     jet arrays have space for at most fourty jets
C     have we exceeded this ?
      IF (NJ .GT. KTBOMX) THEN
        KTBO = -1
C       QGPTCLUS - too many jets found
        GOTO 999
      ENDIF
C     have now found NJ jets
C     put their momenta and energies into the output jet vectors
C     in arrays QTBOX etc.
      DO 105 IJET=1,NJ
        QTBOX(IJET) = PJETX(IJET)
        QTBOY(IJET) = PJETY(IJET)
        QTBOZ(IJET) = PJETZ(IJET)
        QTBOE(IJET) = PJETE(IJET)
  105 CONTINUE
C
      IF(LSCALE) THEN
        YVALUT(1)=(PERPML*2.)/(ENYTOT*ENYTOT)
        YVALUT(2)=(PERPMN*2.)/(ENYTOT*ENYTOT)
      ELSE
        YVALUT(1)=SQRT(PERPML*2.)
        YVALUT(2)=SQRT(PERPMN*2.)
      ENDIF
      YVALUT(3) = EMOVED/ENYTOT
      PLONGT=0.0
      DO 110  J=1,NJ
        PLONGT=PM(J)+PLONGT
  110 CONTINUE
      THRUST=PLONGT/ENYTOT
C
  999 CONTINUE
      END
#endif
