      SUBROUTINE ECLTRK(IER)
C
C***********************************************************************
C!ASSOCIATE CLUSTERS TO TRACKS                                         *
C!                                                                     *
C!  AUTHOR   : J.P. ALBANESE 860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : EFET                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C! PERFORM ASSOCIATION BETWEEN TPC_TRACKS AND EC_CLUSTERS;             *
C!                      ACTIONS>>>                                     *
C!  GET TRACKS                                                         *
C!  LOOP ON TRACKS                                                     *
C!    INITIALIZE TRACK FOR EXTRAPOLATION                               *
C!    LOOP ON EXTRAPOLATION STEPS                                      *
C!       EXTRAPOLATE TRACK TO NEXT POINT (CALL EHLIX)                  *
C!       FIND STOREY IN WICH POINTS LAY (CALL ECDIGI)                  *
C!       FIND CLUSTERS NEARBY WITH AN ALGORITHM SIMILAR TO CLUSTERING  *
C!                    (CALL NEIGHB)                                    *
C!    ENDLOOP                                                          *
C    UPDATE TRACK-CLUSTER RELATIONSHIP (EINSRL)                        *
C!  ENDDOLOOP                                                          *
C1                                                                     *
C!                                                                     *
C!       IER = return code ( 0 if normal)                              *
C!                                                                     *
C***********************************************************************
#ifndef DOC
C
      REAL  XXX(8),XXXM(8)
#include "etp1jj.h"
#include "bcs.h"
#include "ectcom.h"
#include "rparac.h"
#include "ecnamc.h"
#include "bmacro.h"
      IER=0
C
C?   INITIALIZE TRACK-CLUSTER RELATION
C?
      CALL EINIRL(IER)
      IF(IER.NE.0)GO TO 998
C
C
C?   GET TPC TRACKS (LAST POINT)
C?
      CALL EGTTKS(NTRAK,IER)
      IF(IER.NE.0)GO TO 998
C
C?  LOOP ON TRACKS
C?
      DO 60 IDTPC=1,NTRAK
       IEXTR=1
C?
C?  SAY THAT WE HAVE NEVER BEEN IN THE OVERLAP REGION FOR THIS TRACK
C?
      IDEAD=0
C?
C?  INITIALISE TRACK FOR EXTRAPOLATION
C?
      CALL EINITK(IDTPC,IFLG2,XXX)
C?
C?  KEEEP COORDINATES OF TPC EXIT POINT
C?
      DO 61 IDATA=1,8
   61 XXXM(IDATA)=XXX(IDATA)
      IF (IFLG2.EQ.0)GO TO 60
C
C   INITIALISE STEP SIZE
C
         STEP=STPKEC
         CALL EINHLX(STEP,1,XXX)
C
         IFLAG=0
         ISTLD=0
C
         DRUN=0.
   30    CALL EHLIX(XXX)
         DRUN=DRUN+STEP
C
C? Stop a track which spirals for more than 2 m
C
         IF(DRUN.GT.200.)GO TO 60
C
C? For low momentum tracks, be more severe : 50 cm
C
         IF(XXX(7).LT.0.5.AND.DRUN.GT.50.)GO TO 60
         IFLPR=IFLAG
C?
C?      ECDIGI, WILL FIND THE STOREY IN WHICH WE ARE
C?
         CALL ECDIGI(XXX,IFLAG,ISTAD,IROSE)
C
C?  IF POINT IN ACTIVE REGION THEN
C
      IF(IFLAG.EQ.2)THEN
C?
C?     IF SAME STOREY, EXIT (ALREADY DONE
C?
         IF(ISTAD.NE.ISTLD) THEN
C
            ISTLD=ISTAD
C
C?          FIND NEARBY CLUSTERS
C?
            CALL ENIGHB(ISTAD,IROSE,IDTPC)
         ENDIF
C
C?  ELSEIF CURRENT POINT IN BARREL-ENDCAP DEAD REGION
C?          FOR THE FIRST TIME THEN
C?     IF PREVIOUS POINT WAS BEFORE ECAL,THEN
C?       SAY THAT WE HAVE BEEN IN THE OVERLAP REGION
C?       LOOP 51
C?         DIVIDE STEP BY 2,
C?         REINITIALISE HELIX
C?       LOOP 50
C?         MAKE ONE STEP
C?         FIND WHERE NEW POINT IS
C?       IF BEFORE ECAL, GO TO LOOP 51
C?       ELSEIF IN DEAD REGION (OVERLAP) THEN
C?         IF STEP LARGE ENOUGH, THEN
C?           GO TO LOOP 50
C?         ELSE
C?           RESET STEP
C?           REINITIALISE HELIX TO PRESENT POINT
C?         ENDIF
C?       ELSEIF IN ACTIVE REGION
C?         FIND NEIGHBOURS
C?         RESET STEP
C?         REINITIALISE HELIX TO PRESENT POINT
C?       ENDIF
C?     ENDIF
C?  ENDIF
C
      ELSEIF(IFLAG.EQ.1.AND.IDEAD.EQ.0)THEN
         IF(IFLPR.EQ.0)THEN
            IDEAD=1
   50       STEP=STEP*0.5
            CALL EINHLX(STEP,1,XXXM)
   51       CALL EHLIX(XXX)
            CALL ECDIGI(XXX,IFLAG,ISTAD,IROSE)
            IF(IFLAG.EQ.0)THEN
               GO TO 51
            ELSEIF(IFLAG.EQ.1)THEN
               IF(STEP.GT.STMNEC)GO TO 50
               STEP=STPKEC
               CALL EINHLX(STEP,1,XXX)
            ELSE
               CALL ENIGHB(ISTAD,IROSE,IDTPC)
               STEP=STPKEC
               CALL EINHLX(STEP,1,XXX)
            ENDIF
         ENDIF
      ENDIF
C
C?- FIND NEXT POINT EXCEPT IF WE ARE OUTSIDE OF ECAL
C?
       IF(IFLAG.EQ.4) GO TO 70
C
C?  STOP IF PARTICLE RETURNS INSIDE THE CENTRAL DEAD VOLUME
C
       IF(IFLAG.EQ.0.AND.IFLPR.EQ.2) THEN
                IEXTR=0
                GO TO 70
       ENDIF
      GO TO 30
   70 CONTINUE
C
C?  ELSE (IE OUTSIDE ECAL), GO TO NEXT TRACK
C?
C?
C?  FILL LAST POINT IN BANK EFET OF EC EXIT
      KEFET=IW(NAEFET)
      KTRAK=KROW(KEFET,IDTPC)
      DO 40 IDATA=1,6
      RW(KTRAK+IDATA)=XXX(IDATA)
   40 CONTINUE
       IW(KTRAK+JETPEF)=IEXTR
C? NEXT TRACK
   60 CONTINUE
C?
C?     TERMINATE THE RELATION
C?
      CALL ECLORL
  999 CONTINUE
      RETURN
  998 CONTINUE
      IER=1
      RETURN
      END
#endif
