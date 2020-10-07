      SUBROUTINE ITCCUT
C-----------------------------------------------------------------------
C! Make cuts on ITC points to be included on tracks.
C!
C!   Author     :- J. Carr   11/02/91
C!
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "icmrjj.h"
#include "alcons.h"
C-----------------------------------------------------------------------
#include "iasigc.h"
#include "itcinf.h"
C-----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
#include "bmacro.h"
C----------------------------------------------------------------------
C
C
      DO 100 IT=1,NTKITC
C       Cut on latched+dead hits.
        IF(NHTITC(IT)+NDHITC(IT).LE.NHMNIA) THEN
C          wipe out hits
           DO 101 IL=1,8
              IF(FLGITC(IL,IT).LE.0) FLGITC(IL,IT)=6
  101      CONTINUE
           NGHITC(IT) = 0
C       Require 2 hits in 1st road .
        ELSE IF(NGHITC(IT).LT.2) THEN
C          wipe out hits
           DO 102 IL=1,8
             IF(FLGITC(IL,IT).LE.0) FLGITC(IL,IT)=7
  102      CONTINUE
           NGHITC(IT) = 0
C     impose a road around rmean
        ELSE
           ILOST=0
           DO 103 IL=1,8
              IF(FLGITC(IL,IT).LE.0) THEN
                 RES = (RSFITC(IL,IT)-RMNITC(IT))/DERITC(IL,IT)
                 IF( ABS(RES).GT.SMAXIA) THEN
                    ILOST=ILOST+1
                    FLGITC(IL,IT)=9
                    NGHITC(IT) = NGHITC(IT) - 1
                 ENDIF
              ENDIF
  103      CONTINUE
           IF(ILOST.GT.0) THEN
              IF(NGHITC(IT).GE.2) THEN
C                Calculate ITC alone Xi**2 again
                 RMEAN=0.
                 DO 104 IL=1,8
                    IF(FLGITC(IL,IT).LE.0) RMEAN=RMEAN+RSFITC(IL,IT)
  104            CONTINUE
                 RMEAN = RMEAN / NGHITC(IT)
                 RMNITC(IT)=RMEAN
                 RMAX=0.
                 XISITC(IT)=0.
                 DO 105 IL=1,8
                    IF(FLGITC(IL,IT).LE.0) THEN
                       RSC= (RSFITC(IL,IT)-RMEAN)/DERITC(IL,IT)
                       XISITC(IT)=XISITC(IT)+RSC**2
                       IF(ABS(RSC).GT.ABS(RMAX)) THEN
                          RMAX=RSC
                          LRXITC(IT)=IL
                       ENDIF
                    ENDIF
  105            CONTINUE
                 XISITC(IT)=XISITC(IT) / (NGHITC(IT)-1)
              ELSE
C                wipe out hits
                 DO 106 IL=1,8
                    IF(FLGITC(IL,IT).LE.0) FLGITC(IL,IT)=7
  106            CONTINUE
                 NGHITC(IT) = 0
              ENDIF
           ENDIF
        ENDIF
  100 CONTINUE
C
C Create ICMR bank for coord.-to-track relation for resolving
C         assignments of one point to two tracks
C
      KITCO = NLINK('ITCO',0)
      IF (KITCO.LE.0) GOTO 998
      NCO = LROWS(KITCO)
      IF(NCO.LE.0) GOTO 998
      LICMR  = NCO*LICMRA + LMHLEN
      CALL AUBOS('ICMR',0,LICMR,KICMR,IGARB)
      IF (IGARB.EQ.2) GOTO 998
      IW(KICMR+LMHROW) = NCO
      IW(KICMR+LMHCOL) = LICMRA
      CALL BLIST(IW,'T+','ICMR')
C
      DO 200 IT=1,NTKITC
        IF(NGHITC(IT).EQ.0) GO TO 200
        DO 201 IL=1,8
          IF(FLGITC(IL,IT).GT.0) GOTO 201
          ICO = ICOITC(IL,IT)
          KK = KROW(KICMR,ICO)
          IF(IW(KK+JICMCN).EQ.0) THEN
             IW(KK+JICMIL) = IL
             IW(KK+JICMCN) = 1
             IW(KK+JICMT1) = IT
          ELSEIF(IW(KK+JICMCN).EQ.1) THEN
             IW(KK+JICMCN) = 2
             IW(KK+JICMT2) = IT
          ELSE
C            if this hit already used twice junk it.
             IW(KK+JICMCN) = IW(KK+JICMCN)+1
             FLGITC(IL,IT)=10
             NGHITC(IT)=NGHITC(IT)-1
             NRHITC(IT)=NRHITC(IT)+1
          ENDIF
  201   CONTINUE
  200 CONTINUE
C
C      Search for double use of hit.
C
      DO 300 ICO=1,NCO
          KK = KROW(KICMR,ICO)
          IF(IW(KK+JICMCN).GT.2) THEN
C            wipe out hits if used by more than 2 tracks
             IL=IW(KK+JICMIL)
             IT1=IW(KK+JICMT1)
             FLGITC(IL,IT1)=10
             NGHITC(IT1)=NGHITC(IT1)-1
             NRHITC(IT1)=NRHITC(IT1)+1
             IT2=IW(KK+JICMT2)
             FLGITC(IL,IT2)=10
             NGHITC(IT2)=NGHITC(IT2)-1
             NRHITC(IT2)=NRHITC(IT2)+1
          ELSEIF(IW(KK+JICMCN).EQ.2) THEN
C            wipe out both hits if tracks too close
             IL=IW(KK+JICMIL)
             IT1=IW(KK+JICMT1)
             IT2=IW(KK+JICMT2)
             DTRKS=RSFITC(IL,IT1)-RSFITC(IL,IT2)
             DLIMI=FJUNCU*(TERITC(IT1)+TERITC(IT2))
             IF(ABS(DTRKS).LT.DLIMI) THEN
                FLGITC(IL,IT1)=11
                NGHITC(IT1)=NGHITC(IT1)-1
                NRHITC(IT1)=NRHITC(IT1)+1
                FLGITC(IL,IT2)=11
                NGHITC(IT2)=NGHITC(IT2)-1
                NRHITC(IT2)=NRHITC(IT2)+1
             ELSE
C            wipe out one furthest from track
                IF(ABS(RSFITC(IL,IT1)).LT.ABS(RSFITC(IL,IT2)))THEN
                   FLGITC(IL,IT2)=12
                   NGHITC(IT2)=NGHITC(IT2)-1
                   NRHITC(IT2)=NRHITC(IT2)+1
                ELSE
                   FLGITC(IL,IT1)=12
                   NGHITC(IT1)=NGHITC(IT1)-1
                   NRHITC(IT1)=NRHITC(IT1)+1
                ENDIF
             ENDIF
          ENDIF
 300  CONTINUE
      IDUM=NDROP('ICMR',0)
C
      DO 400 IT=1,NTKITC
C        Filter bad points with big contribution to xis
C        drop one point with biggest contribution to xi2
C        if go below NCMNIA(1) hits drop them all
C        otherwise continue no matter what xi2 is.
         IF(XISITC(IT).GE.XISFIL) THEN
  401       CONTINUE
C             wipe out hit with biggest xis if contribution is large
C             use same value of XISFIL for individual chs, for no good reason
              ILAY=LRXITC(IT)
              XISC=( (RSFITC(ILAY,IT)-RMNITC(IT))/DERITC(ILAY,IT) )**2
              ILOST=0
              IF(XISC.GT.XISFIL) THEN
                 ILOST=1
C                make sure this hit was not robbed by another track
                 IF(FLGITC(ILAY,IT).LE.0) THEN
                    FLGITC(ILAY,IT)=13
                    NGHITC(IT) = NGHITC(IT) - 1
                 ENDIF
                 IF(NGHITC(IT).GE.2) THEN
C                   Calculate ITC alone Xi**2 again
                    RMEAN=0.
                    DO 403 IL=1,8
                       IF(FLGITC(IL,IT).LE.0) RMEAN=RMEAN+RSFITC(IL,IT)
  403               CONTINUE
                    RMEAN = RMEAN / NGHITC(IT)
                    RMNITC(IT)=RMEAN
                    RMAX=0.
                    XISITC(IT)=0.
                    DO 404 IL=1,8
                       IF(FLGITC(IL,IT).LE.0) THEN
                          RSC= (RSFITC(IL,IT)-RMEAN)/DERITC(IL,IT)
                          XISITC(IT)=XISITC(IT)+RSC**2
                          IF(ABS(RSC).GT.ABS(RMAX)) THEN
                             RMAX=RSC
                             LRXITC(IT)=IL
                          ENDIF
                       ENDIF
  404               CONTINUE
                    XISITC(IT)=XISITC(IT) / (NGHITC(IT)-1)
                 ELSE
                    ILOST=-1
C                    wipe out hits
                    DO 405 IL=1,8
                       IF(FLGITC(IL,IT).LE.0) FLGITC(IL,IT)=7
  405               CONTINUE
                    NGHITC(IT) = 0
                 ENDIF
              ENDIF
C             loop back if hit was cut and hits >2
            IF(ILOST.EQ.1.AND.NGHITC(IT).GT.2
     ,               .AND.XISITC(IT).GE.XISFIL) GO TO 401
          ENDIF
C         Cut on xis if only 2 hits
          IF(NGHITC(IT).EQ.2.AND.XISITC(IT).GE.XISFIL) THEN
C            wipe out hits
             DO 406 IL=1,8
                IF(FLGITC(IL,IT).LE.0) FLGITC(IL,IT)=14
  406        CONTINUE
             NGHITC(IT) = 0
         ENDIF
C
C        FIND LAYER SPAN
         IL1=0
         DO 450 IL=1,8
            IF(FLGITC(IL,IT).LE.0) THEN
               IF(IL1.EQ.0) IL1=IL
               IL2=IL
            ENDIF
  450    CONTINUE
  400 CONTINUE
C
      RETURN
C Errors
  998 CONTINUE
      CALL RERROR('ITCCUT',1,' No room for ICMR Bank!!!!')
      RETURN
      END
#endif
