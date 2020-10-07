      SUBROUTINE YTOPNW
C
C----------------------------------------------------------*
C!    Steering routine for TOPOLOGY reconstruction
CKEY YTOP STEERING
C!    Author :     M. Bosman , G. Lutz   30/11/88
C!    Modified:    G.Lutz                  /02/91
C!    Modified  :  G. Lutz   30/03/92
C!
C!    Input: Comdeck YPARTO
C!
C!    Description
C!    ===========
C!    This routine steers the topology reconstruction
C!    according to the options selected with the YOPT cards
C!    By default, it reconstruct the primary vertex without
C!    beam crossing constraint.
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "ybcrto.h"
#include "ydimto.h"
#include "ytrkto.h"
#include "yparto.h"
#include "ydumto.h"
C!---------------------------------------------------------*
#if defined(YDEBUG)
      LOGICAL LDMP1,LDMP2
#endif
      DIMENSION DBCR(3)
C
      DATA NENTY/0/
C
      DATA ICNER1/0/,ICNER2/0/,ICNER3/0/,ICNER4/0/,ICNER5/0/,

     &     ICNER6/0/,ICNER7/0/,ICNER8/0/,ICNER9/0/
C
C-- Define the logical unit for printout
C
      LOUT = IW(6)
C
C
C --- GET RUN AND EVENT NUMBER
      CALL ABRUEV(IRUN,IEVT)
C
#if defined(YDEBUG)
      IF(IDUMPP(1).GE.1) THEN
        LDMP1=.TRUE.
      ELSE
        LDMP1=.FALSE.
      ENDIF
C
      IF(IDUMPP(2).GE.1) THEN
        LDMP2=.TRUE.
      ELSE
        LDMP2=.FALSE.
      ENDIF
      IDUMPP(1)=IDUMPP(1)-1
      IDUMPP(2)=IDUMPP(2)-1
C
      IF(LDMP1.OR.LDMP2) THEN
        NENTY=NENTY+1
        WRITE(LOUT,909) NENTY,IRUN,IEVT
  909   FORMAT(' NEW YTOPOL CALLED. ',I5,
     &       ' TIMES','  IRUN=', I5, '  IEVT=',I5)
      ENDIF
      IF(LDMP2) THEN
        WRITE(LOUT,911) NENTY,LRYOLD,LRFRF2,
     &    LRLPVX,LRSVTX,LCONVS,LVZERS,LRUSER,LRPVTX,
     &    LRMVPV,LVBCR0,LRMVPV,BFIELD,PMINRQ,PMINRA,
     &    PMINSE,PIDACP
  911   FORMAT(' NEW YTOPOL CALLED.',I5, ' TIMES'/
     &    5X,' LRYOLD,LRFRF2=',2L2/
     &    5X,' LRLPVX,LRSVTX,LCONVS,LVZERS,LRUSER,LRPVTX,LRMVPV=',7L2/
     &    5X,' LVBCR0,LRMVPV=',2L2, ' BFIELD,PMINRQ,PMINRA=',3E10.3/,
     &    5X,' PMINSE,PIDACP=',2E10.3)
      ENDIF
#endif
C-- drop banks from old event
      IND= NDROP('PYER',0)
      IND= NDROP('PYFR',0)
      IND= NDROP('YNFT',0)
      IND= NDROP('YNMA',0)
      IND= NDROP('YNPE',0)
      IND= NDROP('YNTR',0)
      IND= NDROP('YCFT',0)
      IND= NDROP('YCMA',0)
      IND= NDROP('YCPE',0)
      IND= NDROP('YCTR',0)
C--
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
#if defined(YDEBUG)
      IF(LDMP2) THEN
        WRITE(LOUT,*)' FRFT0 FRFT FRTL',KFRFT0,KFRFT,KFRTL
        WRITE(LOUT,*)' FRFT FRTL BANK NUMBER',IW(KFRFT-2),IW(KFRTL-2)
      ENDIF
#endif
      IF(KFRFT.EQ.0) THEN
        ICNER1=ICNER1+1
#if defined(YDEBUG)
        IF(ICNER1.LT.100)
     &    WRITE(LOUT,*) 'YTOPOL : NO FRFT BANK PRESENT RETURN'
        IF(ICNER1.EQ.100)  WRITE(LOUT,*)
     &    'YTOPOL : BANK FRFT MISSING 100 TIMES STOP PRINTING'
#endif
        RETURN
      ENDIF
      IF(KFRTL.EQ.0) THEN
        ICNER2=ICNER2+1
#if defined(YDEBUG)
        IF(ICNER2.LT.100)
     &    WRITE(LOUT,*) 'YTOPOL : bank FRTL missing RETURN'
        IF(ICNER2.EQ.100)  WRITE(LOUT,*)
     &    'YTOPOL : BANK FRTL MISSING 100 TIMES STOP PRINTING'
#endif
        RETURN
      ENDIF
C FIND TIME LIMIT
      TLIMIT=0.
      INT=IW(NAMIND('TIME'))
      IF(INT.NE.0) TLIMIT=FLOAT(IW(INT+1))

C-- RESET PARTICLE ORIGIN AND IDENTIFICATION FLAG
      DO 1 I=1,MAXTRK
        KPORF0(I)=0
        KPIDF0(I)=0
    1 CONTINUE
C-- intialize the set of tracks selected for vertex search
C
      CALL YTSTRK(IER)
      IF(IER.NE.0) THEN
        ICNER3=ICNER3+1
#if defined(YDEBUG)
        IF(ICNER3.LT.100) WRITE(LOUT,*)
     &    'YTOPOL:fatal error occured in selection of tracks'
        IF(ICNER3.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 errors in track selection STOP PRINTING'
#endif
        RETURN
      ENDIF
C
C-- get particle identity assignement possibilities
C
      IF(LRLPVX.OR.LCONVS.OR.LVZERS) THEN
        CALL YPIDAS(IER)
        IF(IER.NE.0) THEN
          ICNER4=ICNER4+1
#if defined(YDEBUG)
          IF(ICNER4.LT.100) WRITE(LOUT,*)
     &     'YTOPOL:fatal error occured in part. identity ass. of tracks'
          IF(ICNER4.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS IN PART.ID. ASS. STOP PRINTING'
#endif
          RETURN
        ENDIF
      ENDIF
C
C-- find gamma conversions before reconstructing the
C-- primary vertex
C
CHECK TIME
      CALL TIMEL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LCONVS) THEN
        CALL YTCONV(IER)
        IF(IER.NE.0) THEN
          ICNER5=ICNER5+1
#if defined(YDEBUG)
          IF(ICNER5.LT.100) WRITE(LOUT,*)
     &      'YTOPOL : ERROR OCCURED IN GAMMA CONVERSION SEARCH'
          IF(ICNER5.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS IN GAMMA CONV. STOP PRINTING'
#endif
        ENDIF
      ENDIF
C
C-- find V0 tracks before reconstructing the
C-- primary vertex
C
CHECK TIME
      CALL TIMEL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LVZERS) THEN
        CALL YTRV0S(IER)
        IF(IER.NE.0) THEN
          ICNER6=ICNER6+1
#if defined(YDEBUG)
          IF(ICNER6.LT.100) WRITE(LOUT,*)
     &      'YTOPOL : ERROR OCCURED IN V0 SEARCH'
          IF(ICNER6.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS IN V0 search. STOP PRINTING'
#endif
        ENDIF
      ENDIF
C
C-- reconstruct the primary vertex
C
CHECK TIME
      CALL TIMEL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRPVTX) THEN
        CALL YTPVTX(IER)
        IF(IER.GT.1) THEN
          ICNER7=ICNER7+1
#if defined(YDEBUG)
          IF(ICNER7.LT.100) WRITE(LOUT,*)
     &      'YTOPOL : ERROR OCCURED IN PRIMARY VERTEX RECONSTRUCTION'
     &      ' (IER=',IER,')'
          IF(ICNER7.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS in prim.vtx. search STOP PRINTING'
#endif
        ENDIF
      ENDIF
C
C-- search for secondary vertices
C
CHECK TIME
      CALL TIMEL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRSVTX) THEN
        CALL YTOSVT(IER)
        IF(IER.NE.0) THEN
          ICNER8=ICNER8+1
#if defined(YDEBUG)
          IF(ICNER8.LT.100) WRITE(LOUT,*)
     &     'YTOPOL : ERROR OCCURED IN SECONDARY VERTEX SEARCH'
          IF(ICNER8.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS in sec.vtx. search STOP PRINTING'
#endif
        ENDIF
      ENDIF
C
C-- call user routine
C
CHECK TIME
      CALL TIMEL(TLEFT)
      IF(TLEFT.LT.TLIMIT) RETURN
      IF(LRUSER) THEN
        CALL YRUSER(IER)
        IF(IER.NE.0) THEN
          ICNER9=ICNER9+1
#if defined(YDEBUG)
          IF(ICNER9.LT.100) WRITE(LOUT,*)
     &     'YTOPOL : ERROR OCCURED IN USER ROUTINE YRUSER'
          IF(ICNER9.EQ.100)  WRITE(LOUT,*)
     &'YTOPOL : WARNING! 100 ERRORS in user program STOP PRINTING'
#endif
        ENDIF
      ENDIF
C
      RETURN
      END
#endif
