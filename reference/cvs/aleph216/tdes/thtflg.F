      SUBROUTINE THTFLG(ITK,IFLG)
C
C---------------------------------------------------------------------
C! Which TPC padrows is a given track expected to hit?
CKEY TPCDES TRACK TPC / USER
C  Author:  R. Johnson    13-07-89
C
C  Input:     ITK       /I      Track number in FRFT
C  Output:    IFLG(21)  /I      0 if no hit expected on this row
C                               1 if a hit is expected on this row
C
C  NOTE:  the TPC geometry commons must be initialized by a call to
C         TRDDAF before this routine is called
C
C----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpgpar.h"
#include "frftjj.h"
C
      SAVE
      DIMENSION HP(5),HPT(5),IFLG(*)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRFT=NAMIND('FRFT')
      ENDIF
      KFRFT=IW(NFRFT)
      IF (KFRFT.EQ.0) THEN
        DO 10 IR=1,LTPDRO
          IFLG(IR)=0
   10   CONTINUE
      ENDIF
C
C++   Find new helix parameters after scattering at the ITC-TPC wall
C
      HP(1)=RTABL(KFRFT,ITK,JFRFIR)
      HP(2)=RTABL(KFRFT,ITK,JFRFTL)
      HP(5)=RTABL(KFRFT,ITK,JFRFZ0)
      CALL UNEWDP(RW(KROW(KFRFT,ITK)+JFRFIR),HP(4),HP(3))
C
C++   Transform to the TPC frame (alignment correction)
C
      CALL TGHPAL(HP,HPT)
C
C++   Get the padrows expected to be hit
C
      CALL TRHFLG(HPT,IFLG)
C
      RETURN
      END
#endif
