      SUBROUTINE TGHPAL(P,PA)
C
C----------------------------------------------------------------------
C! Transform helix from ALEPH coordinates to TPC coordinates
CKEY TPCDES HELIX TRANSFORM / USER
C     Author:   R. Johnson   19-08-89
C     Modified: R. Johnson   21-1-91 separate the geometry calculations
C                            into another routine which can be called
C                            in a different context
C               D. Casper    05-03-97 handle new style alignment bank
C                            (TNOS)
C
C     Input:
C       - P(5)       /R     Helix parameters in ALEPH frame
C                           1/r,tanl,phi0,d0,z0
C                           (d0>0 = positive ang. mom. about z axis)
C                           (r>0  = counterclockwise rotation)
C     Output:
C       - PA(5)     /R      Helix parameters in TPC frame
C
C   Remarks:  only translations and rotations about the z axis are
C             possible in this case.  Rotations about x and y are
C             ignored since the helix model always assumes a helix
C             about the z axis.
C
C----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "tposjj.h"
#include "tnosjj.h"
#include "evehjj.h"
#include "bcs.h"
C
      DIMENSION P(*),PA(*),DX(3)
      INTEGER ALGTDB
      LOGICAL FIRST
      DOUBLE PRECISION XC,YC
      DATA FIRST/.TRUE./,EPS/0.001/
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPOS=NAMIND('TPOS')
        NTNOS=NAMIND('TNOS')
        NTOAL=NAMIND('TOAL')
        NEVEH=NAMIND('EVEH')
      ENDIF
      DO 10 I=1,5
        PA(I)=P(I)
   10 CONTINUE
C
C++   Find the run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL ALTELL('TGHPAL: cannot find EVEH bank',0,'RETURN')
        RETURN
      ENDIF
      IRUN=IW(KEVEH+JEVERN)
C
C++   Link to the bank of alignment constants.  Read them from the
C++   database if necessary.  Look for TNOS bank (new alignment) first.
C
      KTNOS = 0
      KTPOS = 0
      IF (IW(NTOAL).EQ.0) THEN
        KTNOS=IW(NTNOS)
        IF (KTNOS.EQ.0) THEN
            IRET=ALGTDB(JUNIDB(0),'TNOS',IRUN)
            KTNOS=IW(NTNOS)
        ELSE IF (KTNOS.GT.0 .AND. (IRUN.LT.ITABL(KTNOS,1,JTNOVR) .OR.
     &        IRUN.GT.ITABL(KTNOS,1,JTNOVR+1))) THEN
          IRET=ALGTDB(JUNIDB(0),'TNOS',IRUN)
          KTNOS=IW(NTNOS)
        ENDIF
      ENDIF
      IF (KTNOS.EQ.0) THEN
        KTPOS=IW(NTPOS)
        IF (KTPOS.EQ.0) THEN
            IRET=ALGTDB(JUNIDB(0),'TPOS',IRUN)
            KTPOS=IW(NTPOS)
        ELSE IF (IRUN.LT.ITABL(KTPOS,1,JTPOVR)
     &        .OR. IRUN.GT.ITABL(KTPOS,1,JTPOVR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TPOS',IRUN)
          KTPOS=IW(NTPOS)
        ENDIF
      ENDIF
      IF (KTPOS.EQ.0 .AND. KTNOS.EQ.0) THEN
        CALL ALTELL('TGHPAL: Cannot locate TPOS or TNOS on database',
     &      0,'RETURN')
        RETURN
      ENDIF
C
      IF (KTNOS.GT.0) THEN
        DX(1)=RTABL(KTNOS,1,JTNOTL)
        DX(2)=RTABL(KTNOS,1,JTNOTL+1)
        DX(3)=RTABL(KTNOS,1,JTNOTL+2)
        DPHI=RTABL(KTNOS,1,JTNORT+2)
      ELSE
        DX(1)=RTABL(KTPOS,1,JTPOTL)
        DX(2)=RTABL(KTPOS,1,JTPOTL+1)
        DX(3)=RTABL(KTPOS,1,JTPOTL+2)
        DPHI=RTABL(KTPOS,1,JTPORT+2)
      ENDIF
      CALL THTRAN(P,DX,DPHI,PA)
C
      END
#endif
