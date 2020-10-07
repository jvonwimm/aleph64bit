      SUBROUTINE VLCPOS(IBEG,NSTRP,PHCUT,POS,PSUM,NPOS)
C----------------------------------------------------------------------
C!  - Compute the local position of a final cluster
C!
C!   Modified :- Dave Brown, 18-8-91,
C!  complete rewrite of VREPOS for hit separation,
C!  new algorthim, etc.
C!   Modified 1 1992 new definition of sum pulseheight
C!
C! A newer and better version was written by D. Brown, but the
C! This routine is an adaptation of the one extracted from old code.
C! algorithms were slightly changed. For the moment, and for best
C! compatibility with the old code, we use this.
C! A. Bonissent January 1995
C!
C!
C!  Inputs;     RPULS = Array of pulseheight values
C!              NSTRP = Number of strips
C!              PHCUT = Minimum PH cut
C!  Outputs;    POS   = position relative to first strip, in strip coord
C!              PSUM  = Pulseheight sum of all strips
C!              NPOS  = # of used strips, 0 means bug
C!
C?
C!======================================================================
#ifndef DOC
      INTEGER MAXSTR
      PARAMETER (MAXSTR = 50)
      REAL RPULS(MAXSTR),POS,PHCUT,PSUM,PPSUM
      INTEGER NAVCSG,KVCSG
      INTEGER NAMIND
      INTEGER ISTRP,JSTRP,ILOOP
      INTEGER NPOS,NSTRP,IBEG,INDEX(MAXSTR),IEDGE(2)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL CRCUT
      DATA CRCUT/.75/
#include "vcsgjj.h"
#include "bcs.h"
#include "bmacro.h"
C
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVCSG=NAMIND('VCSG')
      ENDIF
C
C  Check number of strips
C
      IF(NSTRP .GT. MAXSTR)THEN
        NPOS = 0
        RETURN
      END IF
      KVCSG=IW(NAVCSG)
C
C Fill the strip pulse height array
C
      DO ISTRP=1,NSTRP
         RPULS(ISTRP)=RTABL(KVCSG,ISTRP+IBEG-1,JVCSMP)
      ENDDO
C
C  Compute the total pulseheight sum
C
      PSUM = 0.0
      DO 17 ISTRP=1,NSTRP
        PSUM = PSUM + RPULS(ISTRP)
17    CONTINUE
C
C  First, order the pulseheights
C
C     CALL SORTZV(RPULS,INDEX,NSTRP,1=real,1=descending,0=normal sort)
      CALL SORTZV(RPULS,INDEX,NSTRP,1,1,0)
C
C  Check that at least 1 ph is above threshold
C
      IF(RPULS(INDEX(1)).LT.PHCUT)THEN
        NPOS = 0
        RETURN
      END IF
C
C  Include all adjacent strips above the threshold
C
      DO 10 JSTRP=INDEX(1),NSTRP
        IF(RPULS(JSTRP).GT.PHCUT)THEN
          IEDGE(2) = JSTRP
        ELSE
          GOTO 11
        END IF
10    CONTINUE
11    CONTINUE
      DO 12 JSTRP=INDEX(1),1,-1
        IF(RPULS(JSTRP).GT.PHCUT)THEN
          IEDGE(1) = JSTRP
        ELSE
          GOTO 13
        END IF
12      CONTINUE
13      CONTINUE
C
C  If the cluster only has 1 strip, try adding
C  a second strip to improve the position;  For this,
C  it should be adjacent to the primary strip, and have at least 5%
C  of the PH of the primary strip.
C
      IF(NSTRP .GT. 1 .AND. IEDGE(2).EQ.IEDGE(1) .AND.
     &   ABS(INDEX(2)-INDEX(1)).EQ. 1 .AND.
     &   RPULS(INDEX(2)) .GT. .05*RPULS(INDEX(1)))THEN
        IEDGE(1) = MIN(INDEX(1),INDEX(2))
        IEDGE(2) = MAX(INDEX(1),INDEX(2))
      END IF
C
C  Make the position sum
C
      POS = 0.0
      PPSUM = 0.0
      DO 20 ISTRP=IEDGE(1),IEDGE(2)
        PPSUM = PPSUM + RPULS(ISTRP)
        POS = POS + RPULS(ISTRP)*ISTRP
20    CONTINUE
C
C  The position is merely the weighted average
C
      POS = POS/PPSUM
      NPOS = IEDGE(2)-IEDGE(1)+1
      RETURN
      END
#endif
