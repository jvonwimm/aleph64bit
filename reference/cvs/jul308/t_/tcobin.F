      SUBROUTINE TCOBIN(IER)
C
C--------------------------------------------------------------------
C! Bin coordinates for pattern recognition
C!
C!    Author:  R. Johnson     4-12-87
C!
C!    Output:   IER     /I     Error return code=0 for good return
C!
C!    Called by TRKFND
C!
C!-------------------------------------------------------------------
#ifndef DOC
C
#include "alcons.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tcbins.h"
#include "tpcojj.h"
#include "tcrljj.h"
#include "tpgeom.h"
C
      DIMENSION IPTR(LMXBIN)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C---------------------- Statement Function Definitions --------------
C
#include "bmacro.h"
C
C--------------------------------------------------------------------
C
C++   Initialize the common TCBINS on first call
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
        INDBIN=0
C
C++     Fill array used to wrap around the branch 0,2pi when calculating
C++     the bin number for a given value of phi
C
        DO 20 I=1,LMXBIN
          IXWRAP(I+2*NMXWRP+1)=I
   20   CONTINUE
        I2NMX=2*NMXWRP
        DO 30 I=1,I2NMX
          IXWRAP(I+LMXBIN+2*NMXWRP+1)=I
   30   CONTINUE
        IXWRAP(LNWRAP)=IXWRAP(LNWRAP-1)+1
        IF (IXWRAP(LNWRAP).GT.LMXBIN) THEN
          IXWRAP(LNWRAP)=IXWRAP(LNWRAP)-LMXBIN
        ENDIF
        K=LMXBIN+1
        DO 40 I=I2NMX,1,-1
          K=K-1
          IXWRAP(I+1)=K
   40   CONTINUE
        IXWRAP(1)=IXWRAP(2)-1
        IF (IXWRAP(1).LT.1) IXWRAP(1)=IXWRAP(1)+LMXBIN
C
        TCBINW= 2.*TWOPI/FLOAT(LMXBIN)
        TCDELT= TCBINW/4.
        TC2DEL= 2.*TCDELT
        TCOFFS= 1.5 + FLOAT(2*NMXWRP)
      ENDIF
C
C++   Book a workbank for the linked list
C
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) THEN
        CALL RERROR('TCOBIN',2,'No coordinate bank available')
        IER=2
        GO TO 999
      ENDIF
      KTCRL=IW(NTCRL)
      IF (KTCRL.EQ.0) THEN
        CALL RERROR('TCOBIN',3,'No coord. rowlist bank available')
        IER=3
        GO TO 999
      ENDIF
      IW(1)=1
      LEN= 2*LROWS(KTPCO)*LNDBIN + LMHLEN
      CALL WBANK(IW,INDBIN,LEN,*998)
      IW(INDBIN+LMHCOL)= LNDBIN
      IW(INDBIN+LMHROW)= 2*LROWS(KTPCO)
      IW(INDBIN-3)=INTCHA('WBIN')
C
C++   Loop over all padrows
C
      IBLAST=1
      CALL VZERO(IBNPTR,LMXBIN*NTPROW)
      DO 900 IROW=1,NTPROW
        CALL VZERO(IPTR,LMXBIN)
C
C++     Loop over all coordinates in the padrow
C
        NC=ITABL(KTCRL,IROW,JTCRNC)
        DO 800 II=1,NC
          IC= II + ITABL(KTCRL,IROW,JTCROC)
          PHI= RTABL(KTPCO,IC,JTPCPH)
C
C++       For each coordinate, fill pointers into two overlapping bins
C
          IBN1= IXWRAP(2*(INT(PHI/TCBINW)+NMXWRP)+2)
          IF (IPTR(IBN1).EQ.0) THEN
            IBNPTR(IBN1,IROW)=IBLAST
            IW(KROW(INDBIN,IBLAST)+JNDBBW)= -IBN1
          ELSE
            IW(KROW(INDBIN,IPTR(IBN1))+JNDBFW)=IBLAST
            IW(KROW(INDBIN,IBLAST)+JNDBBW)= IPTR(IBN1)
          ENDIF
          IW(KROW(INDBIN,IBLAST)+JNDBCO)= IC
          IW(KROW(INDBIN,IBLAST)+JNDBFW)= 0
          IPTR(IBN1)=IBLAST
C
C++       Note that the two entries for a single coordinate always lie
C++       adjacent to each other in workbank INDBIN.
C
          IBLAST=IBLAST+1
          IBN2= IXWRAP(2*(INT((PHI/TCBINW)+0.5)+NMXWRP)+1)
          IF (IPTR(IBN2).EQ.0) THEN
            IBNPTR(IBN2,IROW)=IBLAST
            IW(KROW(INDBIN,IBLAST)+JNDBBW)= -IBN2
          ELSE
            IW(KROW(INDBIN,IPTR(IBN2))+JNDBFW)=IBLAST
            IW(KROW(INDBIN,IBLAST)+JNDBBW)= IPTR(IBN2)
          ENDIF
          IW(KROW(INDBIN,IBLAST)+JNDBCO)= IC
          IW(KROW(INDBIN,IBLAST)+JNDBFW)= 0
          IPTR(IBN2)=IBLAST
          IBLAST=IBLAST+1
  800   CONTINUE
  900 CONTINUE
      IER=0
      GO TO 999
  998 CONTINUE
      CALL RERROR('TCOBIN',1,'No room in BOS for workbank')
      IER=1
  999 CONTINUE
      RETURN
      END
#endif
