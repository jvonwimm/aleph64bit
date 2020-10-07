      SUBROUTINE TPDTHR
C
C-------------------------------------------------------------------
C! Unpack TPD bad channels
C!
C!    Author:   R. Johnson  8-04-88
C!
C!    Input banks:  TCLB and TKAP banks from TPC online.
C!    Input common: TPGEOM global geometry constants for TPC.
C!                  TPGEOP pad geometry constants for TPC
C!    Output banks:
C!            THPL -  list of bad pads and associated thresholds
C!            THRP -  pointers into THPL for each sector and row
C!
C!    Called by TINIRU
C!
C-------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "tpgpar.h"
#include "thrpjj.h"
#include "thpljj.h"
#include "tclbjj.h"
#include "tpgeom.h"
#include "tpgeop.h"
C
      COMMON/TPDTHW/ ISORTW
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C-------------------- Statement Functions --------------------------
C
#include "bmacro.h"
C
C-------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTHRP=NAMIND('THRP')
        NTHPL=NAMIND('THPL')
        NTKAP=NAMIND('TKAP')
        NTCLB=NAMIND('TCLB')
        ISORTW=0
      ENDIF
C
C++   Check whether there is any input
C
      KTKAP=IW(NTKAP)
      IF (KTKAP.LE.0) GO TO 999
      KTCLB=IW(NTCLB)
      IF (KTCLB.LE.0) GO TO 999
C
C++   Count the total number of bad channels and max. per sector
C
      MXBAD=0
      NBAD=0
      DO 1 I=1,LROWS(KTCLB)
        NT=ITABL(KTCLB,I,JTCLNT)
        MXBAD=MAX(MXBAD,NT)
        NBAD=NBAD+NT
    1 CONTINUE
      IF (NBAD.EQ.0) GO TO 999
C
C++   Get rid of any output banks still floating around
C
      IF (IW(NTHRP).NE.0) CALL BDROP(IW,'THRP')
      IF (IW(NTHPL).NE.0) CALL BDROP(IW,'THPL')
C
C++   Create a workbank for sorting
C
      LEN=MXBAD*3
      IW(1)=1
      CALL WBANK(IW,ISORTW,LEN,*999)
      IW(ISORTW-3)=INTCHA('WSRT')
C
C++   Create bank for list of bad channels
C
      LEN=NBAD*LTHPLA+LMHLEN
      IW(1)=1
      CALL AUBOS('THPL',0,LEN,KTHPL,IGARB)
      IF (IGARB.EQ.2) GO TO 999
      IW(KTHPL+LMHCOL)=LTHPLA
      IW(KTHPL+LMHLEN)=0
      CALL BLIST(IW,'C+','THRPTHPL')
C
C++   Loop over all sectors with threshold banks
C
      KTCLB=IW(NTCLB)
      KTKAP=IW(NTKAP)
      IOFF=LMHLEN
      DO 501 ISLOT=1,LROWS(KTCLB)
        NBAD=ITABL(KTCLB,ISLOT,JTCLNT)
        NBADP=0
        DO 700 I=1,NBAD
          IWORD=IW(KTKAP+IOFF+I)
          JSLOT=IBITS(IWORD,24,8)
          IF (ISLOT.NE.JSLOT) THEN
            CALL RERROR('TPDTHR',1,'Bank TCLB is not in sequence'
     &                             //' with bank TKAP.')
            GO TO 888
          ENDIF
          IROW=IBITS(IWORD,8,8)
          IF (IROW.LT.0 .OR. IROW.GT.LTPDRO) THEN
            CALL RERROR('TPDTHR',2,'Padrow number out of range'
     &                             //' in bank TKAP.')
            GO TO 888
          ENDIF
          IF (IROW.LT.1) GO TO 700
          NBADP=NBADP+1
          IPADN=IBITS(IWORD,0,8)
          IW(ISORTW+NBADP)=IROW
          IW(ISORTW+NBAD+NBADP)=IPADN
  700   CONTINUE
C
        IF (NBADP.GT.0) THEN
          ISTYP=ITPTYP(ISLOT)
          LEN=NTPDRW(ISTYP)*LTHRPA+LMHLEN
          IW(1)=1
          CALL AUBOS('THRP',ISLOT,LEN,KTHRP,IGARB)
          IF (IGARB.EQ.2) GO TO 999
          IF (IGARB.EQ.1) THEN
            KTCLB=IW(NTCLB)
            KTKAP=IW(NTKAP)
            KTHPL=IW(NTHPL)
          ENDIF
          IW(KTHRP+LMHCOL)=LTHRPA
          IW(KTHRP+LMHROW)=NTPDRW(ISTYP)
          CALL VZERO(IW(KTHRP+LMHLEN+1),LTHRPA*NTPDRW(ISTYP))
C
C++       Sort the bad channels by padrow number
C
          CALL SORTZV(IW(ISORTW+1),IW(ISORTW+2*NBAD+1),NBADP,-1,0,0)
C
C++       Fill output banks for this sector, one padrow at a time
C
          IROLD=-1
          DO 780 I=1,NBADP
            IDX=IW(ISORTW+2*NBAD+I)
            IROW=IW(ISORTW+IDX)
            IF (IROW.NE.IROLD) THEN
              IROLD=IROW
              IW(KROW(KTHRP,IROW)+JTHRPT)=LROWS(KTHPL)
            ENDIF
            IW(KROW(KTHRP,IROW)+JTHRNB)=ITABL(KTHRP,IROW,JTHRNB)+1
            IW(KTHPL+LMHROW)=LROWS(KTHPL)+1
            NTHP=LROWS(KTHPL)
            IW(KROW(KTHPL,NTHP)+JTHPPN)=IW(ISORTW+NBAD+IDX)
            IWORD=IW(KTKAP+IOFF+I)
            IW(KROW(KTHPL,NTHP)+JTHPTH)=IBITS(IWORD,16,8)
  780     CONTINUE
C
C++       Go back through and sort all entries per row by pad number
C
          DO 840 IROW=1,LROWS(KTHRP)
            N=ITABL(KTHRP,IROW,JTHRNB)
            IF (N.EQ.0) GO TO 840
            IOF=ITABL(KTHRP,IROW,JTHRPT)
            DO 820 I=1,N
              IW(ISORTW+I)=ITABL(KTHPL,IOF+I,JTHPPN)
              IW(ISORTW+N+I)=ITABL(KTHPL,IOF+I,JTHPTH)
  820       CONTINUE
            CALL SORTZV(IW(ISORTW+1),IW(ISORTW+2*N+1),N,-1,0,0)
            DO 830 I=1,N
              IDX=IW(ISORTW+2*N+I)
              IW(KROW(KTHPL,IOF+I)+JTHPPN)=IW(ISORTW+IDX)
              IW(KROW(KTHPL,IOF+I)+JTHPTH)=IW(ISORTW+N+IDX)
  830       CONTINUE
  840     CONTINUE
        ENDIF
        IOFF=IOFF+NBAD
  501 CONTINUE
C
  999 CONTINUE
C
C++   Set lengths of name banks
C
      CALL AUBPRS('THPLTHRP')
C
C++   Drop the workbank
C
      CALL WDROP(IW,ISORTW)
      RETURN
C
C++   Drop output banks in case of an error
C
  888 CONTINUE
      CALL BDROP(IW,'THPLTHRP')
      CALL WDROP(IW,ISORTW)
      RETURN
      END
#endif
