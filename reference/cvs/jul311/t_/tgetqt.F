      SUBROUTINE TGETQT(IROW,KTSCL,IC,KTSPU,IQE,ITE,TANL,RPULS,
     &                                RTIME,NPUL,K1,IFLG)
C
C-----------------------------------------------------------------------
C! Make charge and time lists for coordinates
C!
C!    Author:    R. Johnson  03-08-87
C!    Modified:  R. Johnson  08-04-88
C!               D. Casper   25-04-96   Support for shaping correction
C!                           20-05-97   Restrict tan(lambda) to range +/-3
C!
C!    Input:
C!       - IROW      /I     Sector row number
C!       - KTSCL     /I     Pointer to subcluster bank in BOS
C!       - IC        /I     Row number of the subcluster in TSCL
C!       - KTSPU     /I     Pointer to subpulse bank in BOS
C!       - IQE       /I     Charge algorithm; see ChargeAlg of TLCT
C!       - ITE       /I     Time algorithm
C!       - TANL      /I     tan(lambda) for shaping correction
C!    Output:
C!       - RPULS     /R     List of charge estimates
C!       - RTIME     /R     List of time estimates
C!       - NPUL      /I     Number of values of RPULS and RTIME
C!       - K1        /I     First pulse in RPULS and RTIME
C!       - IFLG      /I     Quality flag   =0 for a good subcluster
C!                          600  : Cluster of two half pads only.
C!                          601  : Long cluster with two half pads.
C!                          602  : Outer half pad has larger ph. than
C!                                 the inner one.
C!
C!    Called by TCOOR
C!
C!---------------------------------------------------------------------
#ifndef DOC
#include "tparac.h"
#include "tpgpar.h"
#include "bcs.h"
#include "tspujj.h"
#include "tscljj.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpccut.h"
#include "tshpco.h"
C
      DIMENSION RPULS(*),RTIME(*)
C
C--------------- Statement Function Definitions -----------------------
C
#include "bmacro.h"
C
C----------------------------------------------------------------------
C
      IFLG=0
C
C++   Get the sector location and type
C
      ISLOT=IW(KTSCL-2)
      ISTYP=ITPTYP(ISLOT)
C
C++   Get coefficients for shaping correction, if needed
C
      IF (ITE.EQ.1)THEN
        X    = MIN(ABS(TANL),3.)
C -     pad pulse correction term is independent of 1st moment
        SHP0 = TSHPCN(1)*EXP(-0.5*((X-TSHPCN(2))/TSHPCN(3))**2)
     &        +TSHPCN(4)+TSHPCN(5)*X+TSHPCN(6)*X**2
C -     pad-pulse correction term proportional to first moment
        SHP1 = TSHPSL(1)*EXP(-0.5*((X-TSHPSL(2))/TSHPSL(3))**2)
     &        +TSHPSL(4)+TSHPSL(5)*X+TSHPSL(6)*X**2
      ELSE
        SHP0 = 0.
        SHP1 = 0.
      ENDIF
C
C++   Check whether there are any half pads in the cluster.  First
C++   consider the case of having two half pads in the cluster.
C
      NSPL=ITABL(KTSCL,IC,JTSCNS)
      IOFSP=ITABL(KTSCL,IC,JTSCPO)
      IPDMN=ITABL(KTSCL,IC,JTSCFP)
      IPDMX=IPDMN+NSPL-1
      NTPAD=NTPDPR(IROW,ISTYP)+2
      IF (IPDMN.EQ.1 .OR. IPDMX.EQ.NTPAD) THEN
C
C++     Reject clusters composed of two half pads and only two.
C
        IF (NSPL.EQ.2) THEN
          IFLG=600
          GO TO 999
        ENDIF
C
C++     Reject long clusters which include both half pads, since
C++     these probably are broken across a sector boundary.
C
        IF (NSPL.GT.NPHPTP) THEN
          IFLG=601
          GO TO 999
        ENDIF
C
C++     Add the half pad pulses and average their times.
C
        NPUL=NSPL-1
        IF (IPDMN.EQ.1) THEN
          K1=2
          JOUT=1
          JIN=2
          JFST=3
          JLST=NSPL
        ELSE
          K1=1
          JOUT=NSPL
          JIN=NSPL-1
          JFST=1
          JLST=NSPL-2
        ENDIF
        IF (IQE.EQ.1) THEN
          PHO=RTABL(KTSPU,IOFSP+JOUT,JTSPPC)
          PHI=RTABL(KTSPU,IOFSP+JIN,JTSPPC)
        ELSE
          PHO=FLOAT(ITABL(KTSPU,IOFSP+JOUT,JTSPTC))
          PHI=FLOAT(ITABL(KTSPU,IOFSP+JIN,JTSPTC))
        ENDIF
        IF (PHO.GT.PHI) THEN
          IFLG=602
          GO TO 999
        ELSE
          RPULS(JIN)=THPADD(PHI,PHO)
          TCIN = SHP0 + SHP1 * RTABL(KTSPU,IOFSP+JIN,JTSPIT)
          TCOUT = SHP0 + SHP1 * RTABL(KTSPU,IOFSP+JOUT,JTSPIT)
          RTIME(JIN)= (PHO*(RTABL(KTSPU,IOFSP+JOUT,JTSPPT)+TCOUT)
     &                 + PHI*(RTABL(KTSPU,IOFSP+JIN,JTSPPT)+TCIN))
     &               / (PHI+PHO)
        ENDIF
C
C++   Then consider the case of only one half pad in the cluster.
C
      ELSEIF (IPDMN.EQ.2) THEN
        K1=1
        NPUL=NSPL
        JFST=2
        JLST=NSPL
        IF (IQE.EQ.1) THEN
          PHI=RTABL(KTSPU,IOFSP+1,JTSPPC)
        ELSE
          PHI=FLOAT(ITABL(KTSPU,IOFSP+1,JTSPTC))
        ENDIF
        RTIME(1)=RTABL(KTSPU,IOFSP+1,JTSPPT) +
     &    SHP0+SHP1*RTABL(KTSPU,IOFSP+1,JTSPIT)
        RPULS(1)=THPADD(PHI,0.)
      ELSEIF (IPDMX.EQ.(NTPAD-1)) THEN
        K1=1
        NPUL=NSPL
        JFST=1
        JLST=NSPL-1
        IF (IQE.EQ.1) THEN
          PHI=RTABL(KTSPU,IOFSP+NSPL,JTSPPC)
        ELSE
          PHI=FLOAT(ITABL(KTSPU,IOFSP+NSPL,JTSPTC))
        ENDIF
        RTIME(NSPL)=RTABL(KTSPU,IOFSP+NSPL,JTSPPT) +
     &    SHP0+SHP1*RTABL(KTSPU,IOFSP+NSPL,JTSPIT)
        RPULS(NSPL)=THPADD(PHI,0.)
C
C++   Or else set up the pointers for the case of no half pads.
C
      ELSE
        K1=1
        NPUL=NSPL
        JFST=1
        JLST=NSPL
      ENDIF
C
C++   Now we are through dealing with half pads and can go on to
C++   handle the normal pads.
C
      DO 100 JSTOR=JFST,JLST
        ISP=IOFSP+JSTOR
        RTIME(JSTOR)=RTABL(KTSPU,ISP,JTSPPT) +
     &    SHP0 + SHP1 * RTABL(KTSPU,ISP,JTSPIT)
        IF (IQE.EQ.1) THEN
          RPULS(JSTOR)=RTABL(KTSPU,ISP,JTSPPC)
        ELSE
          RPULS(JSTOR)=FLOAT(ITABL(KTSPU,ISP,JTSPTC))
        ENDIF
  100 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
