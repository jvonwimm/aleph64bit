      SUBROUTINE TRKFND(IER)
C
C--------------------------------------------------------------------
C! Track-finding in the TPC
C! using 3-D chain method (ALEPH-TPCDET Note 84-69)
C!
C!   Author:     M.Mermikides    06-10-86
C!   Modified:   R.Johnson        8-04-88
C!   Modified:   M.Mermikides    19-08-89
C!
C!   Output:  IER    /I    Error code, nonzero if error has occurred
C!   Input banks:
C!            'TPCO'       TPC coordinates
C!            'TCRL'       TPC coordinate row list
C!   Output banks:
C!            'TCHA'       Chain parameters
C!            'TCTC'       Chain -> coordinate reference
C!
C!
C!   Called by TPCREC
C!
C!   Strategy:
C!   ========
C!
C!     Three-point links are formed from successive padrows,
C!  starting from the outermost row.(There can be a maximum gap
C!  of MAXGAP between successive rows). Links are sorted in order
C!  of decreasing radius of curvature and propagated inwards
C!  using a local point search to form longest possible chain.
C!  Chains are verified by a helix fit and fitted parameters
C!  are used to project incomplete chains both inwards and
C!  outwards to pick extra coordinates.
C!
C!  Workbanks used internally:
C!                INDCXY    X-Y coordinates corresp. to TPCO
C!                INDLNK    Padrow and hit references and
C!                          helix parameters of links from
C!                          current staring row
C!                JSORTW    Workspace for sorting links
C!                INDBIN    Date structure for binned coordinates
C!
C!  NOTE:  This routine and the subroutines which it calls represent
C!         a MODULE within JULIA.
C-------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tclujj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "tpcojj.h"
#include "tcrljj.h"
#include "tchajj.h"
#include "bcs.h"
#include "tpwork.h"
#include "trfdct.h"
#include "tchain.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "tpgeom.h"
#include "tcbins.h"
C
C----------------------------------------------------------------------
C
      PARAMETER (LNSRT=100)
      LOGICAL LFRST
      CHARACTER TEXT*60
      DATA NDCHA/800/, NXCHA/200/
      DATA NWCHC/1/,NWCXY/2/,MNLNK/4/
      DATA NWLNK/16/, NDLNK/300/
      DATA LFRST/.TRUE./
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C----------------------------------------------------------------------
C
      IF (LFRST) THEN
        LFRST=.FALSE.
        INDLNK=0
        INDCXY = 0
        JSORTW = 0
        NTPCO=NAMIND('TPCO')
        NTCRL=NAMIND('TCRL')
        NTCHA=NAMIND('TCHA')
        NTCTC=NAMIND('TCTC')
      ENDIF
C
      IER=0
C
C++   Check that the input banks are present
C
      KTPCO = IW(NTPCO)
      IF (KTPCO.EQ.0) GO TO 800
      KTCRL = IW(NTCRL)
      IF (KTCRL.EQ.0) GO TO 800
C
C++   Drop any of the output banks still lingering around
C
      IF (IW(NTCHA).NE.0) CALL BDROP(IW,'TCHA')
      IF (IW(NTCTC).NE.0) CALL BDROP(IW,'TCTC')
C
C  Create a workbank for sorting links
C
      IF (JSORTW.EQ.0) THEN
         IW(1) = 1
         LEN= 2*LNSRT
         CALL WBANK(IW,JSORTW,LEN,*801)
         IW(JSORTW-3)=INTCHA('WSRW')
      ENDIF
C
C  Bin the coordinates
C
      CALL TCOBIN(IER)
      IF (IER.NE.0) THEN
         CALL RERROR('TRKFND',5,'Error in binning of coordinates')
         IER=5
         GO TO 999
      ENDIF
C
C  Lift chain bank and add to T list
C
      IW(1) = 1
      CALL AUBOS('TCHA',0,NDCHA,KTCHA,IGARB)
      CALL BLIST(IW,'T+','TCHA')
      IF (IGARB.EQ.2) GO TO 830
      IF (IGARB.EQ.1) THEN
         KTCRL=IW(NTCRL)
         KTPCO=IW(NTPCO)
      ENDIF
      IW(KTCHA + LMHCOL) = LTCHAA
      IW(KTCHA + LMHROW) = 0
C
C  Lift Chain-> Coordinate  ref. bank and add to T list
C
      IW(1) = 1
      CALL AUBOS('TCTC',0,IW(KTPCO+LMHROW)+LMHLEN,KTCTC,IGARB)
      CALL BLIST(IW,'T+','TCTC')
      IF (IGARB.EQ.2) GO TO 830
      IF (IGARB.EQ.1) THEN
         KTCHA = IW(NTCHA)
         KTPCO = IW(NTPCO)
         KTCRL = IW(NTCRL)
      ENDIF
      IW(KTCTC + LMHCOL) = NWCHC
      IW(KTCTC + LMHROW) = 0
C
C  Book work banks to store links starting from given padrow
C
      IW(1) = 1
      CALL WBANK(IW,INDLNK,NDLNK,*810)
      IW(INDLNK + LMHCOL) = NWLNK
      IW(INDLNK + LMHROW) = 0
      IW(INDLNK-3)=INTCHA('WLNK')
C
C  Create INDCXY workbank to store x-y coordinates of pad hits.
C  This saves having to recompute them in the algorithms.
C
      IW(1) = 1
      NDATA = LMHLEN + LROWS(KTPCO)*NWCXY
      CALL WBANK(IW,INDCXY,NDATA,*820)
      IW(INDCXY + LMHCOL) = NWCXY
      IW(INDCXY + LMHROW) = LROWS(KTPCO)
      IW(INDCXY-3)=INTCHA('WCXY')
      JXY = INDCXY + LMHLEN
      KTPCO = IW(NTPCO)
      DO 50 I=1,LROWS(KTPCO)
         R = RTABL(KTPCO,I,JTPCRV)
         PHI = RTABL(KTPCO,I,JTPCPH)
         RW(JXY+1) = R*COS(PHI)
         RW(JXY+2) = R*SIN(PHI)
         JXY = JXY + NWCXY
   50 CONTINUE
C
C  Start looking for 3-point links from outermost padrow.
C  Stop when we reach padrow 4.
C
      DO 30 IROW=NTPROW, 4, -1
C
         KTCRL = IW(NTCRL)
         NCO1 = ITABL(KTCRL,IROW,JTCRNC)
         IF(NCO1.EQ.0) GO TO 30
C
C  Loop over every other phi bin in this row (this will get all
C  coordinates on this row, since they are double binned).
C
         DO 20 IBIN=1,LMXBIN,2
C
C  Loop through the linked list of coordinates for this bin
C
            IPTR= IBNPTR(IBIN,IROW)
  513       IF (IPTR.EQ.0) GO TO 514
C
C  Test all combinations of 3 padrows with max gap MAXGAP between
C  successive ones.
C
               DO 25 J=1,MAXGAP+1
                  IRWM = IROW - J
                  IF (IRWM.LT.3) GO TO 27
                  KTCRL = IW(NTCRL)
                  NCO2 = ITABL(KTCRL,IRWM,JTCRNC)
                  IF (NCO2.EQ.0) GO TO 25
C
                  DO 26 K=1,MAXGAP+1
                     IRWI = IRWM - K
                     IF (IRWI.LT.2) GO TO 26
                     KTCRL = IW(NTCRL)
                     NCO3 = ITABL(KTCRL,IRWI,JTCRNC)
                     IF (NCO3.EQ.0) GO TO 26
C
C  Search for 3-point link(s) from this hit
C
                     CALL TRSLNK(IROW,IRWM,IRWI,IPTR,NL,IER)
                     IF (IER.NE.0) GO TO 999
C
C  Exit the double loop as soon as a valid link is found
C
                     IF (NL.GT.0) GO TO 27
   26             CONTINUE
   25          CONTINUE
   27          CONTINUE
C
C  Find the next coordinate in the linked list for this phi bin
C
               IPTR=ITABL(INDBIN,IPTR,JNDBFW)
               GO TO 513
  514       CONTINUE
   20    CONTINUE
C
C   End of links originating from current pad row.
C   Sort links in decreasing radius of curvature so we get
C   fast tracks first.  Extend workbank if necessary.
C
         NLNKS=LROWS(INDLNK)
         IF (IW(JSORTW).LT.2*NLNKS) THEN
            LEN= 2*NLNKS
            CALL WBANK(IW,JSORTW,LEN,*801)
         ENDIF
         IF (NLNKS.GT.1) THEN
            JWORK= JSORTW+NLNKS
            DO 123 I = 1,NLNKS
               RW(JWORK + I) = ABS(RTABL(INDLNK,I,7))
  123       CONTINUE
            CALL SORTZV(RW(JWORK+1),IW(JSORTW+1),NLNKS,1,1,0,0,0)
         ELSE
            IW(JSORTW+1)=1
         ENDIF
C
C   Try to start a chain from each valid link
C
         DO 10 KK= 1,NLNKS
            ILINK= IW(JSORTW+KK)
C
C   Copy the chain's parameters into the current chain (common/TCHAIN/)
C   If any hits in this link have already been used by a validated
C   chain, then skip it.
C
            CALL UCOPY(IW(KROW(INDLNK,ILINK) + 4),IHCHAI(1),3)
            IF (ITABL(INDBIN,IHCHAI(1),JNDBFW).LT.0) GO TO 10
            IF (ITABL(INDBIN,IHCHAI(2),JNDBFW).LT.0) GO TO 10
            IF (ITABL(INDBIN,IHCHAI(3),JNDBFW).LT.0) GO TO 10
            NHCHAI = 3
            CALL UCOPY(IW(KROW(INDLNK,ILINK) + 1),IRCHAI(1),3)
            CALL UCOPY(RW(KROW(INDLNK,ILINK) + 7),PCHAIN(1),6)
            PCHAIN(JTCHC1) = 999.
            PCHAIN(JTCHC2) = 999.
            CALL VZERO(PCHAIN(JTCHER),15)
            D2CHAI(1) = RW(KROW(INDLNK,ILINK) + 14)
            D2CHAI(2) = RW(KROW(INDLNK,ILINK) + 15)
            D2CHAI(3) = RW(KROW(INDLNK,ILINK) + 16)
            ISTCHA = 0
C
C   Move inward row-by-row to add new points to the chain
C
            CALL TRSEEK
C
C  If we have enough points fit the chain to validate.
C
            IF (NHCHAI.GE.MNLNK)  CALL TRFTCH
C
C  If fit fails and there are enough points, try to rescue chain
C
            IF (ISTCHA.EQ.0 .AND. NHCHAI.GE.8) THEN
               CALL TRRESC
            ENDIF
C
C  Drop chain if unsuccessful; otherwise try to project across all padro
C  to pick up additional hits.
C
            IF (ISTCHA.EQ.0) GO TO 10
C
            IF (NHCHAI.GE.MNLNK) CALL TRPROJ
C
C  Save good chain in BOS bank TCHA
C
            IF (LTCHAA*(IW(KTCHA+LMHROW)+1)+LMHLEN.GT.IW(KTCHA)) THEN
               NDATA = IW(KTCHA) + NXCHA
               IW(1) = 1
               CALL AUBOS('TCHA',0,NDATA,KTCHA,IGARB)
               IF (IGARB.EQ.2) GO TO 810
               IF (IGARB.EQ.1) KTCTC=IW(NTCTC)
            ENDIF
            KCHA1 = KTCHA + LTCHAA*IW(KTCHA+LMHROW) + LMHLEN
C
C Convert first helix parameter from rho to 1/rho
C
            RW(KCHA1 + JTCHIR) = 1./PCHAIN(1)
            CALL UCOPY(PCHAIN(JTCHTL),RW(KCHA1 + JTCHTL),7)
            CALL UCOPY(PCHAIN(JTCHER),RW(KCHA1 + JTCHER),15)
            IW(KCHA1 + JTCHPI) = 0
            IW(KTCHA + LMHROW) = IW(KTCHA + LMHROW) + 1
C
C   Remove associated coordinates from the phi bins, enter
C   hits in TCTC bank and define offset in TCHA.
C
            NHITS = 0
            NLAST = IW(KTCTC+LMHROW)
            DO 11 J=NHCHAI,1,-1
               IF (MCHAIN(J).EQ.0) THEN
                  IPTR= IHCHAI(J)
                  IW(KCHA1+JTCHPI)= IBSET(IW(KCHA1+JTCHPI),IRCHAI(J))
                  NHITS = NHITS + 1
                  IC= ITABL(INDBIN,IPTR,JNDBCO)
                  IW(KTCTC+LMHLEN+NLAST+NHITS) = IC
C
C  Each coordinate must be removed from two phi bins
C
                  IPTBW= ITABL(INDBIN,IPTR,JNDBBW)
                  IPTFW= ITABL(INDBIN,IPTR,JNDBFW)
                  IF (IPTBW.GT.0) THEN
                     IW(KROW(INDBIN,IPTBW)+JNDBFW)= IPTFW
                  ELSE
                     IBNPTR(-IPTBW,IRCHAI(J))= IPTFW
                  ENDIF
                  IF (IPTFW.NE.0) IW(KROW(INDBIN,IPTFW)+JNDBBW)= IPTBW
                  IW(KROW(INDBIN,IPTR)+JNDBFW)= -1
C
C  The other entry for the same coordinate is adjacent to the first
C  in workbank INDBIN, but we do not know to which side.
C
                  IPTR= IPTR-1
                  IF (IPTR.NE.0) THEN
                    IF (ITABL(INDBIN,IPTR,JNDBCO).NE.IC) THEN
                      IPTR= IPTR+2
                    ENDIF
                  ELSE
                    IPTR= IPTR+2
                  ENDIF
                  IPTBW= ITABL(INDBIN,IPTR,JNDBBW)
                  IPTFW= ITABL(INDBIN,IPTR,JNDBFW)
                  IF (IPTBW.GT.0) THEN
                     IW(KROW(INDBIN,IPTBW)+JNDBFW)= IPTFW
                  ELSE
                     IBNPTR(-IPTBW,IRCHAI(J))= IPTFW
                  ENDIF
                  IF (IPTFW.NE.0) IW(KROW(INDBIN,IPTFW)+JNDBBW)= IPTBW
                  IW(KROW(INDBIN,IPTR)+JNDBFW)= -1
               ENDIF
   11       CONTINUE
            IW(KCHA1+JTCHNC) = NHITS
            IW(KCHA1+JTCHOF) = IW(KTCTC+LMHROW)
            IW(KTCTC+LMHROW) = IW(KTCTC+LMHROW) + NHITS
   10    CONTINUE
C
   30 CONTINUE
C
C  Drop work banks
C
      CALL WDROP(IW,INDCXY)
      CALL WDROP(IW,INDLNK)
      CALL WDROP(IW,INDBIN)
C
C  Do small angle pattern recognition
C
C     small angle pattern recognition disabled IF
C     data card NTSA (No Track Small Angle) is present
C
       NNTSA=NAMIND('NTSA')
       IF(IW(NNTSA).EQ.0)CALL TSAPAT
C
C  Compress TCHA, TCTC banks
C
      CALL AUBPRS('TCHATCTC')
      GO TO 999
C
C Set error code and print error messages
C
  801 CONTINUE
      CALL RERROR('TRKFND',6,'Error booking work bank for sorting')
      IER=6
      GO TO 999
  800 CONTINUE
      IER=1
      CALL RERROR('TRKFND',1,'No TPCO or TCRL bank!')
      GO TO 999
  810 CONTINUE
      IER=2
      CALL RERROR('TRKFND',2,'Error booking link work bank')
      GOTO 999
  820 CONTINUE
      IER=3
      CALL RERROR('TRKFND',3,'Error booking INDCXY work bank')
      GOTO 999
  830 CONTINUE
      IER=4
      CALL RERROR('TRKFND',4,'Error lifting/extending TCHA bank')
  999 CONTINUE
C
      RETURN
      END
#endif
