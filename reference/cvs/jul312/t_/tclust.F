      SUBROUTINE TCLUST(ISECT,NSECL,KTPAD,KTPDI,KTPRL,KTLRL,KTISL,
     &                                      KTCLU,KTPUL,JGARB,IER)
C
C---------------------------------------------------------------------
C! Store TPC clusters and pulses
C!
C!      Author:    D. Schlatter
C!      Modified:  R. Johnson    6-05-87
C!
C!      Input:
C!         - ISECT    /I   Sector number to be analyzed
C!         - NSECL    /I   Upper limit on number of clusters in sector
C!         - KTPAD    /I   BOS offset to 'TPAD' bank
C!         - KTPDI    /I   BOS offset to 'TPDI' bank
C!         - KTPRL    /I   BOS offset to 'TPRL' bank
C!         - KTLRL    /I   BOS offset to 'TLRL' bank
C!         - KTISL    /I   BOS offset to 'TISL' bank
C!      Output:
C!         - KTPAD, etc.   updated if garbage collection done
C!         - KTCLU    /I   BOS offset to newly created cluster bank
C!         - KTPUL    /I   BOS offset to newly created pulse bank
C!         - JGARB    /I   =1 if garbage collection done, =0 otherwise
C!         - IER      /I   =1 if BOS runs out of space, =0 if all OK
C!                         =2 if sorting array overflows
C!
C!      Called by TPADS
C!
C!      Description
C!      ===========
C!      This routine decodes the cluster information stored by the TPP
C!      (or TISLND) in the TISL bank.  The TCLU bank is formed and
C!      filled with the cluster information, and the TPUL bank is
C!      filled with pointers into the raw data.  TPUL also will
C!      contain the z charge estimate and z position of each pulse.
C!      The pulses for each cluster are sorted into order of increasing
C!      or decreasing pad number.
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
C----------------------- Common Blocks and Parameters ------------------
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tclujj.h"
#include "tcrljj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "tprljj.h"
#include "bcs.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tcluct.h"
C
#include "tpwork.h"
      COMMON/TCLUWB/IHITTP,JSORT5
C
C----------------------- Local Variables -------------------------------
C
      LOGICAL LGARB,LFRST
      DATA LFRST/.TRUE./
C
C     The following is a description of a work bank used internally:
C
C     -----------------------------------------------------------
C     | +--------+                                              |
C     | | IHITTP |  1         # entries per hit  (=2)           |
C     | +--------+  2         # of hits                         |
C     |                                                         |
C     |---------------------------------------------------------|
C     |      For each hit in TPAD for the row in question:      |
C     |                                                         |
C     |            1     I   position of hit in TPUL            |
C     |            2     I   cluster number of the hit          |
C     -----------------------------------------------------------
C
C----------------------- Function Definitions --------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C-----------------------------------------------------------------------
C
      JGARB=0
      IF (LFRST) THEN
        LFRST=.FALSE.
        IHITTP=0
        JSORT5=0
C
C++     Create a workbank for sorting the pulses by pad number
C
        MXSRT=LMXPDR
        LEN=3*MXSRT
        IW(1)=1
        CALL WBANK(IW,JSORT5,LEN,*997)
        IW(JSORT5-3)=INTCHA('WSOR')
      ENDIF
      IER=0
      ISTYP=ITPTYP(ISECT)
C
C++   Get the total number of pulses in this sector
C
      NSEPU=0
      DO 320 IROW=1,IW(KTPRL+LMHROW)
        NSEPU=NSEPU+IW(KROW(KTPRL,IROW)+JTPRNP)
  320 CONTINUE
C
C++   Create pulse list bank for this sector
C
      IW(1)=1
      CALL AUBOS('TPUL',ISECT,LTPULA*NSEPU+LMHLEN,KTPUL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TPUL')
      LGARB=IGARB.NE.0
      IW(KTPUL+LMHCOL)=LTPULA
      IW(KTPUL+LMHROW)=0
C
C++   Create cluster row list bank for this sector
C
      LEN=LTLRLA*NTPDRW(ISTYP)+LMHLEN
      IW(1)=1
      CALL AUBOS('TLRL',ISECT,LEN,KTLRL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TLRL')
      LGARB=LGARB .OR. IGARB.NE.0
      CALL VZERO(IW(KTLRL+1),LEN)
      IW(KTLRL+LMHCOL)=LTLRLA
      IW(KTLRL+LMHROW)=NTPDRW(ISTYP)
C
C++   Create cluster bank for this sector
C
      IW(1)=1
      CALL AUBOS('TCLU',ISECT,LTCLUA*NSECL+LMHLEN,KTCLU,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TCLU')
      LGARB=LGARB .OR. IGARB.NE.0
      IW(KTCLU+LMHCOL)=LTCLUA
      IW(KTCLU+LMHROW)=0
      IF (LGARB) THEN
        KTPAD=NLINK('TPAD',ISECT)
        KTPDI=NLINK('TPDI',ISECT)
        KTPRL=NLINK('TPRL',ISECT)
        KTISL=NLINK('TISL',ISECT)
        KTPUL=NLINK('TPUL',ISECT)
        KTLRL=NLINK('TLRL',ISECT)
        JGARB=1
      ENDIF
C
C++   Create work bank for temporary storage of pointers and samples
C
      LHITS=LTHIDI
      LEN1=2*LHITS+LMHLEN
      IW(1)=1
      CALL WBANK(IW,IHITTP,LEN1,*997)
      IW(IHITTP+LMHCOL)=2
      IW(IHITTP-3)=INTCHA('WHIT')
C
C++   Loop over the sector rows
C
      IOFI=0
      IPU=0
   10 IF (IOFI.GE.IW(KTISL)) GO TO 50
        IOFI=IOFI+1
        IROW=IBITS(IW(KTISL+IOFI),16,16)
        NCLUS=IBITS(IW(KTISL+IOFI),0,16)
        IOFI=IOFI+1
C
C++     Find where the row begins in TPAD and # of hits
C
        IOFRB=IW(KROW(KTPRL,IROW)+JTPROP)
        NHITR=IW(KROW(KTPRL,IROW)+JTPRNP)
C
C++     Extend the work bank if we are going to run out of space
C
        IF (NHITR.GT.LHITS) THEN
          LHITS=NHITR
          LEN1=2*LHITS+LMHLEN
          IW(1)=1
          CALL WBANK(IW,IHITTP,LEN1,*997)
        ENDIF
C
C++     Fill in offset to row beginning in TCLU into TLRL
C
        IW(KROW(KTLRL,IROW)+JTLROC)=IW(KTCLU+LMHROW)
C
C++     Loop over the clusters (islands)
C
        DO 40 IC=1,NCLUS
          IOFI=IOFI+1
          NP=IBITS(IW(KTISL+IOFI),0,16)
C
C++       Extend the sorting workbank if necessary
C
          IF (NP.GT.MXSRT) THEN
            MXSRT=NP+LMXPCL
            LEN=3*MXSRT
            IW(1)=1
            CALL WBANK(IW,JSORT5,LEN,*997)
          ENDIF
C
C++       Open a new cluster in TCLU
C
          IW(KTCLU+LMHROW)=IW(KTCLU+LMHROW)+1
          ICL=IW(KTCLU+LMHROW)
          IW(KROW(KTCLU,ICL)+JTCLOP)=IPU
          IW(KROW(KTCLU,ICL)+JTCLNP)=NP
          IW(KROW(KTCLU,ICL)+JTCLPN)=IROW
C
C++       Loop over pulses to find min,max sample and so forth. . .
C
          MINBN=999999
          MAXBN=0
          MINPN=999999
          MAXPN=0
          DO 25 IP=1,NP
            IW(JSORT5+IP)=IP
            IOFTP=IW(KTISL+IOFI+IP)
            IPDNR=IBITS(IW(KTPAD+IOFTP),24,8)
            IW(JSORT5+MXSRT+IP)=IPDNR
            IT0=IBITS(IW(KTPAD+IOFTP),0,9)
            IW(JSORT5+2*MXSRT+IP)=IT0
            NSMP=IBITS(IW(KTPAD+IOFTP),16,8)
            MINBN=MIN0(MINBN,IT0)
            MAXBN=MAX0(MAXBN,IT0+NSMP-1)
            MINPN=MIN0(MINPN,IPDNR)
            MAXPN=MAX0(MAXPN,IPDNR)
   25     CONTINUE
          IW(KROW(KTCLU,ICL)+JTCLFB)=MINBN
          IW(KROW(KTCLU,ICL)+JTCLLB)=MAXBN
          IW(KROW(KTCLU,ICL)+JTCLFP)=MINPN
          IW(KROW(KTCLU,ICL)+JTCLLP)=MAXPN
C
C++       Sort the pulses by pad number.  A simple ripple sort is
C++       used, since the order usually will be close to correct.
C++       Multiple pulses on a single pad are sorted also by time.
C
          CALL TPSORT(IW(JSORT5+MXSRT+1),IW(JSORT5+2*MXSRT+1),
     &                            IW(JSORT5+1),NP)
C
C++       Set up the pointers to TCLU and TPUL for each pulse in row
C
          DO 30 IP=1,NP
            IPU=IPU+1
            IDXSR=IW(JSORT5+IP)
            IOFR=IW(KTISL+IOFI+IDXSR)-IOFRB
            IW(KROW(IHITTP,IOFR)+1)=IPU
            IW(KROW(IHITTP,IOFR)+2)=ICL
   30     CONTINUE
          IOFI=IOFI+NP
   40   CONTINUE
        IW(IHITTP+LMHROW)=IPU
C
C++     Now loop over all pulses in this row and fill TPUL
C
        NSTOT=IW(KROW(KTPRL,IROW)+JTPROS)
        DO 305 IP=1,NHITR
          IW(KTPUL+LMHROW)=IW(KTPUL+LMHROW)+1
          IC=IW(KROW(IHITTP,IP)+2)
          IH=IW(KROW(IHITTP,IP)+1)
          MSAMP=IBITS(IW(KTPAD+IOFRB+IP),16,8)
          IPDTI=IBITS(IW(KTPAD+IOFRB+IP),0,9)
          IPADN=IBITS(IW(KTPAD+IOFRB+IP),24,8)
C
C++       Store offset to pulse in TPAD and store number of samples
C
          IW(KROW(KTPUL,IH)+JTPUOP)=IOFRB+IP-1
          IW(KROW(KTPUL,IH)+JTPUNS)=MSAMP
C
C++       Store offset of 1st sample (IN BYTES !!!) into TPDI
C
          IW(KROW(KTPUL,IH)+JTPUOS)=NSTOT
C
C++       Store pointer to the cluster
C
          IW(KROW(KTPUL,IH)+JTPUCP)=IC
          NSTOT=NSTOT+MSAMP
  305   CONTINUE
C
C++     Fill in # clusters into cluster row list (TLRL) for this row
C
        IW(KROW(KTLRL,IROW)+JTLRNC)=NCLUS
C
C++     Here we are finished with this row, so go to the next row.
C
        GO TO 10
   50 CONTINUE
C
C++   Set the length of the TCLU bank.
C
      LEN=IW(KTCLU+LMHCOL)*IW(KTCLU+LMHROW)+LMHLEN
      IF (LEN .LT. IW(KTCLU)-4) THEN
        CALL AUBOS('TCLU',ISECT,LEN,KTCLU,IGARB)
        IF (IGARB.NE.0) JGARB=1
      ENDIF
      GO TO 999
C
  997 CONTINUE
      CALL RERROR('TCLUST',3,'No space for work bank')
      IER=3
      GO TO 999
  998 CONTINUE
      CALL RERROR('TCLUST',1,'No space for named bank')
      JGARB=2
      IER=1
  999 CONTINUE
      CALL WDROP(IW,IHITTP)
      RETURN
      END
#endif
