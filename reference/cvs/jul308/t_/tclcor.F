      SUBROUTINE TCLCOR(KTPAD,KTPDI,KTPUL,KTCLU,KTLRL,ISLOT,JGARB,IER)
C
C-----------------------------------------------------------------------
C! Analyze pulses and clusters
C!
C!    Author:    R. Johnson    22-12-86
C!    Modified:  R. Johnson    19-01-88
C!
C!    Input:
C!         - KTPAD     /I     Pointer to raw data hit bank
C!         - KTPDI     /I     Pointer to raw data sample bank
C!         - KTPUL     /I     Pointer to bank of pulses
C!         - KTCLU     /I     Pointer to bank of islands (clusters)
C!         - KTLRL     /I     Pointer to cluster rowlist bank
C!         - ISLOT     /I     Sector number to be analyzed
C!    Output:
C!         - KTPAD,KTPDI,KTPUL,KTCLU are updated if garbage collection
C!                                   is done.
C!         - JGARB     /I     Set to 1 if garbage collection is done
C!                            Set to 0 otherwise
C!         - IER       /I     Set to 1 if no room in BOS for name banks
C!                            Set to 2 if no room in BOS for work banks
C!                            Otherwise it is set to zero.
C!
C!    Called from TPADS
C!
C!    Description
C!    -----------
C!    This is the driver routine for TPC cluster analysis.  It is called
C!    once for each sector.  It creates the named banks TSCL and TSPU
C!    for output and some workbanks to be used by this module.  Then it
C!    loops over all clusters in TCLU and unpacks the raw data hits and
C!    samples, one cluster at a time, and calls TCLANA to analyze each
C!    cluster.  The rowlist bank TSRL also is filled at this time.
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tpuljj.h"
#include "tclujj.h"
#include "tcrljj.h"
#include "tlrljj.h"
#include "tscljj.h"
#include "tsrljj.h"
#include "bcs.h"
#include "tpccon.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tcluct.h"
#include "tspujj.h"
#include "tclcow.h"
C
C--------------------- Local variables ---------------------------------
C
      LOGICAL LGARB,FIRST
      CHARACTER TEXT*190
      DATA FIRST/.TRUE./
C
C--------------------- Statement function definitions -----------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C-----------------------------------------------------------------------
C
C++   Zero workbank indices on the first call
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        ITPADW=0
        ITPDIW=0
        ITSPUW=0
        ISCLUS=0
        ITPRTW=0
      ENDIF
      JGARB=0
C
C++   Store the sector slot number in local common TCLCOW
C
      ISLTCL=ISLOT
C
C++   Get the pedistal value for this sector
C
      IPDST=JPEDST(ISLOT,1)
      LVLSAT= 255 - IPDST
C
C++   Create work banks.
C
      LEN=LMHLEN+LWTPAD*MXPLCL
      MXPL=MXPLCL
      IW(1)=1
      CALL WBANK(IW,ITPADW,LEN,*997)
      IW(ITPADW+LMHCOL)=LWTPAD
      IW(ITPADW-3)=INTCHA('WTPA')
C
      MXSP=MXSPCL
      IW(1)=1
      CALL WBANK(IW,ITPDIW,MXSPCL,*997)
      IW(ITPDIW-3)=INTCHA('WTPD')
C
      LEN=LENSPW*MXSBCL+LMHLEN
      IW(1)=1
      CALL WBANK(IW,ITSPUW,LEN,*997)
      IW(ITSPUW+LMHCOL)=LENSPW
      IW(ITSPUW-3)=INTCHA('WTSP')
C
      LEN=LENICL*(MXSBCL/2)+LMHLEN
      IW(1)=1
      CALL WBANK(IW,ISCLUS,LEN,*997)
      IW(ISCLUS+LMHCOL)=LENICL
      IW(ISCLUS-3)=INTCHA('WSCL')
C
      IW(1)=1
      CALL WBANK(IW,ITPRTW,MXSBCL,*997)
      IW(ITPRTW-3)=INTCHA('WTPR')
C
C++   Create subcluster row list bank for this sector
C
      ISTYP=ITPTYP(ISLOT)
      LEN=LTSRLA*NTPDRW(ISTYP)+LMHLEN
      IW(1)=1
      CALL AUBOS('TSRL',ISLOT,LEN,KTSRL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TSRL')
      LGARB=IGARB.NE.0
      CALL VZERO(IW(KTSRL+1),LEN)
      IW(KTSRL+LMHCOL)=LTSRLA
      IW(KTSRL+LMHROW)=NTPDRW(ISTYP)
C
C++   Create subpulse and subcluster banks
C
      MXSPU=MAX(50,IW(KTPUL+LMHROW) + IW(KTPUL+LMHROW)/2)
      LEN=LMHLEN+LTSPUA*MXSPU
      IW(1)=1
      CALL AUBOS('TSPU',ISLOT,LEN,KTSPU,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      LGARB=LGARB .OR. IGARB.NE.0
      CALL BLIST(IW,'T+','TSPU')
      IW(KTSPU+LMHCOL)=LTSPUA
      IW(KTSPU+LMHROW)=0
C
      MXSCL=MAX(25,2*IW(KTCLU+LMHROW))
      LEN=LMHLEN+LTSCLA*MXSCL
      IW(1)=1
      CALL AUBOS('TSCL',ISLOT,LEN,KTSCL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TSCL')
      LGARB=LGARB .OR. IGARB.NE.0
      IF (LGARB) THEN
        KTSPU=NLINK('TSPU',ISLOT)
        KTPAD=NLINK('TPAD',ISLOT)
        KTPDI=NLINK('TPDI',ISLOT)
        KTPUL=NLINK('TPUL',ISLOT)
        KTCLU=NLINK('TCLU',ISLOT)
        KTLRL=NLINK('TLRL',ISLOT)
        KTSRL=NLINK('TSRL',ISLOT)
        JGARB=1
      ENDIF
      IW(KTSCL+LMHCOL)=LTSCLA
      IW(KTSCL+LMHROW)=0
C
C++   Loop over rows in this sector
C
      DO 329 IROW=1,NTPDRW(ISTYP)
        NCLST=IW(KTSCL+LMHROW)
C
C++     Fill subcluster rowlist bank
C
        IW(KROW(KTSRL,IROW)+JTSROS)=NCLST
C
C++     Loop over clusters in this row
C
        DO 320 ICR=1,NRCLU(IROW)
          IC=ICSEC(ICR,IROW)
C
C++       Cut on width and length of cluster
C
          NPADSW=IABS(ITABL(KTCLU,IC,JTCLLP)
     &                             - ITABL(KTCLU,IC,JTCLFP))+1
          IF (NPADSW.LT.ITLCNP) GO TO 320
          IF (NPADSW.GT.ITLCXP) GO TO 320
C
C++       If the simple cluster analysis is required, then we
C++       reject clusters with more than one pulse per pad.
C
          IF (FTLCQK) THEN
            IF (NPADSW.NE.ITABL(KTCLU,IC,JTCLNP)) GO TO 320
          ENDIF
C
          JFSTBK=ITABL(KTCLU,IC,JTCLFB)
          NBUCK=ITABL(KTCLU,IC,JTCLLB)-JFSTBK+1
          IF (NBUCK.LT.ITLCIB) GO TO 320
          IF (NBUCK.GT.ITLCAB) GO TO 320
C
C++       Check length of pulse work bank
C
          NPLS=ITABL(KTCLU,IC,JTCLNP)
          IF (NPLS.GT.MXPL) THEN
            MXPL=NPLS
            LEN=LMHLEN+LWTPAD*MXPL
            IW(1)=1
            CALL WBANK(IW,ITPADW,LEN,*997)
          ENDIF
C
C++       Find whether pads are in order or in reverse order
C
          IPFST=1+ITABL(KTCLU,IC,JTCLOP)
          JPFST=IBITS(IW(KTPAD+ITABL(KTPUL,IPFST,JTPUOP)+1),24,8)
          IF (JPFST.EQ.ITABL(KTCLU,IC,JTCLFP)) THEN
            ISGN=1
            IPD=0
          ELSE
            ISGN=-1
            IPD=NPADSW+1
          ENDIF
C
C++       Temporarily use IPIDXW to point from pads to TPUL pulses
C
          IPDLS=-1
          DO 912 IP=1,NPLS
            IPL=IP+ITABL(KTCLU,IC,JTCLOP)
            IHIT=IW(KTPAD+ITABL(KTPUL,IPL,JTPUOP)+1)
            JPDNR=IBITS(IHIT,24,8)
            IF (JPDNR.NE.IPDLS) THEN
              IPDLS=JPDNR
              IPD=IPD+ISGN
              IPIDXW(JIPDOP,IPD)= IPL-1
              IPIDXW(JIPDNP,IPD)= 0
              IPIDXW(JIPDOU,IPD)= JPDNR
            ENDIF
            IPIDXW(JIPDNP,IPD)=IPIDXW(JIPDNP,IPD)+1
  912     CONTINUE
C
C++       Loop over and unpack the pulses into ITPADW in order
C++       of increasing pad number
C
          ISPOF=0
          IW(ITPADW+LMHROW)=0
          DO 330 IPD=1,NPADSW
            IELST=0
            IT0LS=-1
            NPOP=IPIDXW(JIPDNP,IPD)
            IPOF=IPIDXW(JIPDOP,IPD)
            JPDNR=IPIDXW(JIPDOU,IPD)
            IPIDXW(JIPDNP,IPD)=0
            IPIDXW(JIPDOP,IPD)=LROWS(ITPADW)
            DO 645 IP=1,NPOP
              IPL=IP+IPOF
              IHIT=IW(KTPAD+ITABL(KTPUL,IPL,JTPUOP)+1)
              IT0= IBITS(IHIT,0,9)
C
C++           For this algorithm to work, the pulses on a given pad
C++           must be in order of increasing T0
C
              IF (IT0.LT.IT0LS) THEN
                WRITE(TEXT,281) ISLOT,IROW,IC,JPDNR
  281           FORMAT('Sector ',I2,' row ',I2,' cluster ',I4,
     &                 ' PAD ',I3,'&PULSES OUT OF ORDER IN TIME.')
                CALL RERROR('TCLCOR',5,TEXT(1:66))
              ENDIF
              IT0LS=IT0
              NSMP=IBITS(IHIT,16,8)
              IE=IT0+NSMP-1
C
C++           Combine overlapping pulses.  Note that this causes the
C++           pointer from TSPU to TPUL not to be correct in some
C++           cases (it will point to the TPUL entry just before the
C++           correct one)
C
              IF (IT0.LE.IELST) THEN
                NSKIP=IELST-IT0+1
                II=LROWS(ITPADW)
                NSMP=NSMP-NSKIP
                IW(KROW(ITPADW,II)+JTPDNS)=ITABL(ITPADW,II,JTPDNS)
     &                                                        + NSMP
                IS=ITABL(KTPUL,IPL,JTPUOS)+NSKIP+1
              ELSE
                IPIDXW(JIPDNP,IPD)=IPIDXW(JIPDNP,IPD)+1
                IW(ITPADW+LMHROW)=LROWS(ITPADW)+1
                II=LROWS(ITPADW)
                IW(KROW(ITPADW,II)+JTPDT0)=IT0
                IW(KROW(ITPADW,II)+JTPDNS)=NSMP
                IW(KROW(ITPADW,II)+JTPDOF)=ISPOF
                IW(KROW(ITPADW,II)+JTPDPN)=JPDNR
                IW(KROW(ITPADW,II)+JTPDTP)=IPL
                IS=ITABL(KTPUL,IPL,JTPUOS)+1
              ENDIF
              IELST=IE
C
C++           Check length of sample work bank
C
              IF (ISPOF+NSMP .GT. MXSP) THEN
                MXSP=MAX(ISPOF+NSMP,MXSP+MXSPCL)
                IW(1)=1
                CALL WBANK(IW,ITPDIW,MXSP,*997)
              ENDIF
C
C++           Loop over the samples and unpack them and subtract pedista
C
              IWORD=KTPDI+1+(IS-1)/4
              IBIT0=24-8*MOD(IS-1,4)
              DO 325 L=1,NSMP
                JSPH=IBITS(IW(IWORD),IBIT0,8) - IPDST
                ISPOF=ISPOF+1
                IW(ITPDIW+ISPOF)=JSPH
                IBIT0=IBIT0-8
                IF (IBIT0.LT.0) THEN
                  IWORD=IWORD+1
                  IBIT0=24
                ENDIF
  325         CONTINUE
  645       CONTINUE
  330     CONTINUE
C
C++       Is the cluster at large or small dip angle?
C
          IF (ISTYP.NE.1) THEN
            IROWG=IROW+NTPDRW(1)
          ELSE
            IROWG=IROW
          ENDIF
          JFSTB=ITABL(KTCLU,IC,JTCLFB)
          Z0= ZTPCMX-(FLOAT(JFSTB)-TVOFFS(ISLOT))
     &                             *TVDRFT(IENDTP(ISLOT))
          TANL= Z0/TPPROW(IROWG)
          JWNDOW=INT(RTLCZW(1)+TANL*RTLCZW(2))
          IF (TANL.LT.RTLCTL) THEN
            JTANLM=1
          ELSE
            JTANLM=2
          ENDIF
C
C++       Set the cut for definition of a maximum in TPLANA
C
          COST=TANL/SQRT(1.+TANL**2)
          JPLMXD=INT(10.*(RTLCDM(1)
     &                  + EXP((COST-RTLCDM(2))/RTLCDM(3))))
C
C++       Call routine to analyze the pulses and cluster
C
          IF (FTLCQK) THEN
            CALL TCLSNG
          ELSE
            CALL TCLANA
          ENDIF
C
C++       Check space within the named banks and extend if necessary
C
          NTSPU= LROWS(ITSPUW)
          MXSBP= MXSPU-IW(KTSPU+LMHROW)
          IF (MXSBP.LE.NTSPU) THEN
            WRITE(TEXT,331) IC,IROW,ISLOT
  331       FORMAT('TSPU bank extended at cluster ',I3,' of row ',
     &             I2,' IN SECTOR ',I2,'&IF THIS OCCURS TOO OFTEN,',
     &             ' then increase allocation in this routine.')
            CALL RERROR('TCLCOR',1,TEXT(1:130))
            MXSPU= MXSPU + 2*NTSPU
            LEN= LMHLEN + LTSPUA*MXSPU
            IW(1)=1
            CALL AUBOS('TSPU',ISLOT,LEN,KTSPU,IGARB)
            IF (IGARB.EQ.2) GO TO 998
            IF (IGARB.EQ.1) THEN
              KTSCL=NLINK('TSCL',ISLOT)
              KTPAD=NLINK('TPAD',ISLOT)
              KTPDI=NLINK('TPDI',ISLOT)
              KTPUL=NLINK('TPUL',ISLOT)
              KTCLU=NLINK('TCLU',ISLOT)
              KTLRL=NLINK('TLRL',ISLOT)
              KTSRL=NLINK('TSRL',ISLOT)
              JGARB=1
            ENDIF
          ENDIF
          MXSBC=MXSCL-IW(KTSCL+LMHROW)
          IF (MXSBC.LE.LROWS(ISCLUS)) THEN
            WRITE(TEXT,332) IC,IROW,ISLOT
            CALL RERROR('TCLCOR',2,TEXT(1:130))
  332       FORMAT('TSCL bank extended at cluster ',I3,' of row ',
     &             I2,' IN SECTOR ',I2,'&IF THIS OCCURS TOO OFTEN,',
     &             ' then increase allocation in this routine.')
            MXSCL= MXSCL + 2*LROWS(ISCLUS)
            LEN= LMHLEN + LTSCLA*MXSCL
            IW(1)=1
            CALL AUBOS('TSCL',ISLOT,LEN,KTSCL,IGARB)
            IF (IGARB.EQ.2) GO TO 998
            IF (IGARB.EQ.1) THEN
              KTSPU=NLINK('TSPU',ISLOT)
              KTPAD=NLINK('TPAD',ISLOT)
              KTPDI=NLINK('TPDI',ISLOT)
              KTPUL=NLINK('TPUL',ISLOT)
              KTCLU=NLINK('TCLU',ISLOT)
              KTLRL=NLINK('TLRL',ISLOT)
              KTSRL=NLINK('TSRL',ISLOT)
              JGARB=1
            ENDIF
          ENDIF
C
C++       Call routine to fill in the TSCL and TSPU banks
C
          CALL TSCFIL(IC,KTSCL,KTSPU)
C
  320   CONTINUE
C
C++     Complete the subcluster row list information
C
        IW(KROW(KTSRL,IROW)+JTSRNS)=IW(KTSCL+LMHROW)-NCLST
  329 CONTINUE
C
C++   Fix lengths of named banks
C
      IF (IW(KTSCL+LMHROW).GT.0) THEN
        LEN=IW(KTSCL+LMHROW)*IW(KTSCL+LMHCOL)+LMHLEN
        CALL AUBOS('TSCL',ISLOT,LEN,KTSCL,IGARB)
        IF (IGARB.NE.0) JGARB=1
      ELSE
        KTSCL=NDROP('TSCL',ISLOT)
      ENDIF
      IF (IW(KTSPU+LMHROW).GT.0) THEN
        LEN=IW(KTSPU+LMHROW)*IW(KTSPU+LMHCOL)+LMHLEN
        CALL AUBOS('TSPU',ISLOT,LEN,KTSPU,IGARB)
        IF (IGARB.NE.0) JGARB=1
      ELSE
        KTSPU=NDROP('TSPU',ISLOT)
      ENDIF
C
      GO TO 999
C
  997 CONTINUE
      IER=3
      CALL RERROR('TCLCOR',3,'No space available for work banks')
      GO TO 999
  998 CONTINUE
      IER=4
      CALL RERROR('TCLCOR',4,'No space available for named banks')
      JGARB=2
  999 CONTINUE
C
C++   Drop work banks
C
      CALL WDROP(IW,ITPADW)
      CALL WDROP(IW,ITPDIW)
      CALL WDROP(IW,ITSPUW)
      CALL WDROP(IW,ISCLUS)
      CALL WDROP(IW,ITPRTW)
      RETURN
      END
#endif
