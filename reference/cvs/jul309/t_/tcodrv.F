      SUBROUTINE TCODRV(IER)
C
C----------------------------------------------------------------------
C! Driver for TPC coordinate finding
C!
C!    Author:    R. Johnson 20-09-86
C!    Modified:  R. Johnson  8-04-88
C!
C!    Output:  IER              =0 if all is OK; =1 for space overflow
C!                                               =2 if input missing
C!
C!    Input banks:
C!             TCLU              TPC pad clusters
C!             TSPU,TSCL         TPC pad subpulses and subclusters
C!             TSRL              TPC subcluster row list
C!             THPL,THRP         TPD bad pad lists
C!    Output banks:
C!             TPCO,TCRL         TPC coordinates and row list
C!             TBCO              TPC bad coordinates
C!           { TSCL              Fill pointers to coordinates }
C!    Called by TCODRV
C!
C!    Description
C!    ===========
C!    This routine cycles through all subclusters in TSCL row by
C!    row (using the TSRL bank to point to row beginnings in
C!    TSCL) over all TPC sectors.  For each good cluster, a
C!    coordinate is calculated by TCOOR.   All coordinates for
C!    the entire TPC are filled into the TPCO bank and
C!    the bank TCRL is filled with pointers to the row beginnings
C!    in TPCO.  Also, TCOOR calculates a rough coordinate for each of
C!    those subclusters which fail, and it stores them in TBCO.  TCODRV
C!    also fills into TBCO a rough coordinate for each cluster which
C!    has no subcluster associated.
C!    NOTE:  This routine and the subroutines which it calls represent
C!           a module within JULIA.
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "rcurnt.h"
#include "tbcojj.h"
#include "tcrljj.h"
#include "tsrljj.h"
#include "tclujj.h"
#include "tpcojj.h"
#include "tspujj.h"
#include "tscljj.h"
#include "rlunit.h"
#include "bcs.h"
#include "tpatel.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpccon.h"
#include "tflags.h"
C
      COMMON/TCOOWB/ITSRLW,ITSCLW,ITSPUW,ITHRPW,ICLUSW,ITCLUW
C
C---------------------------------END COMMON---------------------------
C
      DIMENSION ISECR(21),NSECR(2),ISLST(24,2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C++   The following 3 data statements help the program loop over the
C++   sector slots which include a given padrow.
C
      DATA ISECR/9*1,12*2/
      DATA NSECR/12,24/
      DATA ISLST/ 1, 2, 3, 4, 5, 6,19,20,21,22,23,24,12*0,
     &            7, 8, 9,10,11,12,13,14,15,16,17,18,
     &           25,26,27,28,29,30,31,32,33,34,35,36/
C
C----------------------------------------------------------------------
C
#include "bmacro.h"
#include "tsfunc.h"
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        ITSRLW=0
        ITSCLW=0
        ITSPUW=0
        ITCLUW=0
        ICLUSW=0
        ITHRPW=0
        FIRST=.FALSE.
        NTCRL=NAMIND('TCRL')
        NTPCO=NAMIND('TPCO')
        NTBCO=NAMIND('TBCO')
        NTSCL=NAMIND('TSCL')
        NTCLU=NAMIND('TCLU')
        NTSPU=NAMIND('TSPU')
        NTSRL=NAMIND('TSRL')
      ENDIF
C
C++   Check that the input banks exist.
C
      IF (IW(NTSCL).EQ.0 .OR. IW(NTSPU).EQ.0
     &     .OR. IW(NTCLU).EQ.0 .OR. IW(NTSRL).EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
C++   Drop any output banks still lingering around
C
      IF (IW(NTPCO).NE.0) CALL BDROP(IW,'TPCO')
      IF (IW(NTCRL).NE.0) CALL BDROP(IW,'TCRL')
      IF (IW(NTBCO).NE.0) CALL BDROP(IW,'TBCO')
C
C++   Add up the total number of subclusters in the TPC
C
      NCLTO=0
      KTSCL=IW(NTSCL)
  301 IF (KTSCL.EQ.0) GO TO 302
        NCLTO=NCLTO+IW(KTSCL+LMHROW)
        KTSCL=IW(KTSCL-1)
        GO TO 301
  302 CONTINUE
      IF (NCLTO.EQ.0) GO TO 999
C
C++   Create coordinate bank.  MC tapes may already have a TPCO bank
C++   (from faked coordinates), so we must be sure to get rid of it.
C
      KTPCO=IW(NTPCO)
      IF (KTPCO.NE.0) CALL BDROP(IW,'TPCO')
      IW(1)=1
      CALL AUBOS('TPCO',3,LTPCOA*NCLTO+LMHLEN,KTPCO,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TPCO')
      IW(KTPCO+LMHCOL)=LTPCOA
      IW(KTPCO+LMHROW)=0
C
C++   Create coordinate row-list bank
C
      IW(1)=1
      CALL AUBOS('TCRL',0,LTCRLA*NTPROW+LMHLEN,KTCRL,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TCRL')
      IW(KTCRL+LMHCOL)=LTCRLA
      IW(KTCRL+LMHROW)=NTPROW
C
C++   Create a bank for bad coordinates
C
      IW(1)=1
      CALL AUBOS('TBCO',1,LTBCOA*NCLTO+LMHLEN,KTBCO,IGARB)
      IF (IGARB.EQ.2) GO TO 998
      CALL BLIST(IW,'T+','TBCO')
      IW(KTBCO+LMHCOL)=LTBCOA
      IW(KTBCO+LMHROW)=0
C
C++   Link to the cluster row list banks and subcluster banks
C
      CALL BWIND(IW,'TSRL',LTSECT,ITSRLW)
      CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
      CALL BWIND(IW,'TSPU',LTSECT,ITSPUW)
      CALL BWIND(IW,'TCLU',LTSECT,ITCLUW)
C
C++   Link to the TPD threshold banks (lists of bad channels)
C
      CALL BWIND(IW,'THRP',LTSECT,ITHRPW)
C
C++   Loop over rows in global system
C
      KTPCO=IW(NTPCO)
      KTCRL=IW(NTCRL)
      DO 300 IROWG=1,NTPROW
        IW(KROW(KTCRL,IROWG)+JTCRNC)=0
        IW(KROW(KTCRL,IROWG)+JTCRN1)=0
        IW(KROW(KTCRL,IROWG)+JTCROC)=IW(KTPCO+LMHROW)
C
C++     Loop over sectors relevant to this row
C
        INOUT=ISECR(IROWG)
        DO 200 JJ=1,NSECR(INOUT)
          ISLOT=ISLST(JJ,INOUT)
          IF (ISLOT.GT.IW(ITSCLW)) GO TO 201
          ISTYP=ITPTYP(ISLOT)
C
C++       Link to the BOS data for this sector
C
          KTSCL=IW(ITSCLW+ISLOT)
          IF (KTSCL.LE.0) GO TO 200
          KTSRL=IW(ITSRLW+ISLOT)
          KTSPU=IW(ITSPUW+ISLOT)
          KTCLU=IW(ITCLUW+ISLOT)
          IF (ISLOT.GT.IW(ITHRPW)) THEN
            KTHRP=0
          ELSE
            KTHRP=IW(ITHRPW+ISLOT)
          ENDIF
C
C++       Loop over all the subclusters in this sector and row
C
          IF (ISTYP.NE.1) THEN
            IROW=IROWG-NTPDRW(1)
          ELSE
            IROW=IROWG
          ENDIF
          DO 100 ICR=1,IW(KROW(KTSRL,IROW)+JTSRNS)
            IC=ICR+IW(KROW(KTSRL,IROW)+JTSROS)
C
C++         Call routine to calculate the coordinate
C
            CALL TCOOR(IROW,IC,KTCLU,KTSPU,KTSCL,KTHRP)
C
  100     CONTINUE
  200   CONTINUE
  201   CONTINUE
        IF (IW(KROW(KTCRL,IROWG)+JTCRNC).EQ.0) THEN
          IW(KROW(KTCRL,IROWG)+JTCROC)=0
        ENDIF
  300 CONTINUE
C
C++   Make bad coordinates for all clusters which have no subcluster
C
      MXCL=0
      KTCLU=IW(NTCLU)
  370 IF (KTCLU.EQ.0) GO TO 390
        MXCL=MAX(MXCL,LROWS(KTCLU))
        KTCLU=IW(KTCLU-1)
        GO TO 370
  390 CONTINUE
      IF (MXCL.GT.0) THEN
        IW(1)=1
        CALL WBANK(IW,ICLUSW,MXCL,*997)
        IW(ICLUSW-3)=INTCHA('WCLU')
        KTCLU=IW(NTCLU)
  410   IF (KTCLU.EQ.0) GO TO 470
C
C++       Mark those clusters which have subclusters associated
C
          CALL VZERO(IW(ICLUSW+1),LROWS(KTCLU))
          ISLOT=IW(KTCLU-2)
          ISTYP=ITPTYP(ISLOT)
          IF (ISLOT.LE.IW(ITSCLW)) THEN
            KTSCL=IW(ITSCLW+ISLOT)
            IF (KTSCL.NE.0) THEN
              DO 425 ISC=1,LROWS(KTSCL)
                ICL=ITABL(KTSCL,ISC,JTSCCL)
                IW(ICLUSW+ICL)=ISC
  425         CONTINUE
            ENDIF
          ENDIF
C
C++       Make one rough coordinate for each cluster with no associated
C++       subcluster, and add it to TBCO.
C
          DO 440 ICL=1,LROWS(KTCLU)
            IF (IW(ICLUSW+ICL).NE.0) GO TO 440
            NBCO=LROWS(KTBCO)+1
            LEN=NBCO*LTBCOA+LMHLEN
            IF (IW(KTBCO).LT.LEN) THEN
              LEN=LEN+9*LTBCOA
              IW(1)=1
              CALL AUBOS('TBCO',1,LEN,KTBCO,IGARB)
              IF (IGARB.EQ.2) GO TO 998
              IF (IGARB.EQ.1) THEN
                KTCLU=NLINK('TCLU',ISLOT)
                CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
              ENDIF
            ENDIF
            IW(KTBCO+LMHROW)=NBCO
            IPD1=ITABL(KTCLU,ICL,JTCLFP)
            IPD2=ITABL(KTCLU,ICL,JTCLLP)
            IBK1=ITABL(KTCLU,ICL,JTCLFB)
            IBK2=ITABL(KTCLU,ICL,JTCLLB)
            IPDCH=(IPD1+IPD2)/2
            IROW=ITABL(KTCLU,ICL,JTCLPN)
            IF (ISTYP.NE.1) THEN
              IROWG=IROW+NTPDRW(1)
            ELSE
              IROWG=IROW
            ENDIF
            RPHIP=FLOAT(IPD1+IPD2)/2.
            ZB=0.5*FLOAT(IBK1+IBK2)
            CALL TSCOOR(ISLOT,IROW,RPHIP,ZB,RS,RPHIS,ZS)
            IF (FTPC90) THEN
              R=RS
              RPHI=RPHIS
              Z=ZS
            ELSE
              CALL TCTGLB(ISLOT,RS,RPHIS,ZS,R,RPHI,Z)
            ENDIF
            IW(KROW(KTBCO,NBCO)+JTBCIN)=ISLOT*1000+IROWG*100000+IPDCH
            RW(KROW(KTBCO,NBCO)+JTBCRV)=R
            RW(KROW(KTBCO,NBCO)+JTBCPH)=RPHI/R
            RW(KROW(KTBCO,NBCO)+JTBCZV)=Z
            IW(KROW(KTBCO,NBCO)+JTBCRW)=IPD2-IPD1+1
            IW(KROW(KTBCO,NBCO)+JTBCZW)=IBK2-IBK1+1
            IW(KROW(KTBCO,NBCO)+JTBCSC)=-ICL
  440     CONTINUE
C
          KTCLU=IW(KTCLU-1)
          GO TO 410
  470   CONTINUE
      ENDIF
C
C++   Fix up the lengths of the banks.
C
      CALL AUBPRS('TCRLTPCOTBCO')
C
      IER=0
      GO TO 999
C
  997 CONTINUE
      CALL RERROR('TCODRV',2,'No space in BOS for work bank')
      IER=1
      GO TO 999
  998 CONTINUE
      CALL RERROR('TCODRV',1,'No space in BOS for named banks')
      IER=1
  999 CONTINUE
      CALL WDROP(IW,ICLUSW)
      CALL WDROP(IW,ITSRLW)
      CALL WDROP(IW,ITSCLW)
      CALL WDROP(IW,ITSPUW)
      CALL WDROP(IW,ITCLUW)
      RETURN
      END
#endif
