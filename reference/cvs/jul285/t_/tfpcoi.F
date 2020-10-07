      SUBROUTINE TFPCOI(IER)
C
C----------------------------------------------------------------------
C! Create bank PCOI with supplementary coordinate information
C
C  Author:  R. Johnson    12-12-89
C  Modified: D.Cinabro    31-05-90 to include info on 1/2 pads and length
C
C?
C!======================================================================
#ifndef DOC
C
#include "frtljj.h"
#include "tpcojj.h"
#include "pcoijj.h"
#include "tscljj.h"
#include "bcs.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tclujj.h"
C
      COMMON/TFPCWB/ ITSCLW,ITCLUW
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        ITSCLW=0
        ITCLUW=0
        NTSCL=NAMIND('TSCL')
        NFRTL=NAMIND('FRTL')
        NFTCL=NAMIND('FTCL')
        NPCOI=NAMIND('PCOI')
        NTPCO=NAMIND('TPCO')
        NTCLU=NAMIND('TCLU')
      ENDIF
C
      KTSCL=IW(NTSCL)
      IF (KTSCL.EQ.0) THEN
        IER=1
        GO TO 999
      ENDIF
      CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
      KTPCO=IW(NTPCO)
      IF (KTPCO.EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
      KFRTL=IW(NFRTL)
      IF (KFRTL.EQ.0) THEN
        IER=3
        GO TO 999
      ENDIF
      KFTCL=IW(NFTCL)
      IF (KFTCL.EQ.0) THEN
        IER=4
        GO TO 999
      ENDIF
      KTCLU = IW(NTCLU)
      IF (KTCLU.EQ.0) THEN
        IER=5
        GO TO 999
      ENDIF
      CALL BWIND(IW,'TCLU',LTSECT,ITCLUW)
C
      IF (IW(NPCOI).NE.0) CALL BDROP(IW,'PCOI')
      LEN=LROWS(KTPCO)*LPCOIA+LMHLEN
      IW(1)=1
      CALL AUBOS('PCOI',0,LEN,KPCOI,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=6
        GO TO 999
      ENDIF
      IF (IGARB.EQ.1) THEN
        KFRTL=IW(NFRTL)
        KFTCL=IW(NFTCL)
        KTPCO=IW(NTPCO)
        CALL BWIND(IW,'TCLU',LTSECT,ITCLUW)
        CALL BWIND(IW,'TSCL',LTSECT,ITSCLW)
      ENDIF
      IW(KPCOI+LMHCOL)=LPCOIA
C
C++   Loop over coordinates by order of the TPC tracks.  This is
C++   the same order as PTCO is filled in TPAKCO.
C
      NCO=0
      DO 100 ITK=1,LROWS(KFRTL)
        NC=ITABL(KFRTL,ITK,JFRTNT)+ITABL(KFRTL,ITK,JFRTNR)
        IF (NC.EQ.0) GO TO 100
        DO 50 II=1,NC
C
C++       Get the coordinate number in TPCO
C
          IC=IW(KFTCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIT)+II)
C
C++       Get the subcluster number
C
          ISC=ITABL(KTPCO,IC,JTPCCN)
          IF (ISC.EQ.0) THEN
            CALL BDROP(IW,'PCOI')
            IER=7
            RETURN
          ENDIF
C
C++       Link to the cluster banks for this sector
C
          ISLOT=MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
          IF (ISLOT.GT.IW(ITSCLW)) THEN
            CALL BDROP(IW,'PCOI')
            IER=8
            RETURN
          ENDIF
          IF (IW(ITSCLW).GE.ISLOT) THEN
            KTSCL=IW(ITSCLW+ISLOT)
          ELSE
            KTSCL=0
          ENDIF
          IF (KTSCL.EQ.0) THEN
            CALL BDROP(IW,'PCOI')
            IER=9
            RETURN
          ENDIF
          IF (IW(ITCLUW).GE.ISLOT) THEN
            KTCLU=IW(ITCLUW+ISLOT)
          ELSE
            KTCLU=0
          ENDIF
          IF (KTCLU.EQ.0) THEN
            CALL BDROP(IW,'PCOI')
            IER=10
            RETURN
          ENDIF
C
C++       Get the number of pads in the subcluster
C
          NSP=ITABL(KTSCL,ISC,JTSCNS)
C
C++       Get the number of coordinates from this subcluster
C
          NCOS=ITABL(KTSCL,ISC,JTSCNC)
C
C++       Get the total charge of the subcluster
C
          ITC=ITABL(KTSCL,ISC,JTSCTC)
C
C++       Number of half pads in this subcluster
C
          IND = ITABL(KTPCO,IC,JTPCIN)
          IROW = IND/100000
          ISTYP = ITPTYP(ISLOT)
          IF (ISTYP.EQ.1) THEN
            IROW=IROW
          ELSE
            IROW=IROW-NTPDRW(1)
          ENDIF
          KTS = KROW(KTSCL,ISC)
          CALL TNHLFP(ISLOT,IROW,KTS,NHALF)
C
C++       Length of the subcluster
C
          IF (KTCLU.NE.0) THEN
            MIN = ITABL(KTCLU,ITABL(KTSCL,ISC,JTSCCL),JTCLFB)
            MAX = ITABL(KTCLU,ITABL(KTSCL,ISC,JTSCCL),JTCLLB)
            LEN = MAX - MIN
          ELSE
            LEN = 0
          ENDIF
C
C++       Fill an entry into PCOI
C
          NCO=NCO+1
          IW(KROW(KPCOI,NCO)+JPCONP)=NSP
          IW(KROW(KPCOI,NCO)+JPCOPH)=ITC/10
          IF (NCOS.GT.1) THEN
            IW(KROW(KPCOI,NCO)+JPCOTF)=1
          ELSE
            IW(KROW(KPCOI,NCO)+JPCOTF)=0
          ENDIF
          IW(KROW(KPCOI,NCO)+JPCONH)=NHALF
          IW(KROW(KPCOI,NCO)+JPCOPL)=LEN
   50   CONTINUE
  100 CONTINUE
      IW(KPCOI+LMHROW)=NCO
      CALL AUBPRS('PCOI')
      IER=0
  999 CONTINUE
      CALL WDROP(IW,ITCLUW)
      CALL WDROP(IW,ITSCLW)
      END
#endif
