      SUBROUTINE TCPTST(IER)
C
C---------------------------------------------------------------------
C! Create bank for TPC reconstruction status
C
C  Author:  R. Johnson    20-5-91
C  Modified: W. Wiedenmann  3-2-92
C               handle new banks TE1R,TANF,TWNF,TSIR
C  Modified: G. Ganis      25-4-94
C               handle new bank TOER
C
C  Output:   IER        /I      Error code nonzero if BOS error occurs
C
C---------------------------------------------------------------------
#ifndef DOC
C
#include "te1rjj.h"
#include "tanfjj.h"
#include "twnfjj.h"
#include "terrjj.h"
#include "tinfjj.h"
#include "toerjj.h"
#include "tpgpar.h"
#include "ptstjj.h"
#include "bcs.h"
#include "tmonit.h"
C
      INTEGER TRUNC, ROERR, INVHT, FBERR, INVPT, RM
      LOGICAL FIRST,FWIR
      DATA FIRST/.TRUE./
C
C-----------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------
C
      IER=0
      IF (FIRST) THEN
        FIRST=.FALSE.
        NPTST=NAMIND('PTST')
        NTERR=NAMIND('TERR')
        NTINF=NAMIND('TINF')
        NTE1R=NAMIND('TE1R')
        NTANF=NAMIND('TANF')
        NTWNF=NAMIND('TWNF')
      ENDIF
C
C++   Create status bank for the POT.
C
      IW(1)=1
      LEN=LPTSTA*LTSECT+LMHLEN
      CALL AUBOS('PTST',0,LEN,KPTST,IGARB)
      IF (IGARB.EQ.2) THEN
        IER=1
      ELSE
        IW(KPTST+LMHCOL)=LPTSTA
        IW(KPTST+LMHROW)=LTSECT
        DO 497 I=1,LROWS(KPTST)
          IW(KROW(KPTST,I)+JPTSSP)=0
          IW(KROW(KPTST,I)+JPTSTW)=1
          IW(KROW(KPTST,I)+JPTSTP)=1
          IW(KROW(KPTST,I)+JPTSNT)=0
          IW(KROW(KPTST,I)+JPTSTE)=0
  497   CONTINUE
      ENDIF
C
C++   Keep counts of missing sectors
C
      DO 474 ISLOT=1,LTSECT
        KTPAD=NLINK('TPAD',ISLOT)
        KTSIR=NLINK('TSIR',ISLOT)
        KTRIR=NLINK('TRIR',ISLOT)
        KTWIR=NLINK('TWIR',ISLOT)
        IF (KTPAD.EQ.0 .AND.
     >      KTSIR.EQ.0 .AND. KTRIR.EQ.0 .AND. KTWIR.EQ.0) THEN
          NMISTM(ISLOT)=NMISTM(ISLOT)+1
        ELSE
          IF (KPTST.NE.0) THEN
            IW(KROW(KPTST,ISLOT)+JPTSSP)=1
          ENDIF
        ENDIF
  474 CONTINUE
C
      IF (KPTST.EQ.0) RETURN
C
C++   Keep track of DAQ errors from TERR
C
C---  This is the old TERR bank: valid up to <= 1991
      KTERR=IW(NTERR)
  500 IF (KTERR.EQ.0) GO TO 549
        IF (IW(KTERR).LT.3) THEN
          KTERR=IW(KTERR-1)
          GO TO 500
        ENDIF
        IF (IW(KTERR+LMHLEN+JTERTL).GT.0) THEN
          KTERR=IW(KTERR-1)
          GO TO 500
        ENDIF
        IADD=IW(KTERR+LMHLEN+JTERTN)
        CALL TPPADD(IADD,ISLOT,IERR)
        IF (IERR.NE.0) THEN
          KTERR=IW(KTERR-1)
          GO TO 500
        ENDIF
        IW(KROW(KPTST,ISLOT)+JPTSTE)=IW(KTERR+LMHLEN+JTERRM)
        KTERR=IW(KTERR-1)
        GO TO 500
  549 CONTINUE
C
C---  This is the new TE1R bank: valid from  >= 1992
      KTE1R=IW(NTE1R)
  560 IF (KTE1R.EQ.0) GO TO 599
        IF (IW(KTE1R).LT.3) THEN
          KTE1R=IW(KTE1R-1)
          GO TO 560
        ENDIF
        ISLOT=IW(KTE1R+LMHLEN+JTE1TN)
        IF (ISLOT.GT.LTSECT) ISLOT=ISLOT-LTSECT
        IW(KROW(KPTST,ISLOT)+JPTSTE)=IW(KTE1R+LMHLEN+JTE1RM)
        KTE1R=IW(KTE1R-1)
        GO TO 560
  599 CONTINUE
C
C---  This is the new TOER bank: valid from  >= 1994
      DO 610 ISLOT= 1, LTSECT
        KTOER= NLINK( 'TOER', ISLOT )
        IF( KTOER.EQ.0 ) GOTO 610
C---  Error occured ...
        IW(KROW(KPTST,ISLOT)+JPTSSP)= 1
C---  Get mask of truncated TPDs ...
        TRUNC= IW(KTOER+LMHLEN+JTOETM)
C---  Look for truncated PAD TPDs ...
        DO J=1,21
          IF( JBIT( TRUNC, J ).NE.0 )
     +      IW(KROW(KPTST,ISLOT)+JPTSTP)= 0
        END DO
C---  Look for truncated WIRE TPDs ...
        DO J=22,24
          IF( JBIT( TRUNC, J ).NE.0 )
     +      IW(KROW(KPTST,ISLOT)+JPTSTW)= 0
        END DO
C---  Save number of truncated bytes (=buckets in NON-read TPDs )
        IW(KROW(KPTST,ISLOT)+JPTSNT)=IW(KTOER+LMHLEN+JTOENT)
C---  Read-out mask error is the OR of remaining possible problems
        ROERR= IW(KTOER+LMHLEN+JTOERO)
        INVHT= IW(KTOER+LMHLEN+JTOEIH)
        FBERR= IW(KTOER+LMHLEN+JTOEFB)
        INVPT= IW(KTOER+LMHLEN+JTOEIP)
        RM= 0
        DO I=1,24
          IF( JBIT(ROERR,I).NE.0 ) CALL SBIT1(RM,I)
          IF( JBIT(INVHT,I).NE.0 ) CALL SBIT1(RM,I)
          IF( JBIT(FBERR,I).NE.0 ) CALL SBIT1(RM,I)
          IF( JBIT(INVPT,I).NE.0 ) CALL SBIT1(RM,I)
        END DO
        IW(KROW(KPTST,ISLOT)+JPTSTE)= RM
  610 CONTINUE
C
C
C++   Keep track of truncated data from TINF
C
C---  This is the old TINF bank: valid up to <= 1991
      KTINF=IW(NTINF)
  650 IF (KTINF.EQ.0) GO TO 660
        LENO=IW(KTINF+JTINOL)
        IF (LENO.NE.0) THEN
          NR=IW(KTINF-2)
          IF (NR.GT.LTSECT) THEN
            ISLOT=NR-LTSECT
            IW(KROW(KPTST,ISLOT)+JPTSTW)=0
          ELSE
            ISLOT=NR
            IW(KROW(KPTST,ISLOT)+JPTSTP)=0
          ENDIF
          IW(KROW(KPTST,ISLOT)+JPTSNT)=LENO-IW(KTINF+JTINEL)
        ENDIF
        KTINF=IW(KTINF-1)
        GO TO 650
  660 CONTINUE
C
C---  This is the new TANF/TWNF bank: valid from  >= 1992
      KTANF=IW(NTANF)
  670 IF (KTANF.EQ.0) GO TO 679
        LENO=IW(KTANF+JTANTW)
        IF (LENO.NE.0) THEN
          NR=IW(KTANF-2)
          ISLOT=NR
          IW(KROW(KPTST,ISLOT)+JPTSTP)=0
          IW(KROW(KPTST,ISLOT)+JPTSNT)=IW(KTANF+JTANTW)*4
        ENDIF
        KTANF=IW(KTANF-1)
        GO TO 670
  679 CONTINUE
C
      KTWNF=IW(NTWNF)
  680 IF (KTWNF.EQ.0) GO TO 689
        LENO=IW(KTWNF+JTWNTW)
        IF (LENO.NE.0) THEN
          NR=IW(KTWNF-2)
          ISLOT=NR
          IW(KROW(KPTST,ISLOT)+JPTSTW)=0
          IW(KROW(KPTST,ISLOT)+JPTSNT)=IW(KTWNF+JTWNTW)*4
        ENDIF
        KTWNF=IW(KTWNF-1)
        GO TO 680
  689 CONTINUE
C
      END
#endif
