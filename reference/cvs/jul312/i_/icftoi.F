      SUBROUTINE ICFTOI(IDEB,IER)
C-----------------------------------------------------------------------
C! Convert FRFT type banks to ITFT type track banks.
C!
C! Author  : J.Sedgbeer  01/02/92
C! Modified :- E. Lancon             14-FEB-1992
C!       Set NTMIN to 0, so that the number of tracks before and after
C!       is identical
C!
C!   Input:
C!     IDEB  /I   : Debug flag; =0 for none, .GT.0 for lots.
C!     commons    : /BCS/    for BOS banks
C!     params     :  FRFTJJ,FRTLJJ,FICLJJ,FTCLJJ
C!                   ITFTJJ,IGTLJJ,IGCLJJ,TGTLJJ,TGCLJJ
C!
C!   Output:
C!     IER   /I   : Error flag = 0  O.K.
C!                             = 1  Missing FRFT and/or FRTL.
C!                             = 2  Missing FICL and FTCL
C!                                  (must have one or both of these)
C!                             = 3  Inconsistent banks.
C!                             = 4  Not enough BOS space.
C!     BOS banks  : ITFT,IGTL,IGCL,TGTL,TGCL numbers 1 and 2
C!
C! Given banks FRFT, FRTL, FICL and FTCL this routine makes
C! the banks ITFT, IGTL, IGCL, TGTL and TGCL (numbers 1 and 2)
C! The TPC-ITC tracks from FRFT are put into ITFT bank number 1
C! and the other tracks are put into ITFT number 2. The coord.
C! lists in FRTL,FICL and FTCL are similarly split into IGTL,IGCL,
C! TGTL and TGCL numbers 1 and 2.
C! The split between TPC-ITC tracks and the rest is based on the
C! TPC-ITC tracks always having at least NTMIN=3 TPC hits
C! and appearing first in the list of tracks in FRFT, i.e. before
C! the other tracks.
C!
C! ***  N.B. ***
C! Any VDET info. is ignored, i.e. VDET info. in FRTL
C! and FVCL is not used to make VGTL and VGCL.
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ficljj.h"
#include "ftcljj.h"
#include "itftjj.h"
#include "igcljj.h"
#include "igtljj.h"
#include "tgcljj.h"
#include "tgtljj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA LPR/6/
C-----------------------------------------------------------------------
C Min. no. of TPC hits for TPC-ITC track.
      DATA NTMIN/0/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise.
      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRFT = NAMIND('FRFT')
        NFRTL = NAMIND('FRTL')
        NFICL = NAMIND('FICL')
        NFTCL = NAMIND('FTCL')
      ENDIF
C
C Check for banks FRFT and FRTL. Also check for FICL and/or FTCL.
C
      IER = 1
      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      IF(KFRTL.LE.0.OR.KFRFT.LE.0) GOTO 999
      NTK = LROWS(KFRTL)
      IF(NTK.LE.0) GOTO 999
C
      IER = 2
      KFICL = IW(NFICL)
      KFTCL = IW(NFTCL)
      IF(KFICL.LE.0.AND.KFTCL.LE.0) GOTO 999
C
      IER = 0
      NUSEI = 0
      NUSET = 0
      IF(KFICL.GT.0) NUSEI = LROWS(KFICL)
      IF(KFTCL.GT.0) NUSET = LROWS(KFTCL)
C
C-----------------------------------------------------------------------
      IF(IDEB.GT.0) THEN
        WRITE(LPR,1000) NTK,(IW(KFRTL+I),I=0,NTK*LFRTLA+LMHLEN)
 1000   FORMAT(' ICFTOI: No. trks=',I4,' FRTL:len-col-row',3I5,/
     +         (3X,8I5))
        IF(KFICL.GT.0) WRITE(LPR,1001) (IW(KFICL+I),I=0,LMHLEN+NUSEI)
 1001   FORMAT('   FICL:len-col-row',3I5,/(3X,10I5))
        IF(KFTCL.GT.0) WRITE(LPR,1002) (IW(KFTCL+I),I=0,LMHLEN+NUSET)
 1002   FORMAT('   FTCL:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,1003) (IW(KFRFT+I),I=0,2)
 1003   FORMAT(' FRFT:len-col-row',3I5)
        DO 1 I=1,NTK
          KK = KROW(KFRFT,I)
          WRITE(LPR,1004) I,(RW(KK+J),J=1,6)
 1004     FORMAT(' TRACK',I5,' FRFT(1-6)',6E12.4)
    1   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C Create IGTL#1 and TGTL#1
C
      CALL AUBOS('IGTL',1,NTK*LIGTLA+LMHLEN,KIGT1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('TGTL',1,NTK*LTGTLA+LMHLEN,KTGT1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      IF(IGAR.EQ.1) THEN
        KFRTL = IW(NFRTL)
        KIGT1 = NLINK('IGTL',1)
      ENDIF
      IW(KIGT1+LMHCOL) = LIGTLA
      IW(KTGT1+LMHCOL) = LTGTLA
C
C Loop over tracks in FRFT/FRTL - count number of ITC-TPC tracks (#1).
C ITC-TPC tracks always appear first in list and have at least NTMIN
C TPC hits. Fill IGTL#1 and TGTL#1 and count the numbers of ITC and
C TPC coords used on these tracks.
C
      NITC1 = 0
      NTPC1 = 0
      ITK1 = 0
      DO 100 I=1,NTK
        KFR = KROW(KFRTL,I)
        NTPC = IW(KFR+JFRTNT)
        IF(NTPC.GE.NTMIN) THEN
          ITK1 = ITK1 + 1
          KIGT = KROW(KIGT1,ITK1)
          KTGT = KROW(KTGT1,ITK1)
          IW(KIGT+JIGTIO) = IW(KFR+JFRTII)
          IW(KIGT+JIGTN1) = IW(KFR+JFRTNI)
          IW(KIGT+JIGTNR) = IW(KFR+JFRTNE)
          NITC1 = NITC1 + IW(KFR+JFRTNI) + IW(KFR+JFRTNE)
          IW(KTGT+JTGTIO) = IW(KFR+JFRTIT)
          IW(KTGT+JTGTN1) = IW(KFR+JFRTNT)
          IW(KTGT+JTGTNR) = IW(KFR+JFRTNR)
          NTPC1 = NTPC1 + IW(KFR+JFRTNT) + IW(KFR+JFRTNR)
        ELSE
          GOTO 200
        ENDIF
  100 CONTINUE
  200 CONTINUE
C
C Check for inconsistencies
C
      IF(NITC1.GT.NUSEI) GOTO 997
      IF(NTPC1.GT.NUSET) GOTO 997
C
C Adjust length of IGTL#1 and TGTL#1
C
      IW(KIGT1+LMHROW) = ITK1
      IW(KTGT1+LMHROW) = ITK1
      CALL AUBOS('IGTL',1,ITK1*LIGTLA+LMHLEN,KIGT1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('TGTL',1,ITK1*LTGTLA+LMHLEN,KTGT1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
C
C Create ITFT#1, IGCL#1 and TGCL#1 and fill
C
      CALL AUBOS('ITFT',1, ITK1*LITFTA+LMHLEN,KITF1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('IGCL',1,NITC1*LIGCLA+LMHLEN,KIGC1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('TGCL',1,NTPC1*LTGCLA+LMHLEN,KTGC1,IGAR)
      IF(IGAR.EQ.2) GOTO 998
C
      KITF1 = NLINK('ITFT',1)
      KIGC1 = NLINK('IGCL',1)
      KTGC1 = NLINK('TGCL',1)
      IW(KITF1+LMHCOL) = LITFTA
      IW(KITF1+LMHROW) = ITK1
      IW(KIGC1+LMHCOL) = LIGCLA
      IW(KIGC1+LMHROW) = NITC1
      IW(KTGC1+LMHCOL) = LTGCLA
      IW(KTGC1+LMHROW) = NTPC1
C
      KFICL = IW(NFICL)
      KFTCL = IW(NFTCL)
      KFRFT = IW(NFRFT)
      IF(KFICL.GT.0)
     +   CALL UCOPY(IW(KFICL+LMHLEN+1),IW(KIGC1+LMHLEN+1),NITC1)
      IF(KFTCL.GT.0)
     +   CALL UCOPY(IW(KFTCL+LMHLEN+1),IW(KTGC1+LMHLEN+1),NTPC1)
      CALL UCOPY(IW(KFRFT+LMHLEN+1),IW(KITF1+LMHLEN+1),ITK1*LITFTA)
C
C Add to list
C
      CALL BLIST(IW,'E+','ITFTIGTLIGCLTGTLTGCL')
C
C-----------------------------------------------------------------------
      IF(IDEB.GT.0) THEN
        KIGT1 = NLINK('IGTL',1)
        KTGT1 = NLINK('TGTL',1)
        WRITE(LPR,2000) ITK1,(IW(KIGT1+I),I=0,ITK1*LIGTLA+LMHLEN)
 2000   FORMAT(' IGTL#1  No. trks=',I4,' IGTL:len-col-row',3I5,/
     +         (3X,3I5,3X,3I5,3X,3I5))
        WRITE(LPR,2001) (IW(KIGC1+I),I=0,LMHLEN+NITC1)
 2001   FORMAT('   IGCL#1:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,2002) ITK1,(IW(KTGT1+I),I=0,ITK1*LTGTLA+LMHLEN)
 2002   FORMAT(' TGTL#1  No. trks=',I4,' TGTL:len-col-row',3I5,/
     +         (3X,3I5,3X,3I5,3X,3I5))
        WRITE(LPR,2003) (IW(KTGC1+I),I=0,LMHLEN+NTPC1)
 2003   FORMAT('   TGCL#1:len-col-row',3I5,/(3X,10I5))
C
        WRITE(LPR,2004) (IW(KITF1+I),I=0,2)
 2004   FORMAT(' ITFT#1:len-col-row',3I5)
        DO 250 I=1,ITK1
          KK = KROW(KITF1,I)
          WRITE(LPR,2005) I,(RW(KK+J),J=1,6)
 2005     FORMAT(' TRACK',I5,' ITF1(1-6)',6E12.4)
  250   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C Create IGTL,IGCL,TGTL,TGCL and ITFT#2 if necessary
C
      IF(ITK1.EQ.NTK) GOTO 999
      ITK2 = NTK - ITK1
      NITC2 = NUSEI - NITC1
      NTPC2 = NUSET - NTPC1
C
      CALL AUBOS('ITFT',2, ITK2*LITFTA+LMHLEN,KITF2,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('IGTL',2, ITK2*LIGTLA+LMHLEN,KIGT2,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('IGCL',2,NITC2*LIGCLA+LMHLEN,KIGC2,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('TGTL',2, ITK2*LTGTLA+LMHLEN,KTGT2,IGAR)
      IF(IGAR.EQ.2) GOTO 998
      CALL AUBOS('TGCL',2,NTPC2*LTGCLA+LMHLEN,KTGC2,IGAR)
      IF(IGAR.EQ.2) GOTO 998
C
C Fill header words
C
      KITF2 = NLINK('ITFT',2)
      KIGT2 = NLINK('IGTL',2)
      KIGC2 = NLINK('IGCL',2)
      KTGT2 = NLINK('TGTL',2)
      KTGC2 = NLINK('TGCL',2)
      IW(KITF2+LMHCOL) = LITFTA
      IW(KITF2+LMHROW) = ITK2
      IW(KIGT2+LMHCOL) = LIGTLA
      IW(KIGT2+LMHROW) = ITK2
      IW(KIGC2+LMHCOL) = LIGCLA
      IW(KIGC2+LMHROW) = NITC2
      IW(KTGT2+LMHCOL) = LTGTLA
      IW(KTGT2+LMHROW) = ITK2
      IW(KTGC2+LMHCOL) = LTGCLA
      IW(KTGC2+LMHROW) = NTPC2
C
C Fill IGCL,TGCL and ITFT #2
C
      KFICL = IW(NFICL)
      KFTCL = IW(NFTCL)
      KFRFT = IW(NFRFT)
      IF(KFICL.GT.0)
     +   CALL UCOPY(IW(KFICL+LMHLEN+NITC1+1),IW(KIGC2+LMHLEN+1),NITC2)
      IF(KFTCL.GT.0)
     +   CALL UCOPY(IW(KFTCL+LMHLEN+NTPC1+1),IW(KTGC2+LMHLEN+1),NTPC2)
      KF = KROW(KFRFT,ITK1+1)
      CALL UCOPY(IW(KF+1),IW(KITF2+LMHLEN+1),ITK2*LITFTA)
C
C Fill IGTL#2 and TGTL#2
C
      IOI = 0
      IOT = 0
      DO 300 I=ITK1+1,NTK
        KF = KROW(KFRTL,I)
        KI = KROW(KIGT2,I-ITK1)
        KT = KROW(KTGT2,I-ITK1)
        IW(KI+JIGTN1) = IW(KF+JFRTNI)
        IW(KI+JIGTNR) = IW(KF+JFRTNE)
        IW(KI+JIGTIO) = IOI
        IW(KT+JTGTN1) = IW(KF+JFRTNT)
        IW(KT+JTGTNR) = IW(KF+JFRTNR)
        IW(KT+JTGTIO) = IOT
        IOI = IOI + IW(KF+JFRTNI) + IW(KF+JFRTNE)
        IOT = IOT + IW(KF+JFRTNT) + IW(KF+JFRTNR)
  300 CONTINUE
C
C-----------------------------------------------------------------------
      IF(IDEB.GT.0) THEN
        KIGT2 = NLINK('IGTL',2)
        KTGT2 = NLINK('TGTL',2)
        WRITE(LPR,3000) ITK2,(IW(KIGT2+I),I=0,ITK2*LIGTLA+LMHLEN)
 3000   FORMAT(' IGTL#2  No. trks=',I4,' IGTL:len-col-row',3I5,/
     +         (3X,3I5,3X,3I5,3X,3I5))
        WRITE(LPR,3001) (IW(KIGC2+I),I=0,LMHLEN+NITC2)
 3001   FORMAT('   IGCL#2:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,3002) ITK2,(IW(KTGT2+I),I=0,ITK2*LTGTLA+LMHLEN)
 3002   FORMAT(' TGTL#2  No. trks=',I4,' TGTL:len-col-row',3I5,/
     +         (3X,3I5,3X,3I5,3X,3I5))
        WRITE(LPR,3003) (IW(KTGC2+I),I=0,LMHLEN+NTPC2)
 3003   FORMAT('   TGCL#2:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,3004) (IW(KITF2+I),I=0,2)
 3004   FORMAT(' ITFT#2:len-col-row',3I5)
        DO 350 I=1,ITK2
          KK = KROW(KITF2,I)
          WRITE(LPR,3005) I,(RW(KK+J),J=1,6)
 3005     FORMAT(' TRACK',I5,' ITF2(1-6)',6E12.4)
  350   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
      GOTO 999
C
C Error: Bank inconsistencies.
  997 CONTINUE
      IER = 3
      CALL BDROP(IW,'ITFTIGTLIGCLTGTLTGCL')
      GOTO 999
C
C Error: No BOS space. Tidy up.
  998 CONTINUE
      IER = 4
      CALL BDROP(IW,'ITFTIGTLIGCLTGTLTGCL')
C-----------------------------------------------------------------------
  999 CONTINUE
      END
#endif
