      SUBROUTINE ISHUFF(IDEB,IER)
C-----------------------------------------------------------------------
C! Shuffle/Update FRFT type banks from ITFT type track banks.
C!
C! Author  : J.Sedgbeer  03/02/92
C! Modified:
C!
C!   Input:
C!     IDEB   /I : Debug flag; =0 for none, >0 for lots.
C!     commons   : /BCS/    for BOS banks
C!     params    :  FRFTJJ,FRTLJJ,FICLJJ,FTCLJJ,FVCLJJ
C!                  ITFTJJ,IGTLJJ,IGCLJJ,TGTLJJ,TGCLJJ
C!
C!   Output:
C!     IER   /I  : Error flag
C!                  = 0  O.K.
C!                  = 1  Missing or empty input banks. Fxxx unchanged.
C!                  = 2  more rows in ITFT than FRFT!! Fxxx unchanged.
C!                  = 3  Not enough BOS space. Fxxx banks partially
C!                       changed therefore drop Fxxx banks before
C!                       exiting.
C!     Updated Banks  FRFT, FRTL, FICL, FTCL, FVCL
C!
C! ISHUFF:
C! -------
C! Update FRFT,FRTL,FICL,FTCL and FVCL banks using info. in
C! ITFT, IGTL, IGCL, TGTL and TGCL bank numbers 1 and 2.
C! ITFT type track banks are NOT dropped before exiting as they
C! may be used in subsequent routines.
C!
C! ***  N.B. ***
C! This is NOT a general routine: the updating is very
C! specific and is based on the assumption that the ITC coord.
C! assignments may have changed, i.e. the tracks (ITFT type)
C! have been passed through the IREFIN module of the ITC tracking.
C! Hence, it is assumed that:
C! - The number of TPC-ITC tracks (ITFT#1) has not changed and
C!   their track parameters, TPC and VDET coords. are also unchanged,
C!   i.e. only the ITC coord. assignment may have changed. Hence:
C!     FRTL must be updated from IGTL#1
C!     FICL must be updated from IGCL#1
C!
C! - If the total number of tracks (ITFT#1+ITFT#2) is unchanged
C!   (i.e. equal to the number of tracks in FRFT) then the track
C!   parameters, TPC and VDET coords. of all tracks are unchanged
C!   (i.e. only the ITC coord. assignment may have changed). Hence:
C!     FRTL must be updated from IGTL#2
C!     FICL must be updated from IGCL#2
C!
C! - If the total number of tracks ITFT(#1+#2) is less than FRFT
C!   then some ITFT#2 (so called ITC alone) tracks have been
C!   lost due to reassignment of ITC coords. In this case the
C!   track parameters and all coord. assignments (TPC,VDET and ITC)
C!   must be updated from ITFT#2, IGTL#2,IGCL#2, TGTL#2,TGCL#2, i.e.:
C!     FRFT must be updated from ITFT#2
C!     FRTL  "    "    "      "  IGTL#2 and TGTL#2
C!     FICL  "    "    "      "  IGCL#2
C!     FTCL  "    "    "      "  TGCL#2
C!   *** N.B. ***
C!   Also, as there is no stored VDET info. for the ITFT#2
C!   tracks the VDET info cannot be updated and must therefore
C!   be set to zero, i.e. in FRTL and FVCL the VDET coords. on
C!   ITC-alone tracks must be removed.
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "fvcljj.h"
#include "ficljj.h"
#include "ftcljj.h"
#include "itftjj.h"
#include "igtljj.h"
#include "igcljj.h"
#include "tgtljj.h"
#include "tgcljj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND,NLINK
      LOGICAL FIRST,FDEB
      DATA FIRST/.TRUE./
      DATA LPR/6/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise.
      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRFT = NAMIND('FRFT')
        NFRTL = NAMIND('FRTL')
        NFVCL = NAMIND('FVCL')
        NFICL = NAMIND('FICL')
        NFTCL = NAMIND('FTCL')
      ENDIF
C
C Check for banks. Must have FRFT,FRTL,FTCL,
C                      ITFT#1,IGTL#1,IGCL#1,TGTL#1,TGCL#1
C
      IER = 1
      KFRTL = IW(NFRTL)
      KFRFT = IW(NFRFT)
      KFVCL = IW(NFVCL)
      KFICL = IW(NFICL)
      KFTCL = IW(NFTCL)
      IF(KFRTL.LE.0.OR.KFRFT.LE.0.OR.KFTCL.LE.0) GOTO 998
C
      NTF  = LROWS(KFRTL)
      IF(NTF.LE.0) GOTO 998
      NUMB = IW(KFRFT-2)
      NCFI = 0
      NCFV = 0
      IF(KFICL.GT.0) NCFI = LROWS(KFICL)
      IF(KFVCL.GT.0) NCFV = LROWS(KFVCL)
      NCFT = LROWS(KFTCL)
C
      KITF1 = NLINK('ITFT',1)
      KITF2 = NLINK('ITFT',2)
      KIGT1 = NLINK('IGTL',1)
      KIGC1 = NLINK('IGCL',1)
      KIGT2 = NLINK('IGTL',2)
      KIGC2 = NLINK('IGCL',2)
      KTGT1 = NLINK('TGTL',1)
      KTGC1 = NLINK('TGCL',1)
      KTGT2 = NLINK('TGTL',2)
      KTGC2 = NLINK('TGCL',2)
      IF(KITF1.LE.0.OR.KIGT1.LE.0.OR.KIGC1.LE.0) GOTO 998
      IF(KTGT1.LE.0.OR.KTGC1.LE.0)               GOTO 998
C
      NTI1 = LROWS(KITF1)
      NTI2 = 0
      IF(KITF2.GT.0) THEN
        NTI2 = LROWS(KITF2)
        IF(KIGT2.LE.0.OR.KIGC2.LE.0) GOTO 998
      ENDIF
      NTI = NTI1+NTI2
      IF(NTI.LE.0) GOTO 998
C
      NCI1 = 0
      IF(KIGC1.GT.0) NCI1 = LROWS(KIGC1)
      NCI2 = 0
      IF(KIGC2.GT.0) NCI2 = LROWS(KIGC2)
      NCI = NCI1+NCI2
      NCT1 = 0
      IF(KTGC1.GT.0) NCT1 = LROWS(KTGC1)
      NCT2 = 0
      IF(KTGC2.GT.0) NCT2 = LROWS(KTGC2)
      NCT = NCT1+NCT2
C
C ITFT more tracks that FRFT !!
C
      IF(NTI.GT.NTF) THEN
        IER = 2
        GOTO 998
      ENDIF
C
      IER = 0
C-----------------------------------------------------------------------
C Debug print - banks on input
C
      IF(IDEB.GT.0) THEN
        WRITE(LPR,1000) NTF,(IW(KFRTL+I),I=0,NTF*LFRTLA+LMHLEN)
 1000   FORMAT(' ISHUFF: INPUT: No. trks=',I4,' FRTL:len-col-row',3I5,/
     +         (3X,8I5))
        IF(KFVCL.GT.0) WRITE(LPR,1001) (IW(KFVCL+I),I=0,LMHLEN+NCFV)
 1001   FORMAT('   FVCL:len-col-row',3I5,/(3X,10I5))
        IF(KFICL.GT.0) WRITE(LPR,1002) (IW(KFICL+I),I=0,LMHLEN+NCFI)
 1002   FORMAT('   FICL:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,1003) (IW(KFTCL+I),I=0,LMHLEN+NCFT)
 1003   FORMAT('   FTCL:len-col-row',3I5,/(3X,10I5))
        WRITE(LPR,1004) (IW(KFRFT+I),I=0,2)
 1004   FORMAT(' FRFT:len-col-row',3I5)
        DO 1 I=1,NTF
          KK = KROW(KFRFT,I)
          WRITE(LPR,1005) I,(RW(KK+J),J=1,6)
 1005     FORMAT(' TRACK',I5,' FRFT(1-6)',6E12.4)
    1   CONTINUE
C
        WRITE(LPR,1010) NTI1,NTI2
 1010   FORMAT(' # tracks ITFT#1 and #2 ',2I5)
        IF(NTI1.GT.0) THEN
          WRITE(LPR,2000) NTI1,(IW(KIGT1+I),I=0,NTI1*LIGTLA+LMHLEN)
 2000     FORMAT(' IGTL#1  No. trks=',I4,' IGTL:len-col-row',3I5,/
     +         (3X,3I5))
          WRITE(LPR,2001) (IW(KIGC1+I),I=0,LMHLEN+NCI1)
 2001     FORMAT('   IGCL#1:len-col-row',3I5,/(3X,10I5))
          WRITE(LPR,2002) NTI1,(IW(KTGT1+I),I=0,NTI1*LTGTLA+LMHLEN)
 2002     FORMAT(' TGTL#1  No. trks=',I4,' TGTL:len-col-row',3I5,/
     +         (3X,3I5))
          WRITE(LPR,2003) (IW(KTGC1+I),I=0,LMHLEN+NCT1)
 2003     FORMAT('   TGCL#1:len-col-row',3I5,/(3X,10I5))
          WRITE(LPR,2004) (IW(KITF1+I),I=0,2)
 2004     FORMAT(' ITFT#1:len-col-row',3I5)
          DO 2 I=1,NTI1
            KK = KROW(KITF1,I)
            WRITE(LPR,2005) I,(RW(KK+J),J=1,6)
 2005       FORMAT(' TRACK',I5,' ITF1(1-6)',6E12.4)
    2     CONTINUE
        ENDIF
C
        IF(NTI2.GT.0) THEN
          WRITE(LPR,3000) NTI2,(IW(KIGT2+I),I=0,NTI2*LIGTLA+LMHLEN)
 3000     FORMAT(' IGTL#2  No. trks=',I4,' IGTL:len-col-row',3I5,/
     +         (3X,3I5))
          WRITE(LPR,3001) (IW(KIGC2+I),I=0,LMHLEN+NCI2)
 3001     FORMAT('   IGCL#2:len-col-row',3I5,/(3X,10I5))
          IF(KTGT2.GT.0)
     +      WRITE(LPR,3002) NTI2,(IW(KTGT2+I),I=0,NTI2*LTGTLA+LMHLEN)
 3002     FORMAT(' TGTL#2  No. trks=',I4,' TGTL:len-col-row',3I5,/
     +         (3X,3I5))
          IF(KTGC2.GT.0) WRITE(LPR,3003) (IW(KTGC2+I),I=0,LMHLEN+NCT2)
 3003     FORMAT('   TGCL#2:len-col-row',3I5,/(3X,10I5))
          WRITE(LPR,3004) (IW(KITF2+I),I=0,2)
 3004     FORMAT(' ITFT#2:len-col-row',3I5)
          DO 3 I=1,NTI2
            KK = KROW(KITF2,I)
            WRITE(LPR,3005) I,(RW(KK+J),J=1,6)
 3005       FORMAT(' TRACK',I5,' ITF2(1-6)',6E12.4)
    3     CONTINUE
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C If no. of tracks has changed then adjust length of FRFT and update,
C i.e. copy ITFT#2 to FRFT. (ITFT#1 tracks must be same)
C
      IF(NTF.NE.NTI) THEN
        CALL AUBOS('FRFT',NUMB,NTI*LFRFTA+LMHLEN,KFRFT,IGAR)
        IF(IGAR.EQ.2) GOTO 997
        KITF2 = NLINK('ITFT',2)
        IF(NTI2.GT.0) CALL UCOPY(IW(KITF2+LMHLEN+1),
     +                      IW(KFRFT+LMHLEN+NTI1*LFRFTA+1),NTI2*LFRFTA)
        IW(KFRFT+LMHROW) = NTI
      ENDIF
C
C Copy IGCL#1 and #2 to FICL. First make sure FICL is long enough.
C
      CALL AUBOS('FICL',0,NCI+LMHLEN,KFICL,IGAR)
      IF(IGAR.EQ.2) GOTO 997
      IW(KFICL+LMHROW) = NCI
      IW(KFICL+LMHCOL) = 1
      KIGC1 = NLINK('IGCL',1)
      KIGC2 = NLINK('IGCL',2)
      IF(NCI1.GT.0)
     +    CALL UCOPY(IW(KIGC1+LMHLEN+1),IW(KFICL+LMHLEN+1)     ,NCI1)
      IF(NCI2.GT.0)
     +    CALL UCOPY(IW(KIGC2+LMHLEN+1),IW(KFICL+LMHLEN+NCI1+1),NCI2)
C
C If no. of tracks has changed then update FTCL from TGCL#2.
C First make sure FTCL is long enough.
C
      IF(NTF.NE.NTI) THEN
        IF(NCFT.NE.NCT) THEN
          CALL AUBOS('FTCL',0,NCT+LMHLEN,KFTCL,IGAR)
          IF(IGAR.EQ.2) GOTO 997
          IW(KFTCL+LMHROW) = NCT
        ENDIF
        KFTCL = IW(NFTCL)
        KTGC2 = NLINK('TGCL',2)
        IF(NCT2.GT.0)
     +    CALL UCOPY(IW(KTGC2+LMHLEN+1),IW(KFTCL+LMHLEN+NCT1+1),NCT2)
      ENDIF
C
C Update FRTL. If # tracks changed then adjust length and zero all
C ITC,TPC and VDET info. corresponding to ITFT2
C
      IF(NTF.NE.NTI) THEN
        CALL AUBOS('FRTL',0,NTI*LFRTLA+LMHLEN,KFRTL,IGAR)
        IF(IGAR.EQ.2) GOTO 997
        IW(KFRTL+LMHROW) = NTI
        IF(NTI2.GT.0) THEN
          CALL VZERO(IW(KFRTL+LMHLEN+LFRTLA*NTI1+1),LFRTLA*NTI2)
        ENDIF
      ENDIF
C
      KFRTL = IW(NFRTL)
      KIGT1 = NLINK('IGTL',1)
      KIGT2 = NLINK('IGTL',2)
      KTGT1 = NLINK('TGTL',1)
      KTGT2 = NLINK('TGTL',2)
C
C Update ITC coord. assignments in FRTL from IGTL1
C
      DO 100 I=1,NTI1
        KF = KROW(KFRTL,I)
        KG = KROW(KIGT1,I)
        IW(KF+JFRTII) = IW(KG+JIGTIO)
        IW(KF+JFRTNI) = IW(KG+JIGTN1)
        IW(KF+JFRTNE) = IW(KG+JIGTNR)
  100 CONTINUE
C
C Update ITC coord. assignments in FRTL from IGTL2
C If no. of tracks has changed then update TPC coord. assignments
C in FRTL from TGTL2 and update VDET coord. offset column in FRTL
C (recall vdet coord. assignments deleted).
C
C get VDET final coord. offset (i.e. no. of coords. for ITFT#1 tracks)
C
      IOV = 0
      IF(NTI1.GT.0) THEN
        KK = KROW(KFRTL,NTI1)
        IOV = IW(KK+JFRTIV) + IW(KK+JFRTNV)
      ENDIF
C
C Loop to update FRTL
C
      DO 200 I=1,NTI2
        KF = KROW(KFRTL,I+NTI1)
        KG = KROW(KIGT2,I)
        IW(KF+JFRTII) = IW(KG+JIGTIO) + NCI1
        IW(KF+JFRTNI) = IW(KG+JIGTN1)
        IW(KF+JFRTNE) = IW(KG+JIGTNR)
        IF(NTF.NE.NTI) THEN
          IW(KF+JFRTIV) = IOV
          IW(KF+JFRTIT) = NCT1
          IF(KTGT2.GT.0) THEN
            KT = KROW(KTGT2,I)
            IW(KF+JFRTIT) = IW(KT+JTGTIO) + NCT1
            IW(KF+JFRTNT) = IW(KT+JTGTN1)
            IW(KF+JFRTNR) = IW(KT+JTGTNR)
          ENDIF
        ENDIF
  200 CONTINUE
C
C If no. of tracks has changed then update FVCL - drop VDET assignments
C for ITFT2 tracks, i.e. truncate FVCL bank.
C
      IF(NTF.NE.NTI) THEN
        CALL AUBOS('FVCL',0,IOV+LMHLEN,KFVCL,IGAR)
        IF(IGAR.EQ.2) GOTO 997
        IW(KFVCL+LMHROW) = IOV
        IW(KFVCL+LMHCOL) = 1
      ENDIF
C
C-----------------------------------------------------------------------
C Debug print - updated FRFT track banks
C
      IF(IDEB.GT.0) THEN
        WRITE(LPR,5000) NTI,(IW(KFRTL+I),I=0,NTI*LFRTLA+LMHLEN)
 5000   FORMAT(' ISHUFF: OUTPUT:No. trks=',I4,' FRTL:len-col-row',3I5,/
     +         (3X,8I5))
        IF(KFVCL.GT.0) THEN
          NCV = LROWS(KFVCL)
          WRITE(LPR,1001) (IW(KFVCL+I),I=0,LMHLEN+NCV)
        ENDIF
        IF(KFICL.GT.0) WRITE(LPR,1002) (IW(KFICL+I),I=0,LMHLEN+NCI)
        WRITE(LPR,1003) (IW(KFTCL+I),I=0,LMHLEN+NCT)
        WRITE(LPR,1004) (IW(KFRFT+I),I=0,2)
        DO 300 I=1,NTI
          KK = KROW(KFRFT,I)
          WRITE(LPR,1005) I,(RW(KK+J),J=1,6)
  300   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
      GOTO 998
C
C-----------------------------------------------------------------------
C BOS Error:  Tidy up.
C
  997 CONTINUE
      IER = 3
      CALL BDROP(IW,'FRFTFRTLFVCLFICLFTCL')
C
C-----------------------------------------------------------------------
C Do NOT drop ITFT type input banks before exiting
C
  998 CONTINUE
CCCC      CALL BDROP(IW,'ITFTIGTLIGCLTGTLTGCL')
C
      END
#endif
