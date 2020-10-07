      SUBROUTINE ITEXTN
C
C------------------------------------------------------------------------------
C!    Associate lone TPC coordinates with existing ITC tracks
C!
C!    Authors            M J Phillips with J Sedgbeer        15-Jan-1991
C!    Based on work by   L Garrido, L Vitale and J Sedgbeer  05-Sep-1989
C!
C!    Input variables    None
C!    Output variables   None
C!
C!    Description
C!    ===========
C!    This routine associates lone TPC points in the low-angle region
C!    with existing reconstructed ITC tracks.
C?
C?    Scan through existing ITC-only tracks.
C?    IF low angle with respect to beam axis THEN flag as selected.
C?    Scan through lone TPC points.
C?    IF in low-angle region THEN flag as selected.
C?    Each ITC track is extrapolated into the TPC, and those lone TPC
C?    points within its (suitably-defined) vicinity are identified.
C?    We then try to refit the ITC points on the original track together
C?    with the new TPC coordinates.
C?    If the match is good enough, the points are added onto the track,
C?    with the further condition that if we are only left with one TPC
C?    point, then it must lie in the first pad-row.
C?
C!==============================================================================
C
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
#include "rlunit.h"
C
#include "bcs.h"
#include "tpcojj.h"
#include "itftjj.h"
#include "igtljj.h"
#include "tgtljj.h"
C
C  PARAMETERS FOR HELIX FIT ROUTINE
      PARAMETER (IOPT=4,NMULT=1,RMS=29.6,XLEN=0.027)
C
C  PARAMETERS FOR ITEXTN
      PARAMETER (CUT1=3.5,CUT2=10.0,CUT3=30.0,CUT4=35.0)
      PARAMETER (RCRIT1=39.871,RCRIT2=65.471,ZTPC=220.0)
      PARAMETER (THCUT=0.3488)
C
      EXTERNAL NLINK
      REAL CM(15), VV0(6), EE0(21), COV(21)
      COMMON/UFITCB/CHIACU(80)
      INTEGER VLIST
C
#include "bmacro.h"
C
C  Check all required banks are present, and that there are both
C  some TPC points and some ITC tracks.
C
      KITFT = NLINK ('ITFT',2)
      KIGCL = NLINK ('IGCL',2)
      KIGTL = NLINK ('IGTL',2)
      KTPCO = NLINK ('TPCO',0)
      IF ((KITFT.LE.0).OR.
     +    (KIGCL.LE.0).OR.
     +    (KIGTL.LE.0).OR.
     +    (KTPCO.LE.0)) GOTO 999
C
      NTRK   = LROWS (KITFT)
      NTPT   = LROWS (KTPCO)
      IF (NTRK.LE.0) GOTO 999
      IF (NTPT.LE.0) GOTO 999
C
      IF (FDEBRF.AND.JDBDRF(JULIT).GT.4) THEN
            WRITE(LDEBRL,600) NTRK,NTPT
 600  FORMAT('ITEXTN : No. of ITC-only tracks ',I3,
     +       ' : No. TPC coords ',I4)
      ENDIF
C
C  Make a bank (IEXC) flagging TPC points which are candidates
C
      CALL ITPCAN (THCUT,NTPT,NPCAN,IFLAG)
      IF (IFLAG.EQ.3) GOTO 900
      IF (IFLAG.NE.0) GOTO 999
C
C?   Define pointer to IEXC
C
      KIEXC = NLINK ('IEXC',0)
C
C  Initialise a bank (IDSQ) to give mesaures of match between
C  ITC tracks and TPC points. Each element is filled with -1.0
C
      KIDSQ = NLINK ('IDSQ',0)
      IF (KIDSQ.GT.0) CALL BDROP (IW,'IDSQ')
      CALL AUBOS ('IDSQ',0,LMHLEN+NTRK*NTPT,KIDSQ,IGAR)
      IF (IGAR.EQ.2) GOTO 900
      KIDSQ = NLINK ('IDSQ',0)
      CALL BLIST (IW,'T+','IDSQ')
      IW (KIDSQ + LMHCOL) = NTPT
      IW (KIDSQ + LMHROW) = NTRK
      DO 10 M = 1, NTRK * NTPT
            RW (KIDSQ + LMHLEN + M) = -1.0
 10   CONTINUE
C
C  Initialise ITPC bank, which will contain list of TPC points
C  to be added to a particular ITC track
C
      CALL AUBOS ('ITPC',0,102,IND,IGAR)
         IF (IGAR.EQ.2) GOTO 900
      CALL BLIST (IW,'T+','ITPC')
C
C  Initialise new output banks TGCL(2) and TGTL(2)
C
C  TGCL(2) will contain the list of TPC points which will have been
C  joined onto the end of ITC tracks. Initially it will be an empty
C  bank, apart from the miniheader.
C
      CALL AUBOS ('TGCL',2,LMHLEN,IND,IGAR)
            IF (IGAR.EQ.2) GOTO 900
      KTGC2 = NLINK ('TGCL',2)
      IF (KTGC2.LE.0) GOTO 900
      IW (KTGC2 + LMHCOL) = 1
      IW (KTGC2 + LMHROW) = 0
C
C  TGTL(2) will index TGCL(2).
C  Rows in TFTL(2) correspond to rows in ITFT(2) (the ITC track bank).
C  Number of columns in TGTL(2) is the same as the number of columns
C  in TGTL(1), i.e. 3.
C
      NCTGT = 3
      NELE  = NCTGT * NTRK
      CALL AUBOS ('TGTL',2,LMHLEN + NELE,IND,IGAR)
            IF (IGAR.EQ.2) GOTO 900
      KTGT2 = NLINK ('TGTL',2)
      IF (KTGT2.LE.0) GOTO 900
      IW (KTGT2 + LMHCOL) = NCTGT
      IW (KTGT2 + LMHROW) = NTRK
      DO 280 IK = 1, NELE
          IW (KTGT2 + IK + LMHLEN) = 0
 280  CONTINUE
C
C  Loop over all ITC tracks
C
      DO 100 I = 1,NTRK
C
C  Get parameters of track
C
            TRKIR = RTABL (KITFT,I,JITFIR)
            TRKTL = RTABL (KITFT,I,JITFTL)
            TRKP0 = RTABL (KITFT,I,JITFP0)
            TRKD0 = RTABL (KITFT,I,JITFD0)
            TRKZ0 = RTABL (KITFT,I,JITFZ0)
            DO 30 JK=1,15
                  CM (JK) = RTABL (KITFT,I,JITFCM-1+JK)
 30         CONTINUE
C
C  Extrapolated ITC track must hit either TPC endcap between
C  radius RCRIT1 and RCRIT2
C
           IF (TRKTL.GE.0.) ZEND =  ZTPC
           IF (TRKTL.LT.0.) ZEND = -ZTPC
            CALL ITRKZR (ZEND, TRKIR, TRKTL, TRKP0, TRKD0,
     +                  TRKZ0, REND)
           IF (REND.LT.0.0) GOTO 100
            IF ((REND.LT.RCRIT1).OR.
     +          (REND.GT.RCRIT2)) GOTO 100
C
      IF (FDEBRF.AND.JDBDRF(JULIT).GT.4) THEN
            WRITE (LDEBRL,601) I
 601  FORMAT ('ITEXTN : Possible ITC track to be extended ',I3)
      ENDIF
C
C  Loop over each TPC point
C
            DO 50 M = 1, NTPT
C
C  Only consider suitable candidates
C
                  IF (ITABL(KIEXC,M,1).EQ.0) GOTO 50
C
C  Get coordinates of point
C
                  RPNT  = RTABL (KTPCO,M,JTPCRV)
                  PPNT  = RTABL (KTPCO,M,JTPCPH)
                  ZPNT  = RTABL (KTPCO,M,JTPCZV)
C
C  Evaluate phi and z of projected track at radius RPNT
C
                  CALL ITRKEQ
     +           (RPNT,TRKIR,TRKTL,TRKP0,TRKD0,TRKZ0,
     +            ZTRK,PTRK,IFLAG)
                      IF (IFLAG.NE.0) GOTO 50
C
C  Get errors in phi and z of the point, i.e. PPTER and ZPTER
C
                  SGRP2   = RTABL (KTPCO,M,JTPCSR)
                  IF (SGRP2.LT.0.0) SGRP2 = 0.0001
                  SIG     = SQRT (SGRP2)
                  PPTER   = SIG / RPNT
C
                  SGZ2    = RTABL (KTPCO,M,JTPCSZ)
                  IF (SGZ2.LT.0.0) SGZ2 = 0.0064
                  ZPTER   = SQRT (SGZ2)
C
C  Get errors in phi and z of projected track, i.e. PTRER and ZTRER
C
              CALL IHELER (TRKIR,TRKTL,TRKP0,TRKD0,TRKZ0,
     +                     CM,RPNT,PTRK,ZTRK,ZTRER,PTRER,IFLAG)
                  IF (IFLAG.NE.0) GOTO 50
C
C  Calculate square of number of standard deviations between
C  phi of point and that of track
C
                  ANGL2 = (PPNT-PTRK)*(PPNT-PTRK)/
     +                    (PPTER*PPTER + PTRER*PTRER)
C
C  If this exceeds CUT1, then not a good match
C
                  IF (ANGL2.GT.CUT1) GOTO 50
C
C  Calculate a similar quantity relating to z
C
                  DIST2 = (ZPNT-ZTRK)*(ZPNT-ZTRK)/
     +                    (ZPTER*ZPTER + ZTRER*ZTRER)
C
C  Test whether this exceeds CUT2
C
                  IF (DIST2.GT.CUT2) GOTO 50
C
C  Else accept as a possible point to be joined onto the track
C
                  RW (KIDSQ + LMHLEN + (I-1)*NTPT + M)
     +                   = ANGL2

C
      IF (FDEBRF.AND.JDBDRF(JULIT).GT.4) THEN
            WRITE (LDEBRL,602) M
 602  FORMAT ('ITEXTN : Possible TPC point to add ',I4)
      ENDIF
C
 50         CONTINUE
 100  CONTINUE
C
C  Tidy up the IDSQ bank
C
      CALL ITIDY1 (CUT1,IFLAG)
         IF (IFLAG.NE.0) GOTO 999
      CALL ITIDY2 (CUT1,IFLAG)
         IF (IFLAG.NE.0) GOTO 999
C
      KITPC = NLINK ('ITPC',0)
C
C  Loop over ITC tracks
C
      DO 400 I = 1, NTRK
C
C  NTPC will contain number of candidate TPC points for this track.
C  List of these points will be written into bank 'ITPC'
C
            NTPC = 0
            KIDSQ = NLINK ('IDSQ',0)
C
C  Loop over all TPC points. Write those that are flagged by bank
C  'IDSQ' into bank 'ITPC'
C
            DO 300 N = 1, NTPT
                  IF (RTABL(KIDSQ,I,N).LT.0.0) GOTO 300
                  IF (RTABL(KIDSQ,I,N).LT.CUT1) THEN
                        NTPC = NTPC + 1
                        IW (KITPC + LMHLEN + NTPC ) = N
                        IF (NTPC.GE.100) GOTO 400
                  ENDIF
 300        CONTINUE
C
C  If no candidate TPC points for this track, then skip it.
C
            IF (NTPC.EQ.0) GOTO 400
C
C  Set-up correct miniheader for bank 'ITPC'
C
            IW (KITPC + LMHCOL) = NTPC
            IW (KITPC + LMHROW) = 1
C
C  Get a list of ITC points on the ITC track, ready for passing to
C  fit routine
C
            KIGTL = NLINK ('IGTL',2)
                IF (KIGTL.LE.0) GOTO 400
            IOFF  = ITABL (KIGTL,I,JIGTIO)
            NIPT  = ITABL (KIGTL,I,JIGTN1)
            KIGCL = NLINK ('IGCL',2)
                IF (KIGCL.LE.0) GOTO 400
            IF (NIPT.EQ.0) GOTO 400
C
C  Now call the track fitting routine.
C  Pass to it the following points :
C        All the ITC points associated with the current track, and
C        all the candidate TPC points
C        *** NO VDET INFORMATION IS USED HERE ***
C
 112        CALL UFITMS (NTPC, NIPT, 0,
     +                   IW (KITPC + LMHLEN + 1),
     +                   IW (KIGCL + LMHLEN + IOFF + 1),
     +                   VLIST,
     +                   IOPT, NNMULT, RMS, XLEN,
     +                   VV0, EE0, COV, CHI2, IERR)
            IF (IERR.NE.0) GOTO 400
C
C  Scan through the candidate TPC points, and look at the
C  contribution of each to the total chi-squared.
C  If greater than CUT3, then no longer consider this point.
C  NTFIN = number of candidates which survive this test
C  We then remove all points which fail this test from the bank
C  'ITPC'
C
            NTFIN = 0
            DO 320 N = 1, NTPC
                  IF (CHIACU (NIPT + N).LT.CUT3) THEN
                        NTFIN = NTFIN + 1
                        IW (KITPC + LMHLEN + NTFIN)
     +                = IW (KITPC + LMHLEN + N)
                  ENDIF
 320        CONTINUE
C
C  Update miniheader of bank 'ITPC'
C
            IW (KITPC + LMHCOL) = NTFIN
            IF (NTFIN.EQ.0) GOTO 400
C
C  Call fit routine again, with all ITC points, and the
C  surviving TPC candidate points
C
            CALL UFITMS (NTFIN, NIPT, 0,
     +                   IW (KITPC + LMHLEN + 1),
     +                   IW (KIGCL + LMHLEN + IOFF + 1),
     +                   VLIST,
     +                   IOPT, NMULT, RMS, XLEN,
     +                   VV0, EE0, COV, CHI2, IERR)
            IF (IERR.NE.0) GOTO 400
C
C  We will accept these TPC points only if the chi-squared
C  of the complete fit is less than CUT4
C
            IF (CHI2.GT.CUT4) GOTO 400
C
C  Finally, if only one point left, then it must be in the first
C  TPC pad-row, i.e. r-coord must be < 42.0
C
      IF (NTFIN.EQ.1) THEN
            IPT = IW (KITPC + LMHLEN + 1)
            RPT = RTABL (KTPCO,IPT,JTPCRV)
            IF (RPT.GT.42.0) GOTO 400
      ENDIF
C
C   Update banks
C   ------------
C
C   TGCL bank
C   ---------
C
      KTGC2 = NLINK ('TGCL',2)
      IF (KTGC2.LE.0) GOTO 400
      LTGC2 = IW (KTGC2)
      LLIST = LROWS (KTGC2)
      CALL AUBOS ('TGCL',2,LTGC2+NTFIN,IND,IGAR)
          IF (IGAR.EQ.2) GOTO 400
      KTGC2 = NLINK ('TGCL',2)
      IF (KTGC2.LE.0) GOTO 400
      IW (KTGC2+LMHROW) = LLIST + NTFIN
      IOFF = LLIST
      DO 382 IK = 1, NTFIN
            IW (KTGC2+IOFF+LMHLEN+IK) = ITABL (KITPC,1,IK)
 382  CONTINUE
C
C   TGTL bank
C   ---------
C
      KTGT2 = NLINK ('TGTL',2)
      ITR = KTGT2 + LMHLEN + (I-1)*LCOLS(KTGT2)
      IW (ITR + JTGTIO) = IOFF
      IW (ITR + JTGTN1) = NTFIN
      IW (ITR + JTGTNR) = 0
C
C   TPCO bank
C   ---------
C
C   Find number of ITC-TPC tracks (i.e. number of tracks in
C   bank ITFT(1)).
C
      KITFT = NLINK ('ITFT',1)
      N1    = 0
      IF (KITFT.GT.0) N1 = LROWS (KITFT)
C
      KTPCO = NLINK ('TPCO',0)
      DO 379 IK = 1, NTFIN
C
C   ITC track number for a track in bank ITFT(2) is
C   track number in bank ITFT(2) PLUS number of tracks in ITFT(1).
C
      IW (KTPCO + LMHLEN + (ITABL(KITPC,1,IK) -1) * LCOLS (KTPCO)
     +       + JTPCTN) = I + N1
 379  CONTINUE
C
C   ITFT bank
C   ---------
C
      KITFT = NLINK ('ITFT',2)
      ITR = KITFT + LMHLEN + (I-1)*LCOLS(KITFT)
      DO 380 IK = 1, 6
            RW (ITR + IK) = VV0 (IK)
 380  CONTINUE
C
      DO 381 IK = 1, 21
            RW (ITR + IK + JITFCM - 1) = COV (IK)
 381  CONTINUE
C
      RW (ITR + JITFCD) = CHI2
      IW (ITR + JITFND) = 2*(NTFIN+NIPT) - 5
      IW (ITR + JITFFT) = NMULT*100 + IOPT*10 + IERR
C
      IF (FDEBRF.AND.JDBDRF(JULIT).GT.4) THEN
      DO 390 II=1,NTFIN
            WRITE (LDEBRL,604) I,ITABL(KITPC,1,II)
 604  FORMAT ('ITEXTN : Track number ',I3,' + coord number ',I4)
 390  CONTINUE
      ENDIF
C
 400        CONTINUE
            GOTO 999
C
C  Errors
C
 900  CALL RERROR('ITEXTN',1,'No room to create/extend BOS banks')
C
 999        CONTINUE
C
C  Drop banks which are no longer required
C
        CALL BDROP (IW,'IEXCIDSQITPC')
C
 1000  RETURN
      END
#endif
