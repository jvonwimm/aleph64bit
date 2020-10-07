      SUBROUTINE ITPCAN (THCUT,NTPT,NTCAN,IFLAG)
C
C------------------------------------------------------------------------------
C!    Initialise bank IEXC used by subroutine ITEXTN
C!
C!    Author              M J Phillips with J Sedgbeer    15-Jan-1991
C!
C!    Input variables     THCUT      Value of theta below which a
C!                                   "low angle point" must lie
C!    Output variables    NTPT       Total number of TPC points
C!                        NTCAN      Number of TPC points which are
C!                                   candidates
C!                        IFLAG = 0  Successful
C!                                1  No TPCO bank found
C!                                2  No TPC points in TPCO bank
C!                                3  No room for new bank
C!
C!    Description
C!    ===========
C!    A bank IEXC is created which flags those TPC points which are
C!    candidates for adding to extended ITC tracks.
C?    In order to be a candidate, each TPC point must
C?          (i)   lie within a low-angle cone
C?    and   (ii)  be currently unassociated with an existing TPC track.
C?    The IEXC bank has a one-to-one correspondance with the TPCO
C?    bank. If a TPCO point is a candidate, then the bank IEXC will
C?    contain a "1", otherwise it will contain a "0".
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "tpcojj.h"
#include "bmacro.h"
      IFLAG = 0
      NTPT = 0
      NTCAN = 0
      KTPCO = NLINK ('TPCO',0)
      IF (KTPCO.EQ.0) THEN
            IFLAG = 1
            GOTO 20
            ENDIF
      NTPT = LROWS (KTPCO)
      IF (NTPT.EQ.0) THEN
            IFLAG = 2
            GOTO 20
            ENDIF
C
C  If bank already exists, then drop it
C
      KIEXC = NLINK ('IEXC',0)
      IF (KIEXC.GT.0) CALL BDROP(IW,'IEXC')
C
C  Create new bank
C
       CALL AUBOS ('IEXC',0,LMHLEN+NTPT,IND,IGAR)
      IF (IGAR.EQ.2) THEN
            IFLAG = 3
            GOTO 20
            ENDIF
      KIEXC = NLINK ('IEXC',0)
      CALL BLIST (IW,'T+','IEXC')
      IF (KIEXC.EQ.0) THEN
            IFLAG = 3
            GOTO 20
            ENDIF
C
C  Set up miniheader of IEXC bank
C
      IW (KIEXC + LMHCOL) = 1
      IW (KIEXC + LMHROW) = NTPT
C
C  Loop over all TPC points
C
      DO 10 M = 1, NTPT
C
C  Initially set entry (in IEXC) for this point equal to zero
C
            IW (KIEXC + LMHLEN + M) = 0
C
C  Only consider unassociated points
C
            IF (ITABL(KTPCO,M,JTPCTN).NE.0) GOTO 10
C
C  Test if point lies in low-angle region
C
            RPT = RTABL (KTPCO,M,JTPCRV)
            ZPT = RTABL (KTPCO,M,JTPCZV)
            IF (ZPT.LT.0.0001) GOTO 10
            IF (RPT/ABS(ZPT).GT.TAN(THCUT)) GOTO 10
C
C  If it does, then change entry in IEXC bank
C
            IW (KIEXC + LMHLEN + M) = 1
            NTCAN = NTCAN + 1
 10   CONTINUE
 20   RETURN
      END
#endif
