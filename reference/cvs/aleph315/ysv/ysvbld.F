      SUBROUTINE YSVBLD(IER)
C-----------------------------------------------------------------------
C! Find secondary charged and neutral vertices
C  IER = 0 if successful
C
C   Input banks:    FRFT    Track fits
C                   YLV0    High-purity long V0s (if any)
C                   PYER    Primary vertex
C       
C   Output banks:   YSVX    Reconstructed vertex candidates;
C                           NR = 0 contains the primary vertex
C                           NR = 1 contains a fit of each track and
C                                  V0 to the primary
C                           NR < 0 contains all vertices with a number
C                                  of constituents = ABS(NR); hence NR=-2
C                                  contains all two-track vertices, NR=-3
C                                  contains all three-track vertices, etc
C                                  (negative number is done so that a loop
C                                  starting with IW(NAMIND('YSVX')) begins
C                                  with the highest multiplicity vertices
C                                  (which are built from lower multiplicity
C                                  vertices).
C                   
C
C   Author   :- D. Casper   30-jun-1996
C=========================================
#ifndef DOC
C
#include "bcs.h"
#include "bosext.h"
#include "frftjj.h"
#include "ysvwrk.h"
#include "alcons.h"
C Initialize work bank indices
      DATA INDYSVW,INDYSEW,INDYSTW,INDYS0W,
     &      INDYSTL,INDYSTM,INDYS0L,INDYS0M,
     &      INDHXOU,INDVHOU,INDHXNU,INDVHNU,
     &      INDYSCW,INDYSCL,INDYSCM,INDCXOU,
     &      INDVCOU,INDYSKL/18*0/
C
      DATA NFRFT/0/
      SAVE NFRFT
C
#include "bmacro.h"
C --------------------------------------------------------------

      IF (NFRFT.EQ.0) THEN
        NFRFT = NAMIND('FRFT')
      ENDIF

C+  Drop old vertex banks

      CALL BDROP(IW,'YSVX')

C+ bank pointer

      KFRFT = IW(NFRFT)
      IF (KFRFT.EQ.0) GOTO 999

C+ find magnetic field

      FIELM = ALFIEL(DUM)
      IF (FIELM .EQ. 0) GOTO 999

C+ load primary vertex in YSVX/0

      CALL YSVX0L(IER)
      IF (IER.NE.0) GOTO 999

C+  Load the charged tracks to be used into YSTL

      CALL YSTLLD(IER)
      IF (IER.NE.0) GOTO 999

C+  Flag combinations already identified as kinks

      CALL YSKLLD(IER)
      IF (IER.NE.0) GOTO 999

C+  Load the V0s to be used into YS0L

      CALL YS0LLD(IER)
      IF (IER.NE.0) GOTO 999

C+  Load ITC tracks into YSCL

      CALL YSCLLD(IER)
      IF (IER.NE.0) GOTO 999

C+  Fit good charged tracks to the primary vertex

      CALL YSPVFT(IER)
      IF (IER.NE.0) GOTO 999

C+  Fit good pairs of tracks and/or V0 to a common vertex

      CALL YSPAIR(NPAIR,IER)
      IF (IER.NE.0) GOTO 999

C+  Merge vertices sharing tracks

      CALL YSVMRG(NPAIR,IER)
      IF (IER.NE.0) GOTO 999

C+  Try to add tracks to existing vertices

      CALL YSVTRK(IER)
      IF (IER.NE.0) GOTO 999
C
      IER = 0
 999  CONTINUE
C+  Drop some useless banks

C      JYSVX0 = NDROP('YSVX',0)
C      JYSVX1 = NDROP('YSVX',1)

C+  Put YSVX banks on the T-list

      CALL BLIST(IW,'T+','YSVX')

C+  Resize vertex banks

      CALL AUBPRS('YSVX')

C+  Drop all work banks used by this package

      IW(1) = 18
      CALL WDROP(IW,INDYSVW)

      RETURN
      END
#endif
