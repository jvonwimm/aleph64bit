      SUBROUTINE VDWSMR
C ----------------------------------------------------------------------
C!  VDWSMR smears the VDXY and VDZT coordinates in accordance with
C!         the errors in the alignment quantities.
C!
C - Author         Manoj Thulasidas  5-Nov-1994
CKEY V_DECK VDET
C!
C!  Description
C!  ===========
C!  The alignment procedure for the VDET essentially measures the
C!  position of each wafer with respect to the rest of the detector.
C!  The results are given in the form of 6 quantities - 3
C!  translations and 3 rotations to go from the nominal position to
C!  the measure postion.  The statistical accuracy of these
C!  measurements is given in the 6x6 (symmetric) error matrix (21
C!  numbers) for each wafer.  There is no mathematically rigorous way
C!  of treating the uncertainty in the real position of the wafers -
C!  especially because it is measured with respect to the rest of the
C!  detector which is, in turn, subject to the same uncertainties.
C!  and the global correlations are not stored.
C!  Many possible treatments can be thought about:
C!
C!  This subroutine does treatment C.
C!  (Treatment A => VDOSMR   and   Treatment B => VDALIN)
C!
C!  C: Dealign the MC detector.  In each MC julia event, one could
C!  position the wafers at random locations dictated by the alignment
C!  error matrices.  And then one would have to just recompute the
C!  coordinates (VDXY and VDZT entities).  The resolution measured for
C!  each wafer (at each position) would come out better than the data,
C!  but when averaged over all the wafers and all positions, the
C!  agreement between data and MC should be just as good as in B.  Note
C!  that C is better than B in that it gets the correlations within on
C!  wafer right.  It is still not perfect because there is no way one
C!  could take care of wafer to wafer correlations (all the hits in the
C!  same module should have highly correlated errors).  Also, the
C!  random placement of the wafers may not be appealing to some.
C!  However, given the information, this about the best we can do.
C
C
C  Output : smeard VDZT and VDXY banks
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      INTEGER NLINK, VJWABW, VBRJWA, VJWABR, VXYZVU, JUNIDB,
     $   VVUWXY, GTSTUP,ALGTDB, NAMIND, VDYEAR
      EXTERNAL NLINK, VJWABW, VBRJWA, VJWABR, VXYZVU, JUNIDB,
     $   VVUWXY, GTSTUP,ALGTDB, NAMIND, VDYEAR
      INTEGER NAVALC, KVALC, KVDB(2), NRR(2), LDBAS, IROW, NR, IERR,
     $   JWAF, IV, IVDXX, JDUM, NRTR, JROW
      REAL DX(2), AVEC(6), XYZ(3), VUW(3), TNG, SIGU, SIGW
      REAL VDGAUS
      LOGICAL FIRST
      INTEGER IVSTP, IRUN, IRET, KVDSM
      SAVE NAVALC
#include "vdsmjj.h"
#include "vdztjj.h"
#include "vdxyjj.h"
C-- make a stupid array for the quality flag index
      INTEGER JVDQF(2)
      DATA JVDQF/JVDZQF,JVDXQF/
      DATA FIRST /.TRUE./
      SAVE SIGU,SIGW
C   bit 29 now means that smearing has been added to a cluster
      INTEGER IMCSMR
      PARAMETER (IMCSMR=268435456)
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        LDBAS = JUNIDB(0)
        IVSTP = GTSTUP ('VD',IRUN)
        IRET = ALGTDB (LDBAS,'VDSM',IVSTP)
        KVDSM = IW(NAMIND('VDSM'))
        IF (KVDSM.LE.0) THEN
          CALL RERROR('VDWSMR',1,'No VDSM bank!')
        ENDIF
        IF(KVDSM.GT.0)THEN
          SIGU = RTABL(KVDSM,1,JVDSUS)
          SIGW = RTABL(KVDSM,1,JVDSWS)
        ELSE
          SIGU = 0.0
          SIGW = 0.0
        END IF
C
C This is printed only once per job
C
        WRITE (6,*) ' VDWSMR: SIGU =', SIGU, '   SIGW =',SIGW
        NAVALC = NAMIND('VALC')
      ENDIF
      KVALC = IW(NAVALC)
C-- load the right VALC bank from the database
      IF (KVALC.LE.0) THEN
        CALL RERROR('VDWSMR',1,'No alignment matrix!')
        RETURN
      ENDIF
C-- loop over the alignment bank rows
      DO 10 IROW = 1, LROWS(KVALC)
C-- get the encoded wafer number (= nominal bank number for VDZT and VDX
        NR = ITABL(KVALC,IROW,1)
C-- get the global wafer number
        IERR = VJWABW(NR,JWAF)
C-- if a good wafer then
        IF (JWAF.NE.0.AND.IERR.EQ.1) THEN
C-- for the old vdet, get the real bank number to be used in linking VDZT,VDXY
          IF (VDYEAR().EQ.91) THEN
            IERR = VBRJWA(JWAF,1, NRR(1))
            IERR = VBRJWA(JWAF,2, NRR(2))
          ELSE
            NRR(1) = NR
            NRR(2) = NR
          ENDIF
C-- link up VDZT and VDXY banks
          KVDB(1) = NLINK('VDZT',NRR(1))
          KVDB(2) = NLINK('VDXY',NRR(2))
C-- if no banks found quit the loop
          IF (KVDB(1).LE.0 .AND. KVDB(2).LE.0) GOTO 10
C-- calculate the misalignment vector (6 elements) for this wafer
          CALL VDGTMV(IROW,AVEC)
C-- loop over views
          DO 20 IV = 1, 2
C-- loop over this VDxx bank
            IF (KVDB(IV).LE.0) GOTO 20
            DO 19 IVDXX = 1, LROWS(KVDB(IV))
C-- see if the smearing has been already added for this row
              IF(IAND(ITABL(KVDB(IV),IVDXX,JVDQF(IV)),IMCSMR).NE.0)
     $           GOTO 19
C-- get the true global coordinate of the hit from the VDHT bank
C-- also get the tangent of the angle (slope of the track, projected
C-- to the view.
              CALL VDGTXL(IV,NRR(IV),IVDXX,XYZ,TNG,IERX)
C-- recalculate the bank number using the true position and NRR
              IERR = VJWABR(NRR(IV),XYZ(3),JDUM,NRTR)
C-- DONT DO THIS TEST
C              IF (NR.NE.NRTR) GOTO 19
C-- get the displacement using the true position in VUW system
              IERR = VXYZVU(XYZ,JWAF,VUW)
C-- if the local coordinate finding returned an error,
C-- it means that we have the wafer center VUW, correct for that
              IF (IERX.NE.0) THEN
                IF (IV.EQ.1) THEN
                  VUW(3) = RTABL(KVDB(IV),IVDXX,JVDZWC)
                ELSE
                  VUW(2) = RTABL(KVDB(IV),IVDXX,JVDXUC)
                ENDIF
              ENDIF
C-- get the smearing
              CALL VDWSDX(IV,VUW,AVEC,TNG,DX(IV))
C-- put the smeared quantities back in VDXY and VDXY
C-- the row number of the bank
              JROW = KROW(KVDB(IV),IVDXX)
              IF (IV.EQ.1) THEN
                VUW(1) = 0.0
                VUW(2) = 0.0
                VUW(3) = RTABL(KVDB(IV),IVDXX,JVDZWC) + DX(IV)
                IF (SIGW.GT.0.0) VUW(3) = VDGAUS(VUW(3),SIGW)
                CALL VGWFXY(NRR(IV),VUW,XYZ)
                RW(JROW+JVDZZC) = XYZ(3)
                RW(JROW+JVDZWC) = VUW(3)
C  Flag the hit
                IW(JROW+JVDZQF)=IOR(IW(JROW+JVDZQF),IMCSMR)
              ELSE
                VUW(1) = 0.0
                VUW(2) = RTABL(KVDB(IV),IVDXX,JVDXUC) + DX(IV)
                VUW(3) = 0.0
                IF (SIGU.GT.0.0) VUW(2) = VDGAUS(VUW(2),SIGU)
                CALL VGWFXY(NRR(IV),VUW,XYZ)
                RW(JROW+JVDXXC) = XYZ(1)
                RW(JROW+JVDXYC) = XYZ(2)
                RW(JROW+JVDXUC) = VUW(2)
C  Flag the hit
                IW(JROW+JVDXQF)=IOR(IW(JROW+JVDXQF),IMCSMR)
              ENDIF
 19         CONTINUE
 20       CONTINUE
        ENDIF
 10   CONTINUE
      RETURN
      END
#endif
