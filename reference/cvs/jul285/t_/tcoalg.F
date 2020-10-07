      SUBROUTINE TCOALG(ISLOT,IROW,IQW,NPADS,RPULS,
     &                                    PRFS2,IPDMN,RPHI,QCO,IERY)
C
C---------------------------------------------------------------------
C! Calculate r*phi position and charge of cluster.
C!
C!  Author:    T. Pun        10-06-86
C!  Modified:  R. Johnson     8-04-88
C!  Modified:  A. Roth       14-12-89
C!             D. Casper     09-06-95  Add Gaussian charge estimate for
C!                                     pad dE/dx.
C!
C!  Includes possibility of choosing algorithm, depending on NPADS.
C!  NPADS must be <= LMXPCL, because of lookup tables and vectors.
C!
C!  Complete description of all algorithms : ALEPH-TPC Note 86-68,
C!  by Thierry Pun and Dieter Schlatter (especially APPENDIX).
C!
C!  INPUT :
C!    I4/  ISLOT     : Sector number from which the cluster comes
C!    I4/  IROW      : Row number of the sector (not global)
C!    I4/  IQW       : =0 for isolated subcluster, =1 otherwise
C!    I4/  NPADS     : No. of pads in this cluster, in [2 ... LMXPCL].
C!    R4/  RPULS(ip) : P-heights for this cluster : ip in [1 ... NPADS],
C!    R4/  PRFS2     : pad response width
C!    I4/  IPDMN     : Minimum pad number in sector row for this cluster
C!
C!  OUTPUT :
C!    R4/  RPHI      : r*phi coordinate in sector system.
C!    R4/  QCO       : charge of coordinate, using Gaussian estimator
C!    I4/  IERY      : Error code, > 0 if problem.
C!                    111      : (A) WSTD2 < eps.
C!                    121      : (B) negative logarithm.
C!                    131      : (C) threshold <= 1.0.
C!                    161      : (F) < 3 pads;
C!                    162      :     denominator = 0.
C!                    171      : (G) < 3 pads;
C!                    172      :     denominator < eps.
C!                    191      : (I) < 3 pads;
C!                    192      :     denominator < eps for r*phi;
C!                    193      :     PRFS2 < 0.
C!                    300      : illegal value of IALGTP.
C!                    300 + x  : alg number x does not exit.
C!                    350      : max pulse height at position 0.
C!                    400      : less than 2 pads.
C!                    400 + x  : > LMXPCL pads (x = NPADS - LMXPCL).
C!                    500      : RPhi outside physical bounds of
C!                               cluster.  Check PRF and ratio
C!                               of the pulse heights.
C!                    700      : Cluster peaks at sector edge
C!
C!  OTHER INPUT PARAMETERS. . .
C!    from TPCCUT:
C!    I4/  IALGTP   : Algorithm(s) to use:
C!                    IALGTP < 10 : alg nr  1  2  3  4  5  6  7  9
C!                                  (letter A  B  C  D  E  F  G  I.
C!                    IALGTP = 3 digits integer IJK : if NPADS <= I,
C!                    then algorithm (J), else algorithm (K).
C!                    Ex : 2   --> alg 2 (B) whatever pad multiplicity.
C!                         321 --> if NPADS <= 3, then (B), else (A).
C!    R4/  ALGPTP   : INPUT by user. Real, for possible use in algs.
C!                    Currently needed for alg 3 (=C) : threshold.
C!    from TCOCOM:
C!    I4/  MOMPH1(ip): lookup 1st pad moments; initialized: TPLKUP.INC.
C!    I4/  MOMPH2(ip): lookup 2nd pad moments.
C!    I4/  MOMPH3(ip): lookup 3rd pad moments.
C!    I4/  MOMPH4(ip): lookup 4th pad moments.
C!
C!  Called by TCOOR
C!
C!  MODULES called:
C!         TCOWAV  : weighted average and standard-deviation^2;  alg A.
C!         TCOGS3  : 3 pads algorithm (3 equations, 3 unknowns); alg B.
C!         TCOTHR  : threshold algorithm;                        alg C.
C!         TCOWA2  : weighted avg of 2 "2 pads estimates";       alg D.
C!         TCOWA3  : weighted avg of 3 "2 pads estimates";       alg E.
C!         TCOUL2  : unweighted LSE, 2 unknown parameters;       alg F.
C!         TCOWL2  : weighted LSE, 2 unknown parameters;         alg G.
C!         TCOUL3  : unweighted LSE, 3 unknown parameters;       alg I.
C!
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "alcons.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "tpccut.h"
C
C----------------------------END COMMON-----------------------------
C
      LOGICAL DOSTD,FIRST
      CHARACTER TEXT*42
      DIMENSION RPULS(NPADS)
      DATA DOSTD/.FALSE./,FIRST/.TRUE./
C
C----------------------------END TYPES------------------------------
C
C++   Unpack algorithm numbers only on the first call
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        IF ((IALGTP.GE.1) .AND. (IALGTP.LE.9)) THEN
          IALG = IALGTP
          NALGP=0
        ELSEIF ((IALGTP.GE.111) .AND. (IALGTP.LE.999)) THEN
          NALGP=IALGTP/100
          IALG1 = MOD(IALGTP,100)/10
          IALG2 = MOD(MOD(IALGTP,100),10)
        ELSE
          WRITE(TEXT,183)
  183     FORMAT('Non existant algorithm number.  Set to 321')
          CALL RERROR('TCOALG',1,TEXT)
          IALGTP=321
          NALGP=3
          IALG1=2
          IALG2=1
        ENDIF
      ENDIF
C
C++   Zero the coordinate charge estimator
C
      QCO = 0.
C
C++   Select the algorithm number
C
      IF (NALGP.GT.0) THEN
        IF (NPADS.LE.NALGP) THEN
          IALG=IALG1
        ELSE
          IALG=IALG2
        ENDIF
      ENDIF
C
      IF (NPADS .LT. 2) THEN
        IERY = 400
        GO TO 999
      ENDIF
C
      IF (NPADS .GT. LMXPCL) THEN
        IERY = 400 + (NPADS-LMXPCL)
        GO TO 999
      ENDIF
C
C++   Look for max pad pulse height
C
      IPM = 0
      PLMX = 0.
      DO 20 IP = 1, NPADS
        IF (RPULS(IP) .LT. PLMX) GO TO 20
        PLMX = RPULS(IP)
        IPM = IP
   20 CONTINUE
C
C++   Get pad number preceeding maximum.  If the maximum is at
C++   the sector edge, then reject the cluster.  NTPAD is the total
C++   number of pads on the sector row INCLUDING half pads.
C
      ISTYP=ITPTYP(ISLOT)
      NTPAD=NTPDPR(IROW,ISTYP)+2
      IF (IPM .EQ. 0) THEN
        IERY = 350
        GO TO 999
      ELSE IF (IPM .EQ. 1) THEN
        IF (IPDMN.EQ.1) THEN
          IERY=700
          GO TO 999
        ENDIF
        IPAD1 = 1
        GO TO 200
      ELSE IF (IPM .EQ. NPADS) THEN
        IPDMX=IPDMN+NPADS-1
        IF (IPDMX.EQ.NTPAD) THEN
          IERY=700
          GO TO 999
        ENDIF
        IPAD1 = NPADS - 1
        GO TO 200
      ELSE
        IPAD1 = IPM - 1
      ENDIF
C
C++   Handle case of 3 pads (or more!).
C
      GO TO (110, 120, 130, 140, 150, 160, 170, 102, 190), IALG
C
C++   Inexistant algorithm number (such as 8 = H).
C
  102 IERY = 300 + IALG
      GO TO 999
C
C++   Algorithms 1/A, 2/B, 3/C, 4/D, 5/E, 6/F, 7/G, 9/I:
C
  110 CALL TCOWAV (NPADS, RPULS, DOSTD, RPHI, WSTD2, IERY)
      DO I = 1, NPADS
        QCO = QCO + RPULS(I)
      ENDDO
      GO TO 900
C
  120 CALL TCOGS3 (IPAD1, RPULS, RPHI, WSTD2, IERY)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
  130 CALL TCOTHR (IPAD1, RPULS, PRFS2, RPHI, IERY)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
  140 CALL TCOWA2 (IPAD1, RPULS, PRFS2, RPHI)
      CALL TCQGS3 (IPAD1, RPULS, QCO, IERY)
      GO TO 900
C
  150 CALL TCOWA3 (IPAD1, RPULS, PRFS2, RPHI)
      GO TO 900
C
  160 CALL TCOUL2 (NPADS, RPULS, PRFS2, RPHI, IERY, WSTD2)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
  170 CALL TCOWL2 (NPADS, RPULS, PRFS2, RPHI, IERY, WSTD2)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
  190 CALL TCOUL3 (NPADS, RPULS, DOSTD, RPHI, WSTD2, IERY)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
C++   Handle the 2-pad case, or pulses with maximum at one end.
C
  200 CONTINUE
C
      GO TO (210, 220, 220, 220, 220, 220, 220, 202, 220), IALG
C
C++   Inexistant algorithm number.
C
  202 IERY = 300 + IALG
      GO TO 999
C
C++   First consider algorithm 1 (=A).
C
  210 CALL TCOWAV (NPADS, RPULS, DOSTD, RPHI, WSTD2, IERY)
      IF (IERY .NE. 0) GO TO 999
      GO TO 900
C
C++   And then the two pad gaussian algorithm
C
  220 CONTINUE
      Q2    = RPULS(IPAD1+1)
      Q1    = RPULS(IPAD1  )
      RPHI  = FLOAT(IPAD1) + 0.5 + PRFS2*ALOG(Q2/Q1)
C
C++   Calculate coordinate charge using Gaussian
C
      IF(Q1.GT.Q2)THEN
        ARG = (FLOAT(IPAD1)-RPHI)**2/(2.*PRFS2)
        IF(ARG.LE.20.)THEN
          QNORM = Q1 * EXP(ARG)
        ELSE
          QNORM = 0.
        ENDIF
      ELSE
        ARG = (FLOAT(IPAD1+1)-RPHI)**2/(2.*PRFS2)
        IF(ARG.LE.20.)THEN
          QNORM = Q2 * EXP(ARG)
        ELSE
          QNORM = 0.
        ENDIF
      ENDIF
      QCO   = QNORM * SQRT(2.*3.14159 * PRFS2)
C
  900 CONTINUE
C
C++   Reject the coordinate if it lies too close to the subcluster edge.
C++   This cut is loosened by half a pad width if the subcluster is know
C++   to be overlapping another subcluster in r*phi.
C
      IF (MOD(IQW/10,10).EQ.0) THEN
        IF ((RPHI.LT.1.0-CRTLTP)
     &       .OR. (RPHI.GT.FLOAT(NPADS)+CRTLTP)) THEN
          IERY = 500
          GO TO 999
        ENDIF
      ELSE
        TOL=0.5+CRTLTP
        IF (RPHI.LT.(1.0-TOL) .OR. RPHI.GT.(FLOAT(NPADS)+TOL)) THEN
          IERY=500
          GO TO 999
        ENDIF
      ENDIF
C
C++   Calculate the coordinate in terms of pad number.
C++   Until here, RPHI lies between [1 .. NPADS*1.0].
C
      RPHI= FLOAT(IPDMN) - 1.0 + RPHI
      IERY = 0
C
  999 CONTINUE
      RETURN
      END
#endif
