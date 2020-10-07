      SUBROUTINE HCSIMM (ITH0, IPH0, ITH1, IPH1, IRGN)
C*********************************************************************
C!  Given the theta, phi address of an Hcal tower (ITH0, IPH0),      *
C   yields the theta phi address of a tower which by symmetry        *
C   has the same calibration constant (ITH1, IPH1)                   *
C                                                                    *
C   Author: F. Ligabue 11/14/88                                      *
C                                                                    *
C   Numbers used in this routine are taken from Aleph Note           *
C   "Hadron Calorimeter Description in GALEPH"                       *
C                                                                    *
C    region 1   ITH0: 1-4,    59-62   IPH0: 1-24    end-cap          *
C    region 2   ITH0: 5-10,   53-58   IPH0: 1-48    end-cap          *
C    region 3   ITH0: 11-17   46,52   IPH0: 1-96    overlap region   *
C    region 4   ITH0: 18-45           IPH0: 1-96    barrel           *
C                                                                    *
C    region 1   ITH1: 1-4           IPH1: 1-4       end-cap          *
C    region 2   ITH1: 5-10          IPH1: 1-8       end-cap          *
C    region 3   ITH1: 11-17         IPH1: 1-16      overlap region   *
C    region 4   ITH1: 18-31         IPH1: 1-16      barrel           *
C                                                                    *
C    region i   ITH1: ithmi - itmai                                  *
C                                                                    *
C   Input banks:  H1EC, H2EC, H3EC, H4EC                             *
C                                                                    *
C   Called by HCALIB                                                 *
C                                                                    *
C?
C!======================================================================
#ifndef DOC
      LOGICAL XREF
#include "bcs.h"
      PARAMETER(ITMI1=1,ITMI2=5,ITMI3=11,ITMI4=18)
      PARAMETER(ITMA1=4,ITMA2=10,ITMA3=17,ITMA4=31)
#include "bmacro.h"
      XREF = .FALSE.
C
C  CHECK ITH0 IS INSIDE THETA RANGE
C
      ITMIN = ITMI1
      ITMAX = ITMA4
      IF(ITH0.LT.ITMIN.OR.ITH0.GT.ITMAX*2) GOTO 100
C
C...THETA REFLECTION
C
      IF(ITH0.GT.ITMAX) THEN
         ITH1 = 2*ITMAX - ITH0 +1
         XREF = .TRUE.
      ELSE
      ITH1 = ITH0
      ENDIF
      IPH1 = IPH0
C
C...REGION 1
C
      IF(ITH1.LE.ITMA1) THEN
         IRGN = 1
         IPHM = 24
         NPHMD = 4
C
C...REGION 2
C
      ELSE IF(ITH1.GE.ITMI2.AND.ITH1.LE.ITMA2) THEN
         IRGN = 2
         IPHM = 48
         NPHMD = 8
C
C...REGION 3
C
      ELSE IF(ITH1.GE.ITMI3.AND.ITH1.LE.ITMA3) THEN
         IRGN = 3
         IPHM = 96
         NPHMD = 16
C
C...REGION 4
C
      ELSE IF(ITH1.GE.ITMI4.AND.ITH1.LE.ITMA4) THEN
         IRGN = 4
         IPHM = 96
         NPHMD = 8
      ENDIF
C
      IF(IPH1.LE.0.OR.IPH1.GT.IPHM) GOTO 100
C
C...REFLECTION  X -> -X  (YZ PLANE)
C
      IF(XREF) THEN
         IF(IPH1.LE.IPHM/2) THEN
            IPH1 = IPHM/2 - IPH1 + 1
         ELSE
            IPH1 = 3*IPHM/2 - IPH1 + 1
         ENDIF
      ENDIF
      IMOD = INT((FLOAT(IPH1)-.5)/NPHMD) + 1
      IPH1 = MOD(IPH1,NPHMD)
      IF(IPH1.EQ.0) IPH1 = NPHMD
C
C...BARREL SYMMETRY
C
      IF(IRGN.EQ.4) THEN
         IF(IPH1.GT.NPHMD/2) IPH1 = NPHMD - IPH1 + 1
C
C...END-CAP SYMMETRY
C
      ELSE
         IF(IMOD.EQ.3.OR.IMOD.EQ.6) IPH1 = NPHMD - IPH1 + 1
      ENDIF
      GOTO 200
C
C ADDRESS OUT OF RANGE
C
  100 CONTINUE
C     CALL REPORT ('HCSIMM','TOWER ADDRESS OUT OF RANGE',0)
      ITH1 = 0
      IPH1 = 0
  200 RETURN
      END
#endif
