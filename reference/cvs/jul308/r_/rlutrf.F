      SUBROUTINE RLUTRF(KRUN,MKREQ,MSKHV,MSKPC)
C----------------------------------------------------------------------
C! - Return Luminosity coincidence trigger mask and HV mask for run KRUN
C!
C!
C!    Author:     B. Bloch-Devaux   25-May-1993
C!
C!    Input:      - KRUN : current Run number
C!
C!    Output:     - MKREQ mask of Lumi coincidence ( LCAL or SICAL )
C!                - MSKHV mask of HV bits to be on for Lumi  meas't
C!                - MSKPC ident of Luminometer for PECO clusters
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
      PARAMETER (NTREQ=4)
      CHARACTER*8 MNM,TTREQ(NTREQ)
      CHARACTER*4 CHAINT
      DIMENSION ICH(2),IHBIT(12)
#include "xtbnjj.h"
      PARAMETER (NBIT=32)
      PARAMETER (MSKPL = 192 , MSKPS = 256 )
      DATA LRUN /-9999/
      DATA LRUNF/16674/
      DATA IFIR / 0/
C hv bit          EA EB EBR HA HB HBR TPC ITC LC TRIG  SCAL
      DATA IHBIT/ 0,  1,  2, 7, 8,  9, 15,  5, 3,13,14, 10  /
#include "bmacro.h"
C
      IF ( IFIR.EQ.0) THEN
        IFIR = 1
        NAXTBN = NAMIND('XTBN')
C
C Load run-period specific trigger names into storage arrays
C
        TTREQ(1) = 'SiCAL_ME'
        TTREQ(2) = 'SICAL_ME'
        TTREQ(3) = 'LT_HI_LO'
        TTREQ(4) = 'LT_LO_HI'
      ENDIF
C
C Only check when the run changes.
      IF(LRUN.NE.KRUN)THEN
        LRUN  = KRUN
        IERR = 0
        KXTBN=IW(NAXTBN)
C
C Build mask of required bits from their name-->bit assignment in XTBN.
C Before run LRUNF count LCAL lumi triggers
C After  run LRUNF count SICAL lumi triggers
C
        IFIR = 1
        IF ( KRUN.LT.LRUNF) IFIR=3
        ILAS = IFIR+1
        MKREQ = 0
        IF (KXTBN.NE.0) THEN
          DO 100 ITREQ = IFIR,ILAS
            DO 110 ITBIT = 1,NBIT
              IDEF=ITABL(KXTBN,ITBIT,JXTBTN)
              INUM=ITABL(KXTBN,ITBIT,JXTBTB)
C
C XTBN is corrupted so  perform some hardwiring fixes
              IF(INUM.LT.0.OR.INUM.GT.31) THEN
                IERR = 1
                GOTO 888
              ENDIF
C
              IF(IDEF.NE.0.AND.INUM.NE.31)THEN
                ICH(1) =ITABL(KXTBN,ITBIT,JXTBBM)
                ICH(2) =ITABL(KXTBN,ITBIT,JXTBBM+1)
                MNM(1:4) = CHAINT(ICH(1))
                MNM(5:8) = CHAINT(ICH(2))
                IF(MNM.EQ.TTREQ(ITREQ)) MKREQ=IBSET(MKREQ,INUM)
              ENDIF
  110       CONTINUE
  100     CONTINUE
          IF (MKREQ.EQ.0) IERR = 1
        ELSE
C
C No XTBN bank at all -- use hardwire masks
          IERR = 1
        ENDIF
C
  888   CONTINUE
C
C Route all error conditions through hardwiring of required masks
        IF (IERR.NE.0) THEN
          IF(KRUN.LT.LRUNF)  MKREQ=2**14+2**15
          IF(KRUN.GT.LRUNF)  MKREQ=2**4
        ENDIF
C
C   Now HV bits to be on for Lumi measurements ,check SICAL bit only
C   in 93
        NTBIT= 12
        IF ( KRUN.LT.LRUNF) NTBIT = 11
        MSKHV = 0
        DO 210 IT = 1,NTBIT
           III= IHBIT(IT)
           MSKHV = IBSET(MSKHV,III)
 210    CONTINUE
      ENDIF
C identifier to be used to identify Luminosity clusters in PECO
      MSKPC = MSKPS
      IF(KRUN.LT.LRUNF)  MSKPC = MSKPL
 999  RETURN
      END
#endif
