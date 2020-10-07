      SUBROUTINE RPREDA(IREJ)
C----------------------------------------------------------------------
C!  - Prepare raw data by subdetector
C!
C!  Author       J. Bunn
C!
C!  Modified     S. Orteu    27-FEB-1987
C!  Modified :- E. Lancon             14-JUN-1991
C!              remove VREPDA it is called by RECEVT now
C!
C!  Output   IREJ = 0      Event OK
C!                <>0      Event rejected
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rparac.h"
#include "rflags.h"
#include "rcurnt.h"
      LOGICAL RMOD
C--------------------------------------------------------------------
C Prepare subdetector raw data
C
      IREJ = 0
      IER=0
C
C?   In any case add to E list fit banks
C
      CALL BLIST(IW,'E+','FRFTFRTLFTCLFICLFVCLFRID')
C...  ITC
      IF (FDETRF(JULIT)) THEN
        CALL RNXMOD(MODPRI,RMOD)
        CALL IPREDA
      ENDIF
C...  TPC
      IF (FDETRF(JULTP)) THEN
        CALL RNXMOD(MODPRT,RMOD)
        CALL TPREDA
      ENDIF
C...  ECAL
      IF (FDETRF(JULEC)) THEN
        CALL RNXMOD(MODPRE,RMOD)
        CALL EPREDA
      ENDIF
C...  EFlow
      IF (FDETRF(JULEF)) THEN
        CALL EMSKEI
      ENDIF
C...  LCAL
      IF (FDETRF(JULLC)) THEN
        CALL RNXMOD(MODPRL,RMOD)
        CALL LPREDA
      ENDIF
C...  SATR
      IF (FDETRF(JULSA)) THEN
        CALL RNXMOD(MODPRS,RMOD)
        CALL SPREDA
      ENDIF
C...  HCAL
      IF (FDETRF(JULHC)) THEN
        CALL RNXMOD(MODPRH,RMOD)
        CALL HPREDA
      ENDIF
C...  MUONS
      IF (FDETRF(JULMU)) THEN
        CALL RNXMOD(MODPRM,RMOD)
        IF(IRUNRC.LT.2000) THEN
C Monte Carlo data
          CALL MPREDM
        ELSE
C Real data
          CALL MPREDG
        ENDIF
      ENDIF
C...  BOMs
      IF (FDETRF(JULBC)) THEN
        CALL RNXMOD(MODBCA,RMOD)
        CALL BPREDA
      ENDIF
C...  SICAL
      IF (FDETRF(JULSI)) THEN
        CALL RNXMOD(MODPSI,RMOD)
        CALL SIPREDA
      ENDIF
C
      CALL RNXMOD(MODEND,RMOD)
      CALL USPRDA(IREJ)
      IF(FDETRF(JULFA))CALL RFALPD(IREJ)
C--------------------------------------------------------------------
      END
#endif
