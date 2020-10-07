      SUBROUTINE ADBVER (IVERS,IDATE)
C.---------------------------------------------------------------------
CHEY ALEF DBASE STC LTC VERSION / USER
C  F.Ranjard - 881110
C              900111 : introduce LTC version
C! Return  DAF version# and date of last change
C
C  Output: IVERS   = DAF version version
C          IDATE   = DAF date of last change
C
C.---------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "adbsjj.h"
#include "bmacro.h"
C --------------------------------------------------------------------
      LDBAS = JUNIDB(0)
C - get version # and date of last change of the DAF
C
      IND = MDARD (IW,LDBAS,'ADBS',0)
      IF (IND .EQ. 0) THEN
         IDATE = 0
         IVERS = 0
      ELSE
         IDATE = ITABL(IND,1,JADBDC)
         IVERS = ITABL(IND,1,JADBVN)
      ENDIF
      LOUT = IW(6)
      IF (LOUT .GT. 0) THEN
         WRITE (LOUT,777) IVERS,IDATE,LDBAS
 777     FORMAT(/1X,'+++ADBVER+++ ADBSCONS DAF version ',I5,3X,
     &          'date of last change ',I9,3X,'opened on unit#',I3)
      ENDIF
      RETURN
C
      END
#endif
