      SUBROUTINE MINBLM
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Build Mini banks for Monte-Carlo.
C
C     Author: Stephen Haywood      06-Jan-93
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "bmacro.h"
C
C++   Fill reduced FKIN bank.
C
      IF (NLINK('DTMC',0).GT.0) THEN
         CALL MINLIS('DTMC')
      ELSE
         CALL MINTMC
      ENDIF
C
C++   Fill reduced FVER bank.
C
      IF (NLINK('DVMC',0).GT.0) THEN
         CALL MINLIS('DVMC')
      ELSE
         CALL MINVMC
      ENDIF
C
C++   Fill reduced FZFR bank.
C
      IF (NLINK('DFMC',0).GT.0) THEN
         CALL MINLIS('DFMC')
      ELSE
         CALL MINFMC
      ENDIF
C
C++   Add further useful banks.
C
      CALL MINLIS('KEVH')
      CALL MINLIS('ASEV')
      IF (LROWS(NLINK('FPOL',0)).GT.0) CALL MINLIS('FPOL')
      IF (LROWS(NLINK('PASL',0)).GT.0) CALL MINLIS('PASL')
      IF (LROWS(NLINK('PITM',0)).GT.0) CALL MINLIS('PITM')
      IF (LROWS(NLINK('FPOI',0)).GT.0) CALL MINLIS('FPOI')
      IF (LROWS(NLINK('PEMH',0)).GT.0) CALL MINLIS('PEMH')
      IF (LROWS(NLINK('PHMH',0)).GT.0) CALL MINLIS('PHMH')
C
C++   Add old matching banks, if new ones don't exist.
C
      IF (NLINK('PASL',0).LE.0 .OR. NLINK('PITM',0).LE.0) THEN
         IF (IW(NAMIND('IPJT')).GT.0) CALL MINLIS('IPJT')
         IF (IW(NAMIND('IRJT')).GT.0) CALL MINLIS('IRJT')
         IF (LROWS(NLINK('PTMA',0)).GT.0) CALL MINLIS('PTMA')
         IF (LROWS(NLINK('PTML',0)).GT.0) CALL MINLIS('PTML')
      ENDIF
C
      RETURN
      END
#endif
