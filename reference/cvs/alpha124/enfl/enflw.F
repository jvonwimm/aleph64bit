      SUBROUTINE ENFLW(IREJET)
C----------------------------------------------------------------
C! Create the ENFW section if the user wishes
C
C  Inputs:  -- Bank EFOL, EAUX for the MiniDST
C           -- Bank FWEV, FWPA for the MicroDST
C
C  Outputs: -- ALPHA section ENFW
C
C  P. Janot -- 22 august 1992
C----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
      LOGICAL lumok, hvok, first
      INTEGER runqua
      COMMON / outmic / lumok, hvok, runqua
      DATA first /.TRUE./
#include "qcde.h"
#include "qmacro.h"
C----------------------------------------------------------------
C  For the Micros
C
      ifwpa = NLINK('FWPA',0)
      IF ( ifwpa .GT. 0 ) THEN
        IF ( first ) THEN
          CALL parset
          first = .FALSE.
        ENDIF
        CALL mdstrd(lumok,hvok,runqua,ier)
        IF ( ier .EQ. 0 ) THEN
          irejet = 1
        ELSE
          irejet = 0
        ENDIF
C
      ELSE
C
C  For the Minis
C
        IF ( first .AND. xmini ) THEN
          CALL parset
          first = .FALSE.
        ENDIF
        CALL buildal
        if ( nefol .EQ. 0 ) THEN
          irejet = 0
        ELSE
          irejet = 1
        ENDIF
      ENDIF
C
C  Preselection
C
      IF ( irejet .EQ. 1 .AND. ( xmini .OR. ifwpa .GT. 0 ) ) THEN
        nchtot = klstot(1)-kfrtot(1)+ 1
        echtot = qe(lch)
        IF ( echtot .LT. echmin .OR. echtot .GT. echmax .OR.
     .       nchtot .LT. ntrack .OR. nchtot .GT. mtrack ) irejet = 0
      ENDIF
C
      RETURN
      END
#endif
