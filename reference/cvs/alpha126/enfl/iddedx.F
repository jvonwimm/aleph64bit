      SUBROUTINE iddedx(itk,hypele,hyppio,hypkao,hyppro)
C----------------------------------------------------------------------
C! Return the dE/dx for four mass hypotheses in the Micro DSTs
C
C  Input  : itk, charged particle ALPHA index in the ENFW section
C                between kfrtot(1) and klstot(2)
C
C  Output : (I_meas-I_exp)/sigma_exp
C              o Hypele : electron mass hypothesis
C              o Hyppio : pion     mass hypothesis
C              o Hypkao : kaon     mass hypothesis
C              o Hyppro : proton   mass hypothesis
C
C  Patrick Janot -- 12 Jan 1995
C----------------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
      IF ( itk .LT. kfrtot(1) .OR. itk .GT. klstot(2) ) THEN
        hypele = 999.
        hyppio = 999.
        hypkao = 999.
        hyppro = 999.
      ELSE
        hypele = dedxhy(1,itk-kfrtot(1)+1)
        hyppio = dedxhy(2,itk-kfrtot(1)+1)
        hypkao = dedxhy(3,itk-kfrtot(1)+1)
        hyppro = dedxhy(4,itk-kfrtot(1)+1)
        IF ( hypele .GT. 32.766 .AND.
     .       hyppio .GT. 32.766 .AND.
     .       hypkao .GT. 32.766 .AND.
     .       hyppro .GT. 32.766       ) THEN
          hypele = 999.
          hyppio = 999.
          hypkao = 999.
          hyppro = 999.
        ENDIF
      ENDIF
C
  999 RETURN
      END
#endif
