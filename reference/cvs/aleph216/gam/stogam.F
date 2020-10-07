          SUBROUTINE STOGAM(IGAM , NSTOR , LSTOR, NSMXS)
C.----------------------------------------------------------------------
CKEY GAMJUL USER LIST STOREYS/GAMJUL
C   J.C.Brient      Creation  6/11/91
C!  LOAD list of storeys for photon IGAM of the last pass gampec
C   Input :
C           IGAM    photon number of the last gampec pass INTEGER
C           NSMXS   max. number of storey in photon       INTEGER
C   Output:
C           NSTOR   number of storeys in photon           INTEGER
C           LSTOR   list of PEST storeys in photon        INTEGER
C           lstor(NSMXS)
C   Calls: None
C   Called by USER
C.----------------------------------------------------------------------
#ifndef DOC
#include "gastin.h"
        DIMENSION LSTOR(NSMXS)
        N1= NNSTGA(IGAM)
        MM = MIN(NSMXS,NSTTTT)
        IF(N1     .GT. MM  ) N1= MM
        NSTOR = NNSTGA(IGAM)

        DO I = 1 , N1
          LSTOR(I) = LGASTO(IGAM,I)
        ENDDO
        RETURN
        END
#endif
