*DK ehibos
      SUBROUTINE EHIBOS
C.----------------------------------------------------------------------
C Y.Karyotakis M.Rumpf
C! ECAL bos banks init
C  - Called by ECHIT
C  - Calls     BKFMT,BLIST
C.______________________________________________________________________
#ifndef DOC
      SAVE
      PARAMETER (NTTD = 1000)
#include "bcs.h"
#include "ewires.h"
#include "ecnamc.h"
#include "eshijj.h"
#include "ewhijj.h"
C -------------------------------------------------------------------
C
C   Define BOS banks for EC signals
C   Drop them if they exist and then book them again
C
      IDRP = IW(NAESHI)
      IF (IDRP.NE.0) CALL BDROP (IW,'ESHIEWHI')
C
      CALL ALBOS('ESHI',0,NTTD*LESHIA+LMHLEN,KESHI,IGARB)
      IW ( KESHI+LMHCOL) = LESHIA
C
      CALL ALBOS('EWHI',0,NTTD*LEWHIA+LMHLEN,KEWHI,IGARB)
      IW ( KEWHI+LMHCOL ) = LEWHIA
C
      CALL BLIST(IW,'E+','ESHI')
      CALL BLIST(IW,'T+','EWHIETHTEWHT')
C
      END
#endif
