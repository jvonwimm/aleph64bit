         SUBROUTINE HFNDMD(INTET,INPHI,NSTAK,ISUBC,MODUL,
     *   IOVRL)
C-----------------------------------------------------------------
CKEY HCALDES HCAL TOWER MODULE  / USER
C! get module #, subcomp # from I,J,K for Hcal
C!
C!      G.Capon               date : 861001
C! input :
C!         INTET : theta index of storey
C!         INPHI : phi     "        "
C!         NSTAK : stack number
C! output :
C!        ISUBC  : subcomp # (1=endcapA,2=barrel,3=endcapB)
C!        MODUL  : phys module # (1-6 for endcaps,1-24 for barrel)
C!        IOVRL  : position flag (if ne 0 storey is splitted between
C!                       endcap and barrel jonction region)
C!
C-----------------------------------------------------------------
#ifndef DOC
#include "hcgeom.h"
C
         NREG=IHCREG(INTET)
         ISUBC=2
         IF(INTET.LE.13) ISUBC=1
         IF(INTET.GE.50) ISUBC=3
C
         IF (ISUBC.EQ.2) THEN
              MODUL=(INPHI-1)/4+1
              ELSE
              NTSEX=2**(NREG+1)
              MODUL=(INPHI-1)/NTSEX+1
              ENDIF
C
C?        flag stories in barrel/endcap overlap region
C
          IOVRL=0
          IF(INTET.GE.14.AND.INTET.LE.16) IOVRL=1
          IF(INTET.LE.49.AND.INTET.GE.47) IOVRL=1
C
          RETURN
          END
#endif
