      SUBROUTINE ECLOST(ICLU,ECLOSS,IER)
C-----------------------------------------------------------------------
C! Compute correction to total EcObject energy
C!     AUTHOR :D PALLIN
C!       MODIFIED : JP Albanese ---> use of j Badier routines (5/1/90)
C!   Modified :- E. Lancon             23-OCT-1991
C!          This routine is called by :
C!              ECOENT : for analysis of charged clusters
C!              ECLANA : for analysis of electrons
C!          Replace call to EBLEAK by call to ECLEAK with charge = 1
C?
C!======================================================================
#ifndef DOC
      DIMENSION STAC(6),ANGL(2),PHOT(20)
#include "ecnamc.h"
#include "eclujj.h"
#include "bcs.h"
#include "bmacro.h"
       IER = 0
C
       KECLU=IW(NAECLU)
C? GET TOTAL UNCORRECTED ENERGY
       ENER = RTABL(KECLU,ICLU,JECLE4)
C? GET THETA,PHI
       TETA = RTABL(KECLU,ICLU,JECLT4)
       PHI = RTABL(KECLU,ICLU,JECLF4)
C? Call  leakage correction(except overlap)
C? Store all energy in stack(1) for compatibility purpose with neutral a
       STAC(1) = ENER
       STAC(2) = 0.
       STAC(3) = 0.
       STAC(4) = 0.
       STAC(5) = TETA
       STAC(6) = PHI
       CORR    = 1.
       ANGL(1) = TETA
       ANGL(2) = PHI
C
       CALL ECLEAK( 0 , STAC , ANGL , 1, CORR)
       ECOR = ENER*(CORR-1.)
C? Compute cluster correction   (use Badier routine)
       CALL EBCOCL( 0 , 0 , STAC , CORR )
       ECOCL = ENER*(CORR-1.)
       ECLOSS=ECOCL+ECOR
       RETURN
       END
#endif
