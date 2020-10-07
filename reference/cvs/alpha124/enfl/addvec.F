      SUBROUTINE ADDVEC(VEC, IPARTK, LINK1, LINK2, WEIGHT)
C----------------------------------------------------------------
C!  Store particles in class KENFLW (i.e. -3)
C   ipartk --  1 : charged particles
C              2 : V0s
C              3 : photons
C              4 : neutral hadrons
C              5 : lumi + low part of ECAL/HCAL
C              6 : SiCAL clusters
C              7 : killed clusters (Weight=1 : ECAL
C                                   Weight=0 : HCAL)
C   link1  --    : original JULIA index
C   link2  --    : JULIA index of the sister (V0 tracks only)
C   weight --    : prop. of ECAL energy (neutral hadrons only)
C
C  Patrick Janot -- 19 Apr 1990
C
C  Modif -- 01 oct 1990. store temporary particles in kreco
C           13 jun 1992. use banks instead of QVEC
C----------------------------------------------------------------
#ifndef DOC
#include "parcut.h"
#include "parabank.h"
#include "qcde.h"
#include "qhac.h"
#include "qmacro.h"
C----------------------------------------------------------------
      IF ( vec(4) .LE. 0. ) RETURN
C
C -- Prevents from negative mass squared
C
      IF ( idbg .GE. 1 ) CALL looses('NEGAMASS',1)
      xmomen = SQRT ( vec(1)**2 + vec(2)**2 + vec(3)**2 )
      IF ( xmomen .GT. vec(4) ) THEN
        vec(1) = vec(1) * vec(4) / xmomen
        vec(2) = vec(2) * vec(4) / xmomen
        vec(3) = vec(3) * vec(4) / xmomen
        xmomen = SQRT ( vec(1)**2 + vec(2)**2 + vec(3)**2 )
        vec(4) = vec(4) + 0.0001
        IF ( idbg .GE. 1 ) CALL looses('NEGAMASS',ipartk+1)
      ENDIF
      IF ( ipartk .LE. 2 ) THEN
        xmass2 = 0.1395675**2
      ELSEIF ( ipartk .EQ. 3 ) THEN
        xmass2 = 0.
      ELSE
        xmass2 = AMIN1( vec(4)**2 - xmomen**2, qelep**2/4. )
      ENDIF
C
C -- Prevents from too high momenta
C
      IF ( ipartk .LE. 6 ) THEN
        factor = SQRT ( qelep**2/4.-xmass2 ) / xmomen
        IF ( factor .LT. 1. ) THEN
          xmom2 = 0.
          DO ivec = 1 , 3
            vec(ivec) = vec(ivec) * factor
            xmom2 = xmom2 + vec(ivec)**2
          ENDDO
          xmomen = SQRT( xmom2 )
          vec(4) = SQRT( xmomen**2 + xmass2 )
        ENDIF
      ENDIF
C
C -- Fills the appropriate bank:
C
      IW(ifwvc(ipartk)+lmhlen) = IW(ifwvc(ipartk)+lmhlen) + 1
      nfwvc = IW(ifwvc(ipartk)+lmhlen)
      DO iv = 1 , 4
        RW(ifwvc(ipartk)+lmhlen+kfwvc*(nfwvc-1)+iv) = vec(iv)
        RW(ifwvc(ipartk)+lmhlen+iv) =
     .  RW(ifwvc(ipartk)+lmhlen+iv) + vec(iv)
      ENDDO
      RW(ifwvc(ipartk)+lmhlen+kfwvc*(nfwvc-1)+5) = weight
      IW(ifwvc(ipartk)+lmhlen+kfwvc*(nfwvc-1)+6) = link1
      IW(ifwvc(ipartk)+lmhlen+kfwvc*(nfwvc-1)+7) = link2
C
      RETURN
      END
#endif
