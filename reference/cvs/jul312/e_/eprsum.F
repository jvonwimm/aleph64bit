      SUBROUTINE EPRSUM
C----------------------------------------------------------------------
C!  - Print Ecal summary statistics
C!     for events processed NRUNRS(4)
C!     Author:  JP.Albanese 01-06-87
C!
C!     called by : RPRSUM
C!     calls     : none
C!
C?
C!======================================================================
#ifndef DOC
#include "rlunit.h"
#include "rstatr.h"
#include "ecpric.h"
#include "estage.h"
#include "rparac.h"
#include "rflags.h"
      DIMENSION TOUR(3,3)
      CHARACTER*1 BAR(80)
      DATA BAR/80*'-'/
C--------------------------------------------------------------------
      WRITE(LOUTRL,'(''0'',80A)') BAR
C
C? Normalize statistics to total analyzed events NRUNRS(4)
      IF (NRUNRS(4) .GT. 0) THEN
        NSTOEV = NSTOEV/NRUNRS(4)
        ESTOEV = ESTOEV/FLOAT(NRUNRS(4))
        NCLUEV = NCLUEV/NRUNRS(4)
        ECLUEV = ECLUEV/FLOAT(NRUNRS(4))
        ECLUCH = ECLUCH/FLOAT(NRUNRS(4))
        EWEAEV = EWEAEV/FLOAT(NRUNRS(4))
        EWBAEV = EWBAEV/FLOAT(NRUNRS(4))
        EWEBEV = EWEBEV/FLOAT(NRUNRS(4))
        NELEEV = NELEEV/NRUNRS(4)
        NGAMEV = NGAMEV/NRUNRS(4)
C
        WRITE(LOUTRL,500) NSTOEV,ESTOEV,NCLUEV,ECLUEV,ECLUCH,EWEAEV,
     &    EWBAEV,EWEBEV,NELEEV,NGAMEV
  500   FORMAT(' ECAL Summary : ',/,
     1        1X,' Number of fired storeys...................',I6,/,
     1        1X,' Energy of fired storeys...................',F10.3,/,
     2        1X,' Number of found clusters..................',I6,/,
     2        1X,' Energy of found clusters..................',F10.3,/,
     3        1X,' Energy of tracks_associated clusters......',F10.3,/,
     3        1X,' Wire energy (Endcap A, Barrel, Endcap B)..',3F10.3/,
     3        1X,' Number of Electron Candidates ............',I6,/,
     3        1X,' Number of Gamma Candidates ...............',I6,/)
      ENDIF
      IF (JDBDRF(JULEC).GE. 1.AND.EVNTES.GT.0.) THEN
        DO 5 K = 1,3
          DO 4 J = 1,3
            DO 4 I = 1,3
              CLURES(I,J,K) = CLURES(I,J,K) / EVNTES
    4     CONTINUE
          ECLTES(K) = ECLTES(K) / EVNTES
          ECALES(K) = ECALES(K) / EVNTES
    5   CONTINUE
        WRITE (LDEBRL,69) EVNTES
   69   FORMAT(/,5X,F5.0,' Events')
        WRITE (LDEBRL,79) ECLTES,ECALES
   79   FORMAT(/,5X,'Measured stacks  ',3F10.3,/,
     +           5X,'Calculated stacks',3F10.3)
        WRITE (LDEBRL,89) (((CLURES(I,J,K),I=1,3),K=1,3),J=1,3)
   89   FORMAT(/,10X,'Sum per storey',/,
     +  3(/,5X,3F6.3,2X,3F6.3,2X,3F6.3),/)
        SOM = 0.
        DO 3 I=1,3
          DO 3 J=1,3
            TOUR(I,J) = 0.
            DO 1 K=1,3
    1       TOUR(I,J) = TOUR(I,J) + CLURES(I,J,K)
    3   SOM = SOM + TOUR(I,J)
        BARJ = 0.
        BARI = 0.
        DO 2  JK = 1,3
          BARJ = BARJ + TOUR(3,JK) - TOUR(1,JK)
          BARI = BARI + TOUR(JK,3) - TOUR(JK,1)
    2   CONTINUE
        BARJ = BARJ / SOM
        BARI = BARI / SOM
        WRITE (LDEBRL,99) (( TOUR(J,K),K=1,3),J=1,3),BARJ,BARI
   99   FORMAT(/,10X,'Sum per tower',/,3(/,5X,3F8.3),//,5X,
     +   'BARJ,BARI', 2F8.3,/)
      ENDIF
      END
#endif
