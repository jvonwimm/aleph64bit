*DK hcwsum
      SUBROUTINE HCWSUM
C--------------------------------------------
C
C!      Print statistics
C!
C!      Authors     :G.Catanesi,G.Zito  86/12/03
C!      Modified by :G.Catanesi  87/07/10,87/11/19
C!
C!       -Called by : ASCRUN      from this .HLB
C!   -Calls     : none
C -------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "hccoun.h"
#include "jobcom.h"
      CHARACTER*3 TONOF(2)
      DATA TONOF / 'ON ', 'OFF' /
C ---------------------------------------------------------
      WRITE (LOUTIO,'(//3X,''+++HCWSUM+++ HCAL Run Summary '')')
      IF (NHCC05.EQ.0) THEN
         WRITE (LOUTIO,'(//3X,''NO track in HCAL'')')
      ELSE
         HCC01 = FLOAT(NHCC01)/FLOAT(NHCC05)
         HCC02 = FLOAT(NHCC02)/FLOAT(NHCC05)
         HCC03 = FLOAT(NHCC03)/FLOAT(NHCC05)
         HCC04 = FLOAT(NHCC04)/FLOAT(NHCC05)
         HCEAVE = HCEAVE/FLOAT(NHCC05)
         HCANST = HCANST / FLOAT(NHCC05)
C
         WRITE (LOUTIO,'(/10X,''HCAL Run Conditions'')')
         IF (FPARJO) THEN
            WRITE (LOUTIO,'(3X,''GEANTINO parametrisation is ON'')')
         ELSE
            I = ICHCJO(2) + 1
            WRITE (LOUTIO,'(3X,''Parametrisation is '',A)') TONOF(I)
         ENDIF
         I = ICHCJO(3) + 1
         WRITE (LOUTIO,'(3X,''Dead zones are '',A)') TONOF(I)
         I = ICHCJO(4) + 1
         WRITE (LOUTIO,'(3X,''Induction effect is '',A)') TONOF(I)
         I = ICHCJO(5) + 1
         WRITE (LOUTIO,'(3X,''Tube inefficiency is '',A)') TONOF(I)
          I = ICHCJO(6) + 1
         WRITE(LOUTIO,'(3X,''Dead tubes Killing is '',A)')TONOF(I)
C
         WRITE(LOUTIO,500)NHCC05,HCC01,HCC02,HCC03,HCC04,HCANST,HCEAVE
      ENDIF
      RETURN
  500 FORMAT(/3X,'HCAL run summary',I5,' events went through HCAL'/3X,
     +'Average number of track elements / event ',T50,F10.2/ 3X,
     +'Average number of tube segments  / event ',T50,F10.2/ 3X,
     +'Average number of fired tubes    / event ',T50,F10.2/ 3X,
     +'Average number of fired storeys  / event ',T50,F10.2/ 3X,
     +'Average number of streamers  / event ',T50,F10.2/3X,
     +'Average deposited energy in HCAL / event ',T50,F10.2/)
      END
#endif
