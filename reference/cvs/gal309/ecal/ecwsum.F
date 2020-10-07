*DK ecwsum
      SUBROUTINE ECWSUM
C.----------------------------------------------------------------
C M.Rumpf may 86
C!   Modified :- E. Lancon              3-DEC-1990
C! ECAL Run summary
C  -Called by ASCRUN
C.----------------------------------------------------------------
#ifndef DOC
      SAVE
#include "ecstat.h"
#include "ehcond.h"
#include "edcond.h"
#include "iocom.h"
#include "edpara.h"
C
      CHARACTER * 16 TSHMET
      CHARACTER*10  REGNAM(3)
      DATA REGNAM / 'End_Cap A', 'Barrel', 'End_Cap_B'/
C
C
C Print Run Conditions
C
      TSHMET = TIRAGE
      IF(TPARAM.EQ.'NO') TSHMET = 'NONE'
      WRITE(LOUTIO,100)
  100 FORMAT(//,1X,' ECAL Run Conditions ')
      WRITE(LOUTIO,102)
  102 FORMAT(/1X,'    Analog Signals',/)
      WRITE(LOUTIO,103)
  103 FORMAT(16X,' Cut track element   Energy dep. point ',
     &          ' Shower gen. Method  Track seg. Method ',
     &          ' Parametrisation ')
      WRITE(LOUTIO,101) TCUTRK,TSMEAR,TSHMET,TEDEPO,TPARAM
      WRITE(LOUTIO,104)
  104 FORMAT(1X,'    Digitizations ',/)
      WRITE(LOUTIO,105)
  105 FORMAT(16X,' Zone for Noise      Mix Real events   ',
     &          ' Save Noise + Gain   Zero Sup. Method  ')
      WRITE(LOUTIO,106) EDNOXT,EDMIXR,EDSAVG,EDZSUP
  101 FORMAT(18X,5(4X,A16)/)
  106 FORMAT(18X,4(4X,A16)/)
      WRITE(LOUTIO,107)
  107 FORMAT(18X,' Zero Sup. values in MeV for Stacks 1 2 and 3')
      DO 20 IREG =  1,  3
        WRITE(LOUTIO,108) REGNAM(IREG), (EDZTHR(ISTK,IREG),ISTK=1,3)
   20 CONTINUE
  108 FORMAT(25X,A10,3F8.0)
C
C
C Compute average quantities/event
C Print some statistics
C
      IF(NECONT(1).EQ.0) GO TO 999
      DO 10 I=2,4
   10 NECONT(I) = NECONT(I) / NECONT(1)
      DO 11 I=4,6
   11 ECCONT(I) = ECCONT(I) / NECONT(1)
      ECCONT(7) = ECCONT(7) / NECONT(1)
      ECCONT(8) = ECCONT(8) / NECONT(1)
      ECCONT(9) = SQRT((ECCONT(8) - ECCONT(7)**2)/ECCONT(7))
C
      WRITE (LOUTIO,200) NECONT(1)
  200 FORMAT(/,1X,' ECAL Run Statistics for Analog Signal:',I5,
     &       ' events entering ECAL'/)
      WRITE (LOUTIO,201) (NECONT(I),I=2,7),(ECCONT(I),I=4,6),ECCONT(9)
  201 FORMAT(5X,'Average number of Geant  track elements  / event',I10/
     &       5X,'Average number of shower track elements  / event',I10/
     &       5X,'Average number of fired towers           / event',I10/
     &       5X,'Total number of tr.elem out of  planes definition',I9/
     &       5X,'Total number of tr.elem out of columns definition',I9/
     &       5X,'Total number of tr.elem out of rows    definition',I9/
     &    5X,'Average energy deposited in stack 1 (Gev)/ event',F14.3/
     &    5X,'Average energy deposited in stack 2 (Gev)/ event',F14.3/
     &    5X,'Average energy deposited in stack 3 (Gev)/ event',F14.3/
     &    5X,'Delta = sqrt((<e**2>-<e>**2)/<e>)               ',F14.3/)
      RETURN
 999  WRITE (LOUTIO,'(/ ''    No energy deposited in ECAL '')')
      END
#endif
