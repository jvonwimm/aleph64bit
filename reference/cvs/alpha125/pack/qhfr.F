       SUBROUTINE QHFR(ID,X)
CKEY HIST /USER
C----------------------------------------------------------------------
C! Fill Ntuples adding the Run and Event # as two last variables
C Authors  V. Journe - J. Boucrot       10-Nov-1988
C Called from USER
C Calls  HFN                                    from HBOOK4 Lib
C  Input arguments :
C  ID  =  Ntuple identifier ( must be # 0 )
C   X  =  1-Dim array containing the NVAR values for this filling
C----------------------------------------------------------------------
C   To benefit from this facility , the booking of the Ntuple
C must have been done by the routine QBOOKR (from ALPHA package ).
C LIMITATION : Do not define Ntuples with more than 80 variables
C----------------------------------------------------------------------
#ifndef DOC
#include "qcdesh.h"
C Internal common to store the work bank index :
        COMMON  / QHNTUR / KWNTUW
        PARAMETER (NVMAX = 80 , NVNTU = 2 )
        REAL X(*) , XX (NVMAX+2)
#include "bmacro.h"
C----------------------------------------------------------------------
C Find the number of variables of this Ntuple , from work bank :
        IF (KWNTUW.EQ.0) GO TO 999
        DO 1 I=1,LROWS(KWNTUW)
           IF (IW(KROW(KWNTUW,I)+1).EQ.ID) GO TO 2
 1      CONTINUE
        GO TO 999
C
 2      NVAR=IW(KROW(KWNTUW,I)+2)
C
        CALL UCOPY(X,XX,NVAR)
C
C Put NRUN and NEVT into the Ntuple last 2 variables :
C
        XX(NVAR+1)=FLOAT(KRUN)
        XX(NVAR+2)=FLOAT(KEVT)
C
C Fill the Ntuple :
C
        CALL HFN(ID,XX)
C
 999    RETURN
        END
#endif
