        SUBROUTINE HGINIT
C--------------------------------------------------------------
CKEY HCALDES HCAL INIT /USER
C! ini Hcal geometry - Get data base in geometry commons
C!        back-compatibility routine
C!
C!      G.Capon               Jan 88
C!      Mod: G.Catanesi       Jun 88
C-------------------------------------------------------------
#ifndef DOC
C
                PARAMETER(LDBAS=7,LRUN=1)
C
C?             read constants from Data Base
C
        CALL HRDDAF(LDBAS,LRUN,IERR)
C
        RETURN
        END
#endif
