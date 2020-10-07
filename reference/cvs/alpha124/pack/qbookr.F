      SUBROUTINE QBOOKR(ID,NAME,NVAR,USNAM)
CKEY HIST /USER
C----------------------------------------------------------------------
C! Book Ntuples with 2 more variables to store Run/Event numbers
C Authors  V. Journe - J. Boucrot       10-Nov-1988
C Called from USER
C Calls  HBOOKN                                    from HBOOK4 Lib
C  Input arguments :
C    ID  =  Ntuple identifier ( must be # 0 )
C   NAME =  Ntuple name ( up to 80 characters )
C   NVAR =  Number of variables per element of the Ntuple
C  USNAM =  array of user-defined names of the NVAR variables
C           ( 8 characters maximum , as prescribed by HBOOK4 )
C----------------------------------------------------------------------
C Here is prepared the facility to store the event and run number
C   automatically as the two last variables of Ntuple # ID .
C To benefit from this facility , the filling of the Ntuple
C   must be be done by the routine QHFR .
C LIMITATION : Do not define Ntuples with more than 80 variables
C----------------------------------------------------------------------
#ifndef DOC
      SAVE ICALL
#include "qcdesh.h"
      PARAMETER (NVMAX = 80 , NTMAX = 100 , NVNTU = 2 )
C Internal common to store the work bank index :
      COMMON  / QHNTUR / KWNTUW
      CHARACTER*(*) NAME, USNAM(*)
      CHARACTER*8 NTNAM(NVMAX+2)
      DATA ICALL /0/
#include "bmacro.h"
C---------------------------------------------------------------------
C Book work bank on first call :
C
      IF (ICALL.EQ.0) THEN
        KWNTUW=0
        LENW=NVNTU*NTMAX+LMHLEN
        CALL WBANK(IW,KWNTUW,LENW,*990)
        IW(KWNTUW+LMHCOL)=NVNTU
        IW(KWNTUW+LMHROW)=0
        IW(KWNTUW-3)=INTCHA('NTUW')
      ENDIF
C
      ICALL=ICALL+1
      IF (ICALL.GT.NTMAX) GO TO 900
      IF (NVAR.GT.NVMAX)  GO TO 910
C Store ID and NVAR in the work bank :
      KXNTU=KNEXT(KWNTUW)
      IW(KXNTU+1)=ID
      IW(KXNTU+2)=NVAR
      IW(KWNTUW+2)=IW(KWNTUW+2)+1
C
C Add the names 'KRUN' and 'KEVT' to the user list :
C
      DO 10 I=1,NVAR
   10 NTNAM(I)=USNAM(I)
      NTNAM(NVAR+1)='KRUN'
      NTNAM(NVAR+2)='KEVT'
C
C Now book the Ntuple with NVAR+2 variables :
C
      CALL HBOOKN(ID,NAME,NVAR+2,'ALPHA',10000,NTNAM)
      GO TO 999
C
C Error messages :
C
  900 CALL QMTERM ('_HBOOKR_ Too many Ntuples booked ')
      GO TO 999
C
  910 CALL QMTERM ('_HBOOKR_ Too many variables in Ntuple')
      GO TO 999
C
  990 CALL QMTERM ('_HBOOKR_ No room to book work bank')
C
  999 RETURN
      END
#endif
