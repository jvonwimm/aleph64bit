      SUBROUTINE ITDTGO
C.
C...ITDTGO  1.02  950804  13:48                        R.Beuselinck.
C.
C!  Initialise drift time parameterisation.
C.
C.  This routine creates a linear interpolation table for computing
C.  drift time from drift distance.  The table is created using a
C.  polynomial parameterisation of drift distance from drift time.
C.  This is the initialisation entry point for ITDTIM.
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "itelec.h"
#include "itparc.h"
#include "itwirc.h"
      REAL C1(8), C2(8), C3(8), C4(8), C5(8), DMAX(8), DBIN(8),
     +     TIME(101,8), TMAX(8), TLIM(8), CWBY2(8)
      LOGICAL TURN(8), OK
      EQUIVALENCE (DVELIT(1,1), C1(1))
      EQUIVALENCE (DVELIT(1,2), C2(1))
      EQUIVALENCE (DVELIT(1,3), C3(1))
      EQUIVALENCE (DVELIT(1,4), C4(1))
      EQUIVALENCE (DVELIT(1,5), C5(1))
      EXTERNAL RNDM
C
C--  Statement functions.
C--
      DIST(TX,LL) = C1(LL)*TX + C2(LL)*TX**2 + C3(LL)*TX**3 +
     +              C4(LL)*TX**4 + C5(LL)*TX**5
      GRAD(TX,LL) = C1(LL) + 2*C2(LL)*TX + 3*C3(LL)*TX**2 +
     +              4*C4(LL)*TX**3 + 5*C5(LL)*TX**4
      PRED(AA,BB,CC,DD,EE) = EE + (DD-EE)*(CC-BB)/(AA-BB)
C
C--  Complete interpolation table for each layer.
C--
      TOLT = 0.5*TDCRIT
      DO 200 L=1,8
C
C--     Compute the maximum measureable drift time and the corresponding
C--     drift distance for layer.
C--
        TMAX(L) = TDCRIT*(ITTDCH(L)-ITTDCL(L)+1)
        TLIM(L) = TMAX(L)
        T1 = 0.
        T2 = TMAX(L)
        CWBY2(L) = 0.5*CELWIT(L)
C
C--     Make sure we don't take the parameterisation too far if the
C--     TDCs are set for multi-bunch running.  Check that T2 does
C--     not go much beyond the edge of the drift cell.
C--
        DO 150 ITIM=ITTDCL(L),ITTDCH(L)
          TT = ITIM*TDCRIT
          DD = DIST(TT,L)
          IF (DD.GT.1.1*CWBY2(L)) THEN
            T2 = TT
            TMAX(L) = TT
            DMAX(L) = DD
            GO TO 155
          ENDIF
  150   CONTINUE
C
C--     Find if the drift relation turns over (has a maximum) earlier
C--     than the maxmimum drift time.
C--
  155   IF (GRAD(T2,L).GT.0.) THEN
          TURN(L) = .FALSE.
          DMAX(L) = DIST(T2,L)
        ELSE
    5     TT = 0.5*(T1+T2)
          VEL = GRAD(TT,L)
          IF (VEL.GT.0.) THEN
            T1 = TT
          ELSE IF (VEL.LT.0.) THEN
            T2 = TT
          ELSE
            T1 = TT
            T2 = TT
          ENDIF
          IF (T2-T1 .LT. TOLT) THEN
            DMAX(L) = DIST(TT,L)
            TMAX(L) = TT
            TURN(L) = .TRUE.
          ELSE
            GO TO 5
          ENDIF
        ENDIF
C
C--     Now build an interpolation table up to the practical limit,
C--     i.e. smallest of the turn-over point or maximum tdc value.
C--
        DBIN(L) = DMAX(L)/100.
        TOLD = DBIN(L)/100.
        DO 100 I=1,101
          DWANT = (I-1)*DBIN(L)
          T1 = 0.
          T2 = TMAX(L)
          D1 = 0.
          D2 = DIST(T2,L)
   10     TT = PRED(D1,D2,DWANT,T1,T2)
          DD = DIST(TT,L)
          IF (ABS(DD-DWANT).LT.TOLD) THEN
            TIME(I,L) = TT
          ELSE IF (DD.LT.DWANT) THEN
            D1 = DD
            T1 = TT
            GO TO 10
          ELSE
            D2 = DD
            T2 = TT
            GO TO 10
          ENDIF
  100   CONTINUE
  200 CONTINUE
      RETURN
#endif
      ENTRY ITDTIM(LAY, D, T, OK)
C.
C...ITDTIM  1.10  930812  13:37                           R.Beuselinck
C.
C!  Compute drift time from drift distance.
C.
C.  Arguments:
C.  LAY [S,I,INTE] : Layer number.
C.  D   [S,M,REAL] : Drift distance, .LE. max for layer on return (cm).
C.  T   [S,O,REAL] : Drift time (ns).
C.  OK  [S,O,LOGI] : .FALSE. if D is outside maximum tdc range.
C.
C.  Compute the drift time using a linear interpolation table for the
C.  particular wire layer.
C.
C-----------------------------------------------------------------------
#ifndef DOC
C
      IF (D .GT. CWBY2(LAY)) THEN
C
C--     Reject points that are outside the cell width.
C--
        OK = .FALSE.
        T  = 0.
        D  = 0.
      ELSE IF (D .LE. DMAX(LAY)) THEN
C
C--     Normal case. Lookup via table.
C--
        OK = .TRUE.
        DA = ABS(D)
        IDB = DA/DBIN(LAY) + 1
        IDB = MIN(IDB,100)
        DD = DA - (IDB-1)*DBIN(LAY)
        T = (TIME(IDB+1,LAY)-TIME(IDB,LAY))*DD/DBIN(LAY) + TIME(IDB,LAY)
      ELSE IF (TURN(LAY)) THEN
C
C--     For points beyond the turn over point but within the cell
C--     distribute the time randomly between the turn over point and
C--     the end of the TDC range.
C--
        OK = .TRUE.
        T  = TMAX(LAY) + RNDM(DUM)*(TLIM(LAY) - TMAX(LAY))
      ELSE
        OK = .FALSE.
        T  = 0.
        D  = 0.
      ENDIF
      END
#endif
