*DK tordev
      SUBROUTINE TORDEV(IEVNT,IRET)
C-----------------------------------------------------------------------
C!  Read the event header and track element bank for this event
C
C  Called from:  TSTEER
C  Calls:        TOEVIN, BDROP, NAMIND
C
C  Inputs:   PASSED:    --IEVNT, event number to be read
C            READ:      --list 'E', track elements, event header;
C                         unit 33.  Track element bank 'TPTE':
C                         IW(INDT+1) = num words per track elt
C                         IW(INDT+2) = num track elements
C                         KTE = INDT + 2 + (IELT-1)*IW(INDT+1)
C                           IW(KTE+1) = track id number
C                           RW(KTE+1) = X at start point
C                           RW(KTE+1) = Y "    "     "
C                           RW(KTE+1) = Z "    "     "
C                           RW(KTE+1) = dX/dS at start point
C                           RW(KTE+1) = dY/dS  "   "     "
C                           RW(KTE+1) = dZ/dS  "   "     "
C                           RW(KTE+1) = absolute value of momentum
C                           RW(KTE+1) = path length
C                           RW(KTE+1) = TOF at start point
C                           RW(KTE+1) = rest mass
C                           RW(KTE+1) = charge (floating)
C
C  Outputs:  PASSED:    --IRET, return flag indicating whether event
C                               was read successfully
C
C  D. DeMille
C
C-----------------------------------------------------------------------
#include "bcs.h"
#include "tpcond.h"
C
      LOGICAL LDBE2
      DATA ICALL/0/
C
C  Debug level
C
      ICALL = ICALL + 1
      LDBE2 = ( NTPCDE .GE. 2 .AND. ICALL .LE. NCALDE )
C
      IRET = 0
C
C  If we're not starting from the first event, read in and dump the
C  events before this one
C
      IF ( ICALL .EQ. 1 .AND. IEVNT .GT. 1 ) THEN
         DO 1 J = 1, IEVNT-1
            CALL TOEVIN(33,KFLAG)
            IF ( KFLAG .EQ. 1 ) GOTO 998
            CALL BDROP(IW,'E')
 1       CONTINUE
      ENDIF
C
C  Read in and keep the event header and track element bank for
C  this event
C
      CALL TOEVIN(33,KFLAG)
      IF ( KFLAG .EQ. 1 ) GOTO 998
C
C  Debug stuff
C
      NAMI = NAMIND('TPTE')
      INDT = IW(NAMI)
C
      NAMI = NAMIND('EVEH')
      INDEVH = IW(NAMI)
C
      IBGN = INDT + 2
      NTRELE = IW(INDT+2)
      NWPT = IW(INDT+1)
C
      IF ( LDBE2 ) THEN
         WRITE(6,101) IEVNT,RW(INDEVH+IW(INDEVH+1)+1)
         WRITE(6,106)
         DO 20 I=1,NTRELE
            IBGNI = IBGN + (I-1)*NWPT
            WRITE(6,107)I,(RW(IBGNI+J),J=2,12),IW(IBGNI+1)
 20      CONTINUE
      ENDIF
C
      RETURN
C
C  Problem with BOS read
C
 998  WRITE(6,110) IEVNT
      IRET = 1
      RETURN
C_______________________________________________________________________
C
 101  FORMAT('1','   Event ',I5,'   W ',F7.2, ' GeV')
 106  FORMAT(' NTRELE',4X,'X',8X,'Y',8X,'Z',7X,'DX/DS',5X,'DY/DS',5X,
     1       'DZ/DS',5X,'PTOT',7X,'S',9X,'TOF',6X,'MASS',4X,'CHARGE',2X,
     2       'NTRK')
 107  FORMAT(3X,I3,3(2X,F7.2),4(2X,F8.3),2(2X,F8.2),2X,F8.4,2X,
     1       F4.0,4X,I4)
 110  FORMAT(' PROBLEM WITH BOS READ OF EVENT HEADER/TRACK ELEMENTS',I4)
C
      END
