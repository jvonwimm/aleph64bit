      PROGRAM QMAIN
CKEY PACK /INTERNAL
C----------------------------------------------------------------------
C! main program
C!                                                   H.Albrecht 20.09.88
C----------------------------------------------------------------------
C Since CERNLIB 94b , it is necessary to initialise the C run-time
C library here for VM and VMS systems , in order to run HBOOK :
C         initialisation
C
      CALL QMINIT
C
C         read next event
C
   10 CALL QMREAD
C
C         process event
C
      CALL ABRUEV(IRUN,IEVT)
      CALL QMEVNT(IRUN,IEVT)
      GO TO 10
C
      END
