      SUBROUTINE ASPRUN
C ---------------------------------------------------------------------
C - F.RANJARD - 850326
C! Loop over events
C. - called by    GALEPH                                 from this .HLB
C. - calls        ASIEVE, ASPEVE, ASCEVE, ASCRUN, ASRRUN from this .HLB
C.                ABRSEL, ABWSEL                         from ALEPHLIB
C -------------------------------------------------------------------
C ----------------------------------------------------------------------
C
C - initialize event in GEANT framework
C
 1    CONTINUE
      CALL ASIEVE
C
C - Process this event : following the flags set
C   Go to next event if End Of Event
C   Stop if End Of Run.
C
      CALL ASPEVE
C
C - Close this event : statistics, trigger, analysis
C
      CALL ASCEVE
      GOTO 1
C
       END
