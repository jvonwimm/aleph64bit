C-------------------- BEGIN of MAIN-program -----------------
C
C
      PROGRAM D

      INCLUDE 'DALI_CF.INC'
      PARAMETER (MP=49)
      CHARACTER *49 TP
      MORDDC=0
      VRTZDV=0.
      NTVIDT=0
      CALL DSETUP
      IFULDB=0
      CALL DQTIT(0)
1     CALL DBR1
      CALL DW_ERROR_HANDLING_STOP
      CALL DGSTOP
C
C  Resetting terminal state. Close metafile if any.
C
      CALL DGFIME
C
      CALL DWRT(' We hope you use DALI again.')
C     .............. Write without "$", to start after DALI on the next line.
      CALL DW_GET_PLATFORM_TEXT('LL',TP,MP)
      WRITE(6,1000) TWRTD0,TP
 1000 FORMAT(2A,T53,'|')
      END
C
C
C---------------------- END of MAIN-program ----------------
