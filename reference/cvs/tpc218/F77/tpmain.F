*DK tpmain
      PROGRAM TPMAIN
C---------------------------------------------------------------------
C   Main program for TPC simulation
C---------------------------------------------------------------------
C
      PARAMETER (IWL=800000)
      COMMON /BCS/IW(IWL)
      DIMENSION RW(IWL)
      EQUIVALENCE (IW(1),RW(1))
C
      CALL TPCVER(VERTPC)
      WRITE(6,1000) VERTPC
 1000 FORMAT(//,5X,70('*')/5X,'*',68X,'*',
     &        /5X,'*',25X,'T P C S I M   V ',F4.2,23X,'*',
     &        /5X,'*',68X,'*',/5X,70('*'),/,
     &        /15X,'Contacts:      VAX         ',15X,'   IBM      ',
     &        /15X,'          D.Cowen/E.P.     ',15X,'P.Janot/E.P.',
     &        /15X,'          VXALUW::COWEN    ',15X,'JANOT@CERNVM',
     &        /15X,'          COWEN@VXALUW.CERN',15X,'JANOT@FRCPN11'//)
C
C Initialise BOS
C
      CALL BNAMES(500)
      CALL BOS(IW,IWL)
      IW(5) = 12
      CALL BOSTA
C
C Initialise the run
C
      CALL TSINIT
C
C Process events
C
      CALL TSTEER
C
C Close the run
C
      CALL TSFINI
C
      STOP
      END
