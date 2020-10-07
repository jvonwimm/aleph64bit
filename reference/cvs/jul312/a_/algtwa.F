      INTEGER FUNCTION ALGTWA(IRUN)
C----------------------------------------------------------
C!  - Get number of wagons from whatever reliable source of information
C!    This routine should eventually go to the ALEPHLIB
C!
C!  Author : B. Bloch -Devaux  25 October 1994- October 1995
C!  Modified M.Cattaneo        18 July 1997
C!      Run 29977 also had bunch trains (but was DUCK for SiCAL)
C!
C!  Input  :  IRUN Run number
C!
C!  Output :  ALGTWA  = unmber of wagons in the train
C----------------------------------------------------------
#ifndef DOC
      PARAMETER ( IRUNMX = 29900)
C    last run number before the bunch train mode , also valid for MC !!!
      IF ( IRUN.LT.IRUNMX) THEN
         NWAG = 1
C this is now 1995 Data with variable configuration....
      ELSE IF (IRUN.GT.35000) THEN
         NWAG = 2
         IF ( IRUN.LE.35578 ) NWAG = 1
         IF ( IRUN.GE.35593 .and. IRUN.LE.35595 ) NWAG = 1
         IF ( IRUN.GE.35635 .and. IRUN.LE.35642 ) NWAG = 1
         IF ( IRUN.GE.35718 .and. IRUN.LE.35761 ) NWAG = 1
         IF ( IRUN.GE.36049 .and. IRUN.LE.36070 ) NWAG = 1
         IF ( IRUN.GE.36547 .and. IRUN.LE.36559 ) NWAG = 3
         IF ( IRUN.GE.36577 .and. IRUN.LE.36646 ) NWAG = 4
         IF ( IRUN.GE.36814 .and. IRUN.LE.36839 ) NWAG = 1
         IF ( IRUN.GE.36847 .and. IRUN.LE.36852 ) NWAG = 3
         IF ( IRUN.GE.36854 .and. IRUN.LE.36918 ) NWAG = 4
C The 95 scan is done with 3 wagons (B.Bloch, P.Comas, Aug 95)
         IF ( IRUN.GE.36925 .and. IRUN.LE.40000 ) NWAG = 3
C The 140 Gev running in 1995
         IF ( IRUN.GT.40000 ) NWAG =1
C The exploration of the timing curve in 3 bunches mode
         IF ( IRUN.GE.40271 .AND. IRUN.LE.40274 ) NWAG = 3
C     Bunch train tests in November 94
      ELSE IF ((IRUN.EQ.29977).OR.
     .         (IRUN.EQ.29989).OR. (IRUN.EQ.29990).OR.
     .         (IRUN.EQ.29991).OR. (IRUN.EQ.30038).OR.
     .         (IRUN.NE.30042).OR. (IRUN.EQ.30043).OR.
     .         (IRUN.NE.30044).OR. (IRUN.EQ.30045).OR.
     .         (IRUN.EQ.30046).OR. (IRUN.EQ.30060).OR.
     .         (IRUN.EQ.30248).OR. (IRUN.EQ.30249).OR.
     .         (IRUN.EQ.30250).OR. (IRUN.EQ.30251).OR.
     .         (IRUN.EQ.30252).OR. (IRUN.EQ.30253).OR.
     .         (IRUN.EQ.30254).OR. (IRUN.EQ.30255).OR.
     .         (IRUN.EQ.30380).OR. (IRUN.EQ.30381) ) THEN
         NWAG = 4
      ENDIF
      ALGTWA = NWAG
      RETURN
      END
#endif
