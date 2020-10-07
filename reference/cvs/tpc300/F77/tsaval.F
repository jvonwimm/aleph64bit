*DK tsaval
      SUBROUTINE TSAVAL(NEL,TTOT,IRET)
C-----------------------------------------------------------------------
C!  Subroutine to simulate the avalanche of one (or several) electrons
C!  at a TPC anode wire.
C
C  Called from:  TSTEER
C  Calls:        TPACHR
C
C  Inputs:   PASSED:      --NEL,    number of electrons forming the
C                                   avalanche
C                         --TTOT,   the time of the avalanche
C            /TPCOND/     --TPANBN, the length of the analog signal
C                                   time bin
C            /TPCONS/     --NTMXAN, the largest allowed time bin
C
C  Outputs:  PASSED:      --IRET,   return code to indicate if
C                                   avalanche is in a legal time bin
C            /CHANNL/     --NAVELE, the number of electrons released by
C                                   the avalanche
C                         --IBIN1,  the time bin of the avalanche
C A. Caldwell, D. DeMille
C-----------------------------------------------------------------------
#include "channl.h"
#include "tpcond.h"
#include "tpcons.h"
#include "hiscom.h"
C
      LOGICAL LDBA1,LDBA3
C
      DATA ICALL/0/
C
      ICALL = ICALL + 1
C
C  Set the return code.  IRET = 1 for invalid bin
C
      IRET = 0
C
C  Debug levels
C
      LDBA1 = ( NTPCDA .GE. 1 .AND. ICALL .LE. NCALDA )
      LDBA3 = ( NTPCDA .GE. 3 .AND. ICALL .LE. NCALDA )
C
C  Get the total charge produced.
C
      CALL TPACHR(NEL,QTOT)
C
      IF ( LDBA3 ) WRITE (6,100) NEL,QTOT
      IF ( LDBA1 ) THEN
           IF ( QTOT .GT. 0. ) THEN
              CALL HF1(IHAVAL+1,ALOG10(QTOT),1.)
           ELSE
              WRITE(6,201) QTOT,NEL
           ENDIF
      ENDIF
C
C  Determine the time bin affected and make sure it is a legal value
C
      IBIN1 = INT( TTOT / TPANBN ) + 1
      IF ( IBIN1 .LE. 0 .OR. IBIN1 .GE. NTMXAN ) THEN
         IRET = 1
         IF ( LDBA1 ) WRITE(6,202) IBIN1
         RETURN
      ENDIF
C
C  Find the number of electrons in the avalanche and fill the common
C  block with the value
C
      NAVELE = INT(QTOT)
      IF ( LDBA3 ) WRITE (6,101) IBIN1,NAVELE
C
      RETURN
C_______________________________________________________________________
C
 100  FORMAT(//,10X,'    START AVALANCHE PROCESS ',
     *        /,' NUMBER OF ELECTRONS IN AVALANCHE :',I4,
     *        /,' TOTAL ELECTRONS DEPOSITED        :',E12.5)
 101  FORMAT(/ ,' FIRST TIME BIN AFFECTED          :',I5,
     *       / ,' ELECTRONS IN BIN                 :',I12)
C
 201  FORMAT(/ ,' +++TSAVAL+++ 0 CHARGE',F10.5,' NO. OF ELECTRONS,',
     *         I12)
 202  FORMAT(/ ,' +++TSAVAL+++ ILLEGAL BIN VALUE:',I12)
C
      END
