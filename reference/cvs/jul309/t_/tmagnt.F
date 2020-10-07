      SUBROUTINE TMAGNT(IERR)
C
C-----------------------------------------------------------------------
C! TPC90 B-Field
C!
C!      Author:    J. Richstein     18-06-84
C!      Modified:  R. Johnson       18-12-86
C!
C!      Output:
C!           IERR =  0   No error
C!                =  2   Overflow
C!                =  4   If field changes by more than 10%
C!
C!      Called by TPREDA
C!
C!      Description
C!      ===========
C!      The routine converts the value of the ADC in bank TMAG
C!      which reads out the hall probe inside TPC90 magnet, and
C!      converts it into a value according to "GAUSS".
C!      the conversion is done by the use of the calibration constants
C!      given by LEHM. They are taken from $WV.LEH.COEF(MONAL) from IBM.
C!      Once at the beginning the value of sor-comment record is
C!      compared with the first found value of magnetic field
C!      mesured by the hall probe. In case it differs by more than 10%
C!      a warning message is printed, but IERR = 0.
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "bcs.h"
#include "tflags.h"
#include "tpccon.h"
C
C------------------------------END COMMON----------------------------
C
      LOGICAL SECND
      CHARACTER TEXT*150
C
C-------------------------------END TYPES-----------------------------
C
      DATA SECND/.TRUE./,IOLD/-9999/
C
C++   If first time  at SOR called remember the B-field
C++   which was written at sor by the user in the
C++   sor-comment record
C
      IF (IOLD.NE.IRUNRC) THEN
        BSOR   = BFIELD
        IF (BSOR.EQ.0.) BSOR = BSOR + 1.E-10
        IOLD   = IRUNRC
        SECND  = .TRUE.
      ENDIF
C
C++   Look for data and fill BUFPTN
C
      BOLD = BFIELD+1.E-10
      IERR=0
C
      KTMAG=NLINK('TMAG',IRUNRC)
      IF (KTMAG.EQ.0) THEN
        WRITE(TEXT,1000) BFIELD
 1000   FORMAT('No data from the Hall probe can be found.',
     &         '&THE MAGNETIC FIELD WILL REMAIN AT ',E12.5,' KG.',
     &         '&TURN OFF THE MAGNET OPTION TO AVOID CALLS TO TMAGNT')
        CALL RERROR('TMAGNT',1,TEXT(1:144))
        RETURN
      ENDIF
C
      IADM=IW(KTMAG+1)
C     IADM=IBITS(IW(KTMAG+1),0,16)
C
C++   Check for overload
C
      IF (IADM.GT.4095) THEN
        IERR=2
        WRITE(TEXT,1001) BFIELD
 1001   FORMAT('Hall probe overflow. BFIELD remains at ',
     &           E12.5,' kG.')
        CALL RERROR('TMAGNT',2,TEXT(1:55))
        RETURN
      ENDIF
C
C++   Convert ADC readout to voltage :
C
      IF (IADM.GT.2048) THEN
        XHPMV=FLOAT(IADM-4096)/0.04096
      ELSE
        XHPMV=FLOAT(IADM)/0.04096
      ENDIF
C
      IHPMV=IFIX(XHPMV)
C
C++   Convert voltage to Magnetic field value in kG
C
      BFIELD=(TBVAL(IHPMV)/10.) *.01
C
C++   At the beginning compare the hall probe measurement
C++   with the user specified b-field in sor comment record, and
C++   in case it differs by more than .1 kG write a warning message.
C
      IF (SECND) THEN
        SECND = .FALSE.
        IF (ABS((BFIELD-BSOR)/BSOR).GT.0.1) THEN
          WRITE(LDEBRL,1002) BSOR,BFIELD
          RETURN
        ENDIF
      ENDIF
C
C++   If field is .gt. 250 gauss ( what is three sigmas in adc error )
C++   then check on change in field with respect to start_of_run value.
C
      IF (ABS(BFIELD).GT.0.25) THEN
C
C++     If more than 10% change in B-field
C++     set error flag to 4 to skip this event
C
        B1DEL = ABS(BFIELD-BSOR)
        B2DEL = ABS(B1DEL/BSOR)
        IF (B2DEL.GT.0.1) THEN
          IERR = 4
          RETURN
        ENDIF
      ENDIF
C
      RETURN
C
 1002  FORMAT(//1X,'******* WARNING IN TMAGNT AT BEGIN OF RUN *******'
     $,/,1X,'      Measured field differs from SOR-value !',/
     $,1X,'SOR-Value was :',1X,G12.6,1X,'kG; new found value is :'
     $,1X,G12.6,' kG',/)
C
      END
#endif
