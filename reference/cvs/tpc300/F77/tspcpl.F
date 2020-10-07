*DK tspcpl
      SUBROUTINE TSPCPL(ISECT,ITYPE,RHIT,PHHIT,NPDHT)
C-----------------------------------------------------------------------
C!  Couple the avalanche to the appropriate pads.
C
C  Called from:  TSTEER
C  Calls:        TSPCPL
C
C  Inputs:   PASSED:      --ISECT,  sector slot number
C                         --ITYPE,  sector type
C                         --RHIT,   radius at avalanche location
C                         --PHHIT,  phi at avalanche location
C            /TPGEOP/     --padrow geometry
C
C  Outputs:  PASSED:      --NPDHT,  the number of long pads affected
C                                   by the current avalanche
C            /CHANNL/     --NAMP,   pad number for each pad hit
C                         --MPPUL,  the number of electrons induced on
C                                   the corresponding pad in NAMP
C  A. Caldwell
C  Modifications:
C     M. Mermikides 21/5/86  Take care of half-pads
C     D. Cowen      21Nov88  Initialize NPDHT, NAMP and MPPUL on each
C                            call BEFORE testing for row number.  This
C                            prevents pad numbers which are too high
C                            for a given pad row from getting to the
C                            output (also pad rows too large for a
C                            given sector).  This was a bitch of a bug.
C-----------------------------------------------------------------------
#include "hiscom.h"
#include "tpcond.h"
#include "tpgpar.h"
#include "tpgeop.h"
#include "channl.h"
C
      LOGICAL LDBC1,LDBC2
C
C  Debug levels
C
      ICALL = ICALL + 1
      LDBC1 = ( NTPCDC .GE. 1 .AND. ICALL .LE. NCALDC )
      LDBC2 = ( NTPCDC .GE. 2 .AND. ICALL .LE. NCALDC )
C
C  Initialize the number of hit pads to zero, the hardware numbers
C  for these pads to zeros, and the charge for these pads to zero.
C
      NPDHT = 0
      IF (LTPDIG) THEN
         CALL VZERO(NAMP(1),MXPHIT)
         CALL VZERO(MPPUL(1),MXPHIT)
      ENDIF
C
C  Find the nearest pad row to this hit, and ensure that this row exists
C
      IF ( ITYPE .EQ. 1 ) THEN
         NROW = INT( ( (RHIT-TPDRBG(ITYPE))/TPDRST(ITYPE) ) + .5 ) + 1
         IF ( NROW .LT. 1 .OR. NROW .GT. 9 ) RETURN
         NROWD2 = NROW
      ELSE
         NROW = INT( ( (RHIT-TPDRBG(ITYPE))/TPDRST(ITYPE) ) + .5 ) + 1
         IF ( NROW .LT. 1 .OR. NROW .GT. 12 ) RETURN
         NROWD2 = NROW + 9
      ENDIF
C
C  Find the radius and phi-acceptance of this pad row;
C  NB  PHI is counted anticlockwise.
C  PHI0 (< 0) is the phi at the right-hand edge of the padrow
C
      RPAD = TPDRBG(ITYPE) + (NROW-1)*TPDRST(ITYPE)
      PHI0 = -TPDPHW(NROW,ITYPE)
      NPADF = NTPDPR(NROW,ITYPE)
C
C  Find the pad number of the pad in this row nearest to the hit
C  IPADF is counted in equivalent full pads, IPADH in half-pad units
C
      IPADH = INT( (PHHIT-PHI0)/(0.5*TPDPHS(NROW,ITYPE)) ) + 1
      IPADF = (IPADH+1)/2
C
C  IPADN corresponds to hardware (real) pad number counted in global
C  convention.
C
      IF(IPADH.LE.2) THEN
         IPADN = IPADH
      ELSE
         IPADN = (IPADH+3)/2
         IF(IPADN.GT.NPADF) IPADN = (IPADH+4)/2
      ENDIF
C
C  Look for significant coupling on NCPAD number of pads on either side
C  of the nearest, and count the number of affected pads as we go.
C
      DO 1 JPAD = -NCPAD,NCPAD
C
         MPAD =  IPADN + JPAD
         IF (MPAD.LE.0.OR.MPAD.GT.NPADF+2) GO TO 1
         IF (MPAD.LE.2) THEN
            PHPAD  = PHI0 + (FLOAT(MPAD)-0.5)*TPDPHS(NROW,ITYPE)/2.
         ELSEIF(MPAD.GT.NPADF) THEN
            PHPAD = PHI0 + (FLOAT(NPADF+MPAD)-2.5)*TPDPHS(NROW,ITYPE)/2.
         ELSE
            PHPAD  = PHI0 + (FLOAT(MPAD-1)-0.5)*TPDPHS(NROW,ITYPE)
         ENDIF
C
C  Make sure we're still actually on the pad row; if we are, get the
C  relative coupling strength
C
         IF ( PHPAD .LT. PHI0 .OR. PHPAD .GT. -PHI0 ) GO TO 1
C
         RELSTR = TPPCST(RPAD,RHIT,PHPAD,PHHIT)
C  Scale down coupling strength if half-pad
C  (This needs to be done better)
C
         IF(MPAD.LE.2.OR.MPAD.GT.NPADF) RELSTR = 0.5*RELSTR
C
C  If there is significant coupling, update the number of
C  pads hit and store the global pad number (i.e., including the row
C  information) and the charge on the pad ( = charge in avalanche *
C  coupling strength ).
C
         IF ( RELSTR .GT. 0. ) THEN
C
            NPDHT = NPDHT + 1
C  Get hardware pad number
            IF (ISECT.GT.18) MPAD = NPADF - MPAD + 3
            NAMP(NPDHT) = (NROW-1)*150 + MPAD
            MPPUL(NPDHT) = INT(FLOAT(NAVELE)*RELSTR)
C
         ENDIF
C
C  End of loop over neighboring pads
C
 1    CONTINUE
C
      IF ( LDBC1 ) CALL HF1(IHCOUP+1,FLOAT(NPDHT),1.)
      IF ( LDBC2 ) THEN
         WRITE(6,101) NPDHT,IBIN1
         WRITE(6,102) (J,NAMP(J),MPPUL(J),J=1,NPDHT)
      ENDIF
C_______________________________________________________________________
C
 101  FORMAT(//,10X,'  FINAL NUMBERS FROM COUPLING: ',
     *       /,' PADS HIT  : ',I2,/,' TIME BIN  : ',I4)
 102  FORMAT(' HIT #: ',I2,'  PAD NUMBER: ',I4,' CHARGE : ',I12)
C
      RETURN
      END
