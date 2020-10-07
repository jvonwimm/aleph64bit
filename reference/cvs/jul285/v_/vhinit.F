      SUBROUTINE VHINIT
C----------------------------------------------------------------------
C!  - Initialization for runs creating new VHOT lists
C!
C! Author: C.Vannini  -  6/1/90
C!
C! Variables of common/VHOTPM/ are initialized here
C! Create an empty bank VHOT with nr=IRUNRC
C! Create VOCC to record the occurrency of each strip/run
C! Control histograms are booked
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C?
C!======================================================================
#ifndef DOC
#include "vprtnc.h"
#include "rcurnt.h"
#include "vhotjj.h"
#include "bcs.h"
C
C  Function calls
C
      INTEGER NBANK,NLINK
      INTEGER VNLAYR,VROMAX,VNRFAC,VNSCRM
C
C  Local variables
C
      INTEGER NLAY,NFAC,NROMX,INCR,IRET
      INTEGER IVIEW,JWAF,ILAY,IFAC,IWFF,DECNUM
      INTEGER IVOCC,IVHOT,IVHFS
      INTEGER IROM,NROM,MODNUM
      INTEGER NCHAN
C
C  Variables of common/VHOTPM/:
C
      NHOTEV = 0
C
C  Create an empty VHOT bank for this run
C
      IVHOT = NLINK('VHOT',IRUNRC)
      IF(IVHOT.NE.0) THEN
        CALL RERROR('VHINIT',1,'VHOT bank will be replaced')
        CALL NDROP('VHOT',IRUNRC)
      ENDIF
      IVHOT = NBANK('VHOT',IRUNRC,LMHLEN)
      IF (IVHOT.LE.0) THEN
        CALL RERROR('VHINIT',2,'Not enough space to book VHOT')
        GOTO 999
      ENDIF
      IW(IVHOT+LMHCOL) = LVHOTA
      IW(IVHOT+LMHROW) = 0
C
C  Loop over views
C
      DO 100 IVIEW=1,2
      NLAY=VNLAYR()
C
C Get the max number and increment of readout modules in a face
C
      IRET = VROMAX(IVIEW,NROMX,INCR)
C
C Get the number of readout channels
C
      NCHAN = VNSCRM(IVIEW)
C
C Loop over layers
C
      DO 100 ILAY=1,NLAY
      NFAC = VNRFAC(ILAY)
C
C Loop over faces
C
      DO 100 IFAC=1,NFAC
C
C  Loop over the readout modules
C
      DO 100 IROM=1,NROMX,INCR
C
C  Create the decimal address for this
C
          CALL VAENWA(DECNUM,ILAY,IROM,IFAC,IVIEW)
C
C  Make a VOCC for each readout module
C
          IVOCC = NBANK ('VOCC',DECNUM,NCHAN)
          IF (IVOCC.EQ.0) THEN
            CALL RERROR('VHINIT',4,'Not enough space to book VOCC')
            GOTO 999
          ENDIF
  100 CONTINUE
C
  999 RETURN
      END
#endif
