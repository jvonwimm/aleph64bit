      SUBROUTINE VCRHOT
C----------------------------------------------------------------------
C!  - Create and fill VHOT the list of noisy strips accumulated by VDNOIS
C!
C!  Author: C.Vannini  -  6/1/90
C!
C!  Modified 17-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C?
C!======================================================================
#ifndef DOC
#include "vprtnc.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "bcs.h"
#include "vrecon.h"
C
C  Function calls
C
      INTEGER NLINK,NBANK,VNSCRM
      INTEGER VNLAYR,VROMAX,VNRFAC
      INTEGER VJFACI, VJWAFF
C
C  Local variables
C
      INTEGER KVOCC,KVHOT
      INTEGER NROM,JROM
      INTEGER JWAF,DECNUM
      INTEGER NHOT(2),NHRO
      INTEGER MODNUM,NSTRP
      INTEGER IADDR,ILAY,IFAC,IVIEW,JFAC
      INTEGER NLAY,NFAC,NROMX,INCR,IRET
      INTEGER ISTRP,JSTRP,NWORDS,IROM
      INTEGER IOCCUP
      REAL OCCUP
C
C  Inline functions
C
      INTEGER IIII
      LOGICAL HIFIL
#include "bmacro.h"
      HIFIL(IIII) = IAND(IIII,4) .EQ. 4
C
C--------------------------------------------------------------
C
C
C  Make VHOT
C
      IF (NHOTEV.LT.NHOTMN) THEN
        CALL RERROR('VCRHOT',4,'Too few events: VHOT will be empty')
        KVHOT = NBANK('VHOT',IRUNRC,LMHLEN)
        GOTO 999
      ELSE
        KVHOT = NBANK('VHOT',IRUNRC,LMHLEN+100)
        IF (KVHOT.LE.0) THEN
          CALL RERROR('VCRHOT',3,'Not enough space to book VHOT')
          GOTO 999
        ENDIF
        IW(KVHOT+LMHCOL) = 1
        IW(KVHOT+LMHROW) = 0
      END IF
      NLAY=VNLAYR()
      NHOT(IVIEW) = 0
C
C  Loop over views
C
      DO 100 IVIEW=1,2
C
C Get the max number and increment of readout modules in a face
C
      IRET = VROMAX(IVIEW,NROMX,INCR)
C
C Get the number of readout channels
C
      NSTRP = VNSCRM(IVIEW)
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
C  IROM is a sort of wafer in face number IWFF
C
      DO 100 IROM=1,NROMX,INCR
        NHRO = 0
C
C  Build the global wafer/readout module number
C  ( First make the global face number)
C
        IRET = VJFACI(ILAY,IFAC,JFAC)
        IRET = VJWAFF(JFAC,IROM,JROM)
C
C  Create the decimal address for this readout module
C
        CALL VAENWA(DECNUM,ILAY,IROM,IFAC,IVIEW)
C
C  Find the associated V0CC bank
C
        KVOCC = NLINK('VOCC',DECNUM)
        IF(KVOCC.LE.0)THEN
          CALL RERROR('VCRHOT',1,'Missing VOCC bank')
          GOTO 999
        END IF
C
C  Loop over the readout strips in this module
C
        DO ISTRP=1,NSTRP
C
C  Compute the occupancy
C
          OCCUP = FLOAT(IW(KVOCC+ISTRP))/FLOAT(NHOTEV)
C
C  Histograms
C
          IF(HIFIL(HISTLV))THEN
            CALL HFILL(HISOFF+210+(IVIEW-1)*2+1,OCCUP,0.,1.)
            CALL HFILL(HISOFF+212+(IVIEW-1)*2+2,OCCUP,0.,1.)
          END IF
C
C   Hot strips defined here
C
          IF (OCCUP.GE.MXOCUP(IVIEW)) THEN
            NHOT(IVIEW) = NHOT(IVIEW) + 1
            NHRO = NHRO + 1
            IOCCUP = NINT(OCCUP*255)
            CALL VPKADD(IADDR,IOCCUP,ILAY,IROM,IFAC,IVIEW,ISTRP)
            IW(KNEXT(KVHOT)+1) = IADDR
            IW(KVHOT+LMHROW) = LROWS(KVHOT) + 1
          END IF
C
C  Check if the bank needs extending
C
          IF ( LFRWRD(KVHOT).LE. 5 ) THEN
            NWORDS = LROWS(KVHOT) + LMHLEN + 100
            KVHOT = NBANK('VHOT',IRUNRC,NWORDS)
            IF (KVHOT.LE.0) THEN
              CALL RERROR('VCRHOT',2,
     &               'Not enough space to extend VHOT')
              GOTO 999
            ENDIF
          END IF
        END DO
        IF(HIFIL(HISTLV))THEN
          CALL HFILL(HISOFF+214+IVIEW,FLOAT(JROM),
     &         FLOAT(NHRO),1.0)
        END IF
  100 CONTINUE
C
C Remake bank to size
C
      NWORDS = LMHLEN + LROWS(KVHOT)
      KVHOT = NBANK('VHOT',IRUNRC,NWORDS)
C
C  Message
C
      WRITE(LOUTRL,1000) NHOTEV,NHOT
1000  FORMAT(' ',80('-')/,
     &       ' ',I7,' events analized for noisy channels detection',/
     &       ' ',I6,'-Z  and  ',I6,'-Phi  hot  channels  were found'/,
     &       //' ',80('-'))
C
C  Clean out banks for next run
C
 999  CONTINUE
      CALL BDROP(IW,'VOCC')
      RETURN
      END
#endif
