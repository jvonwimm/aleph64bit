      INTEGER FUNCTION VDARGE (LUNDAF,IGET)
C ----------------------------------------------------------------------
CKEY VDETDES READ DBASE / INTERNAL
C!  Read VDET geography banks from DAF
C     J. Rothberg      24 Oct. 1995
C - based on Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 31 July 1995, S. Wasserbaech: add NWFBIT to VRDOCO
C
C  Called by: VINIVG
C  Calls:     ALGTDB
C
C - Input:
C   LUNDAF / I  Logical unit number of DAF file
C   IGET   / I  VDET setup code to be read
C
C - Output:
C   VDARGE / I  = VDOK if successful
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
#include "vglobl.h"
#include "bcs.h"
C     Arguments:
      INTEGER LUNDAF, IGET
C
C     Parameters:
C     List of VDET geography banks to read:
      CHARACTER*12 LIST1
      PARAMETER (LIST1 = 'VFMCVUECVXCH')
C
C     Local variables
      INTEGER LOUT, IRET, I
      INTEGER KVFMC, KVUEC, KVXCH, KVMBE, KVUBE
      INTEGER ILAY, IFAC, IMOD
      LOGICAL MISS
C
C     External functions:
      INTEGER ALGTDB, NAMIND, MDARD
#include "bmacrod.h"
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      VDARGE = VDERR
      LOUT = IW(6)
C
C     Initialize all VDET geometry commons:
C
C ----------------------------------------------------------------------
C
C     Read the banks from the DAF:
      IRET = ALGTDB(LUNDAF,LIST1,-IGET)
C
      IF (IRET .EQ. 0) THEN
C       Something went wrong...
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Get the indices to banks just read in:
C
      KVFMC = IW(NAMIND('VFMC'))
      KVUEC = IW(NAMIND('VUEC'))
      KVXCH = IW(NAMIND('VXCH'))
C
      MISS = (KVFMC .LE. 0) .OR. (KVUEC .LE. 0) .OR.
     >       (KVXCH .LE. 0)
C
      IF (MISS) THEN
        GO TO 1000
      ENDIF
C
C ----------------------------------------------------------------------
C Read bonding error banks; bank number is seial module number
      DO I =1,100
        IRET= MDARD(IW,LUNDAF,'VMBE',I)
      ENDDO
C ----------------------------------------------------------------------
C
C     Success!
C
      VDARGE = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
