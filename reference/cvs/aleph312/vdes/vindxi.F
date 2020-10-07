      INTEGER FUNCTION VINDXI ()
C ----------------------------------------------------------------------
CKEY VDETDES INDEX / INTERNAL
C!  Create look-up tables to be used by index conversion functions
C - Joe Rothberg and Steve Wasserbaech, January 1994
C
C  Note: VINDXI builds the index conversion tables using information
C  taken from the common blocks VSLTCO and VZPWCO.  Before calling
C  VINDXI it is necessary to call VDAFRD, which fills these commons
C  from the database banks VSLT and VZPW.
C
C - Input:
C   (none)
C
C - Output:
C   VINDXI / I   = VDOK if successful
C                = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vzpwco.h"
#include "vgindx.h"
C
C     Local variables
      SAVE CHLAY, CHMOD
      INTEGER ILAY, IFAC, IMOD, IWAF, IWFF
      INTEGER JFAC, JMOD, JWAF
      CHARACTER*1 CHLAY(NVLAYR), CHMOD(NVMODF)
      CHARACTER*2 CHFAC
      DATA CHLAY /'I', 'O'/
      DATA CHMOD /'B', 'A'/
C
C
C ----------------------------------------------------------------------
C
      VINDXI = VDERR
C
C     IVSTUP is not filled now.
C     It is filled in VRDDAF, after all of the commons are
C     successfully filled.
C
C     Total number of modules:
      NMODUL = NSLOTS * NVMODF
C
C     Total number of wafers:
      NWAFER = NSLOTS * NWAFEF
C
C     Number of faces in each layer:
      DO JFAC=1,NSLOTS
        IF ((JJLAYF(JFAC) .EQ. 1) .OR. (JJLAYF(JFAC) .EQ. 2))
     >           NFACEL(JJLAYF(JFAC)) = NFACEL(JJLAYF(JFAC)) + 1
      ENDDO
C
C     Number of wafers per module:
      NWAFEM = NWAFEF / NVMODF
C
C ----------------------------------------------------------------------
C
C     Local face index IFAC for global face index JFAC:
C
      DO JFAC = 1, NSLOTS
        JIFACF(JFAC) = JFAC
        IF (JFAC .GT. NFACEL(1))  JIFACF(JFAC) = JFAC - NFACEL(1)
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Local module index IMOD for global module index JMOD:
C
      DO JMOD = 1, NMODUL
        JIMODM(JMOD) = MOD(JMOD-1, NVMODF) + 1
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Local wafer index IWAF for global wafer index JWAF:
C
      DO JWAF = 1, NWAFER
        JIWAFW(JWAF) = MOD(JWAF-1, NWAFEM) + 1
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global layer index JLAY for global face index JFAC:
C     JJLAYF in common VSLTCO, which is already filled.
C
C ----------------------------------------------------------------------
C
C     Global face index JFAC for global module index JMOD:
C
      DO JMOD = 1, NMODUL
        JJFACM(JMOD) = (JMOD-1)/NVMODF + 1
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global module index JMOD for global wafer index JWAF:
C
      DO JWAF = 1, NWAFER
        JJMODW(JWAF) = (JWAF-1)/NWAFEM + 1
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global face index JFAC
C     for local layer and face indices ILAY and IFAC:
C
      DO ILAY = 1, NVLAYR
        DO IFAC = 1, NFACEL(ILAY)
          IJFACE(ILAY,IFAC) = IFAC
          IF (ILAY .GT. 1)
     >      IJFACE(ILAY,IFAC) = IJFACE(ILAY,IFAC) + NFACEL(ILAY-1)
        ENDDO
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global module index JMOD
C     for local layer, face, and module indices ILAY, IFAC, and IMOD:
C
      DO ILAY = 1, NVLAYR
        DO IFAC = 1, NFACEL(ILAY)
          DO IMOD = 1, NVMODF
            IJMODU(ILAY,IFAC,IMOD) = NVMODF*(IJFACE(ILAY,IFAC)-1) + IMOD
          ENDDO
        ENDDO
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global wafer index JWAF for local layer, face, module,
C     and wafer indices ILAY, IFAC, IMOD, and IWAF:
C
      DO ILAY = 1, NVLAYR
        DO IFAC = 1, NFACEL(ILAY)
          DO IMOD = 1, NVMODF
            DO IWAF = 1, NWAFEM
              IJWAFR(ILAY,IFAC,IMOD,IWAF) =
     >                       NWAFEM*(IJMODU(ILAY,IFAC,IMOD)-1) + IWAF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Local wafer-in-face index IWFF for global wafer index JWAF:
C
      DO JWAF = 1, NWAFER
        JMOD = JJMODW(JWAF)
        IMOD = JIMODM(JMOD)
        IWAF = JIWAFW(JWAF)
        IF (IMOD .EQ. 1) THEN
          JIWFFW(JWAF) = NWAFEM + 1 - IWAF
        ELSE
          JIWFFW(JWAF) = NWAFEM + IWAF
        ENDIF
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Global wafer index JWAF for local layer, face, and
C     wafer-in-face indices ILAY, IFAC, IWFF:
C
      DO ILAY = 1, NVLAYR
        DO IFAC = 1, NFACEL(ILAY)
          DO IWFF = 1, NWAFEF
            IMOD = (IWFF-1)/NWAFEM + 1
            IF (IMOD .EQ. 1) THEN
              IWAF = NWAFEM + 1 - IWFF
            ELSE
              IWAF = IWFF - NWAFEM
            ENDIF
            IJWFFR(ILAY,IFAC,IWFF) = IJWAFR(ILAY,IFAC,IMOD,IWAF)
          ENDDO
        ENDDO
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Text module name TXTM for local layer, face, and module
C     indices ILAY, IFAC, IMOD:
C
      DO ILAY = 1, NVLAYR
        DO IFAC = 1, NFACEL(ILAY)
          WRITE (CHFAC,'(I2.2)') IFAC
          DO IMOD = 1, NVMODF
            TXMODU(ILAY,IFAC,IMOD) = CHMOD(IMOD)//CHLAY(ILAY)//CHFAC
          ENDDO
        ENDDO
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Success!
C
      VINDXI = VDOK
C
      RETURN
      END
#endif
