      INTEGER FUNCTION VDETGI ()
C ----------------------------------------------------------------------
CKEY VDETDES / INTERNAL
C!  Fills VDETGE common
C - Steve Wasserbaech, January 1994
C   (Based on VRDDAF, G. Triggiani, 17/02/87.)
C   Modified 2 June 1995, S. Wasserbaech: ISSFLG = face serial number
C   Modified 31 July 1995, SW: add variables for address packing
C
C  This function uses information from the commons VSLTCO, VDLACO,
C  VZPWCO, VWGECO, VRDOCO, and VGINDX to fill the VDETGE common.
C  The five "input" commons are filled in VDAFRD and VINDXI.
C  VDETGE common is initialized in VRDDAF.
C
C  Called by: VRDDAF
C
C - Input:
C   (none)
C
C - Output:
C   VDETGI / I  = VDOK if successful
C               = VDERR if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C     IMPLICIT NONE
#include "vglobl.h"
#include "vsltco.h"
#include "vdlaco.h"
#include "vzpwco.h"
#include "vwgeco.h"
#include "vrdoco.h"
#include "vgindx.h"
#include "vdetge.h"
#include "vhlsbt.h"
#include "alcons.h"
#include "bcs.h"
C
C     Local variables
      INTEGER LOUT, ILAY, IFAC, IWFF, JFAC, JWAF
      REAL YWAF, XCEN, YCEN, RMIN, RMAX
C
C ----------------------------------------------------------------------
C
      LOUT = IW(6)
C
C     Fill coordinates of wafer centers in Aleph coordinate system.
C     Fill RVDMIN, RVDMAX, ZVDMAX.
C
C     ZVDMAX is the maximum abs(z) of the wafers in VDET:
      ZVDMAX = AMAX1(ABS(WAFERZ(1)),ABS(WAFERZ(NWAFEF))) + WSIZEA/2.
C
C     Initialize the other VDET dimensions:
      RVDMIN = 1.E9
      RVDMAX = 0.
C
C     Loop over layers:
      DO ILAY=1,NVLAYR
C
C     tan(wafer tilt angle):
        TNWTLT(ILAY) = TAN(WATILT(ILAY))
C
C     YWAF is the projection of the vector from the wafer center
C     to the origin onto the u direction:
C
        YWAF = RWVDLA(ILAY) * TNWTLT(ILAY)
C
C     RMIN is the minimum radius of the wafers in this layer.
C
        RMIN = SQRT(AMAX1(0.,ABS(YWAF) - WSIZEB/2.)**2 +
     >                  (RWVDLA(ILAY) - WTHICK/2.)**2)
C
C     RMAX is the maximum radius of the wafers in this layer.
C
        RMAX = SQRT((ABS(YWAF) + WSIZEB/2.)**2 +
     >                  (RWVDLA(ILAY) + WTHICK/2.)**2)
C
C     Now loop over faces:
C
C     (The (x,y) coordinates are the same for all wafers in a face.)
        DO IFAC=1,NFACEL(ILAY)
C
C     RVDMIN is the minimum radius of the wafers in VDET.
C     RVDMAX is the maximum radius of the wafers in VDET.
C     For this calculation we skip slots that contain no face:
C
          JFAC = IJFACE(ILAY,IFAC)
          IF (ISSFLG(JFAC) .NE. 0) THEN
            RVDMIN = AMIN1(RVDMIN,RMIN)
            RVDMAX = AMAX1(RVDMAX,RMAX)
          ENDIF
C
C     WARHOC is the distance between the origin and the wafer center
C     in the xy projection:
C
          WARHOC(JFAC) = SQRT(RWVDLA(ILAY)**2 + YWAF**2)
C
C     WAPHIC is the phi coordinate of the wafer center
C     in the Aleph system:
C
          WAPHIC(JFAC) = PHIOFF(JFAC) - WATILT(ILAY)
          IF (WAPHIC(JFAC) .LT. 0.)
     >              WAPHIC(JFAC) = WAPHIC(JFAC) + TWOPI
C
C     CPHIOF and SPHIOF are the cos and sin of PHIOFF:
C
          CPHIOF(JFAC) = COS(PHIOFF(JFAC))
          SPHIOF(JFAC) = SIN(PHIOFF(JFAC))
C
C     XCEN and YCEN are the coordinates of the wafer center in the
C     Aleph system:
C
          XCEN = RWVDLA(ILAY)*CPHIOF(JFAC) + YWAF*SPHIOF(JFAC)
          YCEN = RWVDLA(ILAY)*SPHIOF(JFAC) - YWAF*CPHIOF(JFAC)
C
C     Now loop over wafers to fill WAXCEN, WAYCEN, WAZCEN:
C
          DO IWFF=1,NWAFEF
            JWAF = IJWFFR(ILAY,IFAC,IWFF)
            WAXCEN(JWAF) = XCEN
            WAYCEN(JWAF) = YCEN
            WAZCEN(JWAF) = WAFERZ(IWFF)
          ENDDO
C
C     End of loop over faces:
        ENDDO
C
C     End of loop over layers:
      ENDDO
C
C ----------------------------------------------------------------------
C
C     Extent of the sensitive regions:
C     We are assuming that the sensitive regions extend one-half of a
C     physical strip pitch beyond the first and last physical strips.
C
C     z side:
C
      AMXSRZ = AMNSRZ + FLOAT(NZSTRP)*STPITZ
      BMXSRZ = BMNSRZ + STLENZ
C
C     r-phi side:
C
      AMXSRP = AMNSRP + STLENP
      BMXSRP = BMNSRP + FLOAT(NPSTRP)*STPITP
C
C ----------------------------------------------------------------------
C
C     Readout quantities:
C
C     Multiplexing in the z view?
      IF ((NZRSSC .EQ. 1) .AND. (NWAFEF .LE. IROMAX)) THEN
        LZMULT = .FALSE.
        NZROMM = NWAFEM
      ELSEIF (NZRSSC .EQ. 2) THEN
        LZMULT = .TRUE.
        NZROMM = 1
      ELSE
        WRITE (LOUT,'(1X,A)')
     >        '+++VDETGI+++  Invalid VDET readout configuration'
        WRITE (LOUT,'(1X,A,I5,4X,A,I5)')
     >        '+++VDETGI+++  NZRSSC =', NZRSSC, 'NWAFEF =', NWAFEF
        VDETGI = VDERR
        GO TO 1000
      ENDIF
C
C     Number of strip channels per readout module:
      IF (LZMULT) THEN
        NZSROM = NWAFEM * NRDSTZ / 2
      ELSE
        NZSROM = NRDSTZ
      ENDIF
      NPSROM = NRDSTP
C
C     Number of strip channels per module:
      NZSMOD = NZSROM * NZROMM
      NPSMOD = NPSROM * NPROMM
C
C     Number of readout strips per strip channel, r-phi view:
      NPRSSC = NWAFEM
C
C ----------------------------------------------------------------------
C
C     Variables needed for packing and unpacking cluster addresses:
C
      IF (NWFBIT .EQ. 2) THEN
C
C     Use the parameters for VDET91 (the wafer number gets two bits):
        MSVWAF = MVWAF
        MSVNST = MVNSTR
        ISSLAY = ISLAY
        ISSNST = ISNSTR
C
      ELSEIF (NWFBIT .EQ. 3) THEN
C
C     Use the parameters for VDET95 (the wafer number gets three bits):
        MSVWAF = M3VWAF
        MSVNST = M3VNST
        ISSLAY = I3SLAY
        ISSNST = I3SNST
C
      ENDIF
C
C ----------------------------------------------------------------------
C
C     Finished!
C
      VDETGI = VDOK
C
 1000 CONTINUE
      RETURN
      END
#endif
