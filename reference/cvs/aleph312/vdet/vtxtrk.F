      SUBROUTINE VTXTRK(TPAR,TERR,RSIGU,RSIGW,
     &  NHIWA,IWAFI,LCPOS,LCMOM,LCERR,LCDER,GBPOS,IFLAG,IER,
     &  NFACE,IFACE,FPOS,FMOM,FRAD,JFLAG)
C----------------------------------------------------------------------
C!  Extrapolate a given track to all intersecting VDET wafers
CKEY VDET TRACK
C!
C!   Author   :- Jochen A. Lauber       4-OCT-1990
C!             modified David Brown 14-10-90
C!             modified 23-9-92 to include face extrapolation
C!             (IE including non-active areas).  David Brown
C!             modified 24-5-93 Check Z cylinder extrapolation.G.Taylor
C!      Modified A. Bonissent March 1995 :
C!                Use geometry package, so that Vdet 91 or 95 can be used
C!   Inputs:
C!        TPAR       - 5 Track parameters, as in FRFT
C!        TERR       - 15 parameter error matrix on these parameters
C!        RSIGU,RSIGW- # of sigma beyond edge of active region to keep
C!                     a track- also flag the track if within this # of
C!                     on either side.
C!
C!   Outputs: All sized for the maximum possible number of hit wafers
C!        NHIWA      - number of hit wafers, ranges from 0 to 4
C!        IWAFI(4)   - wafer identifier as in VDENWA
C!        LCPOS(2,4) - UW co-ordinate of extrapolation point,
C!                      EACH HIT WAFER HAS IT'S OWN VUW CO-ORDINATE SYST
C!        GBPOS(3,4) - XYZ co-ordinate of extrapolation point
C!        LCMOM(3,4) - VUW momentum direction unit vector at the ex. poi
C!                         EACH HIT WAFER HAS IT'S OWN VUW CO-ORDINATE S
C!        LCERR(36)  - Triangular error matrix of local position
C!        LCDER(10,4)- derivatives of local coordinates U+W
C!                         to the 5 track parms
C!        IFLAG(4)   - flag to warn for tracks near wafer edges
C!        IER        - status of processing , 0 : everything o.k.
C!
C!        NFACE      - Number of hit faces
C!        IFACE      - Module address of hit face; WAFER ID should be ig
C!        FPOS       - U,W hit position in FACE coordinate system (only
C!                     is different from wafer coordinate system)
C!        FRAD       - Radius of track extrapolation point in ALEPH syst
C!        FMOM       - Momentum direction unit vector in local system
C!        JFLAG(4)   - flag for tracks beyond face borders
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   called once per track
C!   Decides which 4 wafers to try extrapolating the track to (2 per lay
C!   Decides whether to keep the extrapolated points
C!   Calls VTXNWT to do the actual extrapolation
C?
C!======================================================================
#ifndef DOC
C      IMPLICIT NONE
      SAVE
#include "alcons.h"
#include "vtexfb.h"
C
C  Inputs
C
      REAL TPAR(5),TERR(15),RSIGU,RSIGW
C
C  Outputs
C
      INTEGER NHIWA,IWAFI(4),NFACE,IFACE(4)
      REAL LCPOS(2,4),LCMOM(3,4),LCERR(36),LCDER(5,2,4),GBPOS(3,4)
      REAL FPOS(2,4),FMOM(3,4),FRAD(4)
      INTEGER IFLAG(4),IER,JFLAG(4)
C
C  Functions
C
      REAL RVMPY
C
C  Local variables
C
      REAL MINZ2
      INTEGER IWAF2
      INTEGER IRET,ILAY,IPHI(2),JPHI,ITYPE,IWAF,JWAF,MHIWA
      INTEGER IUERR,IWERR,MFACE,ICOR,IWAFF(2)
      REAL LERR(2),MINZ,GPOS0(3)
      REAL RV,VPHIT,ZBUF
      REAL QCDER(5,2,4)
      INTEGER IZVIEW
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL FULLU,FULLW,FULUW(3),ZCENT(6)
      INTEGER NWFAC,NWMOD,IWFF,IVIEW
      REAL ABC(3),XYZ(3),VUACT,VWACT,SAMIN,SAMAX(2),SBMIN,SBMAX(2)
C
      INTEGER VNRWAF,VNRMOD,VABCXY,VSENSI,VWFFND,VFADIM,VDEXCY
      PARAMETER (IZVIEW=1)
      INTEGER IBART(4,4),JBART(4,4),IUU,IUW,IWU,IWW
C
C  Data statements
C
      PARAMETER (ZBUF = 5.)
C  These next are pointers into the diagonal error array,
C  describing where to find certain off-diagonal terms
C
      DATA IBART/0,4,11,22,0,0,13,24,0,0,0,26,0,0,0,0/
      DATA JBART/0,7,16,29,0,0,18,31,0,0,0,33,0,0,0,0/
      IF(FIRST)THEN
         FIRST=.FALSE.
         IRET = VFADIM(FULUW)
C
C  Full size of a face
C
         FULLU = FULUW(2)*.5
         FULLW = FULUW(3)*.5 + ZBUF
         NWFAC = VNRWAF()*VNRMOD()
         ABC(1)=0.
         ABC(2)=0.
         ABC(3)=0.
         NWMOD = NWFAC/2
         DO IWFF = 1,NWFAC
           IF(IWFF.LE.NWMOD)THEN
             JWAF = NWMOD-IWFF+1
           ELSE
             JWAF = IWFF
           ENDIF
           IRET = VABCXY(ABC,JWAF,XYZ)
           ZCENT(IWFF)=XYZ(3)
         ENDDO
         DO IVIEW=1,2
           IRET = VSENSI(IVIEW,SAMIN,SAMAX(IVIEW),SBMIN,SBMAX(IVIEW))
         ENDDO
         VUACT = MAX(SBMAX(1),SBMAX(2))
         VWACT = MAX(SAMAX(1),SAMAX(2))
      ENDIF

C
C  Presets
C
      NHIWA = 0
      NFACE = 0
      IER = 0
C
C  Loop over layers
C
      DO 100, ILAY=1,2
C
C Extrapolate to a Vdet cylinder
C
        IRET = VDEXCY(ILAY,TPAR,ZBUF,GPOS0,IPHI,IWAFF)
        IF(IRET.NE.0)GO TO 100
        DO 110 JPHI=1,2
          IWAF = IWAFF(JPHI)
C
C  Make up the wafer address- always as if to the Z side.  Start
C  even now using the final output arrays- if the point isn't
C  used, the arrays will simply be overwritten.
C
          MHIWA = NHIWA+1
          MFACE = NFACE+1
          CALL VAENWA(IWAFI(MHIWA),ILAY,IWAF,IPHI(JPHI),IZVIEW)
C
C  Precise extrapolation to a single wafer- this includes alignment effe
C
          CALL VTXNWT(IWAFI(MHIWA),TPAR,GPOS0,
     &    LCPOS(1,MHIWA),GBPOS(1,MHIWA),LCMOM(1,MHIWA),
     &    LCDER(1,1,MHIWA),IRET)
C
C  If the precise extrapolation fails, skip this wafer
C
          IF(IRET .NE. 0)GOTO 110
C
C GT 21 May 1993
C check that Z wafer equal that from cylindrical extrapolation
C if not redo with correct Z wafer
C
          MINZ2 = 1000.
          IWAF2 = VWFFND(GBPOS(3,MHIWA))
          IF(IWAF2.NE.IWAF) THEN
            CALL VAENWA(IWAFI(MHIWA),ILAY,IWAF2,IPHI(JPHI),IZVIEW)
            CALL VTXNWT(IWAFI(MHIWA),TPAR,GPOS0,
     &            LCPOS(1,MHIWA),GBPOS(1,MHIWA),LCMOM(1,MHIWA),
     &            LCDER(1,1,MHIWA),IRET)
            IF(IRET .NE. 0)GOTO 110
            IWAF = IWAF2
          ENDIF
C
C  Transform from wafer coordinates into
C  the face coordinates; U is the same, W is given the
C  face-center offset.  Momentum, wafer ID are the same
C
          IFACE(MFACE) = IWAFI(MHIWA)
          FPOS(1,MFACE) = LCPOS(1,MHIWA)
          FPOS(2,MFACE) = LCPOS(2,MHIWA)+ZCENT(IWAF)
          DO ICOR=1,3
            FMOM(ICOR,MFACE) = LCMOM(ICOR,MHIWA)
          END DO
C
C  Get X-Y hit radius from global position
C
          FRAD(MFACE) = SQRT(GBPOS(1,MHIWA)**2+GBPOS(2,MHIWA)**2)
C
C  Calculate new error vector from derrivatives and track co-variance ma
C  U and W error 'vectors' are solved separately
C
          CALL TRSA(TERR,LCDER(1,1,MHIWA),QCDER(1,1,MHIWA),5,1)
          CALL TRSA(TERR,LCDER(1,2,MHIWA),QCDER(1,2,MHIWA),5,1)
C
C  Calculate the diagonals of the
C  error matrix, as these are needed to check the position.
C
          IUERR = (2*MHIWA-1)*MHIWA
          IWERR = (2*MHIWA+1)*MHIWA
          LCERR(IUERR) = RVMPY(5,LCDER(1,1,MHIWA),LCDER(2,1,MHIWA),
     &    QCDER(1,1,MHIWA),QCDER(2,1,MHIWA))
          LCERR(IWERR) = RVMPY(5,LCDER(1,2,MHIWA),LCDER(2,2,MHIWA),
     &    QCDER(1,2,MHIWA),QCDER(2,2,MHIWA))
C
C  Check for negative errors; these are crazy tracks
C  Set the error to 200 microns for U, 2 mm for W if there's no
C  track information
C
          IF(LCERR(IUERR) .GT. 0.)THEN
            LERR(1) = SQRT(LCERR(IUERR))
          ELSE
            LERR(1) = 0.02
            LCERR(IUERR) = LERR(1)**2
          END IF
          IF(LCERR(IWERR) .GT. 0.)THEN
            LERR(2) = SQRT(LCERR(IWERR))
          ELSE
            LERR(2) = .2
            LCERR(IWERR) = LERR(2)**2
          END IF
C
C  Fiducial cuts on the wafer, relative to N sigma
C
          IF(ABS(LCPOS(1,MHIWA)).LT.VUACT+RSIGU*LERR(1).AND.
     &       ABS(LCPOS(2,MHIWA)).LT.VWACT+RSIGW*LERR(2))THEN
            NHIWA = NHIWA + 1
C
C  Check to see if the hit is near a boundary- If so, set a flag bit.
C
            IFLAG(NHIWA) = 0
            IF(ABS(ABS(LCPOS(1,NHIWA))-VUACT).GT.RSIGU*LERR(1))
     &      IFLAG(NHIWA) = NSIGUB
            IF(ABS(ABS(LCPOS(2,NHIWA))-VWACT).GT.RSIGW*LERR(2))
     &      IFLAG(NHIWA) = IOR(IFLAG(NHIWA),NSIGWB)
          END IF
C
C  Fiducial cuts for the face; no great accuracy is needed here.
C  Note that the full 5.12 size is used for the U position
C
          IF(ABS(FPOS(1,MFACE)).LT. FULLU+RSIGU*LERR(1).AND.
     &             ABS(FPOS(2,MFACE)).LT.FULLW+RSIGW*LERR(2))THEN
            NFACE = NFACE + 1
C
C  If the extrapolated position is actually in the face, set the flag
C  to 1; otherwise set the flag to 0
C
            IF(ABS(FPOS(1,MFACE)).LT. FULLU .AND.
     &         ABS(FPOS(2,MFACE)).LT.FULLW)THEN
              JFLAG(NFACE) = 1
            ELSE
              JFLAG(NFACE) = 0
            END IF
          END IF
  110   CONTINUE
  100 CONTINUE
      IF( NHIWA .GT. 0 ) THEN
C
C  Calculate the error matrix off-diagonal terms- the diagonal terms
C  were already calculated when the distance to the wafer edge was
C  checked
C
C
C  First, the correlation between U and W in the same wafer
C
        DO 190 IWAF=1,NHIWA
          IUW = (2*IWAF+1)*IWAF-1
          LCERR(IUW) = RVMPY(5,LCDER(1,1,IWAF),LCDER(2,1,IWAF),
     &                         QCDER(1,2,IWAF),QCDER(2,2,IWAF))
  190   CONTINUE
C
C  Now, the correlations between the U and W hits on different wafers
C
        DO 200 JWAF=1,NHIWA
          DO 200 IWAF=JWAF+1,NHIWA
C
C  Calculate where in the co-variance matrix the given cross term
C  goes- this is stored in 2 arrays.
C
C  Correlation between 2 Us on different wafers
C
            IUU = IBART(IWAF,JWAF)
C
C  Correlation between U of first wafer (JWAF) with W of second (IWAF)
C
            IUW = JBART(IWAF,JWAF)
C
C  Correlation between W of first wafer (JWAF) with U of second (IWAF)
C
            IWU = IUU + 1
C
C  Correlation between 2 Ws
C
            IWW = IUW + 1
C
C  Compute the terms
C
            LCERR(IUU) = RVMPY(5,LCDER(1,1,IWAF),LCDER(2,1,IWAF),
     &                           QCDER(1,1,JWAF),QCDER(2,1,JWAF))
            LCERR(IUW) = RVMPY(5,LCDER(1,1,JWAF),LCDER(2,1,JWAF),
     &                           QCDER(1,2,IWAF),QCDER(2,2,IWAF))
            LCERR(IWU) = RVMPY(5,LCDER(1,2,JWAF),LCDER(2,2,JWAF),
     &                           QCDER(1,1,IWAF),QCDER(2,1,IWAF))
            LCERR(IWW) = RVMPY(5,LCDER(1,2,JWAF),LCDER(2,2,JWAF),
     &                           QCDER(1,2,IWAF),QCDER(2,2,IWAF))
  200   CONTINUE
      END IF
      RETURN
      END
#endif
