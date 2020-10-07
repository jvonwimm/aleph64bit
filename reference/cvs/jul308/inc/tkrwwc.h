      PARAMETER (JTGWIR=1,JTGWTL=2,JTGWP0=3,JTGWD0=4,JTGWZ0=5,
     &           JTGWCL=6,JTGWXC=7,JTGWYC=8,JTGWFR=9,JTGWLR=10,
     &           JTGWTK=11,LTGFTW=11)
      PARAMETER (JTKWTK=1,JTKWZI=2,JTKWYI=3,JTKWCA=4,JTKWFG=5,LTKWRW=5)
      COMMON/TKRWWC/ ITRKSW(3),ITWATW,ITGFTW,ITKWRW,IWROFW,ITKAPW
#if defined(DOC)
C!  Workbank indices for track-wire association.  This common should not
C  be referenced outside of the module TRKWRA.
C
C  IWROFW= Workbank of wire offsets in TWRR for one sector
C
C  ITKAPW= Workbank of dead wires for one sector
C
C  ITWATW= Workbank for accumulation of pointers from tracks to
C          wire pulses
C
C  ITGFTW= Workbank for storage of track parameters for all
C          tracks within a sector
C
C  +-------------------------------------------------------------------+
C  | +--------+                                                        |
C  | | ITGFTW |           Number of words per track                    |
C  | +--------+           Number of tracks in sector                   |
C  |-------------------------------------------------------------------|
C  |   1    JTGWIR        Inverse radius of curvature                  |
C  |   2    JTGWTL        Tangent of dip angle                         |
C  |   3    JTGWP0        Phi zero                     IN SECTOR FRAME |
C  |   4    JTGWD0        D0                                           |
C  |   5    JTGWZ0        Z0                                           |
C  |   6    JTGWCL        Cosine of dip angle
C  |   6    JTGWXC        X of circle center                           |
C  |   7    JTGWYC        Y of circle center                           |
C  |   8    JTGWFR        Innermost padrow for TGFT track, 0 for TARC  |
C  |   9    JTGWLR        Outermost padrow for TGFT track, 0 for TARC  |
C  |  10    JTGWTK        TGFT track number if > 0, TARC if < 0        |
C  +-------------------------------------------------------------------+
C
C  ITKWRW = workbank to store intersection points of tracks with
C           a single wire in a sector.
C
C  +-------------------------------------------------------------------+
C  | +--------+                                                        |
C  | | ITKWRW |           Number of words per track                    |
C  | +--------+           Number of tracks in sector                   |
C  |-------------------------------------------------------------------|
C  |   1    JTKWTK        Track number in ITGFTW                       |
C  |   2    JTKWZI        Z of intersection with wire                  |
C  |   3    JTKWYI        Y of intersection with wire                  |
C  |   4    JTKWCA        Cosine of angle of intersection              |
C  +-------------------------------------------------------------------+
C
C     Workbank definitions for the track-sector list:
C
C  ITRKSW(1)= list of track and chain numbers (negative for chain)
C  ITRKSW(2)= list of sector numbers
C  ITRKSW(3)= indices for sorting by sector number
C
C-----------------------------------------------------------------------
#endif
