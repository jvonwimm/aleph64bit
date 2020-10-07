      INTEGER FUNCTION UFITQL(ITK)
C----------------------------------------------------------------------
C! Decide on track quality
CKEY COMPUTE TRACK FIT
C!   Author   :- D.Cinabro              3-DEC-1990
C!   Modified :- D.Cinabro             21-MAR-1991
C!             Get beam energy and positon with GETLEP and
C!             include 3 sigma error on momentum.
C!   Modified :- M.Cattaneo            16-JUL-1997
C!             Put cut on NTPC hits, COS(theta) earlier in the routine
C!
C!
C!   Inputs: ITK /I Track number in the FRFT bank
C!        -  FRFT,FRTL,YV0V,EVEH
C!
C!   Outputs: Track quality flag
C!        -   1 = Good track from the origen
C!            2 = Good track but momentum > Ebeam
C!            3 = Good track but from a V0
C!            4 = Bad track
C!            Anything else is an error.
C!
C!   Libraries required:
C!
C!   Description
C!   ===========
C!   Decide on track quality.
C!   Good tracks must pass a cut on NTPC hits, COS(theta)
C!   Those in category 1 must pass D0 and Z0 but not in category 2
C!   Those in category 2 pass D0 and Z0 but have P > Ebeam
C!   Those in category 3 fail D0 and/or Z0 but flagged from a V0
C!   Those in category 4 are in no other category
C!   Cuts will be stored in the data base, and defaults stored in data
C!   statements
C?
C!======================================================================
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "yv0vjj.h"
#include "alcons.h"
#include "evehjj.h"
#include "fgtpjj.h"
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INTEGER ALREDB
      DIMENSION XYZ(3),DXYZ(3),XT(3),CXT(3,3),P(3),CP(3,3)
      INTEGER IDUM
C
C Default cut list
C
      DATA COSM /0.95/
      DATA NTPM /4/
C
      DATA D0MX /2./
      DATA Z0MX /5./
      DATA PCEB /1./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRFT = NAMIND('FRFT')
        NFRTL = NAMIND('FRTL')
        NYV0V = NAMIND('YV0V')
        NEVEH = NAMIND('EVEH')
        KFGTP = MDARD(IW,JUNIDB(IDUM),'FGTP',0)
        IF (KFGTP.GT.0) THEN
          NTPM = ITABL(KFGTP,1,JFGTNT)
          COSM = RTABL(KFGTP,1,JFGTCT)
          D0MX = RTABL(KFGTP,1,JFGTD0)
          Z0MX = RTABL(KFGTP,1,JFGTZ0)
          PCEB = RTABL(KFGTP,1,JFGTBE)
        ENDIF
      ENDIF
C
      UFITQL = 0
      KFRFT = IW(NFRFT)
      IF (KFRFT.LE.0) GOTO 999
      KFRTL = IW(NFRTL)
      KEVEH = IW(NEVEH)
      IF (KEVEH.LE.0) GOTO 999
C
C Beam energy and position.
C
      IRUN = IW(KEVEH+JEVERN)
      CALL GETLEP(IRUN,IFOUN,IFILL,NV,ELEP,XYZ,DXYZ)
      EBEAM = ELEP/2.
      IF (IFOUN.EQ.0) THEN
C
C No information found.  Set beam position to (0,0,0).  GETLEP should give
C a reasonable default energy.
C
        XYZ(1) = 0.
        XYZ(2) = 0.
        XYZ(3) = 0.
      ENDIF
C
C Track parameters from beam
C
      PHI0 = RTABL(KFRFT,ITK,JFRFP0)
      D0B = ABS(RTABL(KFRFT,ITK,JFRFD0)-XYZ(1)*SIN(PHI0)+XYZ(2)*COS(
     &  PHI0))
      Z0B = ABS(RTABL(KFRFT,ITK,JFRFZ0)-XYZ(3))
      NT = ITABL(KFRTL,ITK,JFRTNT)
      PS = ALFIEL(DUM)*CLGHT*1.E-5/RTABL(KFRFT,ITK,JFRFIR)
      APZ = PS*RTABL(KFRFT,ITK,JFRFTL)
      PP = SQRT(PS**2 + APZ**2)
      CO = ABS(APZ/PP)
C
C Must pass cos and tpc hits to be considered good
C
      IF (CO.LE.COSM.AND.NT.GE.NTPM) THEN
C
C Calculate the error on P.  It looks nasty but it is not so difficult
C given the above and the error matrix
C
        S1 = -PP/RTABL(KFRFT,ITK,JFRFIR) * RTABL(KFRFT,ITK,JFRFEM+0)
        S2 = PP*RTABL(KFRFT,ITK,JFRFTL) / 
     &       (1.+RTABL(KFRFT,ITK,JFRFTL)**2)*RTABL(KFRFT,ITK,JFRFEM+2)
        COV = RTABL(KFRFT,ITK,JFRFEM+1)
        EP = SQRT(S1**2 + 2.*COV*S1*S2 + S2**2)
C
C PP is measured momentum - 3sigma error
C
        PP = PP - 3.*EP
C
C If it passes D0 and Z0 then good 1 or 2
C
        IF (D0B.LT.D0MX.AND.Z0B.LT.Z0MX) THEN
C
C If momentum less than Ebeam good, flag 1
C
          IF (PP.LT.PCEB*EBEAM) THEN
            UFITQL = 1
C
C Momentum greater than EBEAM, flag 2
C
          ELSE
            UFITQL = 2
          ENDIF
C
C See if these come from a V0 that points toward the origin
C Idea stolen from Patrick Janot's ENFLW program
C
        ELSE
          KYV0V = IW(NYV0V)
          IF (KYV0V.LE.0) THEN
C
C No V0's then flag it as bad
C
            UFITQL = 4
            GOTO 999
          ENDIF
          DO 20 IV0 = 1,LROWS(KYV0V)
C
C Find a row with this track in it.
C
            IF (ITK.EQ.ITABL(KYV0V,IV0,JYV0K1).OR.
     &          ITK.EQ.ITABL(KYV0V,IV0,JYV0K2)) THEN
C
C Calculate these nasty looking quantities
C
              X = RTABL(KYV0V,IV0,JYV0VX) - XYZ(1)
              Y = RTABL(KYV0V,IV0,JYV0VY) - XYZ(2)
              Z = RTABL(KYV0V,IV0,JYV0VZ) - XYZ(3)
              AVTN1 = RTABL(KYV0V,IV0,JYV0PY)*Y
              AVTN2 = RTABL(KYV0V,IV0,JYV0PX)*X
              AVTD = RTABL(KYV0V,IV0,JYV0PY)**2 + RTABL(KYV0V,IV0,
     &           JYV0PX)**2
              AVT = -(AVTN1 + AVTN2)/AVTD
              DVX = (X + AVT*RTABL(KYV0V,IV0,JYV0PX))**2
              DVY = (Y + AVT*RTABL(KYV0V,IV0,JYV0PY))**2
              DV0B = SQRT(DVX + DVY)
              ZV0B = Z + AVT*RTABL(KYV0V,IV0,JYV0PZ)
              PV0 = SQRT(RTABL(KYV0V,IV0,JYV0PX)**2 +
     &                   RTABL(KYV0V,IV0,JYV0PY)**2 +
     &                   RTABL(KYV0V,IV0,JYV0PZ)**2)
C
C Require the V0 track to pass the same cuts as the good charged tracks
C If so good flag 3.
C
              IF (DV0B.LT.D0MX.AND.ZV0B.LT.Z0MX.AND.PV0.LT.PCEB*EBEAM)
     &             THEN
                UFITQL = 3
                GOTO 999
              ENDIF
            ENDIF
   20     CONTINUE
C
C If here then failed to find a good V0 track is labeled bad.
C
          UFITQL = 4
        ENDIF
C
C Bad guys
C
      ELSE
        UFITQL = 4
      ENDIF
C
  999 RETURN
      END
#endif
