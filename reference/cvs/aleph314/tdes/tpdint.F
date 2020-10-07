      SUBROUTINE TPDINT(HP,JBITS,ISLT,RPHIO,ZO)
C----------------------------------------------------------------
C! Find intersection of ALEPH track with TPC padrows, including
C  all alignment corrections
CKEY TPCDES TRACK TPC / USER
C
C  R. Johnson    12-6-90
C
C  Input:   HP(6)      /R     Helix parameters, as from FRFT
C           JBITS       /I    Pattern of bits telling which
C                             rows to calculate (bit 1=row 1)
C           ISLT(21)   /I     Sector slot number or 0 if the program
C                             could not find an intersection
C           RPHIO(21)  /R     R*PHI of intersection, in
C                             sector coordinates
C           ZO(21)     /R     Z of intersection, in sector
C                             coordinates
C
C----------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "alcons.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tpgeop.h"
#include "talign.h"
C
      DIMENSION HP(*),ISLT(*),RPHIO(*),ZO(*)
      DIMENSION HP1(5),HPT(5),S(2),PHI(2),Z(2),HPS(5)
      DIMENSION NSECR(2),ISLST(24,2),ISECR(LTPDRO)
      DIMENSION X(3),XS(3),IOPP(2)
      DATA DEAD/1.2/,IOPP/2,1/
      DATA ISECR/9*1,12*2/
      DATA NSECR/12,24/
      DATA ISLST/ 1, 2, 3, 4, 5, 6,19,20,21,22,23,24,12*0,
     &            7, 8, 9,10,11,12,13,14,15,16,17,18,
     &           25,26,27,28,29,30,31,32,33,34,35,36/
C
C++   Find new helix parameters after scattering at the ITC-TPC wall
C
      HP1(1)=HP(1)
      HP1(2)=HP(2)
      HP1(5)=HP(5)
      CALL UNEWDP(HP,HP1(4),HP1(3))
C
C++   Transform to the TPC frame (alignment correction)
C
      CALL TGHPAL(HP1,HPT)
C
C++   Loop over all TPC padrows
C
      DO 500 IR=1,NTPROW
        ISLT(IR)=0
C
C++     Skip rows which the user doesn't want to calculate
C
        IF (IBITS(JBITS,IR,1).NE.1) GO TO 500
C
C++     Find intersection point with this padrow.  Skip if none.
C
        CALL THLCIR(HPT,TPPROW(IR),S,PHI,Z,IERR)
        IF (IERR.NE.0) GO TO 500
C
C++     Figure out which end we are in
C
        IF (Z(1).LT.0.) THEN
          IEND=2
        ELSE
          IEND=1
        ENDIF
C
C++     We may have screwed up on the choice of ends, due to alignment
C++     corrections.  If so, we will come back here.  Don't make
C++     more than two tries, however, just in case. . .
C
        NTRY=0
  130   CONTINUE
          NTRY=NTRY+1
C
C++       What is the sector row number?
C
          IF (IR.LE.NTPDRW(1)) THEN
            IRS=IR
          ELSE
            IRS=IR-NTPDRW(1)
          ENDIF
C
C++       Loop over all sectors containing this padrow and determine
C++       which sector the intersection will be in.
C
          INOUT=ISECR(IR)
          DO 450 JJ=1,NSECR(INOUT)
            ISLOT=ISLST(JJ,INOUT)
            IF (IENDTP(ISLOT).NE.IEND) GO TO 450
            ISTYP=ITPTYP(ISLOT)
C
C++         Convert the point to the sector reference frame, ignoring
C++         sector-to-sector alignment corrections.
C
            IF (IEND.EQ.2) THEN
              PHIS= PHI(1) - TPPHI0(ISLOT)
            ELSE
              PHIS= TPPHI0(ISLOT) - PHI(1)
            ENDIF
            IF (PHIS.LT.-PI) THEN
              PHIS=PHIS+TWOPI
            ELSEIF (PHIS.GT.PI) THEN
              PHIS=PHIS-TWOPI
            ENDIF
C
C++         DEAD is the half the width of the dead region between
C++         TPC sectors.  Note that we anticipate that the aligment
C++         corrections will never be large enough to move a point
C++         such that it appears to be in the next sector.
C
            PHD=DEAD/TPPROW(IR)
            IF (ABS(PHIS).LT.TPDPHW(IRS,ISTYP)+PHD) GO TO 451
  450     CONTINUE
          GO TO 500
  451     CONTINUE
C
C++       Transform point to sector frame, including sector-to-sector
C++       alignment corrections.
C++       First, convert to cartesion coordinates
C
          X(1)=TPPROW(IR)*COS(PHI(1))
          X(2)=TPPROW(IR)*SIN(PHI(1))
          X(3)=Z(1)
C
C++       Make a rotation followed by a translation
C
          DO 200 I=1,3
            XS(I)=DTPTOS(I,ISLOT)
            DO 100 J=1,3
              XS(I)=XS(I) + ATPTOS(I,J,ISLOT)*X(J)
  100       CONTINUE
  200     CONTINUE
C
C++       Convert back to cylindrical coordinates.  The angle phi
C++       should be between -pi and pi radians.
C
          RS=SQRT(XS(1)**2+XS(2)**2)
          RPHIS=RS*ATAN2(XS(2),XS(1))
          PHIS=RPHIS/RS
          ZS=XS(3)
C
C++       Roughly transform the track to the sector frame
C
          CALL TGHPTS(ISLOT,HPT,HPS)
C
C++       Find direction of the track at the padrow
C
          ALPHA= S(1)*ABS(HPS(1))/SQRT(1.+HPS(2)**2)
          PHITK= HPS(3) + SIGN(1.0,HPS(1))*ALPHA
          DRPDR= PHITK-PHIS
          IF (ABS(DRPDR).GT.1.5) DRPDR=0.
          DZDR= HPS(2)
C
C++       Extrapolate to the precise padrow radius in the sector frame
C
          DR= TPPROW(IR)-RS
          RS= TPPROW(IR)
          RPHIS= RPHIS + DR*DRPDR
          ZS= ZS + DR*DZDR
C
C++     If the z coordinate is past the TPC membrane, then we must
C++     have chosen the wrong TPC end.  Try again.
C
        IF (ZS.GT.ZTPCMX) THEN
          IF (NTRY.LT.2) THEN
            IEND=IOPP(IEND)
            GO TO 130
          ELSE
            ZS=ZTPCMX
          ENDIF
        ENDIF
C
        RPHIO(IR)= RPHIS
        ZO(IR)= ZS
        ISLT(IR)=ISLOT
C
  500 CONTINUE
C
      END
#endif
