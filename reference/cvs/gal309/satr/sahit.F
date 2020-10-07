*DK sahit
      SUBROUTINE SAHIT
C-----------------------------------------------------------------------
C!    calculate hit and fill SAHI bank
C                                   H.Burkhardt   October 1986
C                       modified by E.Neugebauer  December 1989
C                         (introduce alignment corrections)
C!    Modified:   H. Meinhard       03-Aug-1990  (1)
C     (read alignment constants from LALI)
C
C      called by GUSTEP                      from this .HLB
C      when track is in SATR acceptance
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
#include "alcons.h"
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "sanamc.h"
#include "satrco.h"
#include "trkcom.h"
C
C shifts of modules 1 to 4 due to alignment corrections:
      DIMENSION DMX(4), DMY(4), DMZ(4)
C
C rotations of modules 1 to 4 due to alignment corrections:
      DIMENSION ROX(4), ROY(4), ROZ(4)
C
C rotation, position of track element, result of cross product:
      DIMENSION ROT(3), POT(3), DRO(3)
C
C shifted track elements TRKNXT and TRKELE:
      DIMENSION TRKN(3), TRKE(3)
C
C nominal z-position of LCAL front:
      DATA ZNOM / 262.5 /
C
      LOGICAL DEBUG, LFIRST
      DATA LFIRST / .TRUE. /
C
#include "bmacro.h"
C
C initialisation: get the alignment constants
C
      IF ( LFIRST ) THEN
C
         LFIRST = .FALSE.
         KLALI = IW( NAMIND( 'LALI' ) )
         IF ( KLALI .EQ. 0 )                                GOTO 901
         DO 100 I = 1, 4
            DMX( I ) = RTABL( KLALI, 5-I, 4 )
            DMY( I ) = RTABL( KLALI, 5-I, 5 )
            DMZ( I ) = RTABL( KLALI, 5-I, 6 )
            ROX( I ) = - RTABL( KLALI, 5-I, 7 )
            ROY( I ) = - RTABL( KLALI, 5-I, 8 )
            ROZ( I ) = - RTABL( KLALI, 5-I, 9 )
  100    CONTINUE
C
      ENDIF
C
C     digitize only when particle is charged and
C     leaving the detector
      IF(ITRKEL(8).NE.2.) GOTO 901
      IF(FBEGJO(6)) THEN
         DEBUG = FDEBJO .AND. ICSAJO(1).GT.0
C       initialize SAHIt bank for new event
        FBEGJO(6)=.FALSE.
C       count the number of events which hit the SATR
        XEVTSA=XEVTSA+1.
C       make sure no bank exits (for example read from INPUT)
        CALL BDROP(IW,'SAHT')
C       book new bank, maximum length is MXSAHT columns
         CALL ALBOS ('SAHT',0,LCSAHT*MXSAHT+LMHLEN,KSAHT,IGARB)
        CALL BLIST(IW,'E+','SAHT')
C       define the columns length
        IW(KSAHT+1)=LCSAHT
C       start with 0 rows
        IW(KSAHT+2)=0
      ENDIF
C
C     average between TRKELE and TRKNXT to get the
C     coordinates at the middle of the path through the
C     chamber
      X=0.5*(TRKNXT(1)+TRKELE(1))
      Y=0.5*(TRKNXT(2)+TRKELE(2))
      Z=0.5*(TRKNXT(3)+TRKELE(3))
C     go from x,y,z to cylindrical system with r and phi,theta
      RTOT=SQRT(X**2+Y**2+Z**2)
      RXY=SQRT(X**2+Y**2)
      PHI=ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C     calculate isid,ilay,isec,iwir from position
      IF(Z.GE.0.) THEN
        ISID=1
      ELSE
        ISID=2
      ENDIF
      ZLUM=ABS(Z)-ZLUMSA
      ILAY=INT(ZLUM/1.9)+1
      ISEC=MOD(INT( (PHI+TWOPI-PHIBSA(ILAY))/PIBY4 ),NSECSA)+1
      IF(DEBUG) THEN
        IF(RTOT.NE.0.) COST=Z/RTOT
        THETA=ACOS(COST)*RADEG
        PHID=PHI*RADEG
        WRITE(LOUTIO,*)
     &  ' +++SAHIT+++ BEGIN OF DEBUG PRINT   ',
     &  'VOLUME name and number: ',TRKVOL,ITRKEL(6)
        WRITE(LOUTIO,*)'  RXY,RTOT= ',RXY,RTOT,
     &  '    ZPOS relativ to SATR window= ',COST,PHI
      ENDIF
C
C get the module number:
C
      IF ( ISID .EQ. 1 ) THEN
         IF ( ISEC .LE. 4 ) THEN
            MODU = 1
         ELSE
            MODU = 2
         ENDIF
      ELSE
         IF ( ISEC .LE. 4 ) THEN
            MODU = 3
         ELSE
            MODU = 4
         ENDIF
      ENDIF
C
C rotation vector of this module:
C
      ROT( 1 ) = ROX( MODU )
      ROT( 2 ) = ROY( MODU )
      ROT( 3 ) = ROZ( MODU )
C
C shift the vector TRKNXT:
C
      POT( 1 ) = TRKNXT( 1 )
      POT( 2 ) = TRKNXT( 2 )
      POT( 3 ) = TRKNXT( 3 )
      IF ( POT( 3 ) .GT. .0 ) THEN
         POT( 3 ) = POT( 3 ) - ZNOM
      ELSE
         POT( 3 ) = POT( 3 ) + ZNOM
      ENDIF
      CALL CROSS( ROT, POT, DRO )
      TRKN( 1 ) = TRKNXT( 1 ) - ( DMX( MODU ) + DRO( 1 ) )
      TRKN( 2 ) = TRKNXT( 2 ) - ( DMY( MODU ) + DRO( 2 ) )
      TRKN( 3 ) = TRKNXT( 3 ) - ( DMZ( MODU ) + DRO( 3 ) )
C
C shift the vector TRKELE:
C
      POT( 1 ) = TRKELE( 1 )
      POT( 2 ) = TRKELE( 2 )
      POT( 3 ) = TRKELE( 3 )
      IF ( POT( 3 ) .GT. .0 ) THEN
         POT( 3 ) = POT( 3 ) - ZNOM
      ELSE
         POT( 3 ) = POT( 3 ) + ZNOM
      ENDIF
      CALL CROSS( ROT, POT, DRO )
      TRKE( 1 ) = TRKELE( 1 ) - ( DMX( MODU ) + DRO( 1 ) )
      TRKE( 2 ) = TRKELE( 2 ) - ( DMY( MODU ) + DRO( 2 ) )
      TRKE( 3 ) = TRKELE( 3 ) - ( DMZ( MODU ) + DRO( 3 ) )
C
C average between TRKE and TRKN to get the
C coordinates at the middle of the path through the
C chamber
      X = 0.5 * ( TRKN(1) + TRKE(1) )
      Y = 0.5 * ( TRKN(2) + TRKE(2) )
      Z = 0.5 * ( TRKN(3) + TRKE(3) )
C     go from x,y,z to cylindrical system with r and phi,theta
      RTOT=SQRT(X**2+Y**2+Z**2)
      RXY=SQRT(X**2+Y**2)
      PHI=ATAN2(Y,X)
      IF(PHI.LT.0.) PHI=PHI+TWOPI
C     calculate isid,ilay,isec,iwir from position
      IF(Z.GE.0.) THEN
        ISID=1
      ELSE
        ISID=2
      ENDIF
      ZLUM=ABS(Z)-ZLUMSA
      ILAY=INT(ZLUM/1.9)+1
      ISEC=MOD(INT( (PHI+TWOPI-PHIBSA(ILAY))/PIBY4 ),NSECSA)+1
      IF(DEBUG) THEN
        IF(RTOT.NE.0.) COST=Z/RTOT
        THETA=ACOS(COST)*RADEG
        PHID=PHI*RADEG
        WRITE(LOUTIO,*)
     &  ' +++SAHIT+++ BEGIN OF 2. DEBUG PRINT   ',
     &  'VOLUME name and number: ',TRKVOL,ITRKEL(6)
        WRITE(LOUTIO,*)'  RXY,RTOT= ',RXY,RTOT,
     &  '    ZPOS relativ to SATR window= ',COST,PHI
      ENDIF
C
C     checking on dead zones
C
C     rotate to a system where the sector starts at 0 degrees
C     to do that subtract the angle of the beginning of the sector
C     do the same for the end of the sector
      PHIRO=PHI+TWOPI-(PHIBSA(ILAY)+FLOAT(ISEC-1)*PIBY4)
      YR1=RXY*SIN(PHIRO)
      YR2=RXY*SIN(PHIRO-PIBY4)
      IF(YR1.LT.DEADSA.OR.YR2.GT.-DEADSA) THEN
      IF (DEBUG) WRITE(LOUTIO,*)'     Hit lost  ILAY,ISEC= ',ILAY,
     &  ISEC,'   left, right dist. to boundary= ',YR1,YR2,
     &       '   in edge region between sectors'
        NLOSSA=NLOSSA+1
        XLOSSA=XLOSSA+1.
        GOTO 900
      ENDIF
C     sectors 1 and 5 start with a gas channel, 1.6 cm dead space
C     sectors 4 and 8 end   with a gas channel
      IF(((ISEC.EQ.1.OR.ISEC.EQ.5).AND.YR1.LT. DGASSA).OR.
     .   ((ISEC.EQ.4.OR.ISEC.EQ.8).AND.YR2.GT.-DGASSA)) THEN
      IF (DEBUG) WRITE(LOUTIO,*)'     Hit lost  ILAY,ISEC= ',ILAY,
     &  ISEC,'   left, right dist. to boundary= ',YR1,YR2,
     &       '   in gas channel'
        NLOSSA=NLOSSA+1
        XLOSSA=XLOSSA+1.
        GOTO 900
      ENDIF
C
C     rotate to a system where the sector is symmetric to 0
C     in this system it is easy to get the distance wire, hit
      PHIRO=PHIRO-PIBY8
      POS=RXY*COS(PHIRO)
      DIST=POS-RMINSA
      IWIR=INT(DIST/BRTHSA)+1
C check if we are still in the sensitive area (may be not the
C case anymore due to alignment corrections!):
      IF ( IWIR .LT. 1 .OR. IWIR .GT. 14 ) GOTO 900
      DIST=ABS(AMOD(DIST,BRTHSA)-0.5*BRTHSA)
      NHITSA=NHITSA+1
      XHITSA=XHITSA+1.
      NHLASA(ILAY)=NHLASA(ILAY)+1
      IF (DEBUG) WRITE(LOUTIO,*)
     & '     GOOD HIT IN ISID,ILAY,ISEC,IWIR= ',ISID,ILAY,ISEC,IWIR,
     & ' DIST= ',DIST
C     now pack all the information about this hit in one word
      KSAHT=IW(NASAHT)
C     the dist is always between 0. and .5 cm, multiply with
C     2**16 to use the full 16 bit precision
      IDAT=ISHFT(ISID,28)+ISHFT(ILAY,24)+ISHFT(ISEC,20)+
     &     ISHFT(IWIR,16)+INT(DIST*65536.)
      IF(KSAHT.EQ.0) GOTO 900
      LCOL=LCOLS(KSAHT)
      LROW=LROWS(KSAHT)
      IF(LFRWRD(KSAHT).LT.LCOL) THEN
        NEW=IW(KSAHT)+100*LCSAHT
         CALL ALBOS ('SAHT',0,NEW,KSAHT,IGARB)
      ENDIF
      KSA=KNEXT(KSAHT)
C     fill in the next col  (in this case only a single hit)
      IW(KSA+1)=IDAT
C     increment number of columns
      IW(KSAHT+2)=IW(KSAHT+2)+1
C     exit for dead regions, no digitization, bank problems
  900 CONTINUE
  901 CONTINUE
      END
#endif
