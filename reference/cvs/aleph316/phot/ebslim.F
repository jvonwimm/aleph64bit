        SUBROUTINE EBSLIM( TETA , PHI , SLIM , IER )
C ----------------------------------------------------
C   AUTHOR   : J.Badier    17/04/89
C! Stacks widths crossed along a projective line of flight.
CKEY PHOTONS STACK / INTERNAL
C
C   Along a line of flight , one crosses 3 'stacks' whose limits are
C   calculated and put into SLIM(6) after a conversion in radiation
C   lengthes. The origin is the end of the TPC gaz.
C   SLIM(1) : Begining of stack 1 = TPC outer wall + ECAL front plate.
C   SLIM(2) : End of stack 2 = SLIM(1) + stack 2 width.
C   SLIM(3) : Begining of stack 2 = SLIM(2) + separation layer
C   SLIM(4) .........
C   .................
C
C   In the barrel endcap overlap one uses the following convention :
C   Irow = 51,52,177 or 178 :
C                        'stack 1' = Barrel stacks 1 , 2 and 3
C                        'stack 2' = Petal stack 1
C                        'stack 3' = Petal stacks 2 and 3
C   Irow = 53,54,55,174,175 or 176 :
C                        'stack 1' = Barrel stack 1
C                        'stack 2' = Barrel stacks 2 and 3
C                        'stack 3' = Petal stacks 1 , 2 and 3
C   Near the overlap the truncation of the modules is taken in
C   account.
C   The lines of flight which are into the cracks between modules
C   are out of the sensitive region : the SLIM output correspond to the
C   nearest good region.
C
C   Input  :  TETA      Theta angle of the line of flight in radians.
C             PHI       Phi angle in radians.
C
C   Output :  SLIM(6)   Stacks limits in rad. lengthes.
C             IER       Error code.
C                       0 : if succesful SLIM calculation.
C                       1 : Missing bank.
C                       2 : Too many storeys
C                       3 : No storey in this cluster.
C                       4 : Energy > EMIN = .05
C                       5 : Energy < EMIN = 80.
C                      -1 : Truncated endcap , 3 stacks.
C                      -2 : Truncated endcap , 2 stacks.
C                      -3 : Overlap near endcap , 2 stacks.
C                      -4 : Overlap near endcap , 3 stacks.
C                      -5 : Overlap near barrel.
C                      -6 : Truncated barrel , 2 stacks.
C                      -7 : Truncated barrel , 3 stacks.
C
C   BANKS :
C     INPUT   : EGVP , EGTH
C     OUTPUT  : NONE
C     CREATED : NONE
C
C   Calls  EVOLPL , EBCOSI , EMPACT, EFNDSC , EFNDMD , EFNDLG , EFNDCL
C          NAMIND  EVRADL
C   Called by EBPRGA
C-----------------------------------------------------
#ifndef DOC
      SAVE
      DIMENSION SLIM(*)
      DIMENSION PLAN(4,8) , DIST(6,2) , DRAD(6,2) , RADL(5,2)
      DIMENSION PIOP(12) , PIOM(12) , PIN(3,2) , POU(3,2)
      DIMENSION DIR(3) , DIN(2) , DOU(2)
C   Radiation lengthes before the sensitive regions.
      PARAMETER( RDBA = .286 , RDEC = .566 )
C   Theta angle limits for untroncated modules.
C   Overlap limits.
      PARAMETER( LEC1 = 46 , LBA1 = 55 , LBA2 = 174 , LEC2 = 183 )
C   Coefficients to interpolate between modules.
      PARAMETER( DPHI = .03 , EPHI = .51 , RADA = 37. )
#include "egthjj.h"
#include "egvpjj.h"
#include "bcs.h"
      EXTERNAL EFNDSC , EFNDMD , EFNDLG , EFNDCL , EVRADL , NAMIND
      INTEGER  EFNDSC , EFNDMD , EFNDLG , EFNDCL
      DATA KDEB / 0 /
#include "bmacro.h"
C
      IER = 0
      IF( KDEB .EQ. 0 ) THEN
        KDEB = 1
C
C   Initialisation. ____________________________________________
C
      NEGVP = NAMIND( 'EGVP' )
      NEGTH = NAMIND( 'EGTH' )
C   The table DIST gives the positions of the stacks planes in cm.
C   Barrel
        CALL EVOLPL('B stack1',2,0,LEPL,PLAN)
        DIST(1,1) = - PLAN(4,1)
        DIST(2,1) =   PLAN(4,2)
        CALL EVOLPL('B stack2',2,0,LEPL,PLAN)
        DIST(3,1) = - PLAN(4,1)
        DIST(4,1) =   PLAN(4,2)
        CALL EVOLPL('B stack3',2,0,LEPL,PLAN)
        DIST(5,1) = - PLAN(4,1)
        DIST(6,1) =   PLAN(4,2)
C   End cap.
        CALL EVOLPL('E stack1',1,0,LEPL,PLAN)
        DIST(1,2) = - PLAN(4,6)
        DIST(2,2) =   PLAN(4,5)
        CALL EVOLPL('E stack2',1,0,LEPL,PLAN)
        DIST(3,2) = - PLAN(4,6)
        DIST(4,2) =   PLAN(4,5)
        CALL EVOLPL('E stack3',1,0,LEPL,PLAN)
        DIST(5,2) = - PLAN(4,6)
        DIST(6,2) =   PLAN(4,5)
C   Radiation lengthes of the materials : RADL(I,IDET)
C   I = 1 : Stack 1 ; I = 2 : Intermediate layer ; I = 3 : Stack 2
C   I = 4 : Intermediate layer ; I = 5 : Stack 3 .
        RADL(1,1) = EVRADL('B stack1')
        RADL(3,1) = RADL(1,1)
        RADL(5,1) = EVRADL('B stack3')
        RADL(1,2) = EVRADL('E stack1')
        RADL(3,2) = RADL(1,2)
        RADL(5,2) = EVRADL('E stack3')
        RADL(2,1) = EVRADL('B PVC layer 1')
        RADL(4,1) = RADL(2,1)
        RADL(2,2) = EVRADL('E Alu layer 1')
        RADL(4,2) = RADL(2,2)
C   DRAD : Distances of the planes limiting the stacks expressed
C   in units of radiation lengthes. The origin is the end of the TPC gas.
C   Outer wall of the TPC and EMCAL front plate :.
        DRAD(1,1) = RDBA
        DRAD(1,2) = RDEC
C   J = 1 : Barrel ; J = 2 : End cap .
        DO 1 J = 1 , 2
          DO 2 I = 1 , 5
            DRAD(I+1,J) = DRAD(I,J) +
     +      ( DIST(I+1,J) - DIST(I,J) ) / RADL(I,J)
    2     CONTINUE
    1   CONTINUE
      ENDIF
C
C   End of initialisation. _______________________________________
C
      DO 3 I = 1,6
    3   SLIM(I) = 0.
C   Search theta range.
C   Endcap extremity ?
      KEGTH = IW( NEGTH )
      IF( KEGTH .EQ. 0 ) GO TO 101
C   Theta angle range.
C   Out of ECAL.
      IF( TETA .LE. RTABL(KEGTH,1,JEGTT1)     .OR.
     +    TETA .GE. RTABL(KEGTH,2,JEGTT1) ) GO TO 103
C   End cap edge.
      IF( TETA .LT. RTABL(KEGTH,1,JEGTT2)     .OR.
     +    TETA .GT. RTABL(KEGTH,2,JEGTT2) ) GO TO 104
C   Overlap endcap-barrel ?
      IF( TETA .LT. RTABL(KEGTH,1,JEGTT3) ) THEN
        ISC = 1
        IDET = 2
      ELSE
        IF( TETA .GT. RTABL(KEGTH,2,JEGTT3) ) THEN
          ISC = 3
          IDET = 2
        ELSE
          IF( TETA .LE. RTABL(KEGTH,1,JEGTT6)     .OR.
     +        TETA .GE. RTABL(KEGTH,2,JEGTT6) ) GO TO 20
          ISC = 2
          IDET = 1
        ENDIF
      ENDIF
C
C   Out of the overlap ,module full width.
C
      CALL EBCOSI( TETA , PHI , ISC , COSI )
      DO 11 I = 1 , 6
        SLIM(I) = DRAD(I,IDET) / COSI
   11 CONTINUE
      IER = 0
      GO TO 98
C
C Overlap region.
C
   20 CONTINUE
C    Search sensitive region.
      DIR(1) = COS( PHI )
      DIR(2) = SIN( PHI )
      DIR(3) = 1. / TAN( TETA )
      CALL EMPACT( DIR , NSC , PIN , POU )
      IF( NSC .EQ. 0 ) THEN
C   There is a crack.
C   One increases PHI
        PHIP = PHI + DPHI
        DIR(1) = COS( PHIP )
        DIR(2) = SIN( PHIP )
        CALL EMPACT( DIR , NSC , PIOP(1) , PIOP(7) )
        IF( NSC .NE. 1 ) GO TO 105
C   One decreases PHI
        PHIM = PHI - DPHI
        DIR(1) = COS( PHIM )
        DIR(2) = SIN( PHIM )
        CALL EMPACT( DIR , NSC , PIOM(1) , PIOM(7) )
        IF( NSC .NE. 1 ) GO TO 105
C   Mean value
        DO 22 I = 1 , 3
          PIN(I,1) = EPHI * ( PIOP(I) + PIOM(I) )
          POU(I,1) = EPHI * ( PIOP(I+6) + PIOM(I+6) )
   22   CONTINUE
      ENDIF
C   Search subcomponent , module and row.
      ISC = EFNDSC( PIN(1,1) )
      IMD = EFNDMD( ISC , PIN(1,1) )
      IROW = EFNDLG( ISC ,IMD , PIN(1,1) , 'ALEPH' )
C   Number of detectors in this row direction.
      IF( ( IROW .GE. LEC1 .AND. IROW .LE. LBA1 )     .OR.
     +    ( IROW .LE. LEC2 .AND. IROW .GE. LBA2 ) ) GO TO 30
C
C   Only one truncated detector.
C
      IF( NSC .NE. 1 ) GO TO 105
C   Sensitive length.
   26 SEN1 = 0.
      DO 23  I = 1 , 3
        SEN1 = SEN1 + ( POU(I,1) - PIN(I,1) ) ** 2
   23 CONTINUE
      CALL EBCOSI( TETA , PHI ,  ISC , COSI )
      IDET = ABS( ISC - 2 ) + 1
      SEN1 = SQRT( SEN1 ) * COSI + DIST(1,IDET)
      SLIM(1) = DRAD(1,IDET)
      DO 24 I = 1 , 5
        IF( SEN1 .LT. DIST(I,IDET) ) THEN
          SLIM(I+1) = SLIM(I)
        ELSE
          IF( SEN1 .LT. DIST(I+1,IDET) ) THEN
            SLIM(I+1) = SLIM(I) +
     +      (SEN1 - DIST(I,IDET)) / RADL(I,IDET)
          ELSE
            SLIM(I+1) = SLIM(I) +
     +      (DIST(I+1,IDET) - DIST(I,IDET)) / RADL(I,IDET)
          ENDIF
        ENDIF
   24 CONTINUE
      DO 25 I = 1 , 6
        SLIM(I) = SLIM(I) / COSI
   25 CONTINUE
      IF( SLIM(5) .GE. SLIM(6) ) THEN
        SLIM(6) = SLIM(5)
        IER = -2
        IF( ISC .EQ. 2 )  IER = -6
      ELSE
        IER = -1
        IF( ISC .EQ. 2 ) IER = -7
      ENDIF
      GO TO 98
C
C   Overlap region with two detectors.
C
   30 CONTINUE
      IF( NSC .EQ. 1 ) THEN
C   It may be a crack one looks aside
C   One increases PHI
        PHIP = PHI + DPHI
        DIR(1) = COS( PHIP )
        DIR(2) = SIN( PHIP )
        CALL EMPACT( DIR , NSC , PIOP(1) , PIOP(7) )
        IF( NSC .NE. 2 ) GO TO 26
C   One decreases PHI
        PHIM = PHI - DPHI
        DIR(1) = COS( PHIM )
        DIR(2) = SIN( PHIM )
        CALL EMPACT( DIR , NSC , PIOM(1) , PIOM(7) )
        IF( NSC .NE. 2 ) GO TO 26
C   Mean value
        DO 32 I = 1 , 6
          PIN(I,1) = EPHI * ( PIOP(I) + PIOM(I) )
          POU(I,1) = EPHI * ( PIOP(I+6) + PIOM(I+6) )
   32   CONTINUE
      ENDIF
C   Search first subcomponent.
      ISC = EFNDSC( PIN(1,1) )
C   Incidence angle : COSI
      CALL EBCOSI( TETA , PHI , ISC , COSI )
C   Search second subcomponent.
      ISC2 = EFNDSC( PIN(1,2) )
C   Incidence angle : COS2
      CALL EBCOSI( TETA , PHI , ISC2 , COS2 )
C   Sensitive regions.
      DO 35 J = 1 , 2
        DIN(J) = 0.
        DOU(J) = 0.
        DO 33 I = 1 , 3
          DIN(J) = DIN(J) + PIN(I,J) ** 2
          DOU(J) = DOU(J) + POU(I,J) ** 2
   33   CONTINUE
   35 CONTINUE
C   Search barrel row.
      IF( ISC .EQ. 2 ) THEN
C   Barrel in first then endcap.
        IMD = EFNDMD( ISC , PIN(1,1) )
        IROW = EFNDLG( ISC , IMD , PIN(1,1) , 'ALEPH' )
        ICOL = EFNDCL( ISC , IMD , PIN(1,1) , 'ALEPH' )
        COSB = COSI
        COSE = COS2
        DEB = SQRT( DIN(1) )
        SEN1 = SQRT( DOU(1) )
        GAP = SQRT( DIN(2) )
        SEN2 = SQRT( DOU(2) )
      ELSE
C   Endcap in first then barrel.
        IMD = EFNDMD( ISC2 , PIN(1,2) )
        IROW = EFNDLG( ISC2 ,IMD , PIN(1,2) , 'ALEPH' )
        ICOL = EFNDCL( ISC2 ,IMD , PIN(1,2) , 'ALEPH' )
        COSB = COS2
        COSE = COSI
        DEB = SQRT( DIN(2) )
        SEN1 = SQRT( DOU(2) )
        GAP = SQRT( DIN(1) )
        SEN2 = SQRT( DOU(1) )
      ENDIF
C   Sensitive widthes.
      SEN2 = SEN2 - GAP
      GAP = GAP - SEN1
      SEN1 = SEN1 - DEB
C   One searches in the EGVP table the mean radiation length of
C   the matter between barrel and endcap as a function of the azimuth.
      KEGVP = IW( NEGVP )
      IF( KEGVP .NE. 0 ) THEN
        ICC = MOD( ICOL - 1 , 16 )
        ICR = 1 + ( ICOL - 1 ) / 16
        IF( TETA .GT. 1. ) ICR = ICR + 24
        MXCL = LROWS( KEGVP )
        IF(ICR .LE. 0 .OR. ICR .GT. MXCL) THEN
          RAFI = RADA
        ELSE
          RAFI =  RTABL(KEGVP,ICR,ICC+JEGVRL)
        ENDIF
      ELSE
        RAFI = RADA
      ENDIF
C   The two rows of towers near the endcap are named ENDCAP.
C   The three remaining are named BARREL.
      IF( IROW .EQ. LBA1-4 .OR. IROW .EQ. LBA1-3 .OR.
     +    IROW .EQ. LBA2+4 .OR. IROW .EQ. LBA2+3 ) THEN
C   ENDCAP type tower.
        SLIM(1) = DRAD(1,1) / COSB
        SLIM(2) = SLIM(1) + SEN1 / RADL(1,1)
        SLIM(3) = SLIM(2) + GAP / RAFI
        DELT = ( DIST(2,2) - DIST(1,2) ) / COSE
        SLIM(4) = SLIM(3) + MIN( DELT , SEN2 ) / RADL(1,2)
        IF( SEN2 .LE. DELT ) THEN
          SLIM(5) = SLIM(4)
          SLIM(6) = SLIM(5)
          IER = -3
        ELSE
          SLIM(5) = SLIM(4) +
     +    ( DIST(3,2) - DIST(2,2) ) / ( COSE * RADL(2,2) )
          SLIM(6) = SLIM(5) + ( SEN2 - DELT ) / RADL(3,2)
          IER = -4
        ENDIF
C
      ELSE
C   BARREL type tower.
        SLIM(1) = DRAD(1,1) / COSB
        SLIM(2) = SLIM(1) +
     +  ( DIST(2,1) - DIST(1,1) ) / ( COSB * RADL(1,1) )
        SLIM(3) = SLIM(2) +
     +  ( DIST(3,1) - DIST(2,1) ) / ( COSB * RADL(2,1) )
        DELT = ( DIST(3,1) - DIST(1,1) ) / COSE
        SLIM(4) = SLIM(3) + ( SEN1 - DELT ) / RADL(3,1)
        SLIM(5) = SLIM(4) + GAP / RAFI
        SLIM(6) = SLIM(5) + SEN2 / RADL(1,2)
        IER = -5
      ENDIF
      GO TO 98
C
C =================== error ===================================
C   No banks.
  101 IER = 1
      GO TO 98
C   Angle out of range.
  103 IER = 3
C   Endcap inner edge
  104 IER = 4
      GO TO 98
C   Pathologic point inside the overlap.
  105 IER = 5
   98 CONTINUE
      RETURN
      END
#endif
