      SUBROUTINE YV0ONH(KPOI,PSIB,XM,Z,BV,IFAIL)
C-----------------------------------------------------------------
C! Find the point of helix close to the vertex and the inverse E.M.
CKEY YV0 HELIX VERTEX /INTERNAL
C      AUTHOR    : M.A. Ciocci, L. Rolandi 24/2/88
C      MODIFIED  : M.A. CIOCCI 2/2/90
C
C       INPUT : KPOI/I    The track number in the FRFT bank
C             : PSIB/R    Angle psi for the helix point close
C                         to the vertex
C
C       OUTPUT: XM/D      The inverse of covariance matrix for a
C                         variation of coordinates/momenta around
C                         the values BV(6)
C
C             : Z/D       Derivatives of parameters respect
C                         to the coordinates and momenta
C             : BV/D      BV(6) contains B(1.2.3) the coordinate
C                         B(4.5.6) the momenta computed at the
C                         angle PSIB
C
C             : IFAIL/I   If not 0 error in matrix inversion
C
C              BANK:
C                    FRFT
C
C         CALLED BY YNV0VE
C
C
C                   DESCRIPTION
C                   ===========
C
C     1- Get track parameters from BOS BANK FRFT
C     2- Computes coordinates B(3) at angle PSIB
C     3- Computes momenta P(3) at angle PSIB (Momenta are
C        measured in cm !!!)
C     4- Computes the derivatives Z(5,6) of the track
C        parameters (5) respect to a variation of coordinates/
C        momenta (6)
C     5- Computes the Chi**2 matrix XM(6,6) for a variation
C        of coordinates/momenta as
C                          t
C                   XM =  Z C Z
C
C        REQUIRED LIBRARIES:  BOS,CERN
C---------------------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "frftjj.h"

C
       INTEGER KPOI,IFAIL
       REAL PSIB
       DOUBLE PRECISION BV(6)
C
       DOUBLE PRECISION Q,RIN,LAM,FI0,D0,Z0,RO
       DOUBLE PRECISION SF0,CF0
C
        DOUBLE PRECISION C(5,5),WM1(5,6)
        DOUBLE PRECISION XM(6,6),Z(5,6),SS
        DOUBLE PRECISION B(3),P(3),AX,BX,A2B2
        DOUBLE PRECISION DMY
C
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
      INTEGER NFRFT
      DATA NFRFT / 0 /
#include "bmacro.h"
C
C
             DO 5 JR=1,5
               DO 6 JS=1,6
                Z(JR,JS)=0.
  6            CONTINUE
  5          CONTINUE
         IFAIL=0
        IF ( NFRFT .EQ. 0 ) THEN
          NFRFT = NAMIND( 'FRFT' )
        ENDIF
        KFRFT = IW( NFRFT )
        IF (KFRFT.EQ.0) GOTO 9999
C
C    GET TRACK PARAMETERS
C
        RIN             =-RTABL(KFRFT,KPOI,JFRFIR)
        LAM             =RTABL(KFRFT,KPOI,JFRFTL)
        FI0             =RTABL(KFRFT,KPOI,JFRFP0)
        D0              =RTABL(KFRFT,KPOI,JFRFD0)*SIGN(1.D0,-RIN)
        Z0              =RTABL(KFRFT,KPOI,JFRFZ0)
        Q               =SIGN(1.D0,RIN)
        RO              =Q/RIN
C
        NPOIN=JFRFEM-1
        DO 1 I=1,5
        DO 2 J=1,I
        NPOIN=NPOIN+1
        C(I,J)=RTABL(KFRFT,KPOI,NPOIN)
        C(J,I)=C(I,J)
  2     CONTINUE
  1     CONTINUE
        CALL YFIXIS(C,-RIN,IFAIL)
           IF(IFAIL.NE.0)GO TO 9999
C
C  BUILD UP THE MATRIX XM
C
C
        SF0     =SIN(FI0)
        CF0     =COS(FI0)
        P(1)   = RO*COS(Q*DBLE(PSIB)-FI0)
        P(2)   =-RO*SIN(Q*DBLE(PSIB)-FI0)
        P(3)   = RO*LAM
        B(1)   = Q*(RO-D0)*SF0-Q*P(2)
        B(2)   =-Q*(RO-D0)*CF0+Q*P(1)
        B(3)   = Z0+DBLE(PSIB)*P(3)
C
C  BUILD MATRIX Z
C
        AX=B(1)+Q*P(2)
        BX=B(2)-Q*P(1)
        A2B2=AX*AX+BX*BX
        Z(1,4)=-Q*P(1)/RO/RO/RO
        Z(1,5)=-Q*P(2)/RO/RO/RO
        Z(2,4)=-LAM*P(1)/RO/RO
        Z(2,5)=-LAM*P(2)/RO/RO
        Z(2,6)= 1.D0/RO
        Z(3,1)=-BX/A2B2
        Z(3,2)= AX/A2B2
        Z(3,4)=-Q*AX/A2B2
        Z(3,5)=-Q*BX/A2B2
        Z(4,1)=-AX/(RO-D0)
        Z(4,2)=-BX/(RO-D0)
        Z(4,4)=-(-Q*BX/(RO-D0)-P(1)/RO)
        Z(4,5)=-(Q*AX/(RO-D0)-P(2)/RO)
        Z(5,1)=-Q*P(3)*Z(3,1)
        Z(5,2)=-Q*P(3)*Z(3,2)
        Z(5,3)=-Q*P(3)*Z(3,3)+1
        Z(5,4)=-Q*P(3)*(Z(3,4)+P(2)/RO/RO)
        Z(5,5)=-Q*P(3)*(Z(3,5)-P(1)/RO/RO)
        Z(5,6)=-PSIB
#ifndef BIT64
        CALL DMMLT(5,5,6,C(1,1),C(1,2),C(2,1),
     $  Z(1,1),Z(1,2),Z(2,1),WM1(1,1),WM1(1,2),WM1(2,1),DMY)
        CALL DMMLT(6,5,6,Z(1,1),Z(2,1),Z(1,2),
     $  WM1(1,1),WM1(1,2),WM1(2,1),XM(1,1),XM(1,2),XM(2,1),DMY)
#else
        CALL RMMLT(5,5,6,C(1,1),C(1,2),C(2,1),
     $  Z(1,1),Z(1,2),Z(2,1),WM1(1,1),WM1(1,2),WM1(2,1),DMY)
        CALL RMMLT(6,5,6,Z(1,1),Z(2,1),Z(1,2),
     $  WM1(1,1),WM1(1,2),WM1(2,1),XM(1,1),XM(1,2),XM(2,1),DMY)
#endif
        DO 501 I=1,3
        BV(I)=B(I)
        BV(I+3)=P(I)
 501    CONTINUE
        RETURN
 9999     CONTINUE
          IFAIL=1
        RETURN
        END
#endif
