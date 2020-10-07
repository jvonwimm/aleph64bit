      SUBROUTINE YV0ONE(KPOI,PSIB,XM,BV,Z,IFAIL)
C-----------------------------------------------------------------
C! Find the point of helix close to the vertex and the inverse E.M.
C!
C!      AUTHOR    : M.A. Ciocci, L. Rolandi 24/2/88
C!      MODIFIED  :
C!
C!       INPUT : KPOI/I    The track number in the FRFT bank
C!             : PSIB/R    Angle psi for the helix point close
C!                         to the vertex
C!
C!       OUTPUT: XM/D      The inverse of covariance matrix for a
C!                         variation of coordinates/momenta around
C!                         the values BV(6)
C!
C!             : BV/D      BV(6) contains B(1.2.3) the coordinate
C!                         B(4.5.6) the momenta computed at the
C!                         angle PSIB
C!
C!             : Z/D       Z(5,6) contains derivatives of
C!                         track parameters
C!
C!             : IFAIL/I   IF = 1 -- Matrix singular
C!                         IF = 2 -- FRFT bank missing
C?
C?                   DESCRIPTION
C?                   ===========
C?
C?     1- Get track parameters from BOS BANK FRFT
C?     2- Computes coordinates B(3) at angle PSIB
C?     3- Computes momenta P(3) at angle PSIB (Momenta are
C?        measured in cm !!!)
C?     4- Computes the drivatives Z(5,6) of the track
C?        parameters (5) respect to a variation of coordinates/
C?        momenta (6)
C?     5- Computes the Chi**2 matrix XM(6,6) for a variation
C?        of coordinates/momenta as
C?                        t
C?                   XM =  Z C Z
C?
C?        REQUIRED LIBRARIES:  BOS,CERN
C?---------------------------------------------------
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
        REAL CC(5,5)
        DOUBLE PRECISION C(5,5),WM1(5,6)
        DOUBLE PRECISION XM(6,6),Z(5,6),SS
        DOUBLE PRECISION B(3),P(3),AX,BX,A2B2
        DOUBLE PRECISION DMY
#include "bmacro.h"
C
C
        IFAIL=0
        KFRFT=IW(NAMIND('FRFT'))
        IF (KFRFT.EQ.0) GOTO 888
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
        CALL YFIX(C,-RIN,IFAIL)
        IF(IFAIL.NE.0) GOTO 999
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
        DO 11 J=1,6
           DO 10 I=1,5
              Z(I,J) = 0
 10        CONTINUE
 11     CONTINUE
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
C
        GOTO 999
 888   CONTINUE
C       'THE POINTER KFRFT EQ 0'
        IFAIL=1
 999    RETURN
        END
#endif
