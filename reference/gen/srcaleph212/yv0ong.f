      SUBROUTINE YV0ONG(KPOI,PSIB,XX,PP,BV,XI,IFAIL)
C-----------------------------------------------------------------
C! Find the point of helix close to the vertex and the inverse E.M.
CKEY YV0 HELIX VERTEX /INTERNAL
C      AUTHOR    : M.A. Ciocci 24/3/90
C      MODIFIED  :
C
C       INPUT : KPOI/I    The track number in the FRFT bank
C             : PSIB/R    Angle psi for the helix point close
C                         to the vertex
C
C       OUTPUT: XX/D      covariance matrix of coordinates  at psi
C               PP/D      covariance matrix of the momenta  at psi
C
C               BV/D      BV(6) contains B(1.2.3) the coordinate
C                         B(4.5.6) the momenta computed at the
C                         angle PSIB
C               XI/D      inverse of covariance matrix of the
C                          coordinates  at psi
C
C             : IFAIL/I   If not 0 error in matrix inversion
C
C              BANK:
C                    FRFT
C
C         called by YNV0NF
C
C
C                   DESCRIPTION
C                   ===========
C
C     1- Get track parameters from BOS BANK FRFT
C     2- Computes coordinates B(3) at angle PSIB
C     3- Computes momenta P(3) at angle PSIB (Momenta are
C        measured in cm !!!)
C     4- Computes the derivatives Z(3,5) of the track
C        coordinates  respect to a variation of the parameters
C     5- Computes the derivatives Z1(3,5) of the track
C        momenta  respect to a variation of the parameters
C
C        REQUIRED LIBRARIES:  BOS,CERN
C---------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
C
       INTEGER KPOI,IFAIL
       REAL PSIB
       DOUBLE PRECISION BV(6)
C
       DOUBLE PRECISION Q,RIN,LAM,FI0,D0,Z0,RO
       DOUBLE PRECISION SF0,CF0
C
        DOUBLE PRECISION C(5,5),WM1(3,5),WM2(3,5)
        DOUBLE PRECISION XX(3,3),PP(3,3),Z(3,5),Z1(3,5)
        DOUBLE PRECISION XI(3,3),DMY,S
        REAL WS(3)
        DOUBLE PRECISION B(3),P(3)
C
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C
C
              DO 5 JR=1,3
               DO 6 JS=1,5
                Z(JR,JS)=0.
                Z1(JR,JS)=0.
  6            CONTINUE
  5          CONTINUE
         IFAIL=0
        KFRFT=IW(NAMIND('FRFT'))
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
       S = SIGN(1.D0,-RIN)
       DO 3 I=1,5
          C(1,I) = -C(1,I)
           C(I,1) = -C(I,1)
            C(4,I) = C(4,I)*S
             C(I,4) = C(I,4)*S
  3    CONTINUE
C
C  BUILD UP THE MATRIX XX PP
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
        Z(1,1)=(-SF0-SIN(Q*DBLE(PSIB)-FI0))/RO/RO
        Z(1,3)=Q*(RO-D0)*CF0-Q*RO*COS(Q*DBLE(PSIB)-FI0)
        Z(1,4)=-Q*SF0
        Z(2,1)=(CF0-COS(Q*DBLE(PSIB)-FI0))/RO/RO
        Z(2,3)=-Q*D0*SF0+Q*RO*SIN(Q*DBLE(PSIB)-FI0)
        Z(2,4)=Q*CF0
        Z(3,1)=-Q*LAM*DBLE(PSIB)/RO/RO
        Z(3,2)= RO*DBLE(PSIB)
        Z(3,5)=1.D0
        Z1(1,1)=-Q*COS(Q*DBLE(PSIB)-FI0)/RO/RO
        Z1(1,3)=RO*SIN(Q*DBLE(PSIB)-FI0)
        Z1(2,1)=Q*SIN(Q*DBLE(PSIB)-FI0)/RO/RO
        Z1(2,3)=RO*COS(Q*DBLE(PSIB)-FI0)
        Z1(3,1)=-Q*LAM/RO/RO
        Z1(3,2)= RO
        CALL DMMLT(3,5,5,Z(1,1),Z(1,2),Z(2,1),
     $  C(1,1),C(1,2),C(2,1),WM1(1,1),WM1(1,2),WM1(2,1),DMY)
        CALL DMMLT(3,5,3,WM1(1,1),WM1(1,2),WM1(2,1),
     $  Z(1,1),Z(2,1),Z(1,2),XX(1,1),XX(1,2),XX(2,1),DMY)
        CALL DMMLT(3,5,5,Z1(1,1),Z1(1,2),Z1(2,1),
     $  C(1,1),C(1,2),C(2,1),WM2(1,1),WM2(1,2),WM2(2,1),DMY)
        CALL DMMLT(3,5,3,WM2(1,1),WM2(1,2),WM2(2,1),
     $  Z1(1,1),Z1(2,1),Z1(1,2),PP(1,1),PP(1,2),PP(2,1),DMY)
        DO 501 I=1,3
        BV(I)=B(I)
        BV(I+3)=P(I)
 501    CONTINUE
         DO 4 J=1,3
          DO 7 L=1,3
           XI(J,L)=XX(J,L)
  7       CONTINUE
  4      CONTINUE
       CALL DINV(3,XI,3,WS,IFAIL)
       IF(IFAIL.NE.0)GO TO 9999
        RETURN
 9999     CONTINUE
          IFAIL=1
        RETURN
        END
