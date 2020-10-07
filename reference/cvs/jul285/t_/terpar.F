      SUBROUTINE TERPAR( PCA, ZCO, TALA, NHP, ICOR, SIGRFI, SIGZ )
C
C----------------------------------------------------------------C
C! Parameterize TPC coordinate errors after the initial fit      C
C                                                                C
C---        TPC error parametrization                            C
C           Author: T.LOHSE        7.DEC 1989                    C
C           Modified: R. Johnson  20-OCT-1990                    C
C                     To read database if necessary              C
C                                                                C
C     INPUT:    PCA   Pad-crossing angle at coordinate [rad]     C
C               ZCO   Z coordinate value [cm]                    C
C               TALA  TAN(DIP ANGLE)    (global track parameter) C
C               NHP   Number of half-pads in this coordinate     C
C               ICOR  Landau correction flag (two digits):       C
C                     x1 -> r-phi coordinate corrected           C
C                     1x -> z coordinate corrected               C
C                     (Note:  currently a dummy argument         C
C                                                                C
C     OUTPUT:   SIGRFI   Error on R*PHI coordinate  [cm]         C
C               SIGZ     Error on Z coordinate  [cm]             C
C                                                                C
C----------------------------------------------------------------C
#ifndef DOC
C
#include "bcs.h"
#include "ternjj.h"
#include "evehjj.h"
C
C
      INTEGER AGETDB
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTERN=NAMIND('TERN')
        NEVEH=NAMIND('EVEH')
      ENDIF
      SIGRFI= 0.02
      SIGZ= 0.12
C
C++   Get the run number
C
      KEVEH=IW(NEVEH)
      IF (KEVEH.EQ.0) THEN
        CALL RERROR('TERPAR',1,'No EVEH bank for run number'//
     +               '  TPC coordinate errors will be rubbish.')
        RETURN
      ELSE
        IRUN=IW(KEVEH+JEVERN)
      ENDIF
C
C++   Get the database bank with the constants
C
      KTERN=IW(NTERN)
      IF (KTERN.EQ.0) THEN
        IRET=AGETDB('TERN',IRUN)
        IF (IRET.EQ.0) THEN
          CALL RERROR('TERPAR',2,'Cannot find the TERN bank.'//
     +               '  TPC coordinate errors will be rubbish.')
          RETURN
        ENDIF
        KTERN=IW(NTERN)
      ELSE
        IF (IRUN.LT.IW(KTERN+LMHLEN+JTERVR)
     &       .OR. IRUN.GT.IW(KTERN+LMHLEN+JTERVR+1)) THEN
          IRET=AGETDB('TERN',IRUN)
          IF (IRET.EQ.0) THEN
            CALL RERROR('TERPAR',2,'Cannot find the TERN bank.'//
     +                 '  TPC coordinate errors will be rubbish.')
            RETURN
          ENDIF
          KTERN=IW(NTERN)
        ENDIF
      ENDIF
C
      KTERN=KTERN+LMHLEN
      SU0= RW(KTERN+JTERU0)
      SUA= RW(KTERN+JTERUA)
      SUZ1= RW(KTERN+JTERU1)
      SUZ2= RW(KTERN+JTERU2)
      SUZ3= RW(KTERN+JTERU3)
      SZ0= RW(KTERN+JTERZ0)
      SZL= RW(KTERN+JTERZL)
      ZA     = ABS ( ZCO )
      XLA    = ABS ( ATAN( TALA ) )
      TPCA   = TAN(PCA)
      SIGRFI = SQRT( SU0**2 + (SUA*TPCA)**2 )
      SIGRFI = SIGRFI* ( SUZ1 + SUZ2*ZA + SUZ3*ZA**2 )
      SIGZ   = SZ0 + XLA*SZL
C
C++   Special treatment for half-pad coordinates
C
      IF ( NHP .EQ. 1 ) SIGRFI = SIGRFI * RW(KTERN+JTERE1)
      IF ( NHP .EQ. 2 ) SIGRFI = SIGRFI * RW(KTERN+JTERE2)
C
      RETURN
      END
#endif
