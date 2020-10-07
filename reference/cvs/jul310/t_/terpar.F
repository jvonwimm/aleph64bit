      SUBROUTINE TERPAR( PCA, ZCO, TALA, NHP, ICOR, SIGRFI, SIGZ )
C
C----------------------------------------------------------------C
C! Parameterize TPC coordinate errors after the initial fit      C
C                                                                C
C---        TPC error parametrization                            C
C           Author: T.LOHSE        7.DEC 1989                    C
C           Modified: R. Johnson  20-OCT-1990                    C
C                     To read database if necessary              C
C                     D. Casper   12-Jun-1995                    C
C                     Treat wire-corrected coordinates           C
C                     Replace TERN bank with TLCE                C
C                                                                C
C     INPUT:    PCA   Pad-crossing angle at coordinate [rad]     C
C               ZCO   Z coordinate value [cm]                    C
C               TALA  TAN(DIP ANGLE)    (global track parameter) C
C               NHP   Number of half-pads in this coordinate     C
C               ICOR  Wire correction flag                       C
C                     First two digits flag Landau corrections   C
C                     0 = neither coordinate was corrected       C
C                     1 = z coordinate (only) corrected          C
C                    10 = rphi coordinate (only) was corrected   C
C                    11 = both rphi and z coordinates corrected  C
C                     Higher digits indicate that the coordinate C
C                     has wire data in TWZZ                      C
C                ####XX = row of TWZZ bank containing wire       C
C                                                                C
C     OUTPUT:   SIGRFI   Error on R*PHI coordinate  [cm]         C
C               SIGZ     Error on Z coordinate  [cm]             C
C                                                                C
C----------------------------------------------------------------C
#ifndef DOC

#include "bcs.h"
#include "evehjj.h"
#include "tlcejj.h"
C
      INTEGER AGETDB,LRUN
      EXTERNAL AGETDB
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA LRUN/0/
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NEVEH=NAMIND('EVEH')
        NTLCE=NAMIND('TLCE')
      ENDIF
C
C++   Default values in case something goes wrong
C
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
C++ Find TLCE bank
C
      KTLCE=IW(NTLCE)
      IF (KTLCE.EQ.0 .OR. IRUN.NE.LRUN) THEN
        IRET=AGETDB('TLCE',IRUN)
        IF (IRET.EQ.0)THEN
            CALL RERROR('TERPAR',2,'Cannot find TLCE bank.'//
     +               '  TPC coordinate errors will be rubbish.')
            RETURN
        ENDIF
        KTLCE=IW(NTLCE)
        LRUN = IRUN
      ENDIF
C
      LCOR = MOD(ICOR,100)
      IROWU = KROW(KTLCE,LCOR/10+1)
      IROWZ = KROW(KTLCE,MOD(LCOR,10)+1)
      ZA     = ABS ( ZCO )
      XLA    = ABS ( ATAN( TALA ) )
      TPCA   = TAN(PCA)
      SSU0   = RW(IROWU+JTLCU0)
      IF (TPCA.GT.0)THEN
        SSUA  = RW(IROWU+JTLCUR)
      ELSE
        SSUA  = RW(IROWU+JTLCUL)
      ENDIF
      SUZ1   = RW(IROWU+JTLCU1)
      SUZ2   = RW(IROWU+JTLCU2)
      SUZ3   = RW(IROWU+JTLCU3)
      SSZ0   = RW(IROWZ+JTLCZ0)
      SSZL1  = RW(IROWZ+JTLCZ1)
      SSZL2  = RW(IROWZ+JTLCZ2)
C
      SIGRFI = SQRT( SSU0**2 + (SSUA*TPCA)**2 )
      SIGRFI = SIGRFI* ( SUZ1 + SUZ2*ZA + SUZ3*ZA**2 )
      SIGZ   = SSZ0 + XLA*SSZL1 + XLA**2*SSZL2
C
C++   Special treatment for half-pad coordinates
C
      IF ( NHP .EQ. 1 ) SIGRFI = SIGRFI * RW(IROWU+JTLCE1)
      IF ( NHP .EQ. 2 ) SIGRFI = SIGRFI * RW(IROWU+JTLCE2)
C
      RETURN
      END
#endif
