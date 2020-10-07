      SUBROUTINE GTYEAR
CKEY QSELEP/INTERNAL
C-------------------------------------------------------------------------------
C! Auxiliary to QSELEP
C  Author                           I. Ten Have     27-Jul-1994
C
C  Calculates the year the data was taken in or the year a MC run is
C  supposed to represent.
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
C
C Private variables and common initialised by LTINIT.
C
      INTEGER LASTRN,NLPTEV,YEARID,LTFWCD
      LOGICAL LTFRST
      COMMON /LTPRIV/ LASTRN,NLPTEV,LTFRST,YEARID,LTFWCD
C
C Variables.
C
      INTEGER IDBANK,NLINK
C
      REAL LTSCFA
      PARAMETER(LTSCFA = 1000000.)
C
      INTEGER NCNT
      PARAMETER(NCNT = 25)
      INTEGER LEPCNT(NCNT)
C
C     QSELEP counters
C
      INTEGER LTNCHR,LTQLPR,LTECHR,LTACHR,LTBOSC,LTBOSS,LTNJET,LTJTAS,
     @        LTPTFL,LTHVDQ,LTHAD ,LTNLEP,LTYLEP,LTCONV,LTELMU,LTNEMI,
     @        LTNEPL,LTNMMI,LTNMPL,LTNCNV,LTCALL,LTNEFO,LTGTYF,LTCPDF,
     @        LTBRFL
C
      PARAMETER(LTNCHR= 1,LTQLPR= 2,LTECHR= 3,LTACHR= 4,LTBOSC= 5,
     @          LTBOSS= 6,LTNJET= 7,LTJTAS= 8,LTPTFL= 9,LTHVDQ=10,
     @          LTHAD =11,LTNLEP=12,LTYLEP=13,LTCONV=14,LTELMU=15,
     @          LTNEMI=16,LTNEPL=17,LTNMMI=18,LTNMPL=19,LTNCNV=20,
     @          LTCALL=21,LTNEFO=22,LTGTYF=23,LTCPDF=24,LTBRFL=25)
C
       COMMON /LPTPUB/ LEPCNT
#include "qmacro.h"
C-------------------------------------------------------------------------------
      YEARID = 0
      IF (XMCEV) THEN
        IDBANK = NLINK('ASIM',0)
        IF (IDBANK.GT.0) YEARID = (ITABL(IDBANK,1,1)/100)+1900
        IF (YEARID.LT.1989.OR.YEARID.GT.1999) YEARID = 0
      ELSE
        IF (KRUN.LT.25000) THEN
          IF (KRUN.GE.4000) YEARID = 1989
          IF (KRUN.GE.6000) YEARID = 1990
          IF (KRUN.GE.10000) YEARID = 1991
          IF (KRUN.GE.14000) YEARID = 1992
          IF (KRUN.GE.20000) YEARID = 1993
        ELSE
          IDBANK = NLINK('EVEH',0)
          IF (IDBANK.GT.0) YEARID = (IW(IDBANK+4)/10000)+1900
          IF (YEARID.LT.1989.OR.YEARID.GT.1999) YEARID = 0
        ENDIF
      ENDIF
C
      RETURN
C
      END
#endif
