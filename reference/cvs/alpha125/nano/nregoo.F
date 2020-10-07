      SUBROUTINE NREGOO(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : get NANO-specific goodies for ALPHA
C!  -
C!
C!   Author   :- Gerrit Graefe         23-MAR-1994
C!   Modified :- Gerrit Graefe         27-FEB-1995  Link to NDHE if
C!                                                  NAVERS>115
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=filled QNCDE
C!                                       1=can't find NDHE
C!                                       2=can't find NDST
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine gets all the goodies from the NanoDst and makes them
C?   accessible inside ALPHA. To use the names from SANDY for those  who used
C?   it before and to avoid problems with ALPHA conventions all variables are
C?   in a commonblock QNCDE.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "ndhejj.h"
#include "nehejj.h"
#include "ndstjj.h"
#include "qncde.h"
      INTEGER NLINK,NVERS,NAMIND,NAVERS
      INTEGER I,J,INDHE,IRET,INDST,INAME
      INTEGER LPBLIS,LRBLIS,LNBLIS
      CHARACTER*4 NAME
      CHARACTER*4 CHAINT
#include "qmacro.h"
C----------------------------------------------------------------------
      IRET=3
      CALL GETNAV(NAVERS)
      IF(NAVERS.EQ.115)THEN
C
C!..LINK TO BANK NDHE
C
        INDHE=IW(NAMIND('NDHE'))
        IF (INDHE.EQ.0)THEN
          IRET=1
          GOTO 999
        ENDIF
        BJSPCH=FLOAT(IW(INDHE+LMHLEN+JNDHSC))/1000000.0
        BJSPEF=FLOAT(IW(INDHE+LMHLEN+JNDHSE))/1000000.0
        BBSPR1=FLOAT(IW(INDHE+LMHLEN+JNDHS1))/1000000.0
        BBSPR2=FLOAT(IW(INDHE+LMHLEN+JNDHS2))/1000000.0
        BEETT1=FLOAT(IW(INDHE+LMHLEN+JNDHT1))/1000000.0
        BEETT2=FLOAT(IW(INDHE+LMHLEN+JNDHT2))/1000000.0
        BEETM1=FLOAT(IW(INDHE+LMHLEN+JNDHM1))/1000000.0
        BEETM2=FLOAT(IW(INDHE+LMHLEN+JNDHM2))/1000000.0
        NXHVOK=BTEST(IW(INDHE+LMHLEN+JNDHPF),25)
        NXDEOK=BTEST(IW(INDHE+LMHLEN+JNDHPF),26)
        NXVDOK=BTEST(IW(INDHE+LMHLEN+JNDHPF),27)
        NXMUOK=BTEST(IW(INDHE+LMHLEN+JNDHPF),28)
        NXLEPT=BTEST(IW(INDHE+LMHLEN+JNDHPF),29)
        NXBJSC=BTEST(IW(INDHE+LMHLEN+JNDHPF),30)
        NXBJSF=BTEST(IW(INDHE+LMHLEN+JNDHPF),31)
        NXBEES=BTEST(IW(INDHE+LMHLEN+JNDHP2), 8)
        NXGAMP=BTEST(IW(INDHE+LMHLEN+JNDHP2),11)
        NXQFVX=BTEST(IW(INDHE+LMHLEN+JNDHP2),25)
        NXQFOK=BTEST(IW(INDHE+LMHLEN+JNDHP2),26)
C
        KREVDS=IW(INDHE+LMHLEN+JNDHDS)
      ELSE
C
C!..LINK TO BANK NEHE
C
        INDHE=IW(NAMIND('NEHE'))
        IF (INDHE.EQ.0)THEN
          IRET=1
          GOTO 999
        ENDIF
        BJSPCH=0.0
        BJSPEF=0.0
        BBSPR1=0.0
        BBSPR2=0.0
        BEETT1=0.0
        BEETT2=0.0
        BEETM1=0.0
        BEETM2=0.0
        NXHVOK=BTEST(IW(INDHE+LMHLEN+JNEHPF),25)
        NXDEOK=BTEST(IW(INDHE+LMHLEN+JNEHPF),26)
        NXVDOK=BTEST(IW(INDHE+LMHLEN+JNEHPF),27)
        NXMUOK=BTEST(IW(INDHE+LMHLEN+JNEHPF),28)
        NXLEPT=BTEST(IW(INDHE+LMHLEN+JNEHPF),29)
        NXBJSC=.FALSE.
        NXBJSF=.FALSE.
        NXBEES=.FALSE.
        NXGAMP=BTEST(IW(INDHE+LMHLEN+JNEHP2),11)
        NXQFVX=BTEST(IW(INDHE+LMHLEN+JNEHP2),25)
        NXQFOK=BTEST(IW(INDHE+LMHLEN+JNEHP2),26)
C
        KREVDS=IW(INDHE+LMHLEN+JNEHDS)
      ENDIF
C
C!..LINK TO BANK NDST
C
      IRET=3
      INDST=IW(NAMIND('NDST'))
      IF (INDST.EQ.0)THEN
        IRET=2
        GOTO 999
      ENDIF
      MINTRK = IW(INDST+LMHLEN+JNDSMT)
C
      PCUTMI = FLOAT(IW(INDST+LMHLEN+JNDSLP)) /    1000.0
      PCUTMA = FLOAT(IW(INDST+LMHLEN+JNDSUP)) /    1000.0
      THCCUT = FLOAT(IW(INDST+LMHLEN+JNDSCT)) / 1000000.0
      NITCUT = IW(INDST+LMHLEN+JNDSIH)
      NTPCUT = IW(INDST+LMHLEN+JNDSTH)
      BD0CUT = FLOAT(IW(INDST+LMHLEN+JNDSD0)) /   10000.0
      BZ0CUT = FLOAT(IW(INDST+LMHLEN+JNDSZ0)) /   10000.0
C
      PCUMIJ = FLOAT(IW(INDST+LMHLEN+JNDSJL)) /    1000.0
      PCUMAJ = FLOAT(IW(INDST+LMHLEN+JNDSJU)) /    1000.0
      THCCUJ = FLOAT(IW(INDST+LMHLEN+JNDSJC)) / 1000000.0
      NITCUJ = IW(INDST+LMHLEN+JNDSJI)
      NTPCUJ = IW(INDST+LMHLEN+JNDSJT)
      BD0CUJ = FLOAT(IW(INDST+LMHLEN+JNDSJD)) /   10000.0
      BZ0CUJ = FLOAT(IW(INDST+LMHLEN+JNDSJZ)) /   10000.0
C
      BMYCUT = FLOAT(IW(INDST+LMHLEN+JNDSJY)) /    1000.0
      ENEVIS = FLOAT(IW(INDST+LMHLEN+JNDSVE)) /    1000.0
      NAME = CHAINT(IW(INDST+LMHLEN+JNDSRO))
      RECOOP = NAME(1:2)
      PHOCUT = FLOAT(IW(INDST+LMHLEN+JNDSLE)) /    1000.0
      MCOPTI = IW(INDST+LMHLEN+JNDSMC)
      PMINMC = FLOAT(IW(INDST+LMHLEN+JNDSMM)) /    1000.0
      ECHMIN = FLOAT(IW(INDST+LMHLEN+JNDSCE)) /    1000.0
      REFMIN = FLOAT(IW(INDST+LMHLEN+JNDSME)) /    1000.0
      RMYCUT = FLOAT(IW(INDST+LMHLEN+JNDSEM)) /    1000.0
      V0CHSI = FLOAT(IW(INDST+LMHLEN+JNDSCS)) /    1000.0
      V0CHBO = FLOAT(IW(INDST+LMHLEN+JNDSCB)) /    1000.0
C
      NCPROD = CHAINT(IW(INDST+LMHLEN+JNDSPR))
C?..- POT/DST/MINI BANKS
      LPBLIS = IW(INDST+LMHLEN+JNDSNP)
      NCLPOT = ' '
      DO 30 J=1,LPBLIS
        INAME = IW(INDST+LMHLEN+JNDSPB+J-1)
        NCLPOT(((J-1)*4+1):) = CHAINT(INAME)
   30 CONTINUE
C
C?..- RUN BANKS
      LRBLIS = IW(INDST+LMHLEN+JNDSNR)
      NCLRUN = ' '
      DO 40 J=1,LRBLIS
        INAME = IW(INDST+LMHLEN+JNDSRB+J-1)
        NCLRUN(((J-1)*4+1):) = CHAINT(INAME)
   40 CONTINUE
C
C?..- NDST BANKS
      LNBLIS = IW(INDST+LMHLEN+JNDSNN)
      NCLNDS = ' '
      DO 50 J=1,LNBLIS
        NAME = CHAINT(IW(INDST+LMHLEN+JNDSNB+J-1))
        NCLNDS(((J-1)*4+1):) = NAME
   50 CONTINUE
      IRET=0
  999 RETURN
      END
#endif
