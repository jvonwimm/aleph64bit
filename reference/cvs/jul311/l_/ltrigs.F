      SUBROUTINE LTRIGS(ITYPE)
C-----------------------------------------------------------------------
C! Count trigger types and background
C!
C!   Author   : P. H. Hansen  890701
C! Modified   : J. Wear       891119  Count GBX's from trigger scalar ba
C! Modified   : E. Lancon     10-APR-1991  Use ALTRIG to Trigger bits
C! Modified   : P. H. Hansen  01-MAY-1996  Update backgr every 10 single
C!
C!     Input  : 'X1SC'           Trigger info -- GBX count since last ev
C!              'XTEB'/'XTRB'    Trigger bit mask from level 1
C!              'XTCN'           Trigger info -- GBX count (incorrect)
C!              'LSIN'           Singles storage
C!
C!     Output :  ITYPE=0         : Non-lumi
C!                    =1         : exclusive single low
C!                    =2         : exclusive single high
C!                    =3         : exclusive single very high
C!                    =4         : exclusive low-low
C!                    =5         : high-low
C!              'LONL'           Lcal Online trigger statistics
C!              'LBAK'           Beamgas background to Bhabhas
C!   Description :
C!   ===========
C?   Classify and count trigger types
C?   IF (not Monte Carlo) THEN
C?       Count GBX and RANDOM
C?       Decide if time is up for background estimate
C?   ELSE
C?       Count events
C?       Decide if time is up for background estimate
C?   ENDIF
C?   IF (time is up) THEN
C?       Update background statistics -- call LBLDBA
C?       Reset GBX and RANDOM counters
C?       Reset singles storage
C?   ENDIF
C?
C-----------------------------------------------------------------------
C
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rcurnt.h"
#include "lcnamc.h"
#include "lonljj.h"
#include "lsinjj.h"
#include "alcons.h"
      INTEGER ALTRIG
      EXTERNAL ALTRIG
      EXTERNAL NAMIND,NDROP
      LOGICAL FCOMB,XLTRG(5)
      DATA NSING,MXSIN/0,10/
#include "bmacro.h"
C---------------------------------------------------------------------
C
      FCOMB = .FALSE.
      ITYPE = 0
      KLONL = IW(NALONL)
      IF(KLONL.LE.0)                                      GOTO 999
C
C Test trigger bits
      ITB = 0
      IF ( ALTRIG(IT1,IT2,IL2).GT.0 ) ITB = IL2
C
C Get Lumi trigger type
C
      CALL LTGTYP(IRUNRC,ITB,XLTRG)
      DO 10 JTYPE = 1,5
 10     IF (XLTRG(JTYPE)) ITYPE = JTYPE
C
C Count trigger types
      RW(KLONL+LMHLEN+JLONTR+ITYPE) = RW(KLONL+LMHLEN+JLONTR+ITYPE)
     &                                + 1.
C
C IF not Monte-Carlo
      IF(.NOT.FMCRUN) THEN
C
C Count GBXs and single arm triggers
        NGBX = 0
        CALL LGNGBX(NGBX)
        RW(KLONL+LMHLEN+JLONGB)=RW(KLONL+LMHLEN+JLONGB)+FLOAT(NGBX)
C
C IF Monte-Carlo
      ELSE
        RW(KLONL+LMHLEN+JLONGB) = RW(KLONL+LMHLEN+JLONGB)+1.
      ENDIF
C
C Debug print
      IF(FDEBRF.AND.JDBDRF(5).GE.2) THEN
        WRITE(IW(6),'(/,2X,A,Z8,A,I4,A,E12.4)')
     & '+++LTRIGS+++ Trigger bits ',ITB,'  Type',ITYPE,
     & ' Bunch cross/1024',RW(KLONL+LMHLEN+JLONGB)
      ENDIF
C
C Time is up to build fake combinations of stored singles?
      KLSIN=IW(NALSIN)
      IF(KLSIN.GT.0.AND.LROWS(KLSIN).GE.MXSIN) CALL LBLDBA
C
  999 CONTINUE
      END
#endif
