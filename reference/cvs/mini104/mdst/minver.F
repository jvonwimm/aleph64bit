      SUBROUTINE MINVER
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill vertex bank DVER for Mini-DST.
C
C     Author: Stephen Haywood      22-Jan-90
C
C     Input  : PYER bank
C     Output : DVER bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "minfac.h"
#include "pyerjj.h"
#include "dverjj.h"
#include "bmacro.h"
C
C++   Identify the number of vertices.
C++   If no vertices, return without creating DVER bank.
C
      KPYER = NLINK('PYER',0)
      IF(KPYER.GT.0) THEN
         NPYER = LROWS(KPYER)
      ELSE
         NPYER = 0
      ENDIF
      IF(NPYER.LE.0) RETURN
C
C++   Create the DVER bank.
C
      NDVER = NPYER
      LEN = LMHLEN + LDVERA * NDVER
      CALL AUBOS('DVER',0,LEN, KDVER,IGARB)
      IF(IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINVER: Cannot create DVER bank'')')
         RETURN
      ELSE IF(IGARB.NE.0) THEN
         KPYER = NLINK('PYER',0)
      ENDIF
      IW(KDVER+LMHCOL) = LDVERA
      IW(KDVER+LMHROW) = NDVER
C
C++   Loop over the PYER bank and fill the DVER bank.
C
      DO 100 I=1,NPYER
C
C++      Vertex position.
C
         IW(KROW(KDVER,I)+JDVEX0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVX))
         IW(KROW(KDVER,I)+JDVEY0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVY))
         IW(KROW(KDVER,I)+JDVEZ0) = NINT(DFACTM * RTABL(KPYER,I,JPYEVZ))
C
C++      Vertex fit probability.
C
         CHISQ = RTABL(KPYER,I,JPYEC2)
         NDEG  = ITABL(KPYER,I,JPYEDF)
         PRCHI = PROB(CHISQ,NDEG)
         IW(KROW(KDVER,I)+JDVEFP) = NINT(1000.*PRCHI)
C
C++      Set main vertex bit.
C
         IW(KROW(KDVER,I)+JDVEMV) = 1
C
C++      Link to parent track - zero for main vertices.
C
         IW(KROW(KDVER,I)+JDVEDT) = 0
C
  100 CONTINUE
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DVER')
C
      RETURN
      END
#endif
