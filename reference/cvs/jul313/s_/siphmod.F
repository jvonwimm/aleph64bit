      REAL FUNCTION SIPHMOD(MODU,IPH)
C------------------------------------------------------------
C!  - Return SICAL final 'ad hoc' Phi correction , including a possible
C!    bunch dependance 
C!
C!    Author B.Bloch 10/99 
C!    Input:  
C!            MODU  Module ( 1 or 2)
C!            IPH  Phi bin number ( 1 to 32)
C!    Output: correction to be applied 
C!    BANKS:  SCPH or SCBP 
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "sibun.h"
      SAVE
C
      DATA IFIR/0/
C--------------------------------------------------------------------
#include "bmacro.h"
C -------------------------------------------------------------------
      IF (IFIR.EQ.0) THEN
        IFIR = 1
        NASCBP = NAMIND ('SCBP')
        NASCPH = NAMIND ('SCPH')
      ENDIF
      phico = 1.
C  use SCBP or SCPH dpending on which one was loaded by SIINIRU
      KSCPH = IW(NASCBP)
      If (KSCPH.LE.0) KSCPH = IW(NASCPH) 
      IF (KSCPH.LE.0) GO TO 98
C IF BUNCH IS ONE OR UNKNOWN, setup as 1
      IFAM = 1
      if (IBUNE.EQ.2) IFAM = IBUNE
      KPOI = KROW(KSCPH,2*(ifam-1)+modu)
      phico = rw(kpoi+iph)
C
   98 CONTINUE
      SIPHMOD = phico
  999 RETURN
      END
#endif
