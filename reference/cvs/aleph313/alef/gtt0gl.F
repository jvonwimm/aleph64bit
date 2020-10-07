      FUNCTION GTT0GL (IRUN)
C -----------------------------------------------------
CKEY ALEF GET T0GL
C! Get T0 depending on run number and number of bunches
C - F.Ranjard - 920924
C               940323 : T0GL depends on setup code
C - D.Casper  - 970325 : Support for new alignment (TNFV)
C               970423 : Check alignment type before
C                        applying t0 correction
C               970512 : Correct bug in the above mod; TNLO
C                        has not been read in yet the first
C                        time this routine is called, and
C                        checking for it puts the wrong
C                        value in JCON!  Instead, check for
C                        a valid TNFV entry and absence of
C                        user card (TOAL) to disable it.
C - Input   : IRUN    / I = run number
C - Output  : GTT0GL  / R = T0 or 0. in case of error
C ----------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "t0gljj.h"
#include "tnfvjj.h"
#include "rlepjj.h"
      SAVE T0, OF
      INTEGER AGETDB,GTSTUP,TNFVRU
      DATA NT0GL, NRLEP, IRLST /3*0/
#include "bmacro.h"
C -----------------------------------------------------
C - 1st entry
      IF (NT0GL.EQ.0) THEN
         NT0GL = NAMIND('T0GL')
         NRLEP = NAMIND('RLEP')
         NTOAL = NAMIND('TOAL')
      ENDIF
C
C - next entry
      IF (IRUN.NE.IRLST) THEN
         IRLST = IRUN
         IF (IRUN.LE.2000) THEN
            ITP = GTSTUP ('TP',IRUN)
         ELSE
            ITP = IRUN
         ENDIF
         IER = AGETDB ('T0GL',-ITP)
         JT0GL = IW(NT0GL)
         IF (JT0GL.EQ.0) THEN
            T0 = 0.
            OF = 0.
            GTT0GL = 0.
            GOTO 999
         ELSE
            T0 = RTABL(JT0GL,1,JT0GGT)
            IF (LCOLS(JT0GL).GE.JT0GOF) THEN
               OF = RTABL(JT0GL,1,JT0GOF)
               IF (IW(NTOAL).EQ.0) THEN
                JTNFV = TNFVRU(ITP, IROW)
                JTNFV = ABS(JTNFV)
                IF (JTNFV.GT.0 .AND. IROW.GT.0) THEN
                  DT = RTABL(JTNFV,IROW,JTNFDT)
                  T0 = T0 + DT
                  WRITE(IW(6),1) DT
 1    FORMAT(1x,'GTT0GL - Applying ',F12.8,'us t0 correction from TNFV')
                ENDIF
               ENDIF
            ELSE
               OF = 0.
            ENDIF
         ENDIF
      ENDIF
C
      JRLEP = IW(NRLEP)
      IF (JRLEP.EQ.0) THEN
         GTT0GL = T0
      ELSE
         IF (ITABL(JRLEP,1,JRLELD).EQ.8) THEN
            GTT0GL = T0 - OF
         ELSE
            GTT0GL = T0
         ENDIF
      ENDIF
C
 999  CONTINUE
      END
#endif
