      SUBROUTINE SELENU (ECNUOK)
C-----------------------------------------------------------------------
CKEY EDIR
C!Look for events with at least one ECAL cluster above some threshold
C!in a module with some wire energy and a reasonable pastis
C-
C     Input  : None
C     Output : ECNUOK is TRUE if good ECAL cluster found
C-
C     Called by  : SELEVT
C     Calls      : nothing
C     Banks used : PEWI (or PWEI), PECO
C-
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C --
#include "bcs.h"
#include "pewijj.h"
#include "pecojj.h"
C --
      LOGICAL ECNUOK
      REAL EECAL, TECAL, EWCAL(36) 
      REAL EECUT, EWCUT, TECUTL, TECUTH

      EXTERNAL NAMIND
 
C     Cut definition
      DATA EECUT / 1.5 /
      DATA TECUT / 200. /
      DATA EWCUT / 0.5 /

C Name indices
      DATA NAPEWI / 0 /
      DATA NAPWEI / 0 /
      DATA NAPECO / 0 /

#include "bmacro.h"
C-----------------------------------------------------------------------
 
C -- Initialise
      ECNUOK = .FALSE.
      NCLUS = 0
      EECAL = 0.
      IEWOK = 0
      CALL VZERO(EWCAL,36)
 
C -- Ecal analysis: wires....allow for PEWI to PWEI change
      ITIMCO = JPEWTI
      IF( NAPEWI .EQ. 0 ) NAPEWI = NAMIND('PEWI')
      KPEWI = IW(NAPEWI)
      IF (KPEWI.EQ.0) THEN
         IF( NAPWEI .EQ. 0 ) NAPWEI = NAMIND('PWEI')
         KPEWI = IW(NAPWEI)
         ITIMCO = 63
      ENDIF

      IF (KPEWI.NE.0) THEN
         NWI = LROWS(KPEWI)
 
C -- Loop over modules:
        DO 30 IP=1,NWI
          IMOD = ITABL(KPEWI,IP,JPEWMN)
 
C -- Add energies of all planes in this module.
C -- Notice that PEWI has only the modules above 50 MeV.
          ENSUM = 0.
          DO 20 IADC=1,45
            ENSUM = ENSUM + FLOAT(ITABL(KPEWI,IP,JPEWPD+IADC-1))/1.E6
   20     CONTINUE
          EWCAL(IMOD) = ENSUM

C -- Check if this energy is above the wire cut
          IF (ENSUM.GT.EWCUT) IEWOK = 1

   30   CONTINUE
 
      ENDIF

C Get the pastis timing
      CALL TIZERN(TECAL, TMEAN, IT0)

C -- Make the wire check
      IF (IEWOK.EQ.0.OR.ABS(TECAL).GT.TECUT) GOTO 130

C -- Now have a look for some clusters
      IF( NAPECO .EQ. 0 ) NAPECO = NAMIND('PECO')
      KPECO = IW(NAPECO)
      IF (KPECO.NE.0) THEN
         NPD = LROWS(KPECO)

         DO IC = 1,NPD

C -- Record the cluster energy and position
C
            CLUSE = RTABL(KPECO,IC,JPECEC)
            PHI   = RTABL(KPECO,IC,JPECPH)
            THETA = RTABL(KPECO,IC,JPECTH)

C -- Check that the cluster energy is above the threshold
            IF (CLUSE.GE.EECUT) THEN
               ECNUOK = .TRUE.
               GOTO 130
            ENDIF

  120       CONTINUE

         ENDDO 

      ENDIF

 130  CONTINUE

      RETURN
      END
#endif




