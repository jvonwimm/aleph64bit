      INTEGER FUNCTION ALKEVH (NRDN,NTRK,NVRTX,IDPRO,WEIT)
C ------------------------------------------------------------------
C - F.Ranjard - 870401
C! Create kine event header KEVH
C - Input :  NRDN   = 1st random number used(3)
C            NTRK   = # of input tracks
C            NVRTX  = # of input vertices
C            IDPRO  = process identification
C            WEIT   = weght of the event
C - Output : ALKEVH = KEVH bank index
C                     0 means not enough space to book the bank
C
C   --------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "kevhjj.h"
      INTEGER NRDN(*)
      DATA IFI /0/
#include "bmacro.h"
C ----------------------------------------------------------------------
C - at 1st entry, get the random generator type
      IF (IFI .EQ. 0) THEN
         IFI = 1
         CALL ALSEED (IRGTY,ISD1,ISD2)
      ENDIF
C - Book the 'KEVH' bank
      CALL AUBOS ('KEVH',0,LMHLEN+LKEVHA,JKEVH,IGARB)
      IF (JKEVH.EQ.0) GOTO 999
      IW(JKEVH+LMHCOL) = LKEVHA
      IW(JKEVH+LMHROW) = 1
      CALL BKFMT ('KEVH','6I,F,2I')
C
C - Fill 'KEVH'
      KKEVH = JKEVH + LMHLEN
      IW(KKEVH+JKEVRN) = NRDN(1)
      IW(KKEVH+JKEVNT) = NTRK
      IW(KKEVH+JKEVNV) = NVRTX
      IW(KKEVH+JKEVPI) = IDPRO
C
      RW(KKEVH+JKEVWT) = WEIT
C
      IF (IRGTY .GE. 2) IW(KKEVH+JKEVSR) = NRDN(2)
      IF (IRGTY .EQ. 3) IW(KKEVH+JKEVTR) = NRDN(3)
C
 999  CONTINUE
      ALKEVH = JKEVH
      END
#endif
