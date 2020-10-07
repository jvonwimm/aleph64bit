      SUBROUTINE VDKLCP(IER)
C----------------------------------------------------------------------
C! sets quality flag for hits in VDXY,VDZT banks according to dead chip
C! making sure that the multiplexed hits get killed together
C!        Manoj Thulasidas 24-June-1996
CKEY VDET
C! - Output IER =0 OK ,
C!              =-1 problem with banks
C!              = 1 routine will do nothing
C!
C! - Input banks : VDXY, VDZT, VDEM, VDGC, VDMR
C! - Output banks: VDXY, VDZT only the word(quality flag)
C!
C
C       It takes the first element in the VDEM bank and applies that 
C       efficiency to all the hits in both views.  Ad-hoc correction.
C                     24-June-1996 Manoj Thulasidas
C
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
C
#include "bcs.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdemjj.h"
C
      SAVE
      INTEGER IER
C  Functions
C  Local variables
C
      REAL EFFCUT
      PARAMETER ( EFFCUT = 0.5 )
      INTEGER IVDEM,IVDXY,IVDZT,IVNEX
      INTEGER KVDEM,KVDXY,KVDZT,KEVEH,KVDGC,KVDMR,KVKIL,KRVKIL
      INTEGER NVDXY,NVDZT
      INTEGER MVDXY,MVDZT,MVDEM,NEVEH,MVDGC      
      INTEGER NHXY,NHZT,IHXY,IHZT,JL,NKILL,NVDGC,IVDGC
      INTEGER NDATA, IGARB
      INTEGER NPERI,IPERI,NCOUN
      LOGICAL FIRST, NOTDO
C - bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag
C   to indicate a MC inefficiency rejected hit.
      INTEGER IMCNEF       
      PARAMETER (IMCNEF=536870912)
C
C  BOS functions
C
      INTEGER NAMIND, NLINK
      EXTERNAL NAMIND, NLINK
C
C  Variables for inline functions
C
      INTEGER I,J
      INTEGER ISEED,IDICE
      REAL RNVEC(100)
C
C  Inline functions
C
      INTEGER IOR
C
C  Define variables used in BMACRO
C
#include "bmacrod.h"
C
      DATA FIRST/.TRUE./,NOTDO/.FALSE./ 
#include "bmacro.h"
C -------------------------------------------------------------
C  initialization
C
      IER=1
      IF(FIRST) THEN
        FIRST=.FALSE.
        MVDXY = NAMIND('VDXY')
        MVDZT = NAMIND('VDZT')
        MVDEM = NAMIND('VDEM')
        MVDGC = NAMIND('VDGC')
        NEVEH = NAMIND('EVEH')
C       check-up on the efficiency map VDEM banks,
        IVDEM=IW(MVDEM)
        IF(IVDEM.LE.0) THEN
          NOTDO=.TRUE.
          IER=-1
          RETURN
        ENDIF
        NPERI=LROWS(IVDEM)
        IF(NPERI.LE.0) THEN
          NOTDO=.TRUE.
          RETURN
        ENDIF
C       set index to start on efficiency map for highest period
        IPERI=NPERI
        KVDEM=KROW(IVDEM,IPERI)
        NCOUN=0
      ENDIF
C
C     If we had an error in initialization just return
C
      IF(NOTDO) RETURN
      IER=0
C
C     Normal operation start
C
C     choose period
C
      IVDEM=IW(MVDEM)
      KVDEM=KROW(IVDEM,IPERI)
      NCOUN=NCOUN+1
      IF(NCOUN.GT.IW(KVDEM+JVDEPR)) THEN
        IPERI=IPERI+1
        IF(IPERI.GT.NPERI) IPERI=1
        KVDEM=KROW(IVDEM,IPERI)
        NCOUN=1
      ENDIF
C  set seed for random number 1000*run no + event no + 107
      KEVEH = IW(NEVEH)
      ISEED=1000*IW(KEVEH+2)+IW(KEVEH+6)+107
      CALL RMARIN(ISEED,0,0)
      CALL RANMAR(RNVEC,100)
      IDICE=1
C  make a VKIL bank parallel to the VDGC (global coord) bank
      CALL BDROP(IW,'VKIL')
      KVDGC = IW(MVDGC)
      NVDGC = LROWS(KVDGC)
      NDATA = NVDGC+LMHLEN
      CALL ALBOS('VKIL',0,NDATA,KVKIL,IGARB)
      IW(KVKIL+LMHCOL)=1
      IW(KVKIL+LMHROW)=NVDGC

C
C  Loop over the VDGC bank and flag hits
C
      DO IVDGC = 1, NVDGC
        JL = 1
C  Kill hit if efficiency set low
        KRVKIL = KROW(KVKIL,IVDGC)
        IF(RW(KVDEM+JVDEEF-1+JL).LT.EFFCUT) THEN
          IW(KRVKIL+1)=1
        ELSE
C  Kill a few random clusters to simulate inefficiency
          IDICE=IDICE+1
          IF(IDICE.GT.100) THEN
            CALL RANMAR(RNVEC,100)
            IDICE=1
          ENDIF
          IF(RW(KVDEM+JVDEEF-1+JL).LT.RNVEC(IDICE)) THEN
            IW(KRVKIL+1)=1                 
          ENDIF
        ENDIF
      ENDDO
C
C  Loop over all VDXY banks and flag hits that have been killed
C
      IVDXY  = IW(MVDXY)
      IF(IVDXY .LE. 0) GO TO 3
 2    CONTINUE
      NHXY  = LROWS(IVDXY)
      NVDXY = IW(IVDXY-2)
      IVNEX = IW(IVDXY-1)
C  VDMR bank, with NR=NVDXY+IVIEW-1 has the relation from VDXY hit to
C  VDGC hit number
      KVDMR = NLINK('VDMR',NVDXY+1)
      DO 20 IHXY=1,NHXY
        KVDXY=KROW(IVDXY,IHXY)
C  Get the VDGC cluster number
        IVDGC = ITABL(KVDMR,IHXY,1)
C  VKIL is parallel to VDMR, and the entry is set to 1 if the cluster
C  is to be killed
        IF(ITABL(KVKIL,IVDGC,1).EQ.1) THEN
          IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
        ENDIF
 20   CONTINUE
C  Update pointer, and return for next bank
      IVDXY=IVNEX
      IF(IVDXY .GT. 0) GO TO 2
C
C  all done with VDXY, now do VDZT
C
 3    CONTINUE
C
C  Now loop over all VDZT banks and flag hits
C
      IVDZT = IW(MVDZT)
      IF(IVDZT .LE. 0) RETURN
 5    CONTINUE
C  loop over all the hits of the VDZT bank
      NHZT  = LROWS(IVDZT)
      NVDZT = IW(IVDZT-2)
      IVNEX = IW(IVDZT-1)
C  VDMR bank, with NR=NVDZT+IVIEW-1 has the relation from VDZT hit to
C  VDGC hit number
      KVDMR = NLINK('VDMR',NVDZT)
      DO 50 IHZT=1,NHZT
        KVDZT=KROW(IVDZT,IHZT)
C  Get the VDGC cluster number
        IVDGC = ITABL(KVDMR,IHZT,1)
C  VKIL is parallel to VDMR, and the entry is set to 1 if the cluster
C  is to be killed
        IF(ITABL(KVKIL,IVDGC,1).EQ.1) THEN
          IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
        ENDIF
 50   CONTINUE
C  Update pointer, and return for next bank
      IVDZT = IVNEX
      IF(IVDZT .GT. 0) GO TO 5
C
      RETURN
C
      END
#endif
