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
C! - Input banks : VDXY, VDZT, VDCE, VDGC, VDMR
C! - Output banks: VDXY, VDZT only the word(quality flag)
C!
C
C       It takes the first element in the VDEM bank and applies that
C       efficiency to all the hits in both views.  Ad-hoc correction.
C                     24-June-1996 Manoj Thulasidas
C
C       Introduced the new bank VDCE instead of the VDEM bank.
C       It is the VDet Chip Efficiency map
C       It is built from the data base banks
C          VDPR (Vdet efficiency Periods and weights)
C          VDCM (Vdet Chip efficiency map, compact form)
C                     03-Jul-1998 Manoj Thulasidas
C
C       Add protections for garbage collection.
C       Modification of the logic of the VKILL bank to delete the correct hits.
C                     07-Oct-1999 Paschal Coyle, Thomas Kachelhoffer
C
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
C
#include "bcs.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdcejj.h"
C
      SAVE
      INTEGER IER
C  Functions
C  Local variables
C
      REAL EFFCUT, UW
      PARAMETER ( EFFCUT = 0.5 )
      INTEGER IVDCE,IVDXY,IVDZT,IVNEX
      INTEGER KVDCE,KVDXY,KVDZT,KEVEH,KVDGC,KVDMR,KVKIL,KRVKIL
      INTEGER NVDXY,NVDZT
      INTEGER MVDXY,MVDZT,MVDCE,NEVEH,MVDGC
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
      REAL RNVEC(100), CHIPEF
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
C Make the VDCE bank from VDPR (periods) and VDCM (Chip efficiency map
C in compact form)
C Return code is not necessary because if there is a problem, then VDCE
C bank would be missing and that error will be trapped automatically
        CALL VDMKCE
        MVDXY = NAMIND('VDXY')
        MVDZT = NAMIND('VDZT')
        MVDCE = NAMIND('VDCE')
        MVDGC = NAMIND('VDGC')
        NEVEH = NAMIND('EVEH')
C       check-up on the efficiency map VDCE banks,
        IVDCE=IW(MVDCE)
        IF(IVDCE.LE.0) THEN
          NOTDO=.TRUE.
          IER=-1
          RETURN
        ENDIF
        NPERI=LROWS(IVDCE)
        IF(NPERI.LE.0) THEN
          NOTDO=.TRUE.
          RETURN
        ENDIF
C       set index to start on efficiency map for highest period
        IPERI=NPERI
        KVDCE=KROW(IVDCE,IPERI)
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
C  make a VKIL bank parallel to the VDGC (global coord) bank
      CALL BDROP(IW,'VKIL')
      KVDGC = IW(MVDGC)
      NVDGC = LROWS(KVDGC)
      NDATA = NVDGC+LMHLEN
      CALL ALBOS('VKIL',0,NDATA,KVKIL,IGARB)
      IF ( IGARB .EQ. 2 ) THEN
        IER = -1
        RETURN
      ENDIF
      IW(KVKIL+LMHCOL)=1
      IW(KVKIL+LMHROW)=NVDGC
C
C     choose period
C
      IVDCE=IW(MVDCE)
      KVDCE=KROW(IVDCE,IPERI)
      NCOUN=NCOUN+1
      IF(NCOUN.GT.IW(KVDCE+JVDCPR)) THEN
        IPERI=IPERI+1
        IF(IPERI.GT.NPERI) IPERI=1
        KVDCE=KROW(IVDCE,IPERI)
        NCOUN=1
      ENDIF
C  set seed for random number 1000*run no + event no + 107
      KEVEH = IW(NEVEH)
      ISEED=1000*IW(KEVEH+2)+IW(KEVEH+6)+107
      CALL RMARIN(ISEED,0,0)
      CALL RANMAR(RNVEC,100)
      IDICE=1
C
C  Loop over all VDXY banks and flag hits to be killed
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
        KRVKIL = KROW(KVKIL,IVDGC)
C  VKIL is parallel to VDMR, and the entry is set to 1 if the cluster
C  is to be killed (since a multiplexed cluster was killed already)
        IF(ITABL(KVKIL,IVDGC,1).EQ.1) THEN
          IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
        ELSE IF (ITABL(KVKIL,IVDGC,1).EQ.0) THEN
C  This is a new one and therefore should consider deleting
          UW=RW(KVDXY+JVDXUC)
C  get the chip number
          CALL VDGTCH(UW,NVDXY,2,JL)
C          flag hit if efficiency set low
          CHIPEF = RW(KVDCE+JVDCEF-1+JL)
          IF(CHIPEF.LT.EFFCUT) THEN
            IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
C  set the flag to say that the multiplexed ones need to be killed
            IW(KRVKIL+1)=1
          ELSE
C          kill a few random clusters to simulate inefficiency
            IDICE=IDICE+1
            IF(IDICE.GT.100) THEN
              CALL RANMAR(RNVEC,100)
              IDICE=1
            ENDIF
            IF(CHIPEF.LT.RNVEC(IDICE)) THEN
              IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
C  set the flag to say that the multiplexed ones need to be killed
              IW(KRVKIL+1)=1
            ELSE
C  set the flag to say the multiplexed ones should NOT be killed
              IW(KRVKIL+1)=-1
            ENDIF 
          ENDIF
        ELSE IF (ITABL(KVKIL,IVDGC,1).EQ.-1) THEN 
C  this one has already been considered and should NOT be deleted
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
        KRVKIL = KROW(KVKIL,IVDGC)
C  VKIL is parallel to VDMR, and the entry is set to 1 if the cluster
C  is to be killed (since a multiplexed cluster was killed already)
        IF(ITABL(KVKIL,IVDGC,1).EQ.1) THEN
          IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
        ELSE IF (ITABL(KVKIL,IVDGC,1).EQ.0)THEN 
          UW=RW(KVDZT+JVDZWC)
C  get the chip number
          CALL VDGTCH(UW,NVDZT,1,JL)
C          flag hit if efficiency set low
          CHIPEF = RW(KVDCE+JVDCEF-1+JL)
          IF(CHIPEF.LT.EFFCUT) THEN
            IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
C  set the flag to say that the multiplexed ones need to be killed
            IW(KRVKIL+1)=1
          ELSE
C          kill a few random clusters to simulate inefficiency
            IDICE=IDICE+1
            IF(IDICE.GT.100) THEN
              CALL RANMAR(RNVEC,100)
              IDICE=1
            ENDIF
            IF(CHIPEF.LT.RNVEC(IDICE)) THEN
              IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
C  set the flag to say that the multiplexed ones need to be killed
              IW(KRVKIL+1)=1
            ELSE
              IW(KRVKIL+1)=-1
            ENDIF
          ENDIF
        ELSE IF (ITABL(KVKIL,IVDGC,1).EQ.-1) THEN 
C  do nothing
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
