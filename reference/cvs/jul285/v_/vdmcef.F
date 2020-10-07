      SUBROUTINE VDMCEF(IER)
C----------------------------------------------------------------------
C! sets quality flag for hits in VDXY,VDZT banks according to dead line
C! - J.Carr - 16-9-91
CKEY VDET
C! - Output IER =0 OK ,
C!              =-1 problem with banks
C!              = 1 routine will do nothing
C!
C! - Input banks : VDXY, VDZT, VDEM
C! - Output banks: VDXY, VDZT only the word(quality flag)
C!
C!      Modified 27-8-93 to include hit position smearing      D. Brown
C!      This uses the VDSM bank.
C
C       Modified 10-nov-94  to take out the position smearing.
C       This will be organized as a separate subroutine.
C
C       Modified 20-May-96  to tread the new VDET, it takes the first
C       element in the VDEM bank and applies that efficiency to all
C       the hits in both views.  Ad-hoc correction.
C                     Manoj Thulasidas
C
C       Modified 24-June-96, to move the treatment of the new VDET to 
C       VDKLCP.  The multiplexed hits can not be taken care of in the
C       the framework of VDMCEF
C                     Manoj Thulasidas
C
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdemjj.h"
C
       SAVE
       INTEGER IER
C  Functions
       INTEGER VDYEAR
       EXTERNAL VDYEAR
C  Local variables
C
       REAL EFFCUT
       PARAMETER ( EFFCUT = 0.5 )
       INTEGER IVDEM,IVDXY,IVDZT,IVNEX
       INTEGER KVDEM,KVDXY,KVDZT,KEVEH
       INTEGER NVDXY,NVDZT
       INTEGER MVDXY,MVDZT,MVDEM,NEVEH
       INTEGER NHXY,NHZT,IHXY,IHZT,JL,NKILL
       REAL UW
       INTEGER NPERI,IPERI,NCOUN
       LOGICAL FIRST, NOTDO
C - bit 30 (IMCNEF=536870912) is set in VDXY and VDZT quality flag
C   to indicate a MC inefficiency rejected hit.
C   bit 29 now means that smearing has been added to a cluster
      INTEGER IMCNEF,IMCSMR
      PARAMETER (IMCNEF=536870912)
      PARAMETER (IMCSMR=268435456)
C
C  BOS functions
C
      INTEGER NAMIND
      EXTERNAL NAMIND
C
C  Variables for inline functions
C
      INTEGER I,J
      INTEGER ISEED,IDICE
      REAL RNVEC(100)
C
C  Inline functions
C
      INTEGER VDMJLN,IOR
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
        NEVEH = NAMIND('EVEH')

C       check-up on the efficiency map VDEM banks,
        IVDEM=IW(MVDEM)
        IF(IVDEM.LE.0) THEN
            NOTDO=.TRUE.
            IER=-1
            RETURN
        ENDIF
C       write out some stuff to check on VDEM bank
        NPERI=LROWS(IVDEM)
        IF(NPERI.GT.0) THEN
           DO 100 I=1,NPERI
              KVDEM=KROW(IVDEM,I)
              NKILL=0
              DO 101 J=1,288
                 IF(RW(KVDEM+JVDEEF-1+J).LT.EFFCUT) NKILL=NKILL+1
  101         CONTINUE
  100      CONTINUE
        ELSE
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
C  suppress the dummy faces for the new VDET 
C  also do the effiency
      IF (VDYEAR().EQ.95) THEN
        CALL VDFSUP
C  for the new VDET, get a constant efficiency from the first
C  element of the VDEM bank, it should read 0.98 or 0.99
        CALL VDKLCP(IER)
        RETURN
      ENDIF
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
C
C  Loop over all VDXY banks and flag hits
C
      IVDXY  = IW(MVDXY)
      IF(IVDXY .LE. 0) GO TO 3
  2   CONTINUE
C  loop over all the hits of the VDXY bank
      NHXY  = LROWS(IVDXY)
      NVDXY = IW(IVDXY-2)
      IVNEX = IW(IVDXY-1)
      DO 20 IHXY=1,NHXY
           KVDXY=KROW(IVDXY,IHXY)
           UW=RW(KVDXY+JVDXUC)
           JL=VDMJLN(UW,NVDXY,2)
C          flag hit if efficiency set low
           IF(RW(KVDEM+JVDEEF-1+JL).LT.EFFCUT) THEN
               IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
           ELSE
C          kill a few random clusters to simulate inefficiency
               IDICE=IDICE+1
               IF(IDICE.GT.100) THEN
                  CALL RANMAR(RNVEC,100)
                  IDICE=1
               ENDIF
               IF(RW(KVDEM+JVDEEF-1+JL).LT.RNVEC(IDICE)) THEN
                   IW(KVDXY+JVDXQF)=IOR(IW(KVDXY+JVDXQF),IMCNEF)
               ENDIF
           ENDIF
C
  20  CONTINUE
C  Update pointer, and return for next bank
      IVDXY=IVNEX
      IF(IVDXY .GT. 0) GO TO 2
C
C  all done with VDXY, now do VDZT
C
3     CONTINUE
C
C  Now loop over all VDZT banks and flag hits
C
      IVDZT = IW(MVDZT)
      IF(IVDZT .LE. 0) RETURN
C  Loop over all VDZT banks
  5   CONTINUE
C  loop over all the hits of the VDZT bank
      NHZT  = LROWS(IVDZT)
      NVDZT = IW(IVDZT-2)
      IVNEX = IW(IVDZT-1)
      DO 50 IHZT=1,NHZT
           KVDZT=KROW(IVDZT,IHZT)
           UW=RW(KVDZT+JVDZWC)
           JL=VDMJLN(UW,NVDZT,1)
C          flag hit if efficiency set low
           IF(RW(KVDEM+JVDEEF-1+JL).LT.EFFCUT) THEN
               IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
           ELSE
C          kill a few random clusters to simulate inefficiency
               IDICE=IDICE+1
               IF(IDICE.GT.100) THEN
                  CALL RANMAR(RNVEC,100)
                  IDICE=1
               ENDIF
               IF(RW(KVDEM+JVDEEF-1+JL).LT.RNVEC(IDICE)) THEN
                  IW(KVDZT+JVDZQF)=IOR(IW(KVDZT+JVDZQF),IMCNEF)
               ENDIF
           ENDIF
  50  CONTINUE
C  Update pointer, and return for next bank
      IVDZT = IVNEX
      IF(IVDZT .GT. 0) GO TO 5
C
      RETURN
C
      END
#endif
