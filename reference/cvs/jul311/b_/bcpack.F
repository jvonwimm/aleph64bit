      SUBROUTINE BCPACK
C----------------------------------------------------------------------
C!  - This subroutine reduces the length of the BCAL banks BPTR and BHIT
C
C
C        The individual silicon strip hits from the BHIT bank are
C        suppressed. Instead 4 new hits are created with the centroid
C        given by the individual silicon strips.
C        Furthermore, the 'single' triggers are 'prescaled' by a factor
C        given by the variable NEWMAX, which is set to 100.
C        These two changes should reduce the BCAL information by a
C        factor of 20 or so.
C        E. Fernandez     6-Dec-89
C
C        MODIFICATIONS:   13-JUN-90    by  J.A. Perlas
C                         Consider new Trigger Mask types in
C                         the new DAQ implementation.
C
C                         28-AUG-91    by  E. Fernandez
C                         Protect against inexistent banks.
C                         Report exeeding no. of counts with
C                         error reporting subroutine.
C
C     Common /BCADCS/ used to exchange information between
C     the BCPACK and CENTRD subroutines.
C     TRMASK: is the BCAL trigger mask
C     CENTRO: (1-4) centroid of shower in BCAL monitors 1-4
C     ADCSUM: (1-4) tot ADCSUM in BCAL monitors 1-4
C     ADCDAT: (180) ADC in channels 1-180 of BCAL
C     ADCSCI: ADC for scintillators in monitors 1-4 channels 1-5
C     ADCSIL: ADC for sil strips    in monitors 1-4 channels 1-40
C
C?
C!======================================================================
#ifndef DOC
      INTEGER PTEVEH, PTBCNT, PTBHIT, PTBPTR
#include "bcs.h"
#include "bcadcs.h"
      DIMENSION NHIT(2500),NPTR(102)
      INTEGER TRMASK
      DATA ISCAL/0/,IPRESC/100/,NEWMAX/100/

C   Find name-index of banks of interest
      KEVEH = NAMIND('EVEH')
      KBCNT = NAMIND('BCNT')
      KBHIT = NAMIND('BHIT')
      KBPTR = NAMIND('BPTR')
      IF(KEVEH.EQ.0.OR.KBCNT.EQ.0.OR.KBHIT.EQ.0.OR.KBPTR.EQ.0)
     +  GOTO 99

C   Extract info of BCAL events
C   Get pointers to banks
      PTEVEH = IW(KEVEH)
      PTBCNT = IW(KBCNT)
      PTBHIT = IW(KBHIT)
      PTBPTR = IW(KBPTR)
      IF(PTEVEH.EQ.0.OR.PTBCNT.EQ.0.OR.PTBHIT.EQ.0.OR.PTBPTR.EQ.0)
     +  GOTO 99

C   Number of BCAL events in that ALEPH event
      NEVB = IW(PTBPTR + 2)
      IF(NEVB.EQ.0) GO TO 99
      NHIT(1)=1
      NPTR(1)=1

C   FIND ADCS SPECTRA
C   BCAL events loop
      NEWCNT=0
      NLAST=0
      NHT=0
      IFLCNT=0
      DO 200 IEV= 1,NEVB
C   Initialization of ADCs
        DO 2 IJK= 1,180
          ADCDAT(IJK)= 0.
2       CONTINUE

C   Get trigger mask
        TRMASK= IBITS(IW(PTBPTR + 2 + IEV), 0, 8)
        KFIRST= IBITS(IW(PTBPTR + 2 + IEV), 16, 16)
        LEN=    IBITS(IW(PTBPTR + 2 + IEV), 8, 8)

C   Pre-scale non-coincidence triggers
        IF(TRMASK.NE.21 .AND. TRMASK.NE.42  .AND.
     +     TRMASK.NE.85 .AND. TRMASK.NE.106 ) THEN
           ISCAL=ISCAL+1
           IF(MOD(ISCAL,IPRESC).NE.0) GO TO 200
        ENDIF

        NEWCNT=NEWCNT+1
        IF(NEWCNT.GE.NEWMAX) THEN
           IFLCNT=IFLCNT+1
           IF(IFLCNT.EQ.1) THEN
             CALL RERROR('BCPACK',1,'Max. cnts exceeded, stop accum.')
           ENDIF
           GO TO 201
        ENDIF

        NEWHIT=0
        NEWFST=NLAST+1

C   Hits loop
        IF(LEN.GT.0) THEN

          DO 5 IHIT= KFIRST, KFIRST + LEN - 1
            IADC= IBITS(IW(PTBHIT + 2 + IHIT), 16, 16)
            ICH = IBITS(IW(PTBHIT + 2 + IHIT), 8, 8)
            ADCDAT(ICH)= FLOAT(IADC)
            IF(ICH.LE.20) THEN
               NEWHIT=NEWHIT+1
               NHT=NHT+1
               NHIT(2+NHT)=IW(PTBHIT+2+IHIT)
            ENDIF

5         CONTINUE
        ENDIF
C   End of hits loop

C   Convert Silicon ADCs to matrix format
      KK= 20
      DO 10 IMUX= 1,8
        DO 9 IMON= 1,4
          DO 8 I= 1,5
            KK= KK + 1
            J = IMUX + 8 * (I - 1)
            ADCSIL(IMON,J)= ADCDAT(KK)
8         CONTINUE
9       CONTINUE
10    CONTINUE

C   Calculate centroid
      CALL CENTRD
      DO 15 III=1,4
         IF(CENTRO(III).NE.0.) THEN
            NEWHIT=NEWHIT+1
            NHT=NHT+1
            ICENT=NINT(CENTRO(III)*100)
            NHIT(2+NHT)=III+(180+III)*256+ICENT*65536
         ENDIF
15    CONTINUE
      NPTR(2+NEWCNT)=TRMASK+NEWHIT*256+NEWFST*65536
      NLAST=NLAST+NEWHIT

200   CONTINUE

201   CONTINUE

      NHIT(2)=NHT
      NPTR(2)=NEWCNT

C  Copy back the banks to their original location
      IF(NEWCNT.EQ.0) GO TO 99
      DO 210 I=1,NEWCNT+2
         IW(PTBPTR+I)=NPTR(I)
210   CONTINUE
      DO 212 I=1,NLAST+2
         IW(PTBHIT+I)=NHIT(I)
212   CONTINUE

C   Tell BOS that the banks have been overwritten
      CALL AUBOS('BPTR',0,NEWCNT+2,IND1,IGARB)
      CALL AUBOS('BHIT',0,NLAST+2,IND2,IGARB)
      RETURN

99    CONTINUE
      CALL BDROP(IW,'BHITBPTR')
      RETURN
      END
      SUBROUTINE CENTRD
C----------------------------------------------------------------------
C!   Calculate centroid of detected particle using silicon info.
C!
C!   Author:    Josep A. Perlas       10-OCT-1989
C!   ======
C?
C!======================================================================
#ifndef DOC
#include "bcadcs.h"
      INTEGER TRMASK
      DATA ETMIN/1200./

      DO 1 IMON= 1,4
        CENTRO(IMON)= 0.
        ETOT= 0.
        DO 2 ICHAN= 1,40
          CENTRO(IMON)= CENTRO(IMON) + ICHAN * ADCSIL(IMON,ICHAN)
          ETOT= ETOT + ADCSIL(IMON,ICHAN)
 2      CONTINUE
        IF(ETOT.NE.0.) CENTRO(IMON)= CENTRO(IMON)/ETOT
        IF(ETOT.LT.ETMIN) CENTRO(IMON)=0.
 1    CONTINUE
      RETURN
      END
#endif
#endif
