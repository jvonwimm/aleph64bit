      SUBROUTINE KXLUAL (VMAIN,ISTAT,MVX,MTRK)
C ---------------------------------------------------------
C - B.Bloch-Devaux - J.Boucrot - F.Ranjard - 870516
C! Build the event interface LUND-Aleph
CKEY KINE KINGAL LUND FILL   /  USER
C - Fill    : PTRAK(ix,n)  = px,py,pz,mass of track(n)
C                            if mass=0. it will be filled by the system
C             IPVNU(1,n)   = origin vertex # of track(n)
C                  (2,n)   = decay vertex # of track(n)
C                             0 if no decay
C                  (3,n)   = ALEPH particle #
C             IPCOD(n)     = LUND history code of track(n)
C - Book    : KHIS bank filled with IPCOD(n)
C - Call    : KFEVBK (VMAIN,PTRAK,IPVNU,MTRK,JSTAT)
C             to book propagate the decay and fill VERT and KINE
C
C - structure: SUBROUTINE subprogram
C              User Entry Name: KXLUAL
C              External Regerences: NAMIND(BOS77)
C                                   ALTABL/KFEVBK(ALEPHLIB)
C              Comdecks referenced: BCS, LUNDCOM, ALCONS, KIPARA
C
C - usage   : CALL KXLUAL (VMAIN,ISTAT,MVX,MTRK)
C - Input   : VMAIN = vx,vy,vz,tof of the primary vertex
C - Output  : ISTAT = status word ( = 0 means OK)
C                     - 1 means VERT or KINE bank missing
C                     - 2 means not enough space for VERT or KINE
C                     - 3 means too many tracks
C                     - 4 electrons beams not stored as lines 1 and 2
C                     - 5 means Lund status code larger than 5 found
C                     > 0 means unknown LUND particle# ISTAT
C             MVX   = # of vertices
C             MTRK  = # of tracks to be propagated ( no beam electrons )
#ifndef DOC
      SAVE
#include "bcs.h"
#include "lundcom.h"
#include "alcons.h"
#include "kipara.h"
      PARAMETER (ILUD=10000, TLIMI=1.E-15)
      REAL PTRAK(4,LUTRK),VMAIN(4)
      INTEGER IPVNU(3,LUTRK),IPCOD(LUTRK)
      INTEGER ALTABL
      DATA NAPAR/0/
#include "bmacro.h"
#include "kmacro.h"
C ------------------------------------------------------
      IF (NAPAR .EQ. 0) NAPAR = NAMIND ('PART')
C
C - Check particle buffer length
      IF (NPARLU .GT. LUTRK) THEN
         WRITE (IW(6),1001) NPARLU, LUTRK
 1001    FORMAT(/1X,'+++KXLUAL+++ not enough space to save'
     &         ,' the event :# of tracks = ',I2,2X,'allowed = ',I2
     &         /13X,'==>increase LUTRK in KIPARA.h')
         ISTAT = - 3
         GOTO 999
      ENDIF
C
C - Build array containing vertex # and particle # of each track
C
      IBEA=0
      NVER = 1
      DO 10 ITR=1,NPARLU
C Look for "mother" particle
         ILUN  = KODELU(ITR,2)
         IPART = KGPART(ILUN)
         IF (IPART .LE. 0) GOTO 998
         KS=KODELU(ITR,1)/ILUD
         IMOTH=MOD(KODELU(ITR,1),ILUD)
C
C Store now momentum components and codes of the track :
          DO 9 I=1,3
 9        PTRAK(I,ITR-IBEA)=PARTLU(ITR,I)
C         store the current mass
          PTRAK(4,ITR-IBEA)=PARTLU(ITR,5)
          IPVNU(3,ITR-IBEA)=IPART
          IPCOD(ITR-IBEA)=KODELU(ITR,1)
C
             IF (KS.LE.1) THEN
C            Particle not decayed in LUND
C            if stable particle created in initial state ,IMOTH=0
                 IF (IMOTH-IBEA.LE.0 ) THEN
                   IPVNU(1,ITR-IBEA)=1
                ELSE
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)
                ENDIF
                IPVNU(2,ITR-IBEA)=0
             ELSE IF ((KS-2)*(KS-3)*(KS-5).EQ.0) THEN
C            Particle has decayed in LUND
                 IF (IMOTH-IBEA.LE.0 ) THEN
C               Primary parton
                   IPVNU(1,ITR-IBEA)=1
                ELSE
                   IPVNU(1,ITR-IBEA)=IPVNU(2,IMOTH-IBEA)
                ENDIF
C               Decay inside LUND and finite lifetime :
C               this track will be propagated in KFEVBK until its decay
                TLIF = TIMLIF (IPART)
                IF (TLIF.GT.TLIMI .AND. IDBLU3(IABS(ILUN)).GT.0) THEN
                   NVER=NVER+1
                   IPVNU(2,ITR-IBEA)=NVER
                ELSE
C               Decay is immediate ( will not be propagated)
                   IPVNU(2,ITR-IBEA)=IPVNU(1,ITR-IBEA)
                ENDIF
             ELSE IF (KS.EQ.4) THEN
C            electron beams were stored as well
C            check that they appear only on lines 1 or 2
                ILUN=-4
                IF (ITR.GT.2) GO TO 998
                IST=KBKINE(-ITR,PTRAK(1,ITR-IBEA),IPART,0)
                IF (IST.LE.0) THEN
                   ILUN=-2
                   GO TO 998
                ENDIF
                IBEA=IBEA+1
             ELSE IF (KS.GE.6 ) THEN
                ILUN=-5
                GO TO 998
             ENDIF
C
C         Update history code
          IF ( IMOTH .GT. IBEA ) THEN
             IPCOD(ITR-IBEA) = IPCOD(ITR-IBEA) - IBEA
          ELSE
             IPCOD(ITR-IBEA) = IPCOD(ITR-IBEA) - IMOTH
          ENDIF
 10    CONTINUE
C
C - Propagate decays and fill KINE and VERT banks
       NPARL = NPARLU-IBEA
       CALL KFEVBK(VMAIN,PTRAK,IPVNU,NPARL,IFAIL)
C - Fill history bank KHIS
       JKHIS = ALTABL ('KHIS',1,NPARL,IPCOD,'I','E')
C
       MVX = NVER
       MTRK = NPARL
       ISTAT = IFAIL
       GOTO 999
C
C - Error
C      unknown LUND particle
 998   ISTAT = ILUN
C
 999   RETURN
       END
#endif
