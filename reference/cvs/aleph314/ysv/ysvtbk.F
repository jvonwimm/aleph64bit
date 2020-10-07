        SUBROUTINE YSVTBK(NVX, NTK)
C---------------------------------------------------------------------------
C!   Create VSVT vertex->track relation bank
C
C    Returns:  NVX  number of output vertices
C              NTK  number of tracks in output vertices
C
C       Author:  D.Casper  06/07/96
C       Modified:          21/09/97  use hit bitmasks to verify track direction
C
C       Input banks:    YSVX/NR(<0) Reconstructed vertices
C                       YSTL        Internal track list (to find FRFT
C                                   number of each TPC track)
C                       YSCL        Internal circle list (to find FRFT
C                                   number of each ITC-only track)
C
C       Output bank:    YSVT/NR     ABS(NR)=multiplicity, parallel to
C                                   YSVX/NR. Each YSVT bank has a number
C                                   of columns = ABS(NR).  Contains a
C                                   list of the FRFT tracks (positive #'s)
C                                   and YLV0 V0's (negative #'s) associated
C                                   with the corresponding row of of YSVX.
C                                   A track with a number > 10000 is
C                                   identified as the incoming track to
C                                   the vertex.  
C
C---------------------------------------------------------------------------
#ifndef DOC
#include "ysvcut.h"
#include "ystljj.h"
#include "yscljj.h"
#include "ysvtjj.h"
#include "ysvxjj.h"
#include "frftjj.h"
#include "fridjj.h"
#include "alcons.h"
#include "bcs.h"
C
        DIMENSION MASK(32), PRIMARY(3), P_PRIMARY(3),PTRK(3)
        DIMENSION IOK(32),INO(32)
        LOGICAL NUDGE
        DATA NYSVX/0/
        SAVE NYSVX, NYSTL, NYSCL, NFRFT, NFRID
C
#include "bmacro.h"
C
        IF (NYSVX.EQ.0) THEN
            NYSVX = NAMIND('YSVX')
            NYSTL = NAMIND('YSTL')
            NYSCL = NAMIND('YSCL')
            NFRFT = NAMIND('FRFT')
            NFRID = NAMIND('FRID')
            DO I = 1, 32
                MASK(I) = IBSET(0,I-1)
            ENDDO
        ENDIF
C
C+  Drop output bank, if it exists
C
        CALL BDROP(IW,'YSVT')

        KYSTL = IW(NYSTL)
        KYSCL = IW(NYSCL)
        KFRFT = IW(NFRFT)
        KFRID = IW(NFRID)
C
C+ Load primary vertex and its z error (squared)
C
        KYSVX = NLINK('YSVX',0)
        CALL UCOPY(RW(KROW(KYSVX,1)+JYSVVX),PRIMARY,3)
        SIGZ2_PRIMARY = RTABL(KYSVX,1,JYSVVM+5)
C
C+  Start chaining through the YSVX banks
C
        NVX = 0
        NTK = 0
        KYSVX = IW(NYSVX)
        DO WHILE (KYSVX.NE.0 .AND. IW(KYSVX-2).LE.-2)
C
C+  Create a YSVT bank of the needed size
C
            NR = IW(KYSVX-2)
            NVX = NVX + LROWS(KYSVX)
            NTK = NTK + LROWS(KYSVX) * IABS(NR)
            LYSVT = LMHLEN + LROWS(KYSVX)*LYSVTA*IABS(NR)
            CALL AUBOS('YSVT',NR,LYSVT,KYSVT,IGARB)
            IF (IGARB.EQ.1) THEN
                KYSVX = NLINK('YSVX',NR)
                KYSTL = IW(NYSTL)
                KYSCL = IW(NYSCL)
                KFRFT = IW(NFRFT)
            ELSE IF (IGARB.EQ.2) THEN
                GOTO 999
            ENDIF
            IW(KYSVT+LMHCOL) = LYSVTA
            IW(KYSVT+LMHROW) = 0
            CALL BLIST(IW,'T+','YSVT')
C
C+  Loop over the rows of YSVX
C
            DO I = 1, LROWS(KYSVX)
                IYSVX = KROW(KYSVX,I)
C+  Check number of bits set
                NBITS = 0
                DO IWRD = 0, 4
                    NBITS = NBITS + NUMBIT(IW(IYSVX+JYSVTM+IWRD))
                ENDDO
                IF (NBITS.NE.IABS(NR)) THEN
                    CALL BDROP(IW,'YSVT')
                    GOTO 999
                ENDIF
C+  Vertex radius for incoming classification
                RVERT = SQRT(RW(IYSVX+JYSVVX)**2 + RW(IYSVX+JYSVVY)**2)
C+  Maximum pt among tracks consistent with primary
                pt_class2 = 0.
                income_cl2 = 0
                pt_class4 = 0.
                income_cl4 = 0
C+  Fill entries in YSVT
                NBITS_OUT = 0
                IWRD = 0
                DO WHILE(IWRD.LT.5 .AND. NBITS_OUT.LT.IABS(NR))
                    J = 1
                    DO WHILE(J.LE.32 .AND. NBITS_OUT.LT.IABS(NR))
                      IF (IAND(IW(IYSVX+JYSVTM+IWRD),MASK(J)).NE.0)THEN
                        IYSVT = KNEXT(KYSVT)
                        IF (IWRD.EQ.4) THEN     ! V0
                          IW(IYSVT+JYSVTN) = -J
                          IW(IYSVT+JYSVSC) = 1
                          IW(IYSVT+JYSVIO) = 1
                        ELSE IF (IWRD.EQ.3) THEN !ITC track
                          IF (KYSCL.GT.0) THEN
                            IFRFT = ITABL(KYSCL,J,JYSCTN)
                            IW(IYSVT+JYSVTN) = IFRFT
                            PSIP = 0.
                            CALL YHELIX(IFRFT,RW(IYSVX+JYSVVX),
     &                          RW(IYSVX+JYSVVM+5),PSIS,PTRK,IER)
                            PSI_IN = RTABL(KYSCL,J,JYSCRI)
                            PSI_OUT = RTABL(KYSCL,J,JYSCRO)
C Give a little slop around the endpoints
                            OMEGA = RTABL(KFRFT,IFRFT,JFRFIR)
                            OMEGA = SIGN(MAX(1.E-7,ABS(OMEGA)),OMEGA)
                            D0 = RTABL(KFRFT,IFRFT,JFRFD0)
                            DIFF = (1./OMEGA-D0)
                            R_IN = SQRT(MAX(0.,1./OMEGA**2+DIFF**2 
     &                              +2./OMEGA*DIFF*COS(PSI_IN)))
                            R_IN = MAX(1.E-6,R_IN)
                            DRDPSI_IN = -1./OMEGA*DIFF*SIN(PSI_IN)
     &                                                          /R_IN
                            DRDPSI_IN = SIGN(MAX(0.01,
     &                                  ABS(DRDPSI_IN)),DRDPSI_IN)
                            R_OUT = SQRT(MAX(0.,1./OMEGA**2+DIFF**2 
     &                              +2./OMEGA*DIFF*COS(PSI_OUT)))
                            R_OUT = MAX(1.E-6,R_OUT)
                            DRDPSI_OUT = -1./OMEGA*DIFF*SIN(PSI_OUT)
     &                                      /R_OUT
                            DRDPSI_OUT = SIGN(MAX(0.01,
     &                                  ABS(DRDPSI_OUT)),DRDPSI_OUT)
                            RW(IYSVT+JYSVPS) = PSIS
                            RW(IYSVT+JYSVPI) = PSI_IN
                            RW(IYSVT+JYSVPO) = PSI_OUT
                            RW(IYSVT+JYSVPC) = RTABL(KYSCL,J,JYSCCP)
C Decide the direction the track travels
                            CALL YSENSE(PSIP,PSI_IN,
     &                              PSI_OUT,PSIS,ISENSE)
                            IF (ISENSE.EQ.4) THEN
                                NUDGE = .FALSE.
                                IF(PSI_OUT.GT.PSIS) THEN
                                  IF(ABS(DRDPSI_OUT)*(PSI_OUT-PSIS)
     &                                .LT.6.) NUDGE = .TRUE.
                                ENDIF
                                IF (PSI_IN.LT.PSIS) THEN
                                  IF(ABS(DRDPSI_IN)*(PSIS-PSI_IN)
     &                                 .LT.6.) NUDGE = .TRUE.
                                ENDIF
                                IF (NUDGE) THEN
                                 IF (PSI_OUT-PSIS.LT.PSIS-PSI_IN) THEN
                                    ISENSE = 1
                                 ELSE
                                    ISENSE = 2
                                 ENDIF
                                ENDIF
                            ENDIF
                            IF (ISENSE.EQ.1 .OR. ISENSE.EQ.3
     &                                 .OR. ISENSE.EQ.4) THEN
                              CALL UCOPY(PTRK,RW(IYSVT+JYSVXP),3)
                              IF (ISENSE.EQ.1 .OR. SENSE.EQ.3) THEN
                                IW(IYSVT+JYSVIO) = 1
                              ELSE
                                IF ((RTABL(KYSCL,J,JYSCCP).LE.YSCTPV)
     &                            .OR.(ABS(RTABL(KYSCL,J,JYSCH0+3))
     &                                                  .LE.0.5))THEN
                                  IF (PTRK(1)**2 + PTRK(2)**2 .GT.
     &                                  PT_CLASS4) THEN
                                    PT_CLASS4 = PTRK(1)**2+PTRK(2)**2
                                    INCOME_CL4 = IYSVT
                                  ENDIF
                                ENDIF
                                IF (PSIS-PSI_IN .LT. PSI_OUT-PSIS) THEN
                                  IW(IYSVT+JYSVIO) = 1
                                ELSE
                                  IW(IYSVT+JYSVIO) = -1
                                ENDIF
                              ENDIF
                            ELSE IF (ISENSE.EQ.2) THEN
                              IF ((RTABL(KYSCL,J,JYSCCP).LE.
     &                         0.5*YSCTPV) .OR. 
     &                         (ABS(RTABL(KYSCL,J,JYSCH0+3)).LE.1 ))THEN
                                    IF (PTRK(1)**2 + PTRK(2)**2 .GT.
     &                                  PT_CLASS2) THEN
                                      PT_CLASS2 = PTRK(1)**2+PTRK(2)**2
                                      INCOME_CL2 = IYSVT
                                    ENDIF
                              ENDIF
                              RW(IYSVT+JYSVXP) = -PTRK(1)
                              RW(IYSVT+JYSVYP) = -PTRK(2)
                              RW(IYSVT+JYSVZP) = -PTRK(3)
                              IW(IYSVT+JYSVIO) = -1
                            ELSE
                              CALL UCOPY(PTRK,RW(IYSVT+JYSVXP),3)
                              IF (PSIS-PSI_IN .LT. PSI_OUT-PSIS) THEN
                                IW(IYSVT+JYSVIO) = 1
                              ELSE
                                IW(IYSVT+JYSVIO) = -1
                              ENDIF
                            ENDIF
                            IW(IYSVT+JYSVSC) = ISENSE
                          ENDIF
                        ELSE                    ! Track
                          IF (KYSTL.GT.0) THEN
                            IFRFT = ITABL(KYSTL,32*IWRD+J,JYSTTN)
                            IW(IYSVT+JYSVTN) = IFRFT
                            PSIP = 0.
                            CALL YHELIX(IFRFT,RW(IYSVX+JYSVVX),
     &                          RW(IYSVX+JYSVVM+5),PSIS,PTRK,IER)
                            PSI_IN = RTABL(KYSTL,J,JYSTRI)
                            PSI_OUT = RTABL(KYSTL,J,JYSTRO)
                            OMEGA = RTABL(KFRFT,IFRFT,JFRFIR)
                            OMEGA = SIGN(MAX(1.E-7,ABS(OMEGA)),OMEGA)
                            D0 = RTABL(KFRFT,IFRFT,JFRFD0)
                            DIFF = (1./OMEGA-D0)
                            R_IN = SQRT(MAX(0.,1./OMEGA**2+DIFF**2 
     &                              +2./OMEGA*DIFF*COS(PSI_IN)))
                            R_IN = MAX(1.E-6,R_IN)
                            DRDPSI_IN = -1./OMEGA*DIFF*SIN(PSI_IN)
     &                                                          /R_IN
                            DRDPSI_IN = SIGN(MAX(0.01,
     &                                  ABS(DRDPSI_IN)),DRDPSI_IN)
                            IF (DRDPSI_IN.GT.0) THEN
                                DPSI_IN = 6./DRDPSI_IN
                            ELSE
                                DPSI_IN = -12./DRDPSI_IN
                            ENDIF
                            R_OUT = SQRT(MAX(0.,1./OMEGA**2+DIFF**2 
     &                              +2./OMEGA*DIFF*COS(PSI_OUT)))
                            R_OUT = MAX(1.E-6,R_OUT)
                            DRDPSI_OUT = -1./OMEGA*DIFF*SIN(PSI_OUT)
     &                                      /R_OUT
                            DRDPSI_OUT = SIGN(MAX(0.01,
     &                                  ABS(DRDPSI_OUT)),DRDPSI_OUT)
                            RW(IYSVT+JYSVPS) = PSIS
                            RW(IYSVT+JYSVPI) = PSI_IN
                            RW(IYSVT+JYSVPO) = PSI_OUT
                            RW(IYSVT+JYSVPC) = RTABL(KYSTL,J,JYSTCP)
C Decide the direction the track travels
                            CALL YSENSE(PSIP,PSI_IN,
     &                              PSI_OUT,PSIS,ISENSE)
                            IF (ISENSE.EQ.4) THEN
C Be careful here, because this case is the trickiest.  If the track has
C a lot of missing hits on one side of the secondary, it probably isn't going
C in that direction...
                             IF (KFRID.GT.0) THEN
                              IBITOK = ITABL(KFRID,IFRFT,JFRIBP)
                              IBITNO = ITABL(KFRID,IFRFT,JFRIDZ)
                              CALL UBITS(IBITOK,32,IOK,NOK)
                              CALL UBITS(IBITNO,32,INO,NNO)
                              CALL YPRVER(RW(IYSVX+JYSVVX),IVTX,.FALSE.)
                              IVTX = IVTX - 1
                              IF (IVTX.EQ.0) THEN
                                ISENSE = 1
                              ELSE
                                NCOKIN = 0
                                NCNOIN = 0
                                NCOKOUT = 0
                                NCNOOUT = 0
                                DO L = 1, NOK
                                  ILAY = IOK(L)
                                  IF (ILAY.LE.IVTX) THEN
                                    NCOKIN = NCOKIN + 1
                                  ELSE
                                    NCOKOUT = NCOKOUT + 1
                                  ENDIF
                                ENDDO
                                DO L = 1, NNO
                                  ILAY = INO(L)
                                  IF (ILAY.LE.IVTX) THEN
                                    NCNOIN = NCNOIN + 1
                                  ELSE
                                    NCNOOUT = NCNOOUT + 1
                                  ENDIF
                                ENDDO
                                OCCIN = FLOAT(NCOKIN)
     &                              /MAX(1,NCOKIN+NCNOIN)
                                OCCOUT = FLOAT(NCOKOUT)
     &                              /MAX(1,NCOKOUT+NCNOOUT)
                                IF (OCCIN.GT.0.5 
     &                              .AND. OCCOUT.LT.0.5)THEN
                                    ISENSE = 2
                                ELSE IF (OCCIN.LT.0.5
     &                              .AND. OCCOUT.GT.0.5) THEN
                                    ISENSE = 1
                                ELSE IF (OCCIN.GE.0.5
     &                              .AND. OCCOUT.GE.0.5) THEN
                                    ISENSE = 4
                                ELSE IF (OCCIN.GT.OCCOUT) THEN
                                    ISENSE = 2
                                ELSE
                                    ISENSE = 1
                                ENDIF
                              ENDIF
                             ELSE
                                NUDGE = .FALSE.
                                IF(PSI_OUT.GT.PSIS) THEN
                                  IF(ABS(DRDPSI_OUT)*(PSI_OUT-PSIS)
     &                                .LT.6.) NUDGE = .TRUE.
                                ENDIF
                                IF (PSI_IN.LT.PSIS) THEN
                                  IF(ABS(DRDPSI_IN)*(PSIS-PSI_IN)
     &                                 .LT.6.) NUDGE = .TRUE.
                                ENDIF
                                IF (NUDGE) THEN
                                 IF (PSI_OUT-PSIS.LT.PSIS-PSI_IN) THEN
                                    ISENSE = 1
                                 ELSE
                                    ISENSE = 2
                                 ENDIF
                                ENDIF
                             ENDIF
                            ENDIF
                            IF (ISENSE.EQ.1 .OR. ISENSE.EQ.3
     &                                 .OR. ISENSE.EQ.4) THEN
                              CALL UCOPY(PTRK,RW(IYSVT+JYSVXP),3)
                              IF (ISENSE.EQ.1 .OR. SENSE.EQ.3) THEN
                                IW(IYSVT+JYSVIO) = 1
                              ELSE
                                IF ((RTABL(KYSTL,J,JYSTCP).LE.YSCTPV/2.)
     &                            .OR.(ABS(RTABL(KYSTL,J,JYSTH2+3))
     &                                                .LE.0.25))THEN
                                  IF (PTRK(1)**2 + PTRK(2)**2 .GT.
     &                                  PT_CLASS4) THEN
                                    PT_CLASS4 = PTRK(1)**2+PTRK(2)**2
                                    INCOME_CL4 = IYSVT
                                  ENDIF
                                ENDIF
                                IF (PSIS-PSI_IN .LT. PSI_OUT-PSIS) THEN
                                  IW(IYSVT+JYSVIO) = 1
                                ELSE
                                  IW(IYSVT+JYSVIO) = -1
                                ENDIF
                              ENDIF
                            ELSE IF (ISENSE.EQ.2) THEN
                              IF ((RTABL(KYSTL,J,JYSTCP).LE.
     &                                                1.5*YSCTPV) .OR. 
     &                            (ABS(RTABL(KYSTL,J,JYSTH2+3))
     &                                                  .LE.1 ))THEN
                                    IF (PTRK(1)**2 + PTRK(2)**2 .GT.
     &                                  PT_CLASS2) THEN
                                      PT_CLASS2 = PTRK(1)**2+PTRK(2)**2
                                      INCOME_CL2 = IYSVT
                                    ENDIF
                              ENDIF
                              RW(IYSVT+JYSVXP) = -PTRK(1)
                              RW(IYSVT+JYSVYP) = -PTRK(2)
                              RW(IYSVT+JYSVZP) = -PTRK(3)
                              IW(IYSVT+JYSVIO) = -1
                            ELSE
                              CALL UCOPY(PTRK,RW(IYSVT+JYSVXP),3)
                              IF (PSIS-PSI_IN .LT. PSI_OUT-PSIS) THEN
                                IW(IYSVT+JYSVIO) = 1
                              ELSE
                                IW(IYSVT+JYSVIO) = -1
                              ENDIF
                            ENDIF
                            IW(IYSVT+JYSVSC) = ISENSE
                          ENDIF
                        ENDIF
                        IW(KYSVT+LMHROW) = IW(KYSVT+LMHROW)+1
                        NBITS_OUT = NBITS_OUT + 1
                      ENDIF
                      J = J + 1
                    ENDDO
                    IWRD = IWRD + 1
                ENDDO
C  Make a *final* decision on the incoming track, if necessary
                IF (INCOME_CL2.GT.0) THEN
C  All class two tracks have had their momentum reversed - reverse it back
C  for the one, true incoming track.
                    RW(INCOME_CL2+JYSVXP) = -RW(INCOME_CL2+JYSVXP)
                    RW(INCOME_CL2+JYSVYP) = -RW(INCOME_CL2+JYSVYP)
                    RW(INCOME_CL2+JYSVZP) = -RW(INCOME_CL2+JYSVZP)
                    IW(INCOME_CL2+JYSVSC) = 0
                ELSE IF (INCOME_CL4.GT.0) THEN
C  Class 4 guys have not been reversed, so just flag them
                    IW(INCOME_CL4+JYSVSC) = 0
                ENDIF
            ENDDO
            KYSVX = IW(KYSVX-1)
        ENDDO
 999    CONTINUE
        CALL AUBPRS('YSVT')
        RETURN
        END
#endif
