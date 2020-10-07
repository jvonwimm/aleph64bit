        SUBROUTINE YSVTRK(IER)
C----------------------------------------------------------------------------
C!  Try to add helices, circles, and V0's to existing vertices
C
C   Returns:    IER     0 if no error
C
C   This routine loops over objects not yet associated with a vertex
C   and tries to add them to existing vertices.
C
C       Author:  D. Casper  18/07/96
C     Modified:  D. Casper  30/09/97
C          Put only JYSVQU=0 objects in mask of objects already used
C                D. Casper   3/03/98
C          Check track quality flag before using
C          
C
C       Input banks:   YSVX/NR(<0)  Vertices found thus far; ABS(NR) is
C                                   the multiplicity of the vertex
C                      YSTL         Helices
C                      YSCL         Circles
C                      YS0L         V0's
C
C       Output bank:   YSVX/NR(<0)  Any new vertices formed are added to
C                                   the appropriate YSVX bank, depending
C                                   on the new multiplicity.
C
C----------------------------------------------------------------------------
#ifndef DOC
C
#include "ysvcut.h"
#include "ystljj.h"
#include "yscljj.h"
#include "ys0ljj.h"
#include "ysvxjj.h"
#include "bcs.h"
C
        DIMENSION MASKV(5), MASKN(5), MASKA(5)
        DATA NYSTL/0/
C
#include "bmacro.h"
C
        IER = 0
        IF (NYSTL.EQ.0) THEN
            NYSTL = NAMIND('YSTL')
            NYSCL = NAMIND('YSCL')
            NYS0L = NAMIND('YS0L')
            NYSVX = NAMIND('YSVX')
        ENDIF
C
        KYSVX = IW(NYSVX)
        KYSTL = IW(NYSTL)
        KYSCL = IW(NYSCL)
        KYS0L = IW(NYS0L)
        IF (KYSVX.EQ.0 .OR. KYSTL.EQ.0 .OR. KYSCL.EQ.0
     &          .OR. KYS0L.EQ.0) THEN
            IER = 1
            GOTO 999
        ENDIF
        IF (IW(KYSVX-2).GT.-2) RETURN
C
C+  Build a mask of all objects already used
C
        CALL VZERO(MASKA,5)
        DO WHILE (KYSVX.NE.0)
            DO IV = 1, LROWS(KYSVX)
              IF (ITABL(KYSVX,IV,JYSVQU).EQ.0) THEN
                DO IWRD = 1, 5
                    MASKA(IWRD) = IOR(MASKA(IWRD),
     &                  ITABL(KYSVX,IV,JYSVTM+IWRD-1))
                ENDDO
              ENDIF
            ENDDO
            KYSVX = IW(KYSVX-1)
            IF (IW(KYSVX-2).GT.-2) KYSVX = 0
        ENDDO

C
C+  Loop over the objects to add (helices, circles, and V0's)
C
        DO IOBJ = 1, 3
          IF (IOBJ .EQ. 1) THEN
            NYOBJ = NYSTL
            JC2P  = JYSTCP
            JQU   = JYSTQF
          ELSE IF (IOBJ .EQ. 2) THEN
            NYOBJ = NYSCL
            JC2P  = JYSCCP
            JQU   = JYSCQF
            ITOBJ = 4
          ELSE
            NYOBJ = NYS0L
            JC2P  = JYS0CP
            JQU   = JYS0QF
            ITOBJ = 5
          ENDIF
          KYOBJ = IW(NYOBJ)
C
C+  Loop over good vertices
C
          KYSVX = IW(NYSVX)
          DO WHILE (KYSVX.NE.0)
            NRYSVX = IW(KYSVX-2)
            DO IV = 1, LROWS(KYSVX)
             IF (ITABL(KYSVX,IV,JYSVQU).EQ.0) THEN
              IYSVX = KROW(KYSVX,IV)
              IROW_NOW = IV
              NR_NOW = NRYSVX
              DO WHILE (IYSVX.NE.0)
                CALL UCOPY(IW(IYSVX+JYSVTM),MASKV,5)
C
C+  Loop over objects of the chosen type
C
                KYSVX_NEW = 0
                IYSVX_NEW = 0
                DO IT = 1, LROWS(KYOBJ)
C
C+  Check object quality flag to protect against too many tracks and
C+  other catastrophes
C
                 IF (ITABL(KYOBJ,IT,JQU).EQ.0) THEN
                  IER = 0
                  IF (IOBJ.EQ.1) THEN
                    ITWRD = (IT-1)/32 + 1
                  ELSE
                    ITWRD = ITOBJ
                  ENDIF
                  ITBIT = MOD(IT-1,32)
                  IF (IAND(MASKA(ITWRD),IBSET(0,ITBIT)).EQ.0) THEN
C  Cut on positive arclength for objects consistent with primary
                   IF (RTABL(KYOBJ,IT,JC2P).GT.YSCTPV .OR.
     &                 YSGETS(IYSVX,IT,IOBJ).GE.0) THEN
                    CALL UCOPY(MASKV,MASKN,5)
                    MASKN(ITWRD) = IOR(MASKN(ITWRD),IBSET(0,ITBIT))
                    NR = 0
                    DO IWRD = 1, 5
                        NR = NR - NUMBIT(MASKN(IWRD))
                    ENDDO
                    CALL YSFTLD(MASKN,IER)
                    IF (IER.EQ.0) THEN
                        CALL YSVFIT(.FALSE.,NR,IER,IGARB)
                        IF (IGARB.GT.0) THEN
                            KYSVX = NLINK('YSVX',NRYSVX)
                            IYSVX = KROW(NLINK('YSVX',NR_NOW),IROW_NOW)
                            KYOBJ = IW(NYOBJ)
                            IF (KYSVX_NEW.GT.0) THEN
                                KYSVX_NEW = NLINK('YSVX',NR_NEW)
                                IYSVX_NEW = KROW(KYSVX_NEW,IROW_NEW)
                            ENDIF
                        ENDIF
                    ENDIF
                    IF (IER.EQ.0) THEN
                      IF (KYSVX_NEW .EQ. 0) THEN
                        KYSVX_NEW = NLINK('YSVX',NR)
                        IYSVX_NEW = KROW(KYSVX_NEW,LROWS(KYSVX_NEW))
                        IROW_NEW = LROWS(KYSVX_NEW)
                        NR_NEW = NR
                      ELSE
                        IF (RTABL(KYSVX_NEW,LROWS(KYSVX_NEW),JYSVC2).LT.
     &                    RW(IYSVX_NEW+JYSVC2))THEN
                           IW(IYSVX_NEW+JYSVQU) =
     &                          IOR(IW(IYSVX_NEW+JYSVQU),512)
                          IYSVX_NEW = KROW(KYSVX_NEW,LROWS(KYSVX_NEW))
                          IROW_NEW = LROWS(KYSVX_NEW)
                          NR_NEW = NR
                        ELSE
                          IW(KYSVX_NEW+LMHROW) = IW(KYSVX_NEW+LMHROW)-1
                        ENDIF
                      ENDIF
                    ELSE IF (IER.LT.0) THEN
                        IER = -1
                        GOTO 999
                    ENDIF
                   ENDIF
                  ENDIF
                 ENDIF  !Track quality check
                ENDDO   !Loop over tracks
C
C+  Now see if this chi^2 is acceptable or not
C
                IF (IYSVX_NEW.GT.0) THEN
                  IF (RW(IYSVX_NEW+JYSVC2)
     &                  -RW(IYSVX+JYSVC2).LE.YSCTCR) THEN
                    IW(IYSVX+JYSVQU) = IOR(IW(IYSVX+JYSVQU),512)
                    DO IWRD = 1,5
                        MASKA(IWRD) = IOR(MASKA(IWRD),
     &                      IW(IYSVX_NEW+JYSVTM+IWRD-1))
                    ENDDO
                    IYSVX = IYSVX_NEW
                    NR_NOW = NR_NEW
                    IROW_NOW = IROW_NEW
                  ELSE
                    IW(IYSVX_NEW+JYSVQU) = IOR(IW(IYSVX_NEW+JYSVQU),
     &                  512)
                    IYSVX = 0
                  ENDIF
                ELSE
                  IYSVX = 0
                ENDIF
              ENDDO  !Loop over successful additions
             ENDIF
            ENDDO  !Loop over vertices
            KYSVX = IW(KYSVX-1)
            IF (IW(KYSVX-2).GT.-2) KYSVX = 0
          ENDDO !Loop over vertex multiplicities
        ENDDO   !Loop over types of objects to add
C
C+  Clean up YSVX
C
        CALL YSVSRT
C
        IER = 0
 999    CONTINUE
        CALL YSFCLR

        RETURN
        END
#endif
