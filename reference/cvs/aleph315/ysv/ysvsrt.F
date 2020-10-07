        SUBROUTINE YSVSRT
C--------------------------------------------------------------------------
C!   Clean up YSVX banks, sorting by chi^2, flagging overlaps, and trim
C
C       Author:  D.Casper  18/7/96
C
C       Modifies:   YSVX/NR(<0)     Vertices which have a track included
C                                   in a higher multiplicity vertex have
C                                   a quality flag set.  Each NR is
C                                   sorted first by chi^2, and then
C                                   by quality flag (preserving chi^2
C                                   ordering for a given quality flag).
C                                   All good vertices (no overlaps, quality
C                                   flag=0) end up at the beginning of the
C                                   bank, in ascending order of chi^2.
C                                   Vertices with impossible chi^2 are
C                                   removed.
C
C--------------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "ysvxjj.h"
#include "ysvcut.h"
        DIMENSION MASKA(5)
        DATA NYSVX/0/
        SAVE NYSVX
C
#include "bmacro.h"
C
        IF (NYSVX.EQ.0) THEN
            NYSVX = NAMIND('YSVX')
        ENDIF
C
C+  Locate YSVX
C
        KYSVX = IW(NYSVX)
        IF (KYSVX.LE.0) GOTO 999
C
C+  Clear mask of tracks taken by any good vertex
C
        CALL VZERO(MASKA,5)
        DO WHILE (KYSVX.NE.0)
C
C+  Sort YSVX by chi^2
C
            CALL SORTRQ(RW(KYSVX+LMHLEN+1),
     &          LCOLS(KYSVX),LROWS(KYSVX),JYSVC2)
C
C+  Sort by quality flag, keeping chi^2 order
C
            CALL SORTI(RW(KYSVX+LMHLEN+1),
     &          LCOLS(KYSVX),LROWS(KYSVX),JYSVQU)
C
C+  Remove rows with bad chi^2 and flag rows which have a track taken
C
            I = 1
            NOVER = 0
            DO WHILE ((I.LE.LROWS(KYSVX)) .AND.
     &         (ITABL(KYSVX,I,JYSVQU).EQ.0) .AND.
     &         (RTABL(KYSVX,I,JYSVC2)/(2*ABS(IW(KYSVX-2))-3)
     &              .LE.YSCTCF))
                IWRD = 1
                DO WHILE ((IWRD.LE.5) .AND.
     &              (IAND(ITABL(KYSVX,I,JYSVTM+IWRD-1),
     &                  MASKA(IWRD)).EQ.0))
                    IWRD = IWRD + 1
                ENDDO
                IF (IWRD.EQ.6) THEN
                    DO IWRD = 1, 5
                        MASKA(IWRD) = IOR(MASKA(IWRD),
     &                      ITABL(KYSVX,I,JYSVTM+IWRD-1))
                    ENDDO
                ELSE
                    IW(KROW(KYSVX,I)+JYSVQU) = 
     &                  IOR(IW(KROW(KYSVX,I)+JYSVQU),1)
                    NOVER = NOVER + 1
                ENDIF
                I = I+1
            ENDDO
            IW(KYSVX+LMHROW) = I-1
            CALL SORTI(RW(KYSVX+LMHLEN+1),
     &          LCOLS(KYSVX),LROWS(KYSVX),JYSVQU)
            IW(KYSVX+LMHROW) = LROWS(KYSVX) - NOVER
            KYSVX = IW(KYSVX-1)
            IF (IW(KYSVX-2).GT.-2) KYSVX = 0
        ENDDO
 999    CONTINUE
        CALL AUBPRS('YSVX')

        RETURN
        END
#endif
