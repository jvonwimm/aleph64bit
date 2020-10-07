        SUBROUTINE YSKLLD(IER)
C-----------------------------------------------------------------------------
C!  Flag pairs of tracks identified as a kink
C
C   Returns IER = 0 if successful
C
C       Author - D. Casper      15/07/96
C
C       Modified                08/08/97  Include only charged tracks in
C                                         the kink veto.
C                               03/02/98  Protect against too many tracks
C
C       Input bank:     YKNK    (if present)
C                       YSFT    Translation from FRFT row to YSTL row
C                       YSTL    Track list
C
C       Output bank:    INDYSKL Mask of tracks identified with a kink
C                               (work bank)
C
C------------------------------------------------------------------------------
#ifndef DOC
C
#include "yknkjj.h"
#include "ysftjj.h"
#include "ystljj.h"
#include "ysvwrk.h"
#include "bcs.h"
C
        DATA NYKNK/0/
        SAVE NYKNK
C
#include "bmacro.h"
C
        IF (NYKNK.EQ.0) THEN
            NYKNK = NAMIND('YKNK')
            NYSFT = NAMIND('YSFT')
            NYSTL = NAMIND('YSTL')
        ENDIF
        KYKNK = IW(NYKNK)
        IF (KYKNK.EQ.0) THEN
            IER = 0
            RETURN
        ENDIF
        KYSFT = IW(NYSFT)
        KYSTL = IW(NYSTL)
        IF (KYSFT.LE.0 .OR. KYSTL.LE.0) THEN
            IER = 1
            GOTO 999
        ENDIF
        IER = 2
C
C+ Create the work bank
C
        LYSKL = LMHLEN + 5*LROWS(KYKNK)
        CALL WBANK(IW,INDYSKL,LYSKL,*999)
        IW(INDYSKL-3) = INTCHA('YSKL')
        IW(INDYSKL+LMHCOL) = 5
        IW(INDYSKL+LMHROW) = 0
C
C+ Loop over identified kinks
C
        DO IK = 1, LROWS(KYKNK)
            IF_IN = ITABL(KYKNK,IK,JYKNIT)
            IF (IF_IN .LE. LROWS(KYSFT) .AND. IF_IN.GT.0) THEN
                IY_IN = ITABL(KYSFT,IF_IN,JYSFTN)
            ELSE
                IER = 3
                GOTO 999
            ENDIF
            IF_OUT = ITABL(KYKNK,IK,JYKNOT)
            IF (IF_OUT .LE. LROWS(KYSFT) .AND. IF_OUT.GT.0) THEN
                IY_OUT = ITABL(KYSFT,IF_OUT,JYSFTN)
            ELSE
                IER = 4
                GOTO 999
            ENDIF
            IYSKL = KNEXT(INDYSKL)
            IF (IY_IN.GT.0 .AND. ITABL(KYSTL,IY_IN,JYSTQF).EQ.0) THEN
                ITWRD = (IY_IN-1)/32 + 1
                ITBIT = MOD(IY_IN-1,32)
                IW(IYSKL+ITWRD) = IBSET(IW(IYSKL+ITWRD),ITBIT)
            ENDIF
            IF (IY_OUT.GT.0 .AND. ITABL(KYSTL,IY_OUT,JYSTQF).EQ.0) THEN
                ITWRD = (IY_OUT-1)/32 + 1
                ITBIT = MOD(IY_OUT-1,32)
                IW(IYSKL+ITWRD) = IBSET(IW(IYSKL+ITWRD),ITBIT)
            ENDIF
            IW(INDYSKL+LMHROW) = IW(INDYSKL+LMHROW) + 1
        ENDDO
        IER = 0
 999    CONTINUE
        RETURN
        END
#endif
