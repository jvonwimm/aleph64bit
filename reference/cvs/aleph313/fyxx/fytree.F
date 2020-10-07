      SUBROUTINE FYTREE(IBFUL)
C ------------------------------------------------------------
CKEY FYXX  / INTERNAL
C - J. Hilgart 030987
C! Build arrays which allow conversion from KINE and VERT banks
C  to a true tree structure FVER,FKIN.
C  This means  all secondary tracks from a vertex asspecified by
C  FVER are all listed contiguously.
C
C - Input banks : KINE, VERT
C - Output bank : JDKNFO, JDKOFN, JDVOUT
C - Output
C     : IBFUL = -1 if not enough space to book a bank
C
C Called from  FYKINE                      from ALEPHLIB.HLB
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fyrela.h"
#include "bmacro.h"
#include "kmacro.h"
C ====================================================================
C
C General initialization
      IBFUL = 0
C
      CALL WBANK(IW,JDKOFN,LMHLEN+LTKNUM,*980)
      IW(JDKOFN+LMHCOL) = 1
      IW(JDKOFN+LMHROW) = LTKNUM
      CALL WBANK(IW,JDKNFO,LMHLEN+IW(INDKIN),*980)
      IW(JDKNFO+LMHCOL) = 1
      IW(JDKNFO+LMHROW) = IW(INDKIN)
      CALL WBANK(IW,JDVOUT,LMHLEN+2*LVXNUM,*980)
      IW(JDVOUT+LMHCOL) = 2
      IW(JDVOUT+LMHROW) = LVXNUM
      ICO = 0
C
#if defined(FYDEB)
      WRITE(6,'(/1X,''JDKEKS '',20I5/(8X,20I5))')
     &     (IW(JDKEKS+M),M=1,IW(JDKEKS))
      WRITE(6,'(/1X,''JDVNFO '',20I5/(8X,20I5))')
     *     (IW(JDVNFO+M),M=1,IW(JDVNFO))
#endif
C Loop over VERT banks
      DO 1 IVO = 1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 1
C
C No. of outgoing tracks
         JVER = IW(INDVER+IVO)
         NOUT = NOFVK(JVER)
         ICOV = 0
         ISEC = 0
 2       CONTINUE
         IF (ICOV .LT. NOUT) THEN
            DO 3 IOLD = ISEC+1, IW(INDKIN)
C
C KINE bank index
               KK = IW(INDKIN+IOLD)
C - origin vertex # of a track known by its BOS index JVK
               IVOR = INPVRT(KK)
               IF ( IVOR .EQ. IVO) GOTO 4
 3          CONTINUE
 4          CONTINUE
            ICOV = ICOV + 1
            ISEC = IOLD
            INW = ITABL(JDKEKS,IOLD,1)
            IF (INW .EQ. 0)  GOTO 2
            ICO = ICO + 1
C
C Build relationships old |--> new and vica versa
            IW(KROW(JDKNFO,IOLD)+1) = ICO
            IW(KROW(JDKOFN,ICO)+1) = IOLD
            GOTO 2
         ENDIF
 1    CONTINUE
C
C - reset JDKNKO bank length
      IF (ICO.LT.LTKNUM) THEN
        LTKNUM = ICO
        IW(JDKOFN+LMHROW) = ICO
        CALL WBANK(IW,JDKOFN,ICO+LMHLEN,*980)
      ENDIF
C
#if defined(FYDEB)
      WRITE(6,'(/1X,''JDKNFO '',20I5/(8X,20I5))')
     &     (IW(JDKNFO+M),M=1,IW(JDKNFO))
      WRITE(6,'(/1X,''JDKOFN '',20I5/(8X,20I5))')
     &     (IW(JDKOFN+M),M=1,IW(JDKOFN))
#endif
C
C - Get #s of outgoing tracks for new vertices
C
      DO 10 IVO = 1,IW(INDVER)
         IVN = ITABL (JDVNFO,IVO,1)
         IF (IVN .EQ. 0) GOTO 10
         JVER = IW(INDVER+IVO)
         NOUT = NOFVK(JVER)
         IF (NOUT .EQ. 0) GOTO 10
         NEWT = 0
         IFOUT = LTKNUM+1
         DO 11 N = 1,NOUT
            ITO = IW(KLISVK(JVER)+N)
            ITN = ITABL(JDKNFO,ITO,1)
            IF (ITN .EQ. 0) GOTO 11
            NEWT = NEWT+1
            IFOUT = MIN (IFOUT,ITN)
 11      CONTINUE
         IF (NEWT .EQ. 0) GOTO 10
         IW(KROW(JDVOUT,IVN)+1) = IFOUT-1
         IW(KROW(JDVOUT,IVN)+2) = NEWT
 10   CONTINUE
#if defined(FYDEB)
      WRITE(6,'(/1X,''JDVOUT '',20I5/(8X,20I5))')
     &     (IW(JDVOUT+M),M=1,IW(JDVOUT))
#endif
C
      GOTO 990
C
C sick return: not enough space to book bank...
 980  IBFUL = - 1
 990  CALL WDROP(IW,JDKEKS)
      END
#endif
