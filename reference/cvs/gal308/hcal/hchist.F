*DK hchist
      SUBROUTINE HCHIST (NAME,MR,NAMI,IELE,JELE)
C ----------------------------------------------------------
C - F.Ranjard - 860613
C! - Fill the history bank NAME,(NR=1,MR) with namindex NAMI
C   for that the bank HCSE is sorted in increasing order of
C   element# IELE and then for a each value of element# IELE
C   in increasing order of element# JELE.
C - If the element# IELE is the same for 2 concecutive calls to
C   HCHIST the entry point HCHIS1 can be used instead to save
C   the 1st sort.
C -       NAME, NR=MR-2   # of element JELE belonging to 1 element IELE
C               NR=MR-1   pointer to 1st element JELE belonging to 1
C                         element IELE in the list NR=MR
C               NR=MR     list of elements JELE belonging to each elemen
C                         IELE
C - If MR=4 the correlation is between tracks and hits. In this case
C         NAME, NR=1      list of track#s which have contributed to hits
C - Called by    HCASIG                                 from this .HLB
C - Calls        SORTIQ                                 from GENLIB
C                ALBOS                                  from this .HLB
C ----------------------------------------------------------
#ifndef DOC
      SAVE
#include "jobcom.h"
#include "iocom.h"
#include "bcs.h"
#include "hccoun.h"
#include "hcnamc.h"
#include "hcsejj.h"
      DIMENSION IPELE(1000), KDX(4)
      CHARACTER NAME*4  , TITL(4)*7
C
      DATA TITL /'tracks ','hits   ','tubes  ','storeys'/
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      KHCSE = JDHCSE + LMHLEN
      NHCSE = LROWS (JDHCSE)
      CALL SORTIQ (IW(KHCSE+1),LHCSEA,NHCSE,IELE)
C
      NELE = 0
      LELE = 0
      DO 10 I = 1,NHCSE
         IF (IW(KHCSE+IELE) .EQ. LELE) GOTO 10
         NELE = NELE + 1
         IPELE(NELE) = I
         LELE = IW(KHCSE+IELE)
   10 KHCSE = KHCSE + LHCSEA
      IPELE(NELE+1) = NHCSE + 1
C
      IF (FHCDB1) THEN
         IF (NLINK('DBHC',0).NE.0) THEN
            KHCSE = JDHCSE + LMHLEN
            WRITE (LOUTIO,*)' +++HCHIST+++ trk# hit# tub# sto#'
            WRITE (LOUTIO,500)((IW(K+M),M=12,15), K=KHCSE,KHCSE+NHCSE
     +      *LHCSEA-1,LHCSEA)
         ENDIF
      ENDIF
C
C ---------------------------------------------------------------------
C
      ENTRY HCHIS1 (NAME,MR,NAMI,IELE,JELE)
C
C - Book bank NAME , NR=1,MR
      DO 20 J=1,MR-1
         CALL ALBOS (NAME,J,NELE+LMHLEN,KIND,IGARB)
         IW(KIND+1) = 1
         IW(KIND+2) = NELE
   20 CONTINUE
      CALL ALBOS (NAME,MR,NHCSE+LMHLEN,KIND,IGARB)
      IW(KIND+1) = 1
      CALL BLIST (IW,'E+',NAME)
C
C - Get indices
      KDX(1) = IW(NAMI) + LMHLEN
      DO 30 J=2,MR
         KDX(J) = IW(KDX(J-1)-LMHLEN-1) + LMHLEN
   30 CONTINUE
C
C - FIll banks
      K0MR = KDX(MR)
      DO 50 N=1,NELE
         NHIT = IPELE(N+1) - IPELE(N)
         IW(KDX(MR-1)+N) = KDX(MR)-K0MR
         KHCSE = KROW(JDHCSE,IPELE(N))
         IF (MR.EQ.4) IW(KDX(1)+N) = IW(KHCSE+IELE)
C
         CALL SORTIQ (IW(KHCSE+1),LHCSEA,NHIT,JELE)
C
         LJELE = 0
         NJELE = 0
         DO 40 L=1,NHIT
            IF (IW(KHCSE+JELE) .EQ. LJELE) GOTO 40
            LJELE = IW(KHCSE+JELE)
            NJELE = NJELE + 1
            IW (KDX(MR)+NJELE) = LJELE
   40    KHCSE = KHCSE + LHCSEA
         KDX(MR) = KDX(MR) + NJELE
         IW(KDX(MR-2)+N) = NJELE
   50 CONTINUE
C - fill # of rows of bank NAME , NR=MR
      IW(KIND+LMHROW) = KDX(MR)-K0MR
      CALL AUBPRS (NAME)
C
C - Debug
      IF (FHCDB1) THEN
         KDX(MR) = K0MR
         WRITE (LOUTIO,510) NAME,MR,TITL(IELE-11),TITL(JELE-11)
         IF (MR.EQ.4) THEN
            WRITE (LOUTIO,520) TITL(JELE-11)
         ELSE
            WRITE (LOUTIO,530) TITL(IELE-11)
         ENDIF
         DO 60 I=1,NELE
            N=IW(KDX(MR-2)+I)
            L=IW(KDX(MR-1)+I)
            IF (MR.EQ.4) THEN
               WRITE (LOUTIO,540) I,IW(KDX(1)+I),N,L,(IW(KDX(MR)+L+M),
     +         M =1,N)
            ELSE
               WRITE (LOUTIO,550) I,N,L,(IW(KDX(MR)+L+M),M=1,N)
            ENDIF
   60    CONTINUE
      ENDIF
C
C - END
   70 CONTINUE
      RETURN
  500 FORMAT (1X,4I5)
  510 FORMAT (/1X,'+++HCHIST+++ Bank ',A4,' NR=1-',I1,
     +' giving correlations between ',A7,' and ',A7/)
  520 FORMAT (5X,'trk#  nhit iadd  list of address of fired ',A7)
  530 FORMAT (5X,'nhit iadd  list of tracks which fired this ',A7)
  540 FORMAT (1X,I4,3I5,2X,20I4/(22X,20I4))
  550 FORMAT(1X,I4,2I5,2X,20I4/(17X,20I4))
      END
#endif
