      SUBROUTINE X1MIXI
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 86/08/08  FOR GALEPH 13.0
C. - Modified : A. Putzer  - 87/04/04  FOR GALEPH 17.0
C. - Modified : A. Putzer  - 87/10/26  FOR GALEPH 19.3
C. - Modified : A. Putzer  - 87/12/18  FOR GALEPH 19.6
C. - Modified : C. Geweniger - 88/10/11  for GALEPH 20.1
C. - Modified : E. Blucher - 89/15/2 for ALEPHLIB
C. - Modified : C. Geweniger - 89/09/00  for ALEPHLIB 9.9
C. - Modified : B. Bloch-Devaux - 92/12/10 for Sical in 92
C.
C! - Mix level1 trigger signals
C.
C? - The analog trigger signals are mixed to form the trigger sources
C?   used for the level1 trigger.
C?
C? - The total energy values are calculated.
C.
C. - Banks    : HTTR, HWTR, ETTR, EWTR, LTTR, ITTR are read
C.
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "x1tcom.h"
#include "x1namc.h"
C.
C ----------------------------------------------------------------------
C.
      LOGICAL BTEST
      PARAMETER (NSEGE=12,NTHTE=2,NPHIE=6,NPHIB=12)
      PARAMETER (NTHBN=12,NTHET=8)
      DIMENSION NBETH(NTHET),NENTH(NTHET),NSHWT(NTHET)
      DIMENSION NSHTT(NTHBN)
      DIMENSION ITLIM(16)
C.
C ----------------------------------------------------------------------
C.
      DATA IFI /0/
      DATA NBETH/ 1, 3, 4, 6, 7, 8,10,11/
      DATA NENTH/ 2, 3, 5, 6, 7, 9,10,12/
      DATA NSHTT/  1,  2,  3,  4, 49, 50, 97, 98,145,146,147,148/
      DATA NSHWT/ 0, 0, 6, 6, 6, 6,30,30/
      DATA ITLIM/1,6,7,12,13,24,25,36,37,48,49,60,61,66,67,72/
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NHTTR = NAMIND ('HTTR')
         NHWTR = NAMIND ('HWTR')
         NETTR = NAMIND ('ETTR')
         NEWTR = NAMIND ('EWTR')
         NLTTR = NAMIND ('LTTR')
         NITTR = NAMIND ('ITTR')
      ENDIF
C.
C.
C. - Sum the HCAL tower signals for each trigger segment
C.
C.
      DO 96 I = 1,NSEGM
        IETSUM(I) = 0
        IEWSUM(I) = 0
        IHTSUM(I) = 0
        IHWSUM(I) = 0
 96   CONTINUE
      DO 97 I = 1,NTOEV
        IECTTE(I) = 0
        IECWTE(I) = 0
        IECWTE(NTOEV+I) = 0
        IHCTTE(I) = 0
 97   CONTINUE
      DO 98 I = 1,NSEGL
 98   ILTSUM(I) = 0
      DO 99 I = 1,NSEGI
 99   IITSUM(I) = 0
C
      KHTTR = IW(NHTTR)
      IF (KHTTR.EQ.0) GO TO 199
      KHTTR = KHTTR + LMHLEN
      JSEGM = 0
C  - Loop over theta
      DO 101 JTH = 1,NTHET
        JBEG = NBETH(JTH)
        JEND = NENTH(JTH)
        IF (JTH.LE.NTHTE.OR.JTH.GT.(NTHET-NTHTE)) THEN
C  - Endcaps
          NPHI = NPHIE
        ELSE
C  - Barrel
          NPHI = NPHIB
        ENDIF
        DO 111 ITH = JBEG,JEND
          JND = KHTTR + NSHTT(ITH)
          IF (ITH.LT.5.OR.ITH.GT.8) THEN
C  - Encaps
            KPHI = 12
            KOFF =  4
          ELSE
C  - Barrel
            KPHI = 24
            KOFF =  2
          ENDIF
          MPHI = KPHI/NPHI
C  - Loop over phi
          DO 121 JPH = 1,NPHI
            ISUM = 0
            DO 131 IPH = 1,MPHI
              ISUM = ISUM + IW(JND)
              JND = JND + KOFF
 131        CONTINUE
            ISEGM = JSEGM + JPH
            IHTSUM(ISEGM) = IHTSUM(ISEGM) + ISUM
 121      CONTINUE
 111    CONTINUE
        JSEGM = JSEGM + NPHI
 101  CONTINUE
 199  CONTINUE
C.
C.
C. - Map the HCAL wire trigger signals onto the trigger segments
C.
C.
      KHWTR = IW(NHWTR)
      IF (KHWTR.EQ.0) GO TO 299
      KHWTR = KHWTR + LMHLEN
      JSEGM = 0
C  - Loop over theta
      DO 201 JTH = 1,NTHET
        JND = KHWTR + NSHWT(JTH)
        IF (JTH.LE.NTHTE.OR.JTH.GT.(NTHET-NTHTE)) THEN
C  - Endcaps
          NPHI = NPHIE
        ELSE
C  - Barrel
          NPHI = NPHIB
        ENDIF
C  - Loop over phi
        DO 211 JPH = 1,NPHI
          ISEGM = JSEGM + JPH
C  -   Endcap
          IF (JTH.LT.3.OR.JTH.GT.6) THEN
            IHWSUM(ISEGM) = IW(JND+JPH)
C  -   Overlap
          ELSE IF (JTH.EQ.3) THEN
            JJND = KHWTR + (JPH+1)/2
            IHWSUM(ISEGM) = IW(JND+2*JPH-1) + IW(JND+2*JPH) + IW(JJND)
C  -   Overlap
          ELSE IF (JTH.EQ.6) THEN
            JJND = KHWTR + (JPH+1)/2 + 30
            IHWSUM(ISEGM) = IW(JND+2*JPH-1) + IW(JND+2*JPH) + IW(JJND)
C  -   Barrel
          ELSE
            IHWSUM(ISEGM) = IW(JND+2*JPH-1) + IW(JND+2*JPH)
          ENDIF
 211    CONTINUE
        JSEGM = JSEGM + NPHI
 201  CONTINUE
 299  CONTINUE
C.
c. - Sum the ECAL tower signals for each trigger segment
C.
C.
      KETTR = IW(NETTR)
      IF (KETTR.EQ.0) GO TO 399
      KETTR = KETTR + LMHLEN
      IND = KETTR + 1
      JSEGM = 0
C  - Loop over theta
      DO 301 JTH = 1,NTHET
        JBEG = NBETH(JTH)
        JEND = NENTH(JTH)
        IF (JTH.LT.3.OR.JTH.GT.6) THEN
C  - Endcap
          NPHI = 6
          MPHI = 4
          NOFF = 3
        ELSE
C  -Barrel and Overlap
          NPHI = 12
          MPHI =  2
          NOFF =  0
        ENDIF
        DO 311 ITH = JBEG,JEND
C  - Loop over phi
          DO 321 JPH = 1,NPHI
            ISUM = 0
            DO 331 IPH = 1,MPHI
              JND = IND+NOFF
      IF (NOFF.NE.0.AND.IPH*JPH.EQ.24) JND = JND-72
C  - Sum the three stacks
              ISUM = ISUM + IW(JND) + IW(JND+1) + IW(JND+2)
              IND = IND + 3
 331        CONTINUE
            ISEGM = JSEGM + JPH
            IETSUM(ISEGM) = IETSUM(ISEGM) + ISUM/1000
 321      CONTINUE
 311    CONTINUE
        JSEGM = JSEGM + NPHI
 301  CONTINUE
 399  CONTINUE
C.
C.
C. - Get the ECAL wire trigger signals into local arrays;
C. - the numbering of trigger wire segments in the endcaps is
C. - shifted by 1 compared to ECAL module numbering.
C.
C.
      KEWTR = IW(NEWTR)
      IF (KEWTR.EQ.0) GO TO 499
      KEWTR = KEWTR + LMHLEN
      DO 401 I = 1,NSEGM
        II = I
        IF (I.LE.24) II = MOD(I+1,24)+1
        IF (I.GT.48) II = MOD(I-47,24)+49
        IEWSUM(II) = IW(KEWTR+I)/1000
 401  CONTINUE
 499  CONTINUE
C.
C. - Form LCAL supersegment signals
C.
C.
      KLTTR = IW(NLTTR)
      IF (KLTTR.EQ.0) GO TO 599
      KLTTR = KLTTR + LMHLEN
      DO 501 I = 1,NSEGL
C - Sum the three stacks
        ILTSM = IW(KLTTR+1) + IW(KLTTR+2) + IW(KLTTR+3)
        KLTTR = KLTTR+3
C - Form supersegments of adjacent segments for each ring
        ILTSUM(I) = ILTSUM(I) + ILTSM
        IF (MOD(I,12).EQ.1) THEN
          ILTSUM(I+11) = ILTSUM(I+11) + ILTSM
        ELSE
          ILTSUM(I-1) = ILTSUM(I-1) + ILTSM
        ENDIF
  501 CONTINUE
  599 CONTINUE
C.
C  - Get the ITC trigger bits
C  - into local arrays
C.
C.
      KITTR = IW(NITTR)
      IF (KITTR.EQ.0) GO TO 699
      KITTR = KITTR + LMHLEN
      DO 601 I = 1,NSEGI
        IITSUM(I) = IW(KITTR+I)
  601 CONTINUE
  699 CONTINUE
C.
C.
C.
C  - Calculate the total energy (Endcap A, Endcap B, Barrel, Total)
C.
C.
C -  HC tower trigger
C.
C.
C.
      DO 701 I = 1,NSEGM
        IF (I.LE.NSEGE) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.(NSEGM-NSEGE)) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IHCTTE(KST) = IHCTTE(KST) + IHTSUM(I)
  701 CONTINUE
      IHCTTE(4) = IHCTTE(1) + IHCTTE(2) + IHCTTE(3)
C
C -  EC Tower Trigger
C
      DO 801 I = 1,NSEGM
        IF (I.LE.NSEGE) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.(NSEGM-NSEGE)) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IECTTE(KST) = IECTTE(KST) + IETSUM(I)
  801 CONTINUE
      IECTTE(4) = IECTTE(1) + IECTTE(2) + IECTTE(3)
C.
C.
C -  EC wire trigger
C.
C.
      DO 1001 I = 1,NSEGM,2
        IF (I.LE.24) THEN
C -  ENDCAP A
          KST = 1
        ELSE IF (I.GT.48) THEN
C -  ENDCAP B
          KST = 2
        ELSE
C -  BARREL
          KST = 3
        ENDIF
        IECWTE(KST) = IECWTE(KST) + IEWSUM(I)
        IECWTE(KST+NTOEV) = IECWTE(KST+NTOEV) + IEWSUM(I+1)
 1001 CONTINUE
      IECWTE(4) = IECWTE(1) + IECWTE(2) + IECWTE(3)
      IECWTE(4+NTOEV)=IECWTE(1+NTOEV)+IECWTE(2+NTOEV)+IECWTE(3+NTOEV)
C.
C -  SICAL  energy sums
C.
      CALL SIXAMK
C
      RETURN
      END
#endif
