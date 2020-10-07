      SUBROUTINE X1MIXN
C ----------------------------------------------------------------------
C
C    New version of X1MIXI & X1OUTP combined
C    =======================================
C.
C. - Author   : A. Putzer  - 95/01/10  FOR ALEPHLIB 204
C.
C! - Mix Level1 trigger signals & Fill bank X1IP
C.
C? - The analog trigger signals are mixed to form the trigger sources
C?   used for the Level1 trigger.
C?
C? - The total energy values (Ec wires; EC towers) are calculated
C.
C. - Banks    : HWTR, ETTR, EWTR, LWTR, ITTR are read
C.   Bank     : X1IP                         is filled
C.
C -   Called by          X1TRIG              this .HLB
C -----------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "bcs.h"
#include "x1tsto.h"
#include "x1namc.h"
C.
C ----------------------------------------------------------------------
C.
      PARAMETER (I2P16=65536,ISMAX=I2P16-1)
      PARAMETER (NSEGE=12,NTHTE=2,NPHIE=6,NPHIB=12,NTHET=8)
      DIMENSION NBETH(NTHET),NENTH(NTHET),NSHWT(NTHET)
C.
C ----------------------------------------------------------------------
C.
      DATA IFI /0/
      DATA NBETH/ 1, 3, 4, 6, 7, 8,10,11/
      DATA NENTH/ 2, 3, 5, 6, 7, 9,10,12/
      DATA NSHWT/ 0, 0, 6, 6, 6, 6,30,30/
C.
#include "bmacro.h"
C ----------------------------------------------------------------------
C.
      IF (IFI .EQ. 0) THEN
         IFI = 1
         NHWTR = NAMIND ('HWTR')
         NETTR = NAMIND ('ETTR')
         NEWTR = NAMIND ('EWTR')
         NLWTR = NAMIND ('LWTR')
         NITTR = NAMIND ('ITTR')
      ENDIF
C.
      DO I = 1, NTOEV
        IECTTE(I) = 0
      ENDDO
      DO I = 1, NTEEW
        IECWTE(I) = 0
      ENDDO
      DO I = 1, NSEGM
        IHWSUM(I) = 0
        IETSUM(I) = 0
        IEWSUM(I) = 0
      ENDDO
      DO I = 1, NSEGL
        ILWSUM(I) = 0
      ENDDO
      DO I = 1, NSEGI
        IITSUM(I) = 0
      ENDDO
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
C. - (Presently not used; but might be activated for LEP200)
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
C. - Get LCAL wire signals into local arrays
C.
C.
      KLWTR = IW(NLWTR)
      IF (KLWTR.EQ.0) GO TO 599
      KLWTR = KLWTR + LMHLEN
      DO 501 I = 1,NSEGL
        ILWSUM(I) = IW(KLWTR+I)
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
C
C -  EC Tower energy
C. - (Presently not used; but might be activated for LEP200)
C
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
        IECTTE(KST) = IECTTE(KST) + IETSUM(I)
  701 CONTINUE
      IECTTE(4) = IECTTE(1) + IECTTE(2) + IECTTE(3)
C.
C.
C -  EC wire energy
C.
C.
      DO  801 I = 1,NSEGM,2
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
  801 CONTINUE
      IECWTE(4) = IECWTE(1) + IECWTE(2) + IECWTE(3)
      IECWTE(4+NTOEV)=IECWTE(1+NTOEV)+IECWTE(2+NTOEV)+IECWTE(3+NTOEV)
C.
C -  SICAL  energy sums
C.
C     CALL SIXAMK (Now called from X1TRIG directly)
C.
C -  Fill bank X1IP (Information needed to redo the trigger logic)
C.
C.
      KX1IP=IW(NAX1IP)+LMHLEN
C.
C.
C -  HC wires (packed 2/word)
C
C.
      KX1IP=KX1IP+1
      IW(KX1IP)=INTCHA('HCW ')
      DO 221 I = 1,NSEGM,2
        KX1IP = KX1IP + 1
        IW(KX1IP) = MIN (IHWSUM(I),ISMAX)*I2P16 +
     +              MIN (IHWSUM(I+1),ISMAX)
  221 CONTINUE
C
C.
C -  EC wires (packed 2/word)
C.
C.
      KX1IP=KX1IP+1
      IW(KX1IP)=INTCHA('ECW ')
      DO 241 I = 1,NSEGM,2
        KX1IP = KX1IP + 1
        IW(KX1IP) = MIN (IEWSUM(I)+200,ISMAX)*I2P16 +
     +              MIN( IEWSUM(I+1)+200,ISMAX)
  241 CONTINUE
C.
C.
C -  LC Wires (8 words)
C -  & Ec Wires total energy (8 words)
C -  & ITC bits (4 words)
C.
C.
      KX1IP=KX1IP+1
      IW(KX1IP)=INTCHA('MISC')
      DO 261 I = 1,NSEGL
        KX1IP=KX1IP+1
        IW(KX1IP) = ILWSUM(I)
  261 CONTINUE
      DO 262 I = 1,NTEEW
        KX1IP=KX1IP+1
        IW(KX1IP) = IECWTE(I)
  262 CONTINUE
      DO 263 I = 1,NSEGI
        KX1IP=KX1IP+1
        IW(KX1IP) = IITSUM(I)
  263 CONTINUE
C
  999 CONTINUE
      RETURN
      END
#endif
