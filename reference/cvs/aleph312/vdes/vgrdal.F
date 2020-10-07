      SUBROUTINE VGRDAL (LUN,IRUN,IFLAG)
C ----------------------------------------------------------------------
CKEY VDETDES GEOMETRY
C!  Read alignment banks and fill the VGPAAL common
C - Francesco Forti, 18 August 1990
C - Modified to use new geometry package, S. Wasserbaech, January 1995
C - Modified to use VNGBVNLC if they exist, F.Ranjard, 4 July 1997
C - Modified to use laser data also, G. Sguazzoni, September 1998
C
C  Check existence of the alignment banks in the BOS common and their
C  validity range.
C  If they do not exist yet or are no longer valid try to load them
C  from the data base file. IFLAG is set to 0 if an error occurs.
C  The routine uses the alignment banks to compute the complete
C  transformation that goes from the wafer coordinate to the ALEPH
C  main coordinates and vice versa and stores it in the common VGPAAL.
C
C  Called by :    VDET initialisation routine
C  Calls     :    FUNCTION ALGTDB               from ALEPHLIB
C                 VGEXRO, VGCMTR, VGINTR        from ALEPHLIB
C                 VGRDLS                        from ALEPHLIB
C
C - Input:
C   LUN   / I  Logical unit number of DAF file
C   IRUN  / I  Run number
C
C - Output:
C   IFLAG / I  = 1 if routine ends successfully;
C              = 0 if an error occurred
C ----------------------------------------------------------------------
#ifndef DOC
C      IMPLICIT NONE
#include "vglobl.h"
#include "vagbjj.h"
#include "valcjj.h"
#include "vgpaal.h"
#include "bcs.h"
      INTEGER LUN, IRUN, IFLAG, ITP
      INTEGER I, IRO, ILAY, IWFF, IFAC, IVIEW, IND
      INTEGER KLC, KGB, IFAIL, NTOAL, NEW, IND1, IND2, LEN, LE
      INTEGER  ALGTDB,GTSTUP
      EXTERNAL ALGTDB,GTSTUP
      EXTERNAL NAMIND, NLINK, NDANR, MDARD, NSWAP
      INTEGER  NAMIND, NLINK, NDANR, MDARD, NSWAP
      INTEGER  VNRFAC
      EXTERNAL VNRFAC
      REAL VLOC(LVALCA)
#include "bmacrod.h"
C
      CHARACTER*8 LIST1, LIST2
      CHARACTER*4 NAM1 , NAM2
      DATA LIST1 /'VAGBVALC'/
      DATA LIST2 /'VNGBVNLC'/
      DATA NTOAL /0/
C
C     Transformation structures to be used for intermediate results:
C
      REAL TELC(LVTEXP), TEGB(LVTEXP), TENO(LVTEXP), TE(LVTEXP)
C
#include "bmacro.h"
C ----------------------------------------------------------------------
C
      IF (NTOAL.EQ.0) NTOAL = NAMIND('TOAL')
      IFLAG = 1
C
C  Get the setup code
      IF (IRUN.LE.2000) THEN
         ITP = GTSTUP ('VD',IRUN)
      ELSE
         ITP = IRUN
      ENDIF
C
C  if TOAL data card is present get OLD alignment from LIST1 banks
      IF (IW(NTOAL) .GT. 0) GOTO 10


C! Get banks in LIST2 from DB depending on run and setup code,
C  If a bank VA.. or VN.. is given on data cards (NR=-1) keep it


      LE = LEN(LIST2)
      DO I=1,LE,4
        NAM2 = LIST2(I:I+3)
        NAM1 = LIST1(I:I+3)
        IND2  = NLINK(NAM2,-1)
        IND1  = NLINK(NAM1,-1)
        IF (IND1.EQ.0 .AND. IND2.EQ.0) THEN
           CALL BDROP (IW,NAM2)
           CALL BDROP (IW,NAM1)
           NEW = NDANR (LUN,NAM2,'LE',ITP)
           IND2 = MDARD (IW,LUN,NAM2,NEW)
           IF (IND2.EQ.0) GOTO 10
           IND2 = NSWAP(NAM2,NEW,NAM1,NEW)
#if defined(ARDEB)
           IF (IW(6).GT.0) WRITE(6,*) ' *** VGRDAL*** swap ',
     &                                NAM2,NAM1,NEW
#endif
        ELSEIF (IND2.NE.0) THEN
           IND2 = NSWAP(NAM2,-1,NAM1,-1)
#if defined(ARDEB)
           IF (IW(6).GT.0) WRITE(6,*) ' *** VGRDAL*** swap ',
     &                                NAM2,NAM1,' -1'
#endif
        ENDIF
      ENDDO
      GOTO 20
C
C  If LIST2 banks don't exist or TOAL data card is there
C  get banks in LIST1
 10   CONTINUE
      IND = ALGTDB(LUN,LIST1,-ITP)
C
C     Return if banks have not been correctly accessed:
C
      IF (IND .EQ. 0) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Get the indices to banks just read in:
C
 20   CONTINUE
      KLC = IW(NAMIND('VALC'))
      KGB = IW(NAMIND('VAGB'))
      IF ((KLC .LE. 0) .OR. (KGB .LE. 0)) THEN
        IFLAG = 0
        GO TO 999
      ENDIF
C
C     Initialize the matrices to zero:
C
      CALL VZERO(VTEXPD,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
      CALL VZERO(VTEXPI,LVTEXP*NVFLMX*NVWFMX*NVLAYR)
C
C     Construct the global transformation:
C
      CALL VGEXRO(1,RW(KROW(KGB,1)+JVAGTR),TEGB)
C
C     Modify global transformation using laser data:
C
      CALL VGRDLS(LUN,IRUN,TEGB)
C
C     Loop on the rows of VALC to get transformation of each wafer:
C     
      DO IRO=1,LROWS(KLC)
C     
C     Decode the wafer index, and make sure this is a valid wafer:
C     
         CALL VADEWA(ITABL(KLC,IRO,JVALWI),ILAY,IWFF,IFAC,IVIEW)
         IF (IFAC .LE. VNRFAC(ILAY)) THEN
C     
C     Modify local alignment for face bending
C     
            CALL UCOPY(RW(KROW(KLC,IRO)+JVALTR),VLOC,LVALCA-1)
            CALL VALFCO(VLOC,ILAY,IWFF,IFAC,IRUN)
C     
C     Local alignment of the wafer:
C     
            CALL VGEXRO(2,VLOC,TELC)
C     
C     Nominal transformation for the wafer:
C     
            CALL VGGTNO(ILAY,IWFF,IFAC,TENO)
C     
C     Compute the total transformation for the wafer:
C     
            CALL VGCMTR(TELC,TENO,TE)
            CALL VGCMTR(TE,TEGB,VTEXPD(1,IFAC,IWFF,ILAY))
C     
C     Compute the inverse transformation:
C     
            CALL VGINTR(VTEXPD(1,IFAC,IWFF,ILAY),
     &           VTEXPI(1,IFAC,IWFF,ILAY),IFAIL)
            IF (IFAIL .NE. 0) THEN
               IFLAG = 0
            ENDIF
C     
         ENDIF
      ENDDO
C     
C     Calculate the distance to wafer midplanes:
C     
      CALL VGCADI
C     
C     Routine is terminated and flag has been set properly.
C     
  999 CONTINUE
      RETURN
      END
#endif


