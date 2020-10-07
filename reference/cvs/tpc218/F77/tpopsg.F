*DK tpopsg
      SUBROUTINE TPOPSG(NBLKS,IERR)
C--------------------------------------------------------------------
C!  Manage size of the analog signal workbank
C
C  Called from:  TPSGNL, TSTEER, TORAWI
C  Calls:        WBANK
C
C  Inputs:   PASSED:      NBLKS,  the number of blocks wanted in
C                                 the analog signal bank; NBLKS = -1
C                                 means to use preloaded sizes for
C                                 opening and extension
C            TPCONS.INC:  NTBNAS, number of time bins in each division
C                                 of the analog signal collection time,
C                                 i.e., in each block of the analog
C                                 signal bank
C            TPCBOS.INC:  NDEFCH, default number of blocks to open
C                         NEXTCH, defaulst number of blocks by which
C                                 to extend the bank
C
C  Outputs:  BANKS:       work bank id INDSIG, containing analog
C                         signals as they are collected
C                         IW(INDSIG+1) = number of bins per block
C                         IW(INDSIG+2) = number of blocks open
C                           KHID = INDSIG + 2 + (JHIT-1)*IW(INDSIG+1)
C                           IW(KHID+I) = num of electrons in Ith bin
C                                        of JHITth analog signal
C                                        section hit (see TPSGNL)
C            IERR:        = 1 if no enough BOS space to open/extend bank
C  D. DeMille
C
C  Modifications:
C
C     22feb89 DFC   -- Change WBANK so it takes error return *998.
C
#include "bcs.h"
#include "tpcbos.h"
#include "tpcons.h"
C
      LOGICAL LOPEN, LAUTO
C
C  See if the bank is already open
C
      LOPEN = ( INDSIG .NE. 0 )
C
C  See if this call uses pre-loaded or manual sizes
C
      LAUTO = ( NBLKS .EQ. -1 )
C
C  Decide how many blocks we will end up with
C
      IF ( LAUTO ) THEN
C
         IF ( .NOT. LOPEN ) THEN
            NFINI = NDEFCH
         ELSE
            NFINI = IW(INDSIG+2) + NEXTCH
         ENDIF
C
      ELSE
C
         NFINI = NBLKS
C
      ENDIF
C
C  Extend the bank
C
      IERR = 0
      NDW = 2 + NFINI*NTBNAS
      CALL WBANK(IW,INDSIG,NDW,*998)
C
C  Fill in the header
C
      IW(INDSIG+1) = NTBNAS
      IW(INDSIG+2) = NFINI
C
      RETURN
C
C  Problem opening work bank
C
 998  WRITE(6,999) NBLKS, NFINI
 999  FORMAT(2X,' ++TPOPSG++ PROBLEM OPENING SIGNAL WORK BANKS',
     *          ' PASSED PARAMETER: ',I4,
     *          ' ACTUAL NUMBER OF BLOCKS ATTEMPTED TO OPEN: ',I4)
      IERR = 1
      RETURN
C
      END
