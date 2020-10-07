*DK tpfmt
      SUBROUTINE TPFMT(KEND,ISECT,ITYPE,JCHN,IBIN1,NLEN,NHITS,IERR)
C-----------------------------------------------------------------------
C!  Format MC data to emulate the PAD readout from the in TPP's
C
C  Called from:  TSRESP
C  Calls:        BKFRW, BLIST
C
C  Inputs:   PASSED:      --KEND,   "compress banks" flag
C                         --ISECT,  sector number
C                         --ITYPE,  sector type
C                         --JCHN,   channel number
C                         --IBIN1,  first bin of this signal
C                         --NLEN,   number of bins in this signal
C                         --NHITS,  hit number or number of hits
C            /TPCBOS/     --DIGNAM, names of hl and dig banks
C            /TPGEOP/     --No of pads in this row
C                         --NDIDEF, default lengths of hl and dig banks
C                         --NDIEXT, default extensions
C                         --ITMADC, id for single digitized pulse
C
C  Outputs:  PASSED:      --NDI, current number of digitisations
C            BANKS:       --named bank TPAD, NR = sector number,
C                           list 'E'; hit list
C                           IW(IDHL+1)   = number of hits
C                           IW(IDHL+1+N) = info for hit, bit-packed
C                                          in MTPKHL
C                         --named bank TPDI, NR = sector number,
C                           list 'E'; digitizations
C                           IW(IDDI+Nbytes) = ADC counts in bucket,
C                                             1 bucket per byte
C            IERR:        -- 0 for normal completion, 1 if insufficient
C                            space to increase work bank length
C  D. DeMille, M. Mermikides
C
C  Modifications:
C
C     1.  D. Cowen 24JUN88 -- Change sector number on TPDI, TPAD
C                             banks to accomodate ONLINE's stupid
C                             convention.
C     2.  P. Janot 02MAR89 -- Correct the number of arguments in
C                             WBANK calls.
C-----------------------------------------------------------------------
#include "bcs.h"
#include "tpcbos.h"
#include "tpgpar.h"
#include "tpgeop.h"
C
C  IPADR0 gives cumulative pad no at start of each row
C  for computation of TPD number (half-pads have been included in
C  the count as they constitute distinct channels)
C
      DIMENSION IPADR0(12,3)
      DATA IPADR0/0,61,132,213,304,405,516,637,768, 0 , 0, 0,
     2            0,77,159,246,338,435,537,644,756,891,1031,1176,
     3            0,77,159,246,338,435,537,644,756,855, 959,1068 /
C
      DATA NHLPT,NDIPT/3,4/
C
C  NDI   = current no of samples,
C  NDILW = current no of full words into which NDI are packed
C  IBYTE = byte number of full word for packing next sample
C
      DATA NDI,NDILW,IBYTE/0, 1, 0/
C
      IERR = 0
C
C  If KEND = 1, we compress workbanks and transfer to named banks
C
      IF ( KEND .EQ. 1 ) GOTO 11
C
      IF ( NHITS .EQ. 1 ) THEN
C
C  Start first rowlist. JROW is the starting pointer
C  Reset pointers for dig. encoding
C
         IRLAST = 999
         NROW = 0
         JROW = 0
         NDI = 0
         NDILW = 1
         IBYTE = 0
C
      ENDIF
C
C  If we need more space, extend the banks (Add 2 words in case of new r
C
      NHLNEW =  2*NROW  + NHITS + 2
C
      IF ( NHLNEW .GT. IW(INDHL) ) THEN
         NDWRDS = IW(INDHL) + NDIEXT(1)
         CALL WBANK(IW,INDHL,NDWRDS,*999)
      ENDIF
C
      NDINEW = (NDI+NLEN-1)/4 + 1
C
      IF ( NDINEW .GT. IW(INDDI) ) THEN
         NDWRDS = IW(INDDI) + NDIEXT(2)
         CALL WBANK(IW,INDDI,NDWRDS,*998)
      ENDIF
C
C  Now we're ready to put this hit into the hit list.
C  Start new rowlist on break of row number
C
      IROW = (JCHN-1)/150 + 1
      IF (IROW.NE.IRLAST) THEN
         IF (IRLAST.NE.999) JROW = JROW + IW(INDHL+JROW+2) + 2
         IW(INDHL+JROW+1) = IROW
         IW(INDHL+JROW+2) = 0
         IRLAST = IROW
         NROW = NROW + 1
      ENDIF
C
C  Pack channel no, t0, pad no into rowlist and increment counter
C  IPAD is counted according to hardware convention
C (Anticlock-wise, facing the pad plane from the active side)
C
      IW(INDHL+JROW+2) = IW(INDHL+JROW+2) + 1
      JHIT = IW(INDHL+JROW+2) + JROW + 2
      IPAD = MOD(JCHN,150)
      IF (IPAD.EQ.0) IPAD = 150
C
C  Get pad number in TPD wiring sequence
C
      IF (MOD(IROW,2).EQ.1) THEN
         IWEAVE = IPADR0(IROW,ITYPE) + IPAD
      ELSE
         IWEAVE = IPADR0(IROW,ITYPE) + NTPDPR(IROW,ITYPE) - IPAD + 3
      ENDIF
      ITPD = (IWEAVE - 1)/64
      IPTPD = IWEAVE - ITPD*64 - 1
      ID = IBIN1
      CALL MVBITS(IPTPD,0, 6, ID, 9)
      CALL MVBITS(NLEN, 0, 8, ID, 16)
      CALL MVBITS(IPAD, 0, 8, ID, 24)
      IW(INDHL+JHIT) = ID
C
C  Now the digitizations. Since the digitizations range from 0 to 255,
C  we put them in 1-byte words.  The bytes are stored from left to
C  right according to IBM convention
C
      INDADC = ITMADC + IBIN1 - 1
C
      DO 1 JBUCK = 1, NLEN
C
         NDI = NDI + 1
         IBYTE = IBYTE + 1
C
         IF ( IBYTE .EQ. 5 ) THEN
            IBYTE = 1
            NDILW = NDILW + 1
         ENDIF
C
         NBITLW = 8*(4-IBYTE)
         CALL MVBITS(IW(INDADC+JBUCK),0,8,IW(INDDI+NDILW),NBITLW)
C
 1    CONTINUE
C
C  Normal successful completion
C
      RETURN
C
C  If here, we are taking care of the banks after we are done
C  entering signals
C
 11   CONTINUE
C
      NHLWD = 2*NROW + NHITS
      NDIWD = (NDI-1)/4 + 1
C
C  Compress worbanks
C
      CALL WBANK(IW,INDHL,NHLWD,*999)
      CALL WBANK(IW,INDDI,NDIWD,*998)
C
C  Copy into named banks (NB declare dig bank format as B32, otherwise
C  BKFRW will try to pack).
C  But first, change sector numbers 1 <==> 19, 2 <==> 20, etc.
C
      IF (ISECT .LE. 18) THEN
         ISECT2 = ISECT + 18
      ELSE
         ISECT2 = ISECT - 18
      ENDIF
C
      CALL BKFRW(IW,DIGNAM(NHLPT),ISECT2, IW,INDHL, *898)
      CALL BKFRW(IW,DIGNAM(NDIPT),ISECT2, IW,INDDI, *898)
C
      CALL BLIST(IW,'E+',DIGNAM(NHLPT))
      CALL BLIST(IW,'E+',DIGNAM(NDIPT))
C
C  Normal successful completion
C
      RETURN
C
C  Problem lifting BOS banks
C
 898  WRITE(6,899) ISECT2
 899  FORMAT(' +++TPFMT+++ ERROR LIFTING TPDI OR TPAD BANKS',
     *       ' FOR SECTOR',I3)
  999 WRITE(6,801) ISECT
  801 FORMAT(' +++TPFMT+++ Error extending PAD hitlist',
     .       ' worbank for sector',I5)
      IERR = 1
      RETURN
  998 WRITE(6,802) ISECT
  802 FORMAT(' +++TPFMT+++ Error extending PAD dig.',
     .       ' worbank for sector',I5)
      IERR = 1
C
      RETURN
      END
