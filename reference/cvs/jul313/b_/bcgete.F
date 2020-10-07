      SUBROUTINE BCGETE(kbptr,kbhit,iev,itrmask,iPmt,iApd,iRmx,iPhmx)
C --------------------------------------------------------------------
C! Read in BCAL raw data event
C
C     Author:  Peter Morawitz, 6.11.98 
C              Code from Gonzalo Merino incorporated into JULIA framework
C     Modified:Marco Cattaneo, 9.11.98
C              Cleaned up for JULIA
C
C     Purpose: AUX routine called from BCALJU, reads in the BCAL event,
C              derived from online routine:
C              get_bcal_lumi_event(iev,itrmask,iPmt,iApd,iRmx,iPhmx)
C --------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "bcs.h"

      INTEGER i,j,iev
      INTEGER kbhit,kbptr
      INTEGER i1,i2,ihit,iadc,ich
      INTEGER kfirst,len
      INTEGER itrmask
c
      INTEGER n_ch_sil,n_ch_scnt,n_ch_tot
      PARAMETER (n_ch_sil=384, n_ch_scnt = 8,  n_ch_tot  = 392)
      INTEGER adc(n_ch_tot)
c
      INTEGER N_Mod,N_Waf,N_Pad
      PARAMETER ( N_Mod = 4 , N_Waf = 6 , N_Pad = 16 )
      INTEGER iPmt(N_Mod),iApd(N_Mod),iSil(N_Mod,N_Waf,N_Pad)
      INTEGER iRmx(N_Mod),iPhmx(N_Mod)

      INTEGER ismx,k
c-----------------------------------------------------------------------

c
      CALL vzero(adc,n_ch_tot)

c BPTR bank. pointing to iev event
      i1 = iw(kbptr+2+iev)
c Pointer to start of word
      kfirst = ibits(i1, 16, 16)
c Pointer to length of the word
      len    = ibits(i1, 0, 16)

c Hits loop
      IF(len.GT.0) THEN
        DO  ihit= kfirst, kfirst + len - 1
          i2 = int(iw(kbhit+2+ihit))
c value
          iadc= ibits(i2, 16, 16)
c code
          ich = ibits(i2, 0, 16)

c Get Triggermask (code=500)
          IF ( ich.EQ.500 ) THEN
            itrmask = iadc
c Get the adc values.
          ELSEIF ( ich .LE. n_ch_tot ) THEN
            adc(ich)= iadc
          ENDIF
c
        ENDDO

c Fill Pmt, Apd and Sil
        CALL BCFILE(adc,iPmt,iApd,iSil)

c Look for the Max deposition Location
        DO i = 1, N_Mod
          iSmx = 0
          DO j = 1, N_pad
            DO k = 1, N_Waf
              IF (iSil(i,k,j).gt.iSmx) then
                iSmx = iSil(i,k,j)
                iRmx(i) = j
                iPhmx(i) = k
              ENDIF
            ENDDO
          ENDDO
        ENDDO
c
      ENDIF
c
      RETURN
      END
#endif
