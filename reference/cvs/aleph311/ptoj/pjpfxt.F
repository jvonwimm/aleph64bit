        SUBROUTINE PJPFXT(JFXTR)
C***********************************************************************
C
C!  Create the FXTR bank and fill it from PFXT, if possible
C
C       Author: D.Casper    17/06/97
C
C       Input:  FRFT, PFXT banks
C
C       Output: JFXTR   BOS index of the newly created FXTR bank; zero if
C                       track bank is missing or BOS array is full
C
C***********************************************************************
#ifndef DOC
#include "bcs.h"
#include "fxtrjj.h"
#include "pfxtjj.h"

        data npfxt/0/
#include "bmacro.h"
C
        jfxtr = 0
        if (npfxt.eq.0) then
            npfxt = namind('PFXT')
            nfrft = namind('FRFT')
        endif
        jfrft = iw(nfrft)

C Check presence of tracks

        if (jfrft.le.0) return

C Drop output bank, if it exists

        call bdrop(iw,'FXTR')

C Book the output bank, parallel to FRFT

        ntrks = LROWS(jfrft)
        lfxtr = LMHLEN + LFXTRA * ntrks
        CALL AUBOS('FXTR',0,lfxtr,jfxtr,igarb)
        if (jfxtr.eq.0 .or. igarb.eq.2) return
        call blist(IW,'S+','FXTR')

C Fill bank header

        iw(jfxtr+lmhrow) = ntrks
        iw(jfxtr+lmhcol) = lfxtra

C If no tracks pre-selected (no PFXT bank), we're done

        jpfxt = iw(npfxt)
        if (jpfxt.eq.0) return

C Fill FXTR from PFXT

        do irow = 1, lrows(jpfxt)
            kpfxt = krow(jpfxt,irow)
            itrk  = iw(kpfxt + jpfxtn)
            if (itrk.le.ntrks) then
                kfxtr = krow(jfxtr, itrk)
                iw(kfxtr + jfxtv0) = iw(kpfxt + jpfxv0)
                iw(kfxtr + jfxtnc) = iw(kpfxt + jpfxnc)
                iw(kfxtr + jfxtkn) = iw(kpfxt + jpfxkn)
            endif
        enddo

        return
        end
#endif
