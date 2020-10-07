      SUBROUTINE TPHSRT(NAMH,NAMR)
C -------------------------------------------------------------------
C. - M.Mermikides -  860202
C.
C. -  Sorts hit bank and track element ref. bank in increasing radius
C.    NAMH is name index of hit bank to be sorted
C     NAMR  "   "    "      track element ref. bank
C.    (NB It is implied that the 2nd word (ChanID) is the sort key)
C.
C. -  Calls           NDROP,WBANK,WDROP              from BOS77 lib
C.                    UCOCOP,SORTZV                  from CERN lib
C -------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /JDTLOC/ JDSORT,JDWORK
C
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
      JHIT(IH) = KROW (KHIT,IH)
C
C -------------------------------------------------------------------
      JDSORT = 0
      JDWORK = 0
C
      KHIT = IW(NAMH)
C
C  Exit if no bank
C
      IF (KHIT.EQ.0) GO TO 999
      NWPHT = IW(KHIT + 1)
      NHITS = IW(KHIT + 2)
      IF (NHITS.LT.2) GO TO 999
C
C  Book work banks for sorting
C
      CALL WBANK(IW,JDSORT,NHITS,*800)
      CALL WBANK(IW,JDWORK,NHITS,*800)
C
C Sort according to channel number
C
      DO 150 IH = 1,NHITS
         IW(JDWORK + IH) = IW(JHIT(IH) + 2)
150   CONTINUE
C
      CALL SORTZV(IW(JDWORK+1),IW(JDSORT+1),NHITS,-1,0, 0,0,0)
C
C  Replace bank contents according to key order
      DO 11 K=1,NWPHT
         CALL UCOCOP(IW(KHIT+2+K),IW(JDWORK+1),NHITS,1, NWPHT, 1)
         DO 14 I = 1,NHITS
            IND = IW(JDSORT + I)
            IW(JHIT(I) + K) = IW(JDWORK + IND)
14       CONTINUE
11    CONTINUE
C
C  Now sort track element reference bank (if it exists)
C
      KHITR = IW(NAMR)
      IF(KHITR.EQ.0) GO TO 900
      KHE = KHITR + 2
      CALL UCOPY(IW(KHE+1),IW(JDWORK+1),NHITS)
      DO 175 I=1,NHITS
         IND = IW(JDSORT + I)
         IW(KHE + I) = IW(JDWORK + IND)
175   CONTINUE
C
      GO TO 900
C
C  Error
 800  CONTINUE
      CALL WDROP (IW,JDSORT)
      CALL WDROP (IW,JDWORK)
      CALL ALTELL ('TPHSRT: not enough space to book JDSORT/JDWORK ',
     &              1,'NEXT')
C
C  End
900   CALL WDROP(IW,JDSORT)
      CALL WDROP(IW,JDWORK)
999   RETURN
      END
