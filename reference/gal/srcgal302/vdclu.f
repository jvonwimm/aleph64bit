      SUBROUTINE VDCLU(IMOD,NHITM)
C!----------------------------------------------------------------------
C! Find clusters
CKEY VDET DIGITIZE
C!
C!  Author         A. Bonissent 15-Jan-1994
C!
C!  Description
C!  ===========
C!
C!   Find clusters
C!   Input is the arrays (banks VWC1 and VWC2) of all R/O channels
C!   in one module
C!
C!   For each view
C!   Say that we start from channel 1 and finish at last channel
C!   If first and last channels are fired and it is new Vdet (95)
C!                      and it is view 1 then
C!         there is a cluster spanning over last and first channels
C!         find first not fired channel,
C!         and say that we shall start the clustering from there
C!         and process Nchannels (equal to tot. nr channels in module)
C!         if channel number is larger than max, will be reduced to
C!            ichannel-nmax
C!   endif
C!   Start a cluster when a strip above 0 is found
C!   Finish it when a fired strip is found,
C!       or last channel is reached
C!   Keep a cluster only if at least one strip is above threshold
C!-       Do iloop = 1, maxchannels
C!          ich =iloop+ibeg
C!-         if (signal(ich) .ne. 0) then
C!-           if (nsiz .eq. 0) ibeg = ich
C!            if signal above threshold, cluster is good
C!-           nsiz = nsiz + 1
C!-           endif
C!-           if (nsiz .gt. 0) then
C!-             if (signal(ich).eq.0 .or. ich.eq.maxchannels) then
C!                 If cluster is good then
C!-                  --  here the cluster has been found;  starting
C!-                  --  ibeg, made of nsiz channels, out put to bank
C!                 endif
C!-             nsiz = 0
C!-           endif
C!-         endif
C!-       Enddo
C!-       end
C!   endloop on views
C!
C!
C! Input :  VWC1,VWC2  banks : readout channel content
C! Input : Module number and number of hits (tracks) for this module
C!
C! Output : VCLU bank
C!
C-----------------------------------------------------------------------
C
      SAVE NAVWCX,NAVCLU,VDET95
C
      PARAMETER (NADCL=10)
      DIMENSION THRES(2), NAVWCX(2)
      PARAMETER(JVCLVI=1,JVCLFS=2,JVCLCS=3,LVCLUA=3)
      PARAMETER(JVWCCC=1,JVWCSN=2,JVWCVT=3,LVWC1A=3)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER VNRWAF
      LOGICAL LGOOD,VDET95
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVWCX(1) = NAMIND('VWC1')
        NAVWCX(2) = NAMIND('VWC2')
        NAVCLU = NAMIND('VCLU')
        VDET95 = VNRWAF().EQ.3
      ENDIF
C
C First, get clustering thresholds
C
      CALL VFNDTH(IMOD,THRES)
C
C Drop old cluster bank if any
C and create new empty bank with reasonable size
C
      CALL BDROP(IW,'VCLU')
      NROWS = NHITM*2
      NDATA = NROWS*LVCLUA+LMHLEN
      CALL ALBOS('VCLU',0,NDATA,KVCLU,IGARB)
      IW(KVCLU+LMHCOL)=LVCLUA
      IW(KVCLU+LMHROW)=0
C
C Loop on views and find clusters
C
      DO 100 IV=1,2
        KVWCX = IW(NAVWCX(IV))
        NVWCX = LROWS(KVWCX)
        IFIRC=1
        IF(RTABL(KVWCX,1,JVWCCC).GT.0..AND.
     >     RTABL(KVWCX,NVWCX,JVWCCC).GT.0..AND.
     >     VDET95.AND.IV.EQ.1)THEN
           DO IVWCX=1,NVWCX
              IF(RTABL(KVWCX,1,JVWCCC).EQ.0)THEN
                 IFIRC=IVWCX
                 GO TO 110
              ENDIF
           ENDDO
 110       CONTINUE
        ENDIF
        NSIZ = 0
        LGOOD = .FALSE.
        DO 101 ILOOP=1,NVWCX
          IVWCX = ILOOP+IFIRC-1
          IF(IVWCX.GT.NVWCX)IVWCX=IVWCX-NVWCX
          SIGNL = RTABL(KVWCX,IVWCX,JVWCCC)
          IF(SIGNL.NE.0.)THEN
            LGOOD = (LGOOD.OR.SIGNL.GT.THRES(IV))
            IF(NSIZ.EQ.0)IBEG = IVWCX
            NSIZ = NSIZ + 1
          ENDIF
          IF(NSIZ.GT.0)THEN
            IF(SIGNL.EQ.0..OR.IVWCX.EQ.NVWCX)THEN
               IF(LGOOD)THEN
                  KVCLU=IW(NAVCLU)
                  NCLU=LROWS(KVCLU)+1
                  IF(LFRROW(KVCLU).LT.1)THEN
C If there is not enough space left in the bank, we extend it for
C NADCL more clusters
                    NDATA = IW(KVCLU) + NADCL*LVCLUA
                    CALL ALBOS('VCLU',0,NDATA,KVCLU,IGARB)
                    KVWCX = IW(NAVWCX(IV))
                  ENDIF
                  IW(KVCLU+LMHROW)=NCLU
                  KADDR = KROW(KVCLU,NCLU)
                  IW(KADDR+JVCLVI) = IV
                  IW(KADDR+JVCLFS) = IBEG
                  IW(KADDR+JVCLCS) = NSIZ
               ENDIF
               LGOOD=.FALSE.
               NSIZ = 0
            ENDIF
          ENDIF
 101    CONTINUE
 100  CONTINUE
C
C Adjust bank size
C
      CALL AUBPRS('VCLU')
      RETURN
      END
