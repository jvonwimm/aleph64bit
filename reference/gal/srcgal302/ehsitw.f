      SUBROUTINE EHSITW(ISCMP,MODUL,TYGEO)
C.----------------------------------------------------------------
C Y.Karyotakis M.Rumpf Dec 85
C! ECAL signals -->addresses
C    Process ECAL Signals for one Geant3 Track Element
C    Find Towers and wire planes addresses
C   - Called by EHTRKE,EHSHOW
C   _ Calls     EHSMTW,EHSMWI,EHTTWD
C.----------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LSTCK=3,LPHI=384,LTHET=228)
      PARAMETER (LWPLA=45,LMODU=12,LCOMP=3)
      PARAMETER (NTHSG=12,NPHSG=24)
      PARAMETER (NECST = 30)
      COMMON/ ECSTAT / NECONT(NECST),ECCONT(NECST)
      PARAMETER (LPS1=6, LPS2=300)
      COMMON /ECNAMC/   NAETHT, NAEWHT, NAETTD, NAEWTD, NAETDI, NAEWDI
     &                , NAEWHE
     &                , NAETTR, NAEWTR, NAENDI
     &                , IDPSIG, IDEWTM, IDETTM
     &                , NAESHI, NAEWHI
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
C
      EXTERNAL EFNDCL,EFNDLG
      INTEGER  EFNDCL,EFNDLG
      DIMENSION IPLST(LWPLA)
      CHARACTER * 5 TYGEO
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
      DATA NCALL/0/
C
      IF (NCALL.EQ.0)      THEN
        NCALL = 1
        CALL ECPLST(IPLST)
      ENDIF
C
C -------------------------------------------------------------
      IF (IDPSIG.EQ.0) RETURN
C
      NECSG = LROWS(IDPSIG)
C
      IF (NECSG .EQ. 0)                  RETURN
C
C     Loop over all generated Signals
C
      IPX = IDPSIG + LMHLEN
      DO 1 L = 1 , NECSG
C
C     Tranform HIT coordinates to ICL,ILG,STACK for PADS,and WIRE Plane
C
         ICL = EFNDCL(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ICL.LE.0)      THEN
             NECONT(6) = NECONT(6) + 1
             GO TO 1
         ENDIF
         ILG = EFNDLG(ISCMP,MODUL,RW(IPX+1),TYGEO)
         IF (ILG.LE.0)      THEN
            NECONT(7) = NECONT(7) + 1
            GO TO 1
         ENDIF
C
C probably the previous correction makes this redundant
C
         IF( IW(IPX+6).GT.LWPLA ) IW(IPX+6) = LWPLA
         IST = IPLST( IW(IPX+6) )
         ECCONT(IST) = ECCONT(IST) + IW(IPX+4)
C
C -         Compute tower address and fill 'ESHI'
C
         IADTW = 0
         CALL MVBITS(ICL,0,9,IADTW,2)
         CALL MVBITS(ILG,0,9,IADTW,16)
         CALL CAHIST(NAESHI,IADTW,IST,IW(IPX+4))
C
C-          Compute wire address and fill 'EWHI'
C
         IADWI = (ISCMP-1)*LMODU + MODUL
         CALL CAHIST(NAEWHI,IADWI,IW(IPX+6),IW(IPX+5))
C
    1 IPX = IPX + LCOLS(IDPSIG)
C
      RETURN
      END
