      SUBROUTINE TPCODP(IUNIT)
C----------------------------------------------------------------------
C!    Dump BOS bank TPCO to IUNIT
CKEY PRINT TPC
C!
C!    Author:    R. Johnson  23-10-86
C!    Modified:  R. Johnson  18-06-90
C!
C!    Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!
C!---------------------------------------------------------------------
      SAVE
C
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
C
C----------------------------------------------------------------------
C
C!    set of intrinsic functions to handle BOS banks
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
C
C----------------------------------------------------------------------
C
      KTPCO=IW(NAMIND('TPCO'))
      IF (KTPCO.EQ.0) RETURN
      NC=LROWS(KTPCO)
      KEVEH=IW(NAMIND('EVEH'))
      IF (KEVEH.NE.0) THEN
        IRUN=IW(KEVEH+JEVERN)
        IEVT=IW(KEVEH+JEVEEV)
      ELSE
        IRUN=0
        IEVT=0
      ENDIF
      WRITE(IUNIT,103) NC,IRUN,IEVT
  103 FORMAT(//' Dump of ',I4,' TPC coordinates for run ',I5,' event ',
     &         I6)
      WRITE(IUNIT,102)
  102 FORMAT(/2X,' IC',2X,'sect',2X,' row',2X,' pad',5X,
     &       'radius',5X,' phi',6X,
     &       '   z',8X,'SigRPhi',5X,' SigZ ',3X,' Origin',
     &       2X,'track',1X,'clus',1X,'twin',2X,'sect R*Phi',4X,
     &       'sect Z')
      DO 21 IC=1,NC
        IROW=IW(KROW(KTPCO,IC)+JTPCIN)/100000
        IPAD=MOD(IW(KROW(KTPCO,IC)+JTPCIN),1000)
        ISEC=MOD(IW(KROW(KTPCO,IC)+JTPCIN),100000)/1000
        WRITE(IUNIT,101) IC,ISEC,IROW,IPAD,(RTABL(KTPCO,IC,J),J=2,6),
     &                   (ITABL(KTPCO,IC,J),J=7,10),
     &                   (RTABL(KTPCO,IC,J),J=11,12)
  101   FORMAT(1X,I3,2X,I4,2X,I4,2X,I4,1X,F10.4,1X,F10.6,
     &         3(1X,F10.4),4X,I2,5X,3(2X,I3),2(1X,F10.4))
   21 CONTINUE
      WRITE(IUNIT,104)
  104 FORMAT('-------------------------- End of TPCO dump',
     &      ' ---------------------------------------------------'/)
      RETURN
      END
