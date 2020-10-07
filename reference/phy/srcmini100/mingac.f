      SUBROUTINE MINGAC
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Fill photon bank DGAC for Mini-DST.
C
C     Author: Agnieszka Jacholkowska    17-Oct-94
C
C     Input  : PGAC bank
C     Output : DGAC bank
C
C     Called by MINDST
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
      PARAMETER(JPGAEC=1,JPGATC=2,JPGAPC=3,JPGAR1=4,JPGAR2=5,JPGAF4=6,
     +          JPGADM=7,JPGAST=8,JPGAQU=9,JPGAQ1=10,JPGAQ2=11,
     +          JPGAM1=12,JPGAM2=13,JPGAMA=14,JPGAER=15,JPGATR=16,
     +          JPGAPR=17,JPGAEF=18,JPGAGC=19,JPGAZS=20,JPGAPL=21,
     +          JPGAPF=22,JPGAPN=23,JPGAFA=24,JPGAPE=25,LPGACA=25)
      PARAMETER(JDGAEC=1,JDGATC=2,JDGAPC=3,JDGAR1=4,JDGAR2=5,JDGAF4=6,
     +          JDGADM=7,JDGAST=8,JDGAQU=9,JDGAQ1=10,JDGAQ2=11,
     +          JDGAM1=12,JDGAM2=13,JDGAMA=14,JDGAER=15,JDGATR=16,
     +          JDGAPR=17,JDGAEF=18,JDGAGC=19,JDGAZS=20,JDGAPL=21,
     +          JDGAPF=22,JDGAPN=23,JDGAFA=24,JDGAPE=25,LDGACA=25)
C!    set of intrinsic functions to handle BOS banks
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
C
C++   Pick up PGAC bank.
C
      KPGAC = NLINK('PGAC',0)
      IF (KPGAC.LE.0) RETURN
      NPGAC = LROWS(KPGAC)
      IF (NPGAC.LE.0) RETURN
C
C++   Create the DGAC bank.
C
      NDGAC = NPGAC
      LEN = LMHLEN + LDGACA * NDGAC
      CALL AUBOS('DGAC',0,LEN, KDGAC,IGARB)
      IF (IGARB.GE.2) THEN
         WRITE(IW(6),'('' MINGAC: Cannot create DGAC bank'')')
         RETURN
      ELSE IF (IGARB.NE.0) THEN
         KPGAC = NLINK('PGAC',0)
      ENDIF
      IW(KDGAC+LMHCOL) = LDGACA
      IW(KDGAC+LMHROW) = NDGAC
c     WRITE(IW(6),'('' MINGAC: DGAC bank lifted'')')
c     PRINT *, LDGACA, NPGAC, NDGAC, LMHROW, IW(KDGAC+LMHROW)
C
C++   Loop over PGAC storing information in DGAC.
C
      DO 100 I=1,NDGAC
         IW(KROW(KDGAC,I)+JDGAEC) = NINT(EFACTM * RTABL(KPGAC,I,JPGAEC))
         IW(KROW(KDGAC,I)+JDGATC) = NINT(AFACTM * RTABL(KPGAC,I,JPGATC))
         IW(KROW(KDGAC,I)+JDGAPC) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPC))
         IW(KROW(KDGAC,I)+JDGAR1) = NINT(1000. * RTABL(KPGAC,I,JPGAR1))
         IW(KROW(KDGAC,I)+JDGAR2) = NINT(1000. * RTABL(KPGAC,I,JPGAR2))
         IW(KROW(KDGAC,I)+JDGAF4) = NINT(1000. * RTABL(KPGAC,I,JPGAF4))
         DISTM = RTABL(KPGAC,I,JPGADM)
         IF (DISTM.GT.1000.) DISTM = -0.1
         IW(KROW(KDGAC,I)+JDGADM) = NINT(10. * DISTM)
         IW(KROW(KDGAC,I)+JDGAST) = ITABL(KPGAC,I,JPGAST)
         IW(KROW(KDGAC,I)+JDGAQU) = ITABL(KPGAC,I,JPGAQU)
         IW(KROW(KDGAC,I)+JDGAQ1) = NINT(100. * RTABL(KPGAC,I,JPGAQ1))
         IW(KROW(KDGAC,I)+JDGAQ2) = NINT(100. * RTABL(KPGAC,I,JPGAQ2))
         IW(KROW(KDGAC,I)+JDGAM1) = NINT(100. * RTABL(KPGAC,I,JPGAM1))
         IW(KROW(KDGAC,I)+JDGAM2) = NINT(100. * RTABL(KPGAC,I,JPGAM2))
         IW(KROW(KDGAC,I)+JDGAMA) = NINT(EFACTM * RTABL(KPGAC,I,JPGAMA))
         IW(KROW(KDGAC,I)+JDGAER) = NINT(EFACTM * RTABL(KPGAC,I,JPGAER))
         IW(KROW(KDGAC,I)+JDGATR) = NINT(AFACTM * RTABL(KPGAC,I,JPGATR))
         IW(KROW(KDGAC,I)+JDGAPR) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPR))

         IW(KROW(KDGAC,I)+JDGAEF) = NINT(AFACTM * RTABL(KPGAC,I,JPGAEF))
         IW(KROW(KDGAC,I)+JDGAGC) = NINT(AFACTM * RTABL(KPGAC,I,JPGAGC))
         IW(KROW(KDGAC,I)+JDGAZS) = NINT(AFACTM * RTABL(KPGAC,I,JPGAZS))
         IW(KROW(KDGAC,I)+JDGAPL) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPL))
         IW(KROW(KDGAC,I)+JDGAPF) = NINT(AFACTM * RTABL(KPGAC,I,JPGAPF))
         IW(KROW(KDGAC,I)+JDGAPN) = ITABL(KPGAC,I,JPGAPN)
         IW(KROW(KDGAC,I)+JDGAFA) = ITABL(KPGAC,I,JPGAFA)

         IW(KROW(KDGAC,I)+JDGAPE) = ITABL(KPGAC,I,JPGAPE)
  100 CONTINUE
c     PRINT *, LDGACA, NPGAC, NDGAC, LMHROW, IW(KDGAC+LMHROW)
C
C++   Add the bank to the Mini list.
C
      CALL MINLIS('DGAC')
C
      RETURN
      END
