      SUBROUTINE YVCAME(IPOIN,VRAD,NCO,DISV,IER)
C-----------------------------------------------------------------------
C Author: M.A.CIOCCI
C Date: 20/2/90
C Modified:  J.Sedgbeer 24/03/91 Make NCO = all coords inside in VXD+
C            ITC+TPC. Used to give coords in just 1st detector that
C            had hits (in order vxd,itc,tpc)
C! Algorithm to find if a track have some coordinates before vertex
CKEY YV0 COORDINATE VERTEX /USER
C Input:  Ipoin The index of one FRFT track
C         Vrad = the radius of the materialization point
C Output:NCO  = Number of coordinates with radius inside vrad for
C                track ipoin.  0 if no coordinate information available
C                if there are no such coordinates.
C         DISV = Distance(cm) in radius that the furthest in coordinate
C                is inside of vrad for track ipoin.  0. if no coordinate
C                information is available or there are no such coordinat
C         IER = 0 if all OK
C               1 if error in calculation
C
C Banks Used:FRFT,FRTL,FVCL,FICL,FTCL,TPCO,ITCO,VDCO
C
C The algorithm is based on Cinabro's routine for find if
C ther'are coordinates before a vertex candidate.
C A coordinate is "inside" if it  has Z or Radius from the Z-Axis
C less than the materialization point.  Also
C the Radius from the Z-Axis the furthest out of the points "inside" the
C materialization point from the materialization point is returned for e
C track.  If there are no coordinates inside the materialization point f
C the track then these last two values are zero.  An error results if
C the track numbers passed in are outside of the FRFT bank .
C
C-----------------------------------------------------------------------
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JITCWN=1,JITCRA=2,JITCP1=3,JITCP2=4,JITCZH=5,JITCSR=6,
     +          JITCSZ=7,JITCDT=8,LITCOA=8)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      EXTERNAL NLINK,NAMIND,NBANK,CHAINT,INTCHA,NDROP
      CHARACTER*4 CHAINT
      INTEGER NLINK,NAMIND,NBANK,INTCHA,NDROP
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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
C Get the pointer to FRFT
C
      IF (FIRST) THEN
         NFRFT = NAMIND('FRFT')
         FIRST = .FALSE.
         NFRTL = NAMIND('FRTL')
         NFVCL = NAMIND('FVCL')
         NFICL = NAMIND('FICL')
         NFTCL = NAMIND('FTCL')
         NTPCO = NAMIND('TPCO')
         NITCO = NAMIND('ITCO')
         NVDCO = NAMIND('VDCO')
      ENDIF
C
      IER = 0
      KFRFT = IW(NFRFT)
C
C Is FRFT there?
C
      IF (KFRFT.LE.0) THEN
         IER = 1
         GOTO 999
      ENDIF
C
C is the track inside of FRFT?
C
      LFRFT = LROWS(KFRFT)
      IF (IPOIN.GT.LFRFT) THEN
         IER = 1
         GOTO 999
      ENDIF
C
C OK see if there are any coordinates inside the vertex.
C
      KFRTL = IW(NFRTL)
      KVDCO = IW(NVDCO)
      KFVCL = IW(NFVCL)
      KITCO = IW(NITCO)
      KFICL = IW(NFICL)
      KTPCO = IW(NTPCO)
      KFTCL = IW(NFTCL)
C
C Number of coordinates in each subdetector
C
      ICT = IPOIN
    1 NVDC = ITABL(KFRTL,ICT,JFRTNV)
      NIDC = ITABL(KFRTL,ICT,JFRTNI)
      NTDC = ITABL(KFRTL,ICT,JFRTNT)
      NCOIN = 0
      DIN = 0.
C
C Some in the VDET
C
      IF (NVDC.GT.0.AND.KVDCO.GT.0) THEN
         IVDC = ITABL(KFRTL,ICT,JFRTIV)
         DO 10 K = 1,NVDC
            ICO = ITABL(KFVCL,K+IVDC,1)
            RADC = RTABL(KVDCO,ICO,2)
C
C Coordinate inside the materialization point
C
            IF (RADC.LT.VRAD) THEN
               NCOIN = NCOIN + 1
               DINT = VRAD - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   10    CONTINUE
      ENDIF
C
C Hits in the ITC
C
      IF (NIDC.GT.0.AND.KITCO.GT.0) THEN
         IIDC = ITABL(KFRTL,ICT,JFRTII)
         DO 20 K = 1,NIDC
            ICO = IABS(ITABL(KFICL,K+IIDC,1))
            RADC = RTABL(KITCO,ICO,JITCRA)
            IF (RADC.LT.VRAD) THEN
               NCOIN = NCOIN + 1
               DINT = VRAD - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   20    CONTINUE
      ENDIF
C
C Hits in the TPC
C
      IF (NTDC.GT.0.AND.KTPCO.GT.0) THEN
         ITDC = ITABL(KFRTL,ICT,JFRTIT)
         DO 30 K = 1,NTDC
            ICO = ITABL(KFTCL,K+ITDC,1)
            RADC = RTABL(KTPCO,ICO,JTPCRV)
            IF (RADC.LT.VRAD) THEN
               NCOIN = NCOIN + 1
               DINT = VRAD - RADC
               IF (DINT.GT.DIN) DIN = DINT
            ENDIF
   30    CONTINUE
      ENDIF
C
         NCO = NCOIN
         DISV = DIN
C
C That is it.
C
  999 RETURN
      END
