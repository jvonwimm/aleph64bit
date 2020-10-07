      SUBROUTINE ITMSK1(MASI, IB16, LBUS)
C.
C...ITMSK1  1.00  860518  22:46                     R.Beuselinck
C.
C!  Map the ITC trigger segments onto the 18 bus-lines.
C.  This is the first stage in correlating the ITC trigger to the
C.  calorimeter trigger segments.
C.  To understand this routine you need to look at the curcuit diagram.
C.
C.  Calling arguments:
C.  MASI - Array containing the results of the 48 ITC phi masks  (INPUT)
C.  IB16 - Word containing the 16 software controllable bits.    (INPUT)
C.  LBUS - Word containing the 18 bit bus-line mask.            (OUTPUT)
C.
C-----------------------------------------------------------------------
      INTEGER MASI(48), IB16, IMI(4), IMB(4), I, J, MASK, IAND, IOR
      INTEGER IBT16, LBUS
      INTEGER IAM(12), IAM1(12), IAM2(12), IAM3(12), IAM4(12)
C
C
C--  IMI and IMB are masks for the initial combination of the 48
C--  input sector bits with the 16 software-selectable bits.
C--
      DATA IMI/15,240,3840,61440/
      DATA IMB/4369,8738,17476,34952/
C
C--  IAM and IAMn represent the fan-out of the bits resulting from
C--  the initial AND.  When the appropriate bits are set the following
C--  bit patterns are mapped onto the 18-bit bus line mask.
C--
      DATA IAM/3,6,24,48,192,384,1536,3072,12288,24576,98304,196608/
      DATA IAM1/65536,65536,2,2,16,16,128,128,1024,1024,8192,8192/
      DATA IAM2/131072,1,4,8,32,64,256,512,2048,4096,16384,32768/
      DATA IAM3/16,16,128,128,1024,1024,8192,8192,65536,65536,2,2/
      DATA IAM4/4,8,32,64,256,512,2048,4096,16384,32768,131072,1/
C ------------------------------------------------------------------
C
C--  Loop over twelve identical curcuits.  The 48 ITC sectors are
C--  reduced to 12 phi sectors.  For each one of these there are also
C--  4 "side-bits" which may be set to activate adjacent sectors.
C--  The side-bits are controlled by a pattern of 16 software loadable
C--  bits specified by IB16.
C--
      LBUS = 0
      DO 10 I=1,12
        MASK = 0
        J = (I-1)*4
        IF (MASI(J+1).NE.0) MASK = MASK + IMI(1)
        IF (MASI(J+2).NE.0) MASK = MASK + IMI(2)
        IF (MASI(J+3).NE.0) MASK = MASK + IMI(3)
        IF (MASI(J+4).NE.0) MASK = MASK + IMI(4)
        IBT16 = IAND(MASK,IB16)
        IF (MASK.NE.0) LBUS = IOR(IAM(I),LBUS)
        IF (IAND(IMB(1),IBT16).NE.0) LBUS = IOR(IAM1(I),LBUS)
        IF (IAND(IMB(2),IBT16).NE.0) LBUS = IOR(IAM2(I),LBUS)
        IF (IAND(IMB(3),IBT16).NE.0) LBUS = IOR(IAM3(I),LBUS)
        IF (IAND(IMB(4),IBT16).NE.0) LBUS = IOR(IAM4(I),LBUS)
   10 CONTINUE
      END
