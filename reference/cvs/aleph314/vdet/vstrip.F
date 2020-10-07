      SUBROUTINE VSTRIP(SCENT,IFLAG)
C
C!  Returns the wafer center in strip co-ordinates
CKEY VDET JULIA
C  Outputs; SCENT(ILAY,IWAF,IPHI,IVIEW)
C           = array of wafer centers in strip co-ordinates, where
C           1.0 is the center of the first strip, and 1.0 = 100 microns.
C           IFLAG(ILAY,IWAF,IPHI,IVIEW)
C           = Bonding flag for wafer; a single bit for 100 or 200
C           micron bonding
C           SCENT and IFLAG must be dimensioned by the calling routine :
C                INTEGER IFLAG(LVDL,NWAFN,NSLOM,2)
C                REAL SCENT(LVDL,NWAFN,NSLOM,2)
C  David Brown, 4-2-91.  Modified 13-2 Wolfgang Manner
C
#ifndef DOC
      SAVE
      PARAMETER (NSLOM=15, NWAFN=4, LVDL=2)
      INTEGER NVIEWS
      PARAMETER (NVIEWS=2)
      INTEGER ILAY,IPHI,IVIEW,ITYPE,IWAF
      INTEGER IFLAG(LVDL,NWAFN,NSLOM,NVIEWS)
      INTEGER VNSCRM
      REAL SCENT(LVDL,NWAFN,NSLOM,NVIEWS)
C
C  Bonding patterns flags; 100 or 200 micron
C
      INTEGER VB100U,VB200U
      PARAMETER (VB100U = 65536)
      PARAMETER (VB200U = 131072)
C
C  Loop over all wafers
C
      DO 300 ILAY=1,LVDL
         DO 200 IPHI=1,NSLOM
            DO 100 IWAF=1,NWAFN
C
C  Z view; compute the center from the number of Z strips.
C  Z bonding depends on module type.
C
                 SCENT(ILAY,IWAF,IPHI,1) = (VNSCRM(1)+1)/2.
                 IFLAG(ILAY,IWAF,IPHI,1) = VB100U
C
C  R-phi view; compute the center from the number of R-phi strips,
C  Bonding is always 100 micron
C
               SCENT(ILAY,IWAF,IPHI,2) = (VNSCRM(2)+1)/2.
               IFLAG(ILAY,IWAF,IPHI,2) = VB100U
  100       CONTINUE
  200    CONTINUE
  300 CONTINUE
      RETURN
      END
#endif
