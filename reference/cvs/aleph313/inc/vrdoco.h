C!    Common for VRDO data: Readout configuration
C ----------------------------------------------------------------------
      INTEGER NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP
      INTEGER NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
      COMMON / VRDOCO / NRDSTZ, NRDSTP, NREFRZ, NREFRP, NOFRDZ, NOFRDP,
     >                  NZRSSC, IECORZ, IECORP, NZEROM, NPEROM, NWFBIT
C
#if defined(DOC)
      NRDSTZ      Number of readout strips per wafer, z side
      NRDSTP      Number of readout strips per wafer, r-phi side
      NREFRZ      Readout strip frequency, z side
      NREFRP      Readout strip frequency, r-phi side
      NOFRDZ      Offset (phys strips) of 1st readout strip, z side
      NOFRDP      Offset (phys strips) of 1st readout strip, r-phi side
      NZRSSC      Number of readout strips per strip channel, z view
      IECORZ      Elec chan numbering dir (z) w.r.t. strip channels
      IECORP      Elec chan numbering dir (r-phi) w.r.t. strip channels
      NZEROM      Number of elec channels per elec module, z view
      NPEROM      Number of elec channels per elec module, r-phi side
      NWFBIT      Number of bits for wafer number in hit addresses
#endif
