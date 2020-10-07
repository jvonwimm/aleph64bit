      SUBROUTINE VAENCL (IADDR,MMOD,IVIEW,ISCM,NSCH,ISCMF,IECOR)
C ----------------------------------------------------------------------
C!  Convert strip-channel-in-module to electronics channel and pack it
CKEY VDET ENCODE
C - Steve Wasserbaech, 7 November 1994
C
C   This function converts the strip-channel-in-module number of the
C   first strip channel in a cluster into electronics channel number;
C   if the readout direction is -1, the electronics channel number of
C   the last channel in the cluster is used.  The cluster address is
C   packed using VADDPK.  The strip-channel-in-module number of the
C   first or last strip (whichever one has the smallest electronics
C   channel number) is returned, along with the readout direction,
C   to facilitate subsequent loops over the strip-channels-in-module
C   in the order of increasing electronics channel number.
C
C - Input:
C   MMOD   / I  Signed global module number
C   IVIEW  / I  View number (=1 for z, =2 for r-phi)
C   ISCM   / I  First strip-channel-in-module number in cluster
C   NSCH   / I  Number of strip channels in cluster
C
C - Output:
C   IADDR  / I  Packed cluster address including number of channels;
C               see VADDPK for packing scheme.
C   ISCMF  / I  = ISCM        if readout direction = +1;
C               = ISCM+NSCH-1 if readout direction = -1
C   IECOR  / I  Readout direction for this view
C               = +1 if strip channels and electronics channels are
C                 numbered in the same direction;
C               = -1 if in opposite directions.
C ----------------------------------------------------------------------
C     IMPLICIT NONE
C
C     Arguments:
      INTEGER IADDR, MMOD, IVIEW, ISCM, NSCH, ISCMF, IECOR
C
C     Local variables
      INTEGER IRET, ILAY, IFAC, IROM, ISCH, IECH, ISCML, NSCHL
C
C     External references:
      INTEGER VIMODM, VSCMSC, VCSCEC, VSCMEC
C
C ----------------------------------------------------------------------
C
C     Convert the strip-channel-in-module to readout module and
C     strip channel for the first strip channel in the cluster:
      IRET = VSCMSC(MMOD,IVIEW,ISCM,IROM,ISCH)
C
C     Calculate the first electronics channel from the first
C     strip channel and the number of channels in the cluster:
      NSCHL = MAX(1,NSCH)
      IRET = VCSCEC(IVIEW,ISCH,NSCHL,IECH)
C For some reason, the electronic channels number has been reduced by 1.
C Let us restore ir to what it should have been
C
      IECH=IECH+1
C
C     Calculate the layer and face indices for this module:
      IRET = VIMODM(MMOD,ILAY,IFAC)
C
C     Pack the cluster address:
      CALL VADDPK(IADDR,NSCH,ILAY,IROM,IFAC,IVIEW,IECH)
C
C     Get the limits for looping over strip-channels-in-module;
C     depending on the readout direction, we may need to loop
C     in reverse order:
      IRET = VSCMEC(IVIEW,ISCM,NSCHL,ISCMF,ISCML,IECOR)
C
      RETURN
      END
