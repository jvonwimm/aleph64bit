      INTEGER FUNCTION VSPXYZ(EPOS,IVIEW,ILAY,IFAC,IWFF,NROM,
     >                        VUW,XYZ)
C----------------------------------------------------
C!
C!  - Transform strip position into XYZ glob. coordinates
C!
C!   Author   :- A. Bonissent March 1995.
C!             - updated 11/5/95 Bugfix for wrong NRXYZ
C!  This Input is the decimal strip number, layer, face view and
C!  wafer number. Returns NRXYZ and global position
C!
C!  Inputs;
C!               EPOS = decimal strip number
C!               IVIEW= view
C!               ILAY = layer
C!               IFAC = face
C!               IWFF = wafer in face
C!               NROM = encoded resdout module number
C!
C!  Outputs;
C!               VUW  = local coordinate
C!               XYZ  = global coordinate
C?
C!======================================================================
#ifndef DOC
      INTEGER VDRSPS, VDPSLC, VABCVU, VNSCRM, VDYEAR, VPHSTM
      INTEGER VNRDIR
C
      INTEGER IVIEW,ILAY,IFAC,IWFF,NROMD,NRXYZ,JWAF,IWAF,IPHI
      REAL EPOS, VUW(3), XYZ(3)
      INTEGER IRC
      REAL PSTRP, SPOS, ABC(3)
      INTEGER NROM,MAXST,IND
      REAL PPITCH

      NROMD=NROM
         CALL VJWFFW(ILAY,IFAC,IWFF,JWAF)
         CALL VZERO(ABC,3)
         IF(VDYEAR().NE.95.AND.VNRDIR(IVIEW).EQ.-1)THEN
           SPOS = VNSCRM(IVIEW) - EPOS + 1.0
         ELSE
           SPOS = EPOS
         ENDIF
         IRC = VDRSPS(SPOS,IVIEW,PSTRP)
         IF (IRC.NE.1) THEN
           VSPXYZ = IRC
           RETURN
         ENDIF
         IRC =  VDPSLC(PSTRP,IVIEW,ABC(IVIEW))
         IF (IRC.NE.1) THEN
           VSPXYZ = IRC
           RETURN
         ENDIF
         VSPXYZ = VABCVU(ABC,JWAF,VUW)
         CALL VGWFXY(NROMD,VUW,XYZ)
      RETURN
      END
#endif
