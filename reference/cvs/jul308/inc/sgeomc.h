      PARAMETER (MLASG=9, MGRSG=4, MMDSG=3, MSCSG=2)
      COMMON /SGEOMC/RZERSG,RDELSG,ZZERSG,ZDELSG,ZOFFSG,
     +               PHIBSG(MLASG),PHIOSG,TUBESG,SECDSG,GASDSG,
     +               MLAYSG,MSECSG,MWIRSG,MDORSG(MLASG),MCRASG,
     +               MCRDSG,MTDCSG,MCHASG,KSECSG(MSCSG,MGRSG,MMDSG)
      INTEGER MLAYSG,MSECSG,MWIRSG,MDORSG,MCRASG,MCRDSG,MTDCSG,MCHASG,
     +        KSECSG
      REAL    RZERSG,RDELSG,ZZERSG,ZDELSG,ZOFFSG,PHIBSG,PHIOSG,TUBESG,
     +        SECDSG,GASDSG
#if defined(DOC)
C
C!         SATR detector geometry and electronics descriptions
C
C MLASG    : number of SATR detector layers
C MGRSG    : number of TDC cards per SATR layer
C MMDSG    : number of SATR wire oring modes
C MSCSG    : number of SATR wires ored per electronics channel
C RZERSG   : distance beam line - first tube
C RDELSG   : distance between two wires
C ZZERSG   : z-position of Al frame surface
C ZDELSG   : distance between two layers
C ZOFFSG   : distance between Al frame surface and last layer wire
C PHIBSGi  : left edge azimuth of sector 1, layer i
C PHIOSG   : azimuth offset, added when z  ----> -z
C TUBESG   : thickness of brass tube material
C SECDSG   : dead zone between two inner sectors, per sector
C GASDSG   : dead zone at the edges of a half layer
C MLAYSG   : number of layers in each half part
C MSECSG   : number of sectors per layer
C MWIRSG   : number of wires per sector
C MDORSGi  : mode of oring in layer i
C MCRASG   : number of crates
C MCRDSG   : number of cards per crate
C MTDCSG   : number of TDC's per card
C MCHASG   : number of TDC channels per TDC
C KSECSGijk: sector oring map, two sectors for group j, oring mode k
#endif
