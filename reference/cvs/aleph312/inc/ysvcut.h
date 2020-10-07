      COMMON/YSVCUT/YSCTPV,YSCTSM,YSCTC2,YSCTRL,YSCTRH,YSCTDR,
     &              YSCTCF,YSCTRM,YSCTCR,SPARE
#ifdef DOC
C!   Common block with cuts for secondary vertex selection
C
C    YSCTPV = at least one track must have log10(chi2_primary_vtx) greater
C    YSCTSM = the vertex must be located at least this far along the track
C             which is most consistent with the primary
C    YSCTC2 = maximum chi^2 of a pair vertex fit
C    YSCTRL = minimum vertex radius^2 to consider
C    YSCTRH = maximum vertex radius^2 to consider
C    YSCTDR = maximum radial separation allowed to merge vertices
C    YSCTCF = maximum chi^2/DOF for final vertex
C    YSCTRM = maximum distance from a layer of material to tag
C	 YSCTCR = min. chi^2 improvement to remove a track from merged vertex
#endif
