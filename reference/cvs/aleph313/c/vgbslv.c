#if defined(ALEPH_C)

#include "vgbran.h"

void FORT_CALL(vgbslv) (const int *ntrk, const int nhits[NHITDIM],
			const int nUseHit[][NHITDIM], 
			const int *npat, const struct VGBPAT *pat,
			int *nsoln, int *soln, float *objSoln )
{
   lprec *Mlp;
   int result;
   int col, l;
   int ipat;

   *nsoln = 0;
   
   /* load the LP_SOLVE structure */
   Mlp = vldlps(*ntrk, nhits, nUseHit, *npat, pat);
   if (Mlp == NULL) return;
   
   /* set a few defaults, just to be sure */
   Mlp->anti_degen = FALSE;

   /* write_MPS(Mlp, stderr); */
   
   /* solve!! */
   result = solve(Mlp);

   /* extract the solution */
   if (result == OPTIMAL)
   {
      objSoln[*nsoln] = Mlp->best_solution[0];
      
      col = Mlp->rows + 1;
      for (l=0; l<NHITDIM; l++) col += nhits[l];
      
      for (ipat=0; ipat<*npat; ipat++)
      {
	 if (Mlp->best_solution[col + ipat] > 0.5)
	 {
	    /* be careful about FORTRAN indexing */
	    soln[(*nsoln)*(*ntrk) + pat[ipat].trk - 1] = ipat+1;
	 }
      }
      
      (*nsoln)++;
   }

   /* free the memory */
   delete_lp(Mlp);
}

#endif
