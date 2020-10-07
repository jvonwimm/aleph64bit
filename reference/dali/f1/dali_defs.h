#ifndef DaliDefs_H
#define DaliDefs_H

typedef int logical;
#ifndef False
#define False 0
#define True 1
#endif /* False */

#define _tolower(c) ((c) >= 'A' && (c) <= 'Z' ? (c) | 0x20:(c))
#define _toupper(c) ((c) >= 'a' && (c) <= 'z' ? (c) & 0x5F:(c))

#ifdef __VMS
struct dsc$descriptor_s
{
	unsigned short  dsc$w_length;
	unsigned char   dsc$b_dtype;
	unsigned char   dsc$b_class;
	unsigned char   *dsc$a_pointer;
};
typedef struct dsc$descriptor_s FortranString;
#else
typedef unsigned char FortranString;
#endif /* VMS */

struct pixmapsize_s
{
	int x;
	int y;
};
typedef struct pixmapsize_s Pixmapsize;

#define dtxth 3
#define dtxtv 15
#define dtxthm 2
#define dtxtvm 14
#define hmax 20.0
#define vmax 22.0

#if defined(__cplusplus) || defined(c_plusplus)
#define CXX extern "C"
#else
#define CXX
#endif

#endif /* DaliDefs_H */
