#ifndef NANOVG_HS_WRAPPER_H
#define NANOVG_HS_WRAPPER_H

#include "nanovg.h"

void nvgFillColorHs(
	NVGcontext* ctx, 
	float r,
	float g,
	float b,
	float a
);

void nvgStrokeColorHs(
	NVGcontext* ctx, 
	float r,
	float g,
	float b,
	float a
);


extern char _defaultFontName[5];


// -- Paint wrappers
void nvgStrokePaintHs(NVGcontext* ctx, NVGpaint *paint);
void nvgFillPaintHs(NVGcontext* ctx, NVGpaint *paint);

NVGpaint* nvgLinearGradientHs(
	NVGcontext* ctx, 
	float sx, float sy, 
	float ex, float ey,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
);

NVGpaint* nvgBoxGradientHs(
	NVGcontext* ctx, 
	float x,  float y, 
	float w,  float h,
	float r,  float f,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
);

NVGpaint* nvgRadialGradientHs(
	NVGcontext* ctx, 
	float cx,   float cy, 
	float inr,  float outr,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
);

NVGpaint* nvgImagePatternHs(
	NVGcontext* ctx, 
	float ox, float oy, 
	float ex, float ey,
	float angle, int image, float alpha
);


#define TEXT_LINES_BUFFER_SIZE 15
struct NVGtextRowIter {
	const char* start;
	const char* current;
	const char* end;
	NVGtextRow buffer_rows[TEXT_LINES_BUFFER_SIZE];
	float breakRowWidth;
	int head;
	int capacity;
};
typedef struct NVGtextRowIter NVGtextRowIter;

struct NVGtextRowHs
{
	int start, end;
	float width;
	float minx, maxx;
};
typedef struct NVGtextRowHs NVGtextRowHs;

int nvgIterTextLines(
	NVGcontext*     ctx, 
	NVGtextRowIter* iter, 
	NVGtextRowHs*   result
);

NVGtextRowIter* nvgStartIterTextLines(
	const char* string, const char* end,
	float breakRowWidth
);


#define TEXT_GLYPHS_BUFFER_SIZE 100
struct NVGtextGlyphIter {
	const char* start;
	const char* current;
	const char* end;
	NVGglyphPosition buffer_glyphs[TEXT_GLYPHS_BUFFER_SIZE];
	float x;
	float y;
	int head;
	int capacity;
};
typedef struct NVGtextGlyphIter NVGtextGlyphIter;

struct NVGtextGlyphHs
{
	int position;
	float x;
	float minx, maxx;
};
typedef struct NVGtextGlyphHs NVGtextGlyphHs;

int nvgIterTextGlyph(
	NVGcontext*       ctx, 
	NVGtextGlyphIter* iter, 
	NVGtextGlyphHs*   result
);

NVGtextGlyphIter* nvgStartIterTextGlyph(
	const char* string, const char* end,
	float x, float y
);



#ifndef RELEASE
void printNvgPaint(NVGpaint* paint);
#endif

#endif
