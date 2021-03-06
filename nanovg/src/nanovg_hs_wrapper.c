#include <stdlib.h>
#include <stdio.h>
#include "debug.h"

#include "nanovg.h"
#include "nanovg_gl.h"
#include "nanovg_hs_wrapper.h"

void nvgFillColorHs(
	NVGcontext* ctx, 
	float r,
	float g,
	float b,
	float a
)
{
	nvgFillColor(ctx, (NVGcolor) {
		.r = r, 
		.g = g, 
		.b = b, 
		.a = a
	});
}

void nvgStrokeColorHs(
	NVGcontext* ctx, 
	float r,
	float g,
	float b,
	float a
)
{
	nvgStrokeColor(ctx, (NVGcolor) {
		.r = r, 
		.g = g, 
		.b = b, 
		.a = a
	});
}

// -- Paint wrappers
void nvgStrokePaintHs(NVGcontext* ctx, NVGpaint *paint) {
	nvgStrokePaint(ctx, *paint);
}

void nvgFillPaintHs(NVGcontext* ctx, NVGpaint* paint) {
	LOG("%p %p", ctx, paint);
	nvgFillPaint(ctx, *paint);
}

NVGpaint* nvgLinearGradientHs(
	NVGcontext* ctx, 
	float sx, float sy, 
	float ex, float ey,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
) {
	NVGpaint* to_return = malloc(sizeof(NVGpaint));
	*to_return = nvgLinearGradient(
		ctx, 
		sx, sy, 
		ex, ey,
		(NVGcolor) {.r =  icol_r, .g =  icol_g, .b =  icol_b, .a =  icol_a}, 
		(NVGcolor) {.r =  ocol_r, .g =  ocol_g, .b =  ocol_b, .a =  ocol_a}
	);
	LOG("%f %f %f %f\n",
		 to_return -> radius,
		 to_return -> extent[0],
		 to_return -> extent[1],
		 to_return -> innerColor.r
	);
	return to_return;
}

NVGpaint* nvgBoxGradientHs(
	NVGcontext* ctx, 
	float x,  float y, 
	float w,  float h,
	float r,  float f,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
) {
	NVGpaint* to_return = malloc(sizeof(NVGpaint));
	*to_return = nvgBoxGradient(
		ctx, 
		x, y, 
		w, h,
		r, f,
		(NVGcolor) {.r =  icol_r, .g =  icol_g, .b =  icol_b, .a =  icol_a}, 
		(NVGcolor) {.r =  ocol_r, .g =  ocol_g, .b =  ocol_b, .a =  ocol_a}
	);
	return to_return;
}

NVGpaint* nvgRadialGradientHs(
	NVGcontext* ctx, 
	float cx,   float cy, 
	float inr,  float outr,
	float icol_r, float icol_g, float icol_b, float icol_a,  
	float ocol_r, float ocol_g, float ocol_b, float ocol_a
) {
	NVGpaint* to_return = malloc(sizeof(NVGpaint));
	*to_return = nvgRadialGradient(
		ctx, 
		cx, cy, 
		inr, outr,
		(NVGcolor) {.r =  icol_r, .g =  icol_g, .b =  icol_b, .a =  icol_a}, 
		(NVGcolor) {.r =  ocol_r, .g =  ocol_g, .b =  ocol_b, .a =  ocol_a}
	);
	return to_return;
}


char _defaultFontName[5] = "font\0";

#ifndef RELEASE
void printNvgPaint(NVGpaint* paint) {
	LOG("%p\n", paint);
	printf("%f %f %f %f %f %f\n", 
		paint -> xform[0], paint -> xform[1], paint -> xform[2], 
		paint -> xform[3], paint -> xform[4], paint -> xform[5]
	);


	printf("%f %f\n", 
		paint -> extent[0], paint -> extent[1]
	);

	printf("%f\n", paint -> radius);
	printf("%f\n", paint -> feather);
	printf("%f %f %f %f\n", 
		paint -> innerColor.r, 
		paint -> innerColor.g, 
		paint -> innerColor.b, 
		paint -> innerColor.a
	);
	printf("%f %f %f %f\n", 
		paint -> outerColor.r, 
		paint -> outerColor.g, 
		paint -> outerColor.b, 
		paint -> outerColor.a
	);
	printf("%d\n", paint -> image);
}
#endif


// Creates and returns an image pattern. Parameters (ox,oy) specify the left-top location of the image pattern,
// (ex,ey) the size of one image, angle rotation around the top-left corner, image is handle to the image to render.
// The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
NVGpaint* nvgImagePatternHs(
	NVGcontext* ctx, 
	float ox, float oy, 
	float ex, float ey,
	float angle, 
	int image, 
	float alpha
) {
	NVGpaint* to_return = malloc(sizeof(NVGpaint));
	*to_return = nvgImagePattern(
		ctx,
		ox, oy,
		ex, ey,
		angle, image, alpha
	);
	return to_return;
}


/**************************************
Text Metrics Iterator
***************************************/



NVGtextRowIter* nvgStartIterTextLines(
	const char* string, const char* end,
	float breakRowWidth
) 
{
	NVGtextRowIter* to_return  = malloc(sizeof(NVGtextRowIter));
	to_return -> start         = string;
	to_return -> current       = string;
	to_return -> end           = end;
	to_return -> breakRowWidth = breakRowWidth;
	to_return -> head          = 0;
	to_return -> capacity      = 0;
	return to_return;
}


int nvgIterTextLines(
	NVGcontext*     ctx, 
	NVGtextRowIter* iter, 
	NVGtextRowHs*   result
) 
{
	NVGtextRow row;
	if(iter -> head < iter -> capacity) {
		// Draw from buffer
		row = iter -> buffer_rows[iter -> head++];
	}
	else {
		// load some more
		iter -> capacity = nvgTextBreakLines(
			ctx,
			iter -> current, iter -> end,
			iter -> breakRowWidth,
			iter -> buffer_rows,
			TEXT_LINES_BUFFER_SIZE
		);
		if(iter -> capacity == 0) return 0;
		iter -> current = iter -> buffer_rows[iter -> capacity - 1].next;
		iter -> head    = 1;
		row = iter -> buffer_rows[0];
	}



	result -> start  = row.start - iter -> start;
	result -> end    = row.end   - iter -> start;
	result -> width  = row.width;
	result -> minx   = row.minx; 
	result -> maxx   = row.maxx;

	return 1; 
}



NVGtextGlyphIter* nvgStartIterTextGlyph(
	const char* string, const char* end,
	float x, float y
)
{
	NVGtextGlyphIter* to_return  = malloc(sizeof(NVGtextGlyphIter));
	to_return -> start     = string;
	to_return -> current   = string;
	to_return -> end       = end;
	to_return -> x         = x;
	to_return -> y         = y;
	to_return -> head      = 0;
	to_return -> capacity  = 0;
	return to_return;
}


int nvgIterTextGlyph(
	NVGcontext*       ctx, 
	NVGtextGlyphIter* iter, 
	NVGtextGlyphHs*   result
)
{
	NVGglyphPosition glyph_pos;

	if(iter -> head < iter -> capacity) {
		// Draw from buffer
		glyph_pos = iter -> buffer_glyphs[iter -> head++];
	}
	else if (
		iter -> capacity != 0 &&
		iter -> capacity < TEXT_GLYPHS_BUFFER_SIZE
	) {
		return 0;
	}
	else {
		int firstLoad = iter -> start == iter -> current;
		// load some more
		LOG("Loading some more! From %p\n", iter -> current);
		iter -> capacity = nvgTextGlyphPositions(
			ctx,
			iter -> x, iter -> y,
			iter -> current, iter -> end,
			iter -> buffer_glyphs,
			TEXT_GLYPHS_BUFFER_SIZE
		);
		if(iter -> capacity == 0) return 0;
		iter -> current = iter -> buffer_glyphs[iter -> capacity - 1].str;
		iter -> x       = iter -> buffer_glyphs[iter -> capacity - 1].x;

		if(firstLoad) {
			iter -> head    = 1;
			glyph_pos = iter -> buffer_glyphs[0];
		}
		else {
			iter -> head    = 2;
			glyph_pos = iter -> buffer_glyphs[1];
		}
	}

	result -> position  = glyph_pos.str - iter -> start;
	result -> x         = glyph_pos.x;
	result -> minx      = glyph_pos.minx; 
	result -> maxx      = glyph_pos.maxx;

	return 1; 
}
