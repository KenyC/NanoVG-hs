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

#ifndef RELEASE
void printNvgPaint(NVGpaint* paint);
#endif

#endif
