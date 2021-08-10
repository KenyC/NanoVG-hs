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