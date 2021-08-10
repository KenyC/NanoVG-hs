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

#endif