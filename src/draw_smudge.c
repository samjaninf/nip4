/* Smudge a region on an image.
 *
 * Copied from libvips to get a direct draw path,
 */

#include <stdio.h>
#include <stdlib.h>

#include <vips/vips.h>

#define SMUDGE(TYPE) \
	for (int y = 0; y < clip.height; y++) { \
		TYPE *q = (TYPE *) VIPS_IMAGE_ADDR(image, clip.left, clip.top + y); \
		TYPE *p = q - lskip - bands; \
\
		for (int x = 0; x < clip.width; x++) { \
			for (int b = 0; b < bands; b++) \
				total[b] = 0.0; \
\
			TYPE *p1 = p; \
			for (int y1 = 0; y1 < 3; y1++) { \
				TYPE *p2 = p1; \
				for (int x1 = 0; x1 < 3; x1++) \
					for (int b = 0; b < bands; b++) \
						total[b] += *p2++; \
\
				p1 += lskip; \
			} \
\
			for (int b = 0; b < bands; b++) \
				q[b] = (23 * q[b] + total[b] + 16) / 32; \
\
			p += bands; \
			q += bands; \
		} \
	}

int
draw_smudge(VipsImage *image, VipsRect *area)
{
	/* Double bands for complex images.
	 */
	int bands = vips_image_get_bands(image) *
		(vips_band_format_iscomplex(vips_image_get_format(image)) ? 2 : 1);
	int lskip = bands * vips_image_get_width(image);

	/* Don't do the margins.
	 */
	VipsRect clip = {0, 0, image->Xsize, image->Ysize};
	vips_rect_marginadjust(&clip, -1);

	vips_rect_intersectrect(area, &clip, &clip);
	if (vips_rect_isempty(&clip))
		return 0;

	double *total;
	if (!(total = VIPS_ARRAY(image, bands, double)))
		return -1;

	switch (vips_image_get_format(image)) {
	case VIPS_FORMAT_UCHAR:
		SMUDGE(unsigned char);
		break;
	case VIPS_FORMAT_CHAR:
		SMUDGE(char);
		break;
	case VIPS_FORMAT_USHORT:
		SMUDGE(unsigned short);
		break;
	case VIPS_FORMAT_SHORT:
		SMUDGE(short);
		break;
	case VIPS_FORMAT_UINT:
		SMUDGE(unsigned int);
		break;
	case VIPS_FORMAT_INT:
		SMUDGE(int);
		break;
	case VIPS_FORMAT_FLOAT:
		SMUDGE(float);
		break;
	case VIPS_FORMAT_DOUBLE:
		SMUDGE(double);
		break;
	case VIPS_FORMAT_COMPLEX:
		SMUDGE(float);
		break;
	case VIPS_FORMAT_DPCOMPLEX:
		SMUDGE(double);
		break;

	default:
		g_assert_not_reached();
	}

	return 0;
}
