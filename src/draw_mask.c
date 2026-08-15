/* Draw a mask on an image.
 *
 * Copied into nip4 to get a direct draw path,
 */

#include <stdio.h>
#include <stdlib.h>

#include <vips/vips.h>

static void
LabQ2Lab(float *q, VipsPel *p, int width)
{
    int l;
    int lsbs;

    for (int i = 0; i < width; i++) {
        /* Get extra bits.
         */
        lsbs = p[3];

        /* Build L.
         */
        l = p[0];
        l = (l << 2) | (lsbs >> 6);
        q[0] = (float) l * (100.0F / 1023.0F);

        /* Build a.
         */
        l = (((signed char *) p)[1] << 3) | ((lsbs >> 3) & 0x7);
        q[1] = (float) l * 0.125F;

        /* And b.
         */
        l = (((signed char *) p)[2] << 3) | (lsbs & 0x7);
        q[2] = (float) l * 0.125F;

        p += 4;
        q += 3;
    }
}

static void
Lab2LabQ(VipsPel *q, float *p, int width)
{
    for (int i = 0; i < width; i++) {
        float fval;
        int lsbs;
        int intv;

        /* Scale L up to 10 bits.
         */
        intv = VIPS_ROUND_UINT(10.23 * p[0]);
        intv = VIPS_CLIP(0, intv, 1023);
        lsbs = (intv & 0x3) << 6; /* 00000011 -> 11000000 */
        q[0] = intv >> 2;         /* drop bot 2 bits and store */

        fval = 8.0F * p[1]; /* do a */
        intv = rintf(fval);
        intv = VIPS_CLIP(-1024, intv, 1023);
        lsbs |= (intv & 0x7) << 3; /* 00000111 -> 00111000 */
        q[1] = intv >> 3;          /* drop bot 3 bits & store */

        fval = 8.0F * p[2]; /* do b */
        intv = rintf(fval);
        intv = VIPS_CLIP(-1024, intv, 1023);
        lsbs |= (intv & 0x7);
        q[2] = intv >> 3;

        q[3] = lsbs; /* store lsb band */

        p += 3;
        q += 4;
    }
}

/* Paint ink into an 8 or 16 bit integer image.
 */
#define IBLEND(TYPE, TO, INK) \
	{ \
		TYPE *tto = (TYPE *) (TO); \
		TYPE *tink = (TYPE *) (INK); \
\
		int x, j; \
\
		for (j = 0, x = 0; x < width; x++) \
			for (int i = 0; i < bands; i++, j++) \
				tto[j] = (tink[i] * m[x] + tto[j] * (255 - m[x])) / 255; \
	}

/* Do the blend with doubles.
 */
#define DBLEND(TYPE, TO, INK) \
	{ \
		TYPE *tto = (TYPE *) (TO); \
		TYPE *tink = (TYPE *) (INK); \
\
		int x, j; \
\
		for (j = 0, x = 0; x < width; x++) \
			for (int i = 0; i < bands; i++, j++) \
				tto[j] = ((double) tink[i] * m[x] + \
					(double) tto[j] * (255 - m[x])) / 255; \
	}

/* Blend of complex.
 */
#define CBLEND(TYPE, TO, INK) \
	{ \
		TYPE *tto = (TYPE *) (TO); \
		TYPE *tink = (TYPE *) (INK); \
\
		int x, j; \
\
		for (j = 0, x = 0; x < width; x++) \
			for (int i = 0; i < bands * 2; i += 2, j += 2) { \
				tto[j] = \
					((double) tink[i] * m[x] + \
						(double) tto[j] * (255 - m[x])) / \
					255; \
				tto[j + 1] = \
					((double) tink[i + 1] * m[x] + \
						(double) tto[j + 1] * (255 - m[x])) / \
					255; \
			} \
	}

static int
draw_mask_draw_labq(VipsImage *image, VipsImage *mask, VipsPel *ink,
	VipsRect *image_clip, VipsRect *mask_clip)
{
	int width = image_clip->width;
	int height = image_clip->height;
	int bands = 3;

	float *lab_buffer;
	float lab_ink[3];

	if (!(lab_buffer = VIPS_ARRAY(NULL, width * 3, float)))
		return -1;

	LabQ2Lab(lab_ink, ink, 1);
	for (int y = 0; y < height; y++) {
		VipsPel *to =
			VIPS_IMAGE_ADDR(image, image_clip->left, y + image_clip->top);
		VipsPel *m =
			VIPS_IMAGE_ADDR(mask, mask_clip->left, y + mask_clip->top);

		LabQ2Lab(lab_buffer, to, width);
		DBLEND(float, lab_buffer, lab_ink);
		Lab2LabQ(to, lab_buffer, width);
	}

	g_free(lab_buffer);

	return 0;
}

static int
draw_mask_draw(VipsImage *image, VipsImage *mask, VipsPel *ink,
	VipsRect *image_clip, VipsRect *mask_clip)
{
	int width = image_clip->width;
	int height = image_clip->height;
	int bands = image->Bands;

	for (int y = 0; y < height; y++) {
		VipsPel *to =
			VIPS_IMAGE_ADDR(image, image_clip->left, y + image_clip->top);
		VipsPel *m =
			VIPS_IMAGE_ADDR(mask, mask_clip->left, y + mask_clip->top);

		switch (image->BandFmt) {
		case VIPS_FORMAT_UCHAR:
			IBLEND(unsigned char, to, ink);
			break;

		case VIPS_FORMAT_CHAR:
			IBLEND(signed char, to, ink);
			break;

		case VIPS_FORMAT_USHORT:
			IBLEND(unsigned short, to, ink);
			break;

		case VIPS_FORMAT_SHORT:
			IBLEND(signed short, to, ink);
			break;

		case VIPS_FORMAT_UINT:
			DBLEND(unsigned int, to, ink);
			break;

		case VIPS_FORMAT_INT:
			DBLEND(signed int, to, ink);
			break;

		case VIPS_FORMAT_FLOAT:
			DBLEND(float, to, ink);
			break;

		case VIPS_FORMAT_DOUBLE:
			DBLEND(double, to, ink);
			break;

		case VIPS_FORMAT_COMPLEX:
			CBLEND(float, to, ink);
			break;

		case VIPS_FORMAT_DPCOMPLEX:
			CBLEND(double, to, ink);
			break;

		default:
			g_assert_not_reached();
		}
	}

	return 0;
}

/* Direct path for draw-mask-along-line or draw-mask-along-circle. We want to
 * avoid function dispatch overhead.
 */
int
draw_mask(VipsImage *image, VipsPel *ink, VipsImage *mask, int x, int y)
{
	VipsRect image_rect;
	VipsRect area_rect;
	VipsRect image_clip;
	VipsRect mask_clip;

	if (vips_check_coding_noneorlabq("draw_mask", image) ||
		vips_image_inplace(image) ||
		vips_image_wio_input(mask) ||
		vips_check_mono("draw_mask", mask) ||
		vips_check_uncoded("draw_mask", mask) ||
		vips_check_format("draw_mask", mask, VIPS_FORMAT_UCHAR))
		return -1;

	/* Find the area we draw on the image.
	 */
	area_rect.left = x;
	area_rect.top = y;
	area_rect.width = mask->Xsize;
	area_rect.height = mask->Ysize;
	image_rect.left = 0;
	image_rect.top = 0;
	image_rect.width = image->Xsize;
	image_rect.height = image->Ysize;
	vips_rect_intersectrect(&area_rect, &image_rect, &image_clip);

	/* And the area of the mask image we use.
	 */
	mask_clip = image_clip;
	mask_clip.left -= x;
	mask_clip.top -= y;

	if (!vips_rect_isempty(&image_clip))
		switch (image->Coding) {
		case VIPS_CODING_LABQ:
			if (draw_mask_draw_labq(image, mask, ink, &image_clip, &mask_clip))
				return -1;
			break;

		case VIPS_CODING_NONE:
			if (draw_mask_draw(image, mask, ink, &image_clip, &mask_clip))
				return -1;
			break;

		default:
			g_assert_not_reached();
		}

	return 0;
}
