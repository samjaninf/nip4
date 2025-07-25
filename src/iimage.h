/* a ip image class in a workspace
 */

/*

	Copyright (C) 1991-2003 The National Gallery

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License along
	with this program; if not, write to the Free Software Foundation, Inc.,
	51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

 */

/*

	These files are distributed with VIPS - http://www.vips.ecs.soton.ac.uk

 */

#define IIMAGE_TYPE (iimage_get_type())
#define IIMAGE(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), IIMAGE_TYPE, iImage))
#define IIMAGE_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), IIMAGE_TYPE, iImageClass))
#define IS_IIMAGE(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), IIMAGE_TYPE))
#define IS_IIMAGE_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), IIMAGE_TYPE))
#define IIMAGE_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), IIMAGE_TYPE, iImageClass))

struct _iImage {
	Classmodel parent_class;

	/* Class fields.
	 */
	ImageValue value;

	/* List of classmodel which have displays on us.
	 */
	GSList *classmodels;

	/* List of popup imageview windows we've made.
	 */
	GSList *views;

	/* Track display pos/size/etc. here.
	 */
	int image_left; /* Scroll position */
	int image_top;
	int image_mag; /* Scale */

	/* View attachments.
	 */
	gboolean show_status;
	gboolean show_paintbox;
	gboolean show_convert;

	/* Bar settings we apply.
	 */
	ViewSettings view_settings;

	/* Private ... build iobject caption here.
	 */
	VipsBuf caption_buffer;
};

typedef struct _iImageClass {
	ClassmodelClass parent_class;

	/* My methods.
	 */
} iImageClass;

GType iimage_get_type(void);
gboolean iimage_replace(iImage *iimage, const char *filename);
gboolean iimage_replace_imageinfo(iImage *iimage, Imageinfo *ii);
void iimage_update_view_settings(iImage *iimage, ViewSettings *view_settings);
