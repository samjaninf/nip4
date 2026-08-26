/* region manipulation on a imageui
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

#ifndef __IMAGEUIREGION_H
#define __IMAGEUIREGION_H

#define IMAGEUIREGION_TYPE (imageuiregion_get_type())
#define IMAGEUIREGION(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), IMAGEUIREGION_TYPE, Imageuiregion))
#define IMAGEUIREGION_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), IMAGEUIREGION_TYPE, ImageuiregionClass))
#define IS_IMAGEUIREGION(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), IMAGEUIREGION_TYPE))
#define IS_IMAGEUIREGION_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), IMAGEUIREGION_TYPE))
#define IMAGEUIREGION_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), IMAGEUIREGION_TYPE, ImageuiregionClass))

G_DECLARE_FINAL_TYPE(Imageuiregion, imageuiregion, NIP4, IMAGEUIREGION, GObject)

Imageui *imageuiregion_get_imageui(Imageuiregion *imageuiregion);

void imageuiregion_add_regionview(Imageuiregion *imageuiregion,
	Regionview *regionview);
void imageuiregion_remove_regionview(Imageuiregion *imageuiregion,
	Regionview *regionview);

gboolean imageuiregion_snap_point(Imageuiregion *imageuiregion,
	int x, int y, int *sx, int *sy);
gboolean imageuiregion_snap_rect(Imageuiregion *imageuiregion,
	VipsRect *in, VipsRect *out);

Regionview *imageuiregion_pick_regionview(Imageuiregion *imageuiregion,
	int x, int y);

Imageuiregion *imageuiregion_new(void);

#endif /* __IMAGEUIREGION_H */
