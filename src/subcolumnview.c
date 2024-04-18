/* a subcolumnview button in a workspace
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

/*
#define DEBUG
 */

#include "nip4.h"

G_DEFINE_TYPE(Subcolumnview, subcolumnview, VIEW_TYPE)

static void *
subcolumnview_destroy_sub(Rowview *rview, Subcolumnview *sview)
{
	UNPARENT(rview);

	return NULL;
}

static void
subcolumnview_dispose(GObject *object)
{
	Subcolumnview *sview;

#ifdef DEBUG
	printf("subcolumnview_dispose\n");
#endif /*DEBUG*/

	g_return_if_fail(object != NULL);
	g_return_if_fail(IS_SUBCOLUMNVIEW(object));

	sview = SUBCOLUMNVIEW(object);

	// UNPARENT(sview->group);

	/* Destroying us won't automatically destroy our rowviews, since they
	 * are not true child-widgets. Do it by hand.
	 */
	(void) view_map(VIEW(sview),
		(view_map_fn) subcolumnview_destroy_sub, sview, NULL);
	UNPARENT(sview->grid);

	G_OBJECT_CLASS(subcolumnview_parent_class)->dispose(object);
}

static void
subcolumnview_link(View *view, Model *model, View *parent)
{
	Subcolumnview *sview = SUBCOLUMNVIEW(view);
	Subcolumn *scol = SUBCOLUMN(model);

#ifdef DEBUG
	printf("subcolumnview_link: ");
	if (HEAPMODEL(scol)->row)
		row_name_print(HEAPMODEL(scol)->row);
	else
		printf("(null)");
	printf("\n");
#endif /*DEBUG*/

	VIEW_CLASS(subcolumnview_parent_class)->link(view, model, parent);

	/* Add to enclosing column, if there is one. Attached to enclosing row
	 * by rowview_refresh() if we're a subcolumn.
	 */
	if (!scol->is_top)
		sview->rhsview = RHSVIEW(parent);
}

static void *
subcolumnview_refresh_sub(Rowview *rview, Subcolumnview *sview)
{
	Subcolumn *scol = SUBCOLUMN(VOBJECT(sview)->iobject);
	Row *row = ROW(VOBJECT(rview)->iobject);
	int i;

	/* Most predicates need a sym.
	 */
	if (!row->sym)
		return NULL;

	for (i = 0; i <= scol->vislevel; i++)
		if (subcolumn_visibility[i].pred(row)) {
			rowview_set_visible(rview, TRUE);
			sview->nvis++;
			break;
		}
	if (i > scol->vislevel)
		rowview_set_visible(rview, FALSE);

	return NULL;
}

static void
subcolumnview_refresh(vObject *vobject)
{
	Subcolumnview *sview = SUBCOLUMNVIEW(vobject);
	Subcolumn *scol = SUBCOLUMN(VOBJECT(sview)->iobject);
	int model_rows = icontainer_get_n_children(ICONTAINER(scol));
	int old_nvis = sview->nvis;
	gboolean editable = scol->top_col->ws->mode != WORKSPACE_MODE_NOEDIT;

#ifdef DEBUG
	printf("subcolumnview_refresh\n");
#endif /*DEBUG*/

	sview->rows = model_rows;

	/* Top-level subcolumns look different in no-edit mode.
	 */
	int spacing = scol->is_top && editable ? 0 : 5;
	gtk_grid_set_row_spacing(GTK_GRID(sview->grid), spacing);
	gtk_grid_set_col_spacing(GTK_GRID(sview->grid), spacing);

	/* Nested subcols: we just change the left indent.
	if (!scol->is_top && editable) {
		gtk_alignment_set_padding(GTK_ALIGNMENT(sview->align),
			0, 0, 0, 0);
	}
	else if (!scol->is_top && !editable) {
		gtk_alignment_set_padding(GTK_ALIGNMENT(sview->align),
			0, 0, 15, 0);
	}
	 */

	sview->nvis = 0;
	(void) view_map(VIEW(sview),
		(view_map_fn) subcolumnview_refresh_sub, sview, NULL);

	if (sview->nvis != old_nvis) {
		view_resize(VIEW(sview));
		iobject_changed(IOBJECT(scol->top_col));
	}

	VOBJECT_CLASS(subcolumnview_parent_class)->refresh(vobject);
}

static void
subcolumnview_class_init(SubcolumnviewClass *class)
{
	GObjectClass *object_class = (GObjectClass *) class;
	vObjectClass *vobject_class = (vObjectClass *) class;
	ViewClass *view_class = (ViewClass *) class;

	object_class->dispose = subcolumnview_dispose;

	vobject_class->refresh = subcolumnview_refresh;

	view_class->link = subcolumnview_link;
}

static void
subcolumnview_init(Subcolumnview *sview)
{
	sview->rhsview = NULL;
	sview->rows = 0;
	sview->nvis = 0;

	/*
	sview->align = gtk_alignment_new(0, 0, 1, 1);
	gtk_box_pack_start(GTK_BOX(sview), sview->align, FALSE, FALSE, 0);
	 */

	/*
	sview->grid = gtk_table_new(sview->rows, 4, FALSE);
	gtk_container_add(GTK_CONTAINER(sview->align), sview->table);

	gtk_widget_show_all(sview->align);

	sview->group = gtk_size_group_new(GTK_SIZE_GROUP_HORIZONTAL);
	 */
}

View *
subcolumnview_new(void)
{
	Subcolumnview *sview = g_object_new(SUBCOLUMNVIEW_TYPE, NULL);

	return VIEW(sview);
}
