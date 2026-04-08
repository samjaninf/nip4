/* run the display for a graphic in a workspace
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

G_DEFINE_TYPE(Graphicview, graphicview, VIEW_TYPE)

static void
graphicview_link(View *view, Model *model, View *parent)
{
	Graphicview *graphicview = GRAPHICVIEW(view);

	VIEW_CLASS(graphicview_parent_class)->link(view, model, parent);

	/* Find the enclosing subcolumnview.
	 */
	View *v;
	for (v = parent; v && !IS_SUBCOLUMNVIEW(v); v = v->parent)
		;
	if (v)
		graphicview->sview = SUBCOLUMNVIEW(v);
}

void
graphicview_pressed(GtkGestureClick *gesture,
	guint n_press, double x, double y, Graphicview *graphicview)
{
	Mainwindow *main = MAINWINDOW(view_get_window(VIEW(graphicview)));

	// don't do single-click-select, it'll trigger for doublelick too and be
	// very annoying
	if (n_press > 1) {
		Model *model = MODEL(VOBJECT(graphicview)->iobject);

		model_edit(model, GTK_WINDOW(main));
	}
}

static void
graphicview_class_init(GraphicviewClass *class)
{
	ViewClass *view_class = (ViewClass *) class;

	view_class->link = graphicview_link;
}

static void
graphicview_init(Graphicview *graphicview)
{
}
