/* a rowview in a workspace ... part of a tallycolumn, not a separate widget
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

#define ROWVIEW_TYPE (rowview_get_type())
#define ROWVIEW(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), ROWVIEW_TYPE, Rowview))
#define ROWVIEW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), ROWVIEW_TYPE, RowviewClass))
#define IS_ROWVIEW(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), ROWVIEW_TYPE))
#define IS_ROWVIEW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), ROWVIEW_TYPE))
#define ROWVIEW_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), ROWVIEW_TYPE, RowviewClass))

struct _Rowview {
	View view;

	gboolean visible; /* Currently visible */
	int rnum;		  /* Row of subcolumn we are in */

	GtkWidget *spin;  /* Class display open/close widgets */
	GtkWidget *frame; /* Row name box */
	GtkWidget *label; /* Row label */
	Rhsview *rhsview; /* Our rhs */

	/* Last tooltip we set.
	 */
	char *last_tooltip;

	/* Currently set css class.
	 */
	const char *css_class;
};

typedef struct _RowviewClass {
	ViewClass parent_class;

	/* My methods.
	 */
} RowviewClass;

guint rowview_menu_attach(Rowview *rview, GtkWidget *widget);

GType rowview_get_type(void);
gboolean rowview_paste_filename(const char *filename, void *user_data);
View *rowview_new(void);

void rowview_get_position(Rowview *rview, int *x, int *y, int *w, int *h);
void rowview_set_visible(Rowview *rview, gboolean visible);
gboolean rowview_get_visible(Rowview *rview);
Rowview *rowview_get_top(Rowview *rview);
