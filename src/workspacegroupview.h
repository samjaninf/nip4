/* A view for a Workspacegroup (a set of workspaces) ... display as a notebook.
 */

/*

	Copyright (C) 1991-2003 The National Gallery
	Copyright (C) 2004-2023 libvips.org

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

#define WORKSPACEGROUPVIEW_TYPE (workspacegroupview_get_type())
#define WORKSPACEGROUPVIEW(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), WORKSPACEGROUPVIEW_TYPE, \
		Workspacegroupview))
#define WORKSPACEGROUPVIEW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), WORKSPACEGROUPVIEW_TYPE, \
		WorkspacegroupviewClass))
#define IS_WORKSPACEGROUPVIEW(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), WORKSPACEGROUPVIEW_TYPE))
#define IS_WORKSPACEGROUPVIEW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), WORKSPACEGROUPVIEW_TYPE))
#define WORKSPACEGROUPVIEW_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), WORKSPACEGROUPVIEW_TYPE, \
		WorkspacegroupviewClass))

struct _Workspacegroupview {
	View parent_object;

	GtkWidget *notebook;

	GtkWidget *workspace_menu;
	GtkWidget *column_menu;
	GtkWidget *row_menu;

	GMenu *workspacegroup_column_menu;
};

typedef struct _WorkspacegroupviewClass {
	ViewClass parent_class;

	/* My methods.
	 */
} WorkspacegroupviewClass;

GType workspacegroupview_get_type(void);
View *workspacegroupview_new(void);
