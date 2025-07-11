/* the label on a workspace tab
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

#include "nip4.h"

/*
#define DEBUG
 */

struct _Workspaceviewlabel {
	GtkWidget parent_instance;

	/* The workspaceview we are a label for.
	 */
	Workspaceview *wview;

	/* Widgets.
	 */
	GtkWidget *top;
	GtkWidget *name_edit_stack;
	GtkWidget *label;
	GtkWidget *label_edit;
	GtkWidget *lock;
	GtkWidget *error;
	GtkWidget *right_click_menu;
	GMenu *workspaceviewlabel_menu;

	/* State.
	 */
	gboolean edit;
};

G_DEFINE_TYPE(Workspaceviewlabel, workspaceviewlabel, GTK_TYPE_WIDGET);

enum {
	PROP_WORKSPACEVIEW = 1,
	PROP_EDIT,

	SIG_LAST
};

static void
workspaceviewlabel_dispose(GObject *object)
{
	Workspaceviewlabel *wviewlabel = (Workspaceviewlabel *) object;

#ifdef DEBUG
	printf("workspaceviewlabel_dispose:\n");
#endif /*DEBUG*/

	gtk_widget_dispose_template(GTK_WIDGET(wviewlabel),
		WORKSPACEVIEWLABEL_TYPE);

	G_OBJECT_CLASS(workspaceviewlabel_parent_class)->dispose(object);
}

static void
workspaceviewlabel_set_property(GObject *object,
	guint prop_id, const GValue *value, GParamSpec *pspec)
{
	Workspaceviewlabel *wviewlabel = (Workspaceviewlabel *) object;

	switch (prop_id) {
	case PROP_WORKSPACEVIEW:
		wviewlabel->wview = g_value_get_object(value);
		break;

	case PROP_EDIT:
		if (wviewlabel->edit != g_value_get_boolean(value)) {
			wviewlabel->edit = g_value_get_boolean(value);
			workspaceviewlabel_refresh(wviewlabel);

			if (wviewlabel->edit)
				ientry_grab_focus(IENTRY(wviewlabel->label_edit));
		}
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
workspaceviewlabel_get_property(GObject *object,
	guint prop_id, GValue *value, GParamSpec *pspec)
{
	Workspaceviewlabel *wviewlabel = (Workspaceviewlabel *) object;

	switch (prop_id) {
	case PROP_WORKSPACEVIEW:
		g_value_set_object(value, wviewlabel->wview);
		break;

	case PROP_EDIT:
		g_value_set_boolean(value, wviewlabel->edit);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
workspaceviewlabel_init(Workspaceviewlabel *wviewlabel)
{
#ifdef DEBUG
	printf("workspaceviewlabel_init:\n");
#endif /*DEBUG*/

	gtk_widget_init_template(GTK_WIDGET(wviewlabel));
}

static void *
workspaceviewlabel_add_tab_item(Workspace *ws, void *user_data)
{
	GMenu *tabs = G_MENU(user_data);

	GMenuItem *item = g_menu_item_new(IOBJECT(ws)->name, NULL);
	GVariant *target = g_variant_new_string(IOBJECT(ws)->name);
	g_menu_item_set_action_and_target_value(item, "win.tab-merge", target);
	g_menu_append_item(tabs, item);

	return NULL;
}

static void
workspaceviewlabel_menu(GtkGestureClick *gesture,
	guint n_press, double x, double y, Workspaceviewlabel *wviewlabel)
{
	Workspaceview *wview = wviewlabel->wview;
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);
	Workspacegroup *wsg = workspace_get_workspacegroup(ws);

	// generate the dynamic tab submenu
	GMenu *tabs = g_menu_new();
	workspacegroup_map(wsg, workspaceviewlabel_add_tab_item, tabs, NULL);
	GMenu *tab_menu = wviewlabel->workspaceviewlabel_menu;
	g_menu_remove(tab_menu, 1);
	g_menu_insert_submenu(tab_menu, 1, "Merge tab", G_MENU_MODEL(tabs));

	mainwindow_set_action_view(VIEW(wviewlabel->wview));

	gtk_popover_set_pointing_to(GTK_POPOVER(wviewlabel->right_click_menu),
		&(const GdkRectangle){ x, y, 1, 1 });

	gtk_popover_popup(GTK_POPOVER(wviewlabel->right_click_menu));
}

static void
workspaceviewlabel_pressed(GtkGestureClick *gesture,
	guint n_press, double x, double y, Workspaceviewlabel *wviewlabel)
{
	if (n_press == 2)
		g_object_set(wviewlabel, "edit", TRUE, NULL);
}

static void
workspaceviewlabel_name_edit_activate(iEntry *self, gpointer user_data)
{
	Workspaceviewlabel *wviewlabel = (Workspaceviewlabel *) user_data;
	Workspaceview *wview = wviewlabel->wview;
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);

	g_autofree char *text = NULL;
	g_object_get(self, "text", &text, NULL);
	if (text) {
		workspace_rename(ws, text);
	}

	g_object_set(wviewlabel, "edit", FALSE, NULL);
}

static void
workspaceviewlabel_name_edit_cancel(iEntry *self, gpointer user_data)
{
	Workspaceviewlabel *wviewlabel = (Workspaceviewlabel *) user_data;

	g_object_set(wviewlabel, "edit", FALSE, NULL);
}

static void
workspaceviewlabel_error_pressed(GtkGestureClick *gesture,
	guint n_press, double x, double y, Workspaceviewlabel *wviewlabel)
{
	Workspaceview *wview = wviewlabel->wview;
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);

	(void) workspace_next_error(ws);
	workspace_show_error(ws);
}

static void
workspaceviewlabel_class_init(WorkspaceviewlabelClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

#ifdef DEBUG
	printf("workspaceviewlabel_class_init:\n");
#endif /*DEBUG*/

	G_OBJECT_CLASS(class)->dispose = workspaceviewlabel_dispose;

	BIND_RESOURCE("workspaceviewlabel.ui");
	BIND_LAYOUT();

	BIND_VARIABLE(Workspaceviewlabel, top);
	BIND_VARIABLE(Workspaceviewlabel, name_edit_stack);
	BIND_VARIABLE(Workspaceviewlabel, label);
	BIND_VARIABLE(Workspaceviewlabel, label_edit);
	BIND_VARIABLE(Workspaceviewlabel, lock);
	BIND_VARIABLE(Workspaceviewlabel, error);
	BIND_VARIABLE(Workspaceviewlabel, right_click_menu);
	BIND_VARIABLE(Workspaceviewlabel, workspaceviewlabel_menu);

	BIND_CALLBACK(workspaceviewlabel_menu);
	BIND_CALLBACK(workspaceviewlabel_pressed);
	BIND_CALLBACK(workspaceviewlabel_name_edit_cancel);
	BIND_CALLBACK(workspaceviewlabel_name_edit_activate);
	BIND_CALLBACK(workspaceviewlabel_error_pressed);

	gobject_class->dispose = workspaceviewlabel_dispose;
	gobject_class->set_property = workspaceviewlabel_set_property;
	gobject_class->get_property = workspaceviewlabel_get_property;

	g_object_class_install_property(gobject_class, PROP_WORKSPACEVIEW,
		g_param_spec_object("workspaceview",
			_("Workspaceview"),
			_("The workspaceview we are for"),
			WORKSPACEVIEW_TYPE,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_EDIT,
		g_param_spec_boolean("edit",
			_("edit"),
			_("Edit tab name"),
			FALSE,
			G_PARAM_READWRITE));
}

Workspaceviewlabel *
workspaceviewlabel_new(Workspaceview *wview)
{
	Workspaceviewlabel *wviewlabel;

#ifdef DEBUG
	printf("workspaceviewlabel_new:\n");
#endif /*DEBUG*/

	wviewlabel = g_object_new(workspaceviewlabel_get_type(),
		"workspaceview", wview,
		NULL);

	return wviewlabel;
}

void
workspaceviewlabel_refresh(Workspaceviewlabel *wviewlabel)
{
	Workspaceview *wview = wviewlabel->wview;
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);

	gtk_label_set_text(GTK_LABEL(wviewlabel->label), IOBJECT(ws)->name);
	g_object_set(wviewlabel->label_edit, "text", IOBJECT(ws)->name, NULL);

	gtk_stack_set_visible_child(GTK_STACK(wviewlabel->name_edit_stack),
		wviewlabel->edit ? wviewlabel->label_edit : wviewlabel->label);
	gtk_widget_set_visible(wviewlabel->lock, ws->locked);
	gtk_widget_set_visible(wviewlabel->error, ws->errors != NULL);
}
