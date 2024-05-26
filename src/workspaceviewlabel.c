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

	GtkWidget *top;
	GtkWidget *label;
	GtkWidget *lock;
	GtkWidget *error;
	GtkWidget *right_click_menu;
};

G_DEFINE_TYPE(Workspaceviewlabel, workspaceviewlabel, GTK_TYPE_WIDGET);

enum {
	PROP_WORKSPACEVIEW = 1,

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

static void
workspaceviewlabel_menu(GtkGestureClick *gesture,
	guint n_press, double x, double y, Workspaceviewlabel *wviewlabel)
{
	mainwindow_set_action_view(VIEW(wviewlabel->wview));

	gtk_popover_set_pointing_to(GTK_POPOVER(wviewlabel->right_click_menu),
		&(const GdkRectangle){ x, y, 1, 1 });

	gtk_popover_popup(GTK_POPOVER(wviewlabel->right_click_menu));
}

static void
workspaceviewlabel_pressed(GtkGestureClick *gesture,
	guint n_press, double x, double y, Workspaceviewlabel *wviewlabel)
{
	if (n_press == 2) {
		// rename tab
		printf("workspaceview_tab_pressed: doubleclick\n");
		// workspaceview_rename_cb(wid, NULL, wview);
	}
}

static void
workspaceviewlabel_class_init(WorkspaceviewlabelClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(class);

#ifdef DEBUG
	printf("workspaceviewlabel_class_init:\n");
#endif /*DEBUG*/

	G_OBJECT_CLASS(class)->dispose = workspaceviewlabel_dispose;

	BIND_RESOURCE("workspaceviewlabel.ui");

	gtk_widget_class_set_layout_manager_type(GTK_WIDGET_CLASS(class),
		GTK_TYPE_BIN_LAYOUT);

	BIND_VARIABLE(Workspaceviewlabel, top);
	BIND_VARIABLE(Workspaceviewlabel, label);
	BIND_VARIABLE(Workspaceviewlabel, lock);
	BIND_VARIABLE(Workspaceviewlabel, error);
	BIND_VARIABLE(Workspaceviewlabel, right_click_menu);

	BIND_CALLBACK(workspaceviewlabel_menu);
	BIND_CALLBACK(workspaceviewlabel_pressed);

	gobject_class->dispose = workspaceviewlabel_dispose;
	gobject_class->set_property = workspaceviewlabel_set_property;
	gobject_class->get_property = workspaceviewlabel_get_property;

	g_object_class_install_property(gobject_class, PROP_WORKSPACEVIEW,
		g_param_spec_object("workspaceview",
			_("Workspaceview"),
			_("The workspaceview we are for"),
			WORKSPACEVIEW_TYPE,
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

	if (IOBJECT(ws)->caption)
		set_tooltip(wviewlabel->label, "%s", IOBJECT(ws)->caption);

	if (ws->locked)
		gtk_image_set_from_icon_name(GTK_IMAGE(wviewlabel->lock),
			"system-lock-screen-symbolic");
	else
		gtk_image_clear(GTK_IMAGE(wviewlabel->lock));

	if (ws->errors)
		gtk_image_set_from_icon_name(GTK_IMAGE(wviewlabel->error),
			"dialog-warning-symbolic");
	else
		gtk_image_clear(GTK_IMAGE(wviewlabel->error));
}
