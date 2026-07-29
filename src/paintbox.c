/* a status bar for the image display window
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
 */
#define DEBUG_VERBOSE
#define DEBUG

struct _Paintbox {
	GtkWidget parent_instance;

	/* The imagewindow whose info we display.
	 */
	Imagewindow *win;

	GtkWidget *action_bar;
	GtkWidget *label;

};

G_DEFINE_TYPE(Paintbox, paintbox, GTK_TYPE_WIDGET);

enum {
	PROP_IMAGEWINDOW = 1,
	PROP_REVEALED,

	SIG_LAST
};

static void
paintbox_dispose(GObject *object)
{
	Paintbox *paintbox = (Paintbox *) object;

#ifdef DEBUG
	printf("paintbox_dispose:\n");
#endif /*DEBUG*/

	VIPS_FREEF(gtk_widget_unparent, paintbox->action_bar);

	G_OBJECT_CLASS(paintbox_parent_class)->dispose(object);
}

static void
paintbox_imagewindow_new_image(Imagewindow *win, Paintbox *paintbox)
{
#ifdef DEBUG
	printf("paintbox_imagewindow_new_image:\n");
#endif /*DEBUG*/

}

static void
paintbox_set_imagewindow(Paintbox *paintbox, Imagewindow *win)
{
	/* No need to ref ... win holds a ref to us.
	 */
	paintbox->win = win;

	g_signal_connect_object(win, "new-image",
		G_CALLBACK(paintbox_imagewindow_new_image), paintbox, 0);
}

static void
paintbox_set_property(GObject *object,
	guint prop_id, const GValue *value, GParamSpec *pspec)
{
	Paintbox *paintbox = (Paintbox *) object;

	switch (prop_id) {
	case PROP_IMAGEWINDOW:
		paintbox_set_imagewindow(paintbox,
			IMAGEWINDOW(g_value_get_object(value)));
		break;

	case PROP_REVEALED:
		gtk_action_bar_set_revealed(
			GTK_ACTION_BAR(paintbox->action_bar),
			g_value_get_boolean(value));
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
paintbox_get_property(GObject *object,
	guint prop_id, GValue *value, GParamSpec *pspec)
{
	Paintbox *paintbox = (Paintbox *) object;
	GtkActionBar *action_bar = GTK_ACTION_BAR(paintbox->action_bar);

	switch (prop_id) {
	case PROP_IMAGEWINDOW:
		g_value_set_object(value, paintbox->win);
		break;

	case PROP_REVEALED:
		g_value_set_boolean(value, gtk_action_bar_get_revealed(action_bar));
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
paintbox_init(Paintbox *paintbox)
{
#ifdef DEBUG
	printf("paintbox_init:\n");
#endif /*DEBUG*/

	gtk_widget_init_template(GTK_WIDGET(paintbox));
}

static void
paintbox_class_init(PaintboxClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

#ifdef DEBUG
	printf("paintbox_class_init:\n");
#endif /*DEBUG*/

	BIND_RESOURCE("paintbox.ui");
	BIND_LAYOUT();

	BIND_VARIABLE(Paintbox, action_bar);
	BIND_VARIABLE(Paintbox, label);

	gobject_class->dispose = paintbox_dispose;
	gobject_class->set_property = paintbox_set_property;
	gobject_class->get_property = paintbox_get_property;

	g_object_class_install_property(gobject_class, PROP_IMAGEWINDOW,
		g_param_spec_object("image-window",
			_("Image window"),
			_("The image window we display"),
			IMAGEWINDOW_TYPE,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_REVEALED,
		g_param_spec_boolean("revealed",
			_("revealed"),
			_("Show the display control bar"),
			FALSE,
			G_PARAM_READWRITE));
}

Paintbox *
paintbox_new(Imagewindow *win)
{
	Paintbox *paintbox;

#ifdef DEBUG
	printf("paintbox_new:\n");
#endif /*DEBUG*/

	paintbox = g_object_new(paintbox_get_type(),
		"image-window", win,
		NULL);

	return paintbox;
}
