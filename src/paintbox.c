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

typedef enum _PaintboxRubber {
	PAINTBOX_RUBBER_NONE,
	PAINTBOX_RUBBER_LINE,
	PAINTBOX_RUBBER_CIRCLE,
	PAINTBOX_RUBBER_BOX,
} PaintboxRubber;

struct _Paintbox {
	GtkWidget parent_instance;

	/* The imagewindow we are on.
	 */
	Imagewindow *win;

	/* The imageui we are drawing on, and the signals we are watching.
	 */
	Imageui *imageui;
	GtkWidget *imagedisplay;
	guint drag_begin_sid;
	guint drag_update_sid;
	guint drag_end_sid;
	guint snapshot_sid;

	/* Currently selected tool.
	 */
	PaintboxTool tool;

	double start_x;
	double start_y;

	/* Widgets.
	 */
	GtkWidget *action_bar;
	// tool select toggles
	GtkWidget *pointer;
	GtkWidget *brush;
	GtkWidget *line;
	GtkWidget *text;
	GtkWidget *dropper;
	GtkWidget *tools[PAINTBOX_TOOL_LAST];
	GtkWidget *ink;
	GtkWidget *fill;
	GtkWidget *width;
	GtkWidget *text_string;

	/* Our rubber-banding state. All in image cods.
	 */
	PaintboxRubber rubber;
	int cx, cy;
	int r;
	int x0, y0;
	int x1, y1;
};

G_DEFINE_TYPE(Paintbox, paintbox, GTK_TYPE_WIDGET);

enum {
	PROP_IMAGEWINDOW = 1,
	PROP_REVEALED,

	SIG_LAST
};

GdkRGBA paintbox_border = { 0.9, 1.0, 0.9, 1 };
GdkRGBA paintbox_shadow = { 0.2, 0.2, 0.2, 0.5 };

static void
paintbox_disconnect(Paintbox *paintbox)
{
	if (paintbox->imageui) {
		g_signal_handler_disconnect(paintbox->imageui,
			paintbox->drag_begin_sid);
		g_signal_handler_disconnect(paintbox->imageui,
			paintbox->drag_update_sid);
		g_signal_handler_disconnect(paintbox->imageui,
			paintbox->drag_end_sid);

		paintbox->drag_begin_sid = 0;
		paintbox->drag_update_sid = 0;
		paintbox->drag_end_sid = 0;

		paintbox->imageui = NULL;
	}

	if (paintbox->imagedisplay) {
		g_signal_handler_disconnect(paintbox->imagedisplay,
			paintbox->snapshot_sid);

		paintbox->imagedisplay = NULL;
	}
}

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
paintbox_refresh(Paintbox *paintbox)
{
	GtkToggleButton *button =
		GTK_TOGGLE_BUTTON(paintbox->tools[paintbox->tool]);
	gtk_toggle_button_set_active(button, TRUE);

	// FIXME ... update undo/redo button sensitivity
}

static gboolean
paintbox_drag_begin(Imageui *imageui,
	gdouble start_x, gdouble start_y, GtkGestureDrag *drag, gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(drag);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Paintbox *paintbox = PAINTBOX(user_data);

	paintbox->start_x = start_x;
	paintbox->start_y = start_y;

	double image_x, image_y;
	imageui_gtk_to_image(imageui, start_x, start_y, &image_x, &image_y);

#ifdef DEBUG_VERBOSE
	printf("paintbox_drag_begin: start_x = %g, start_y = %g\n",
		start_x, start_y);
#endif /*DEBUG_VERBOSE*/

	gboolean handled = FALSE;

	switch (paintbox->tool) {
	case PAINTBOX_TOOL_BRUSH:
		handled = TRUE;
		break;

	case PAINTBOX_TOOL_LINE:
		handled = TRUE;
		paintbox->rubber = PAINTBOX_RUBBER_LINE;
		paintbox->x0 = paintbox->x1 = rint(image_x);
		paintbox->y0 = paintbox->y1 = rint(image_y);
		break;

	default:
		break;
	}

	return handled;
}

static gboolean
paintbox_drag_update(Imageui *imageui,
	gdouble offset_x, gdouble offset_y, GtkGestureDrag *drag,
	gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(drag);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Paintbox *paintbox = PAINTBOX(user_data);
	Tilesource *tilesource = imageui_get_tilesource(imageui);

	double gtk_x = paintbox->start_x + offset_x;
	double gtk_y = paintbox->start_y + offset_y;
	double image_x, image_y;
	imageui_gtk_to_image(imageui, gtk_x, gtk_y, &image_x, &image_y);

	/* FIXME fetch from widgets.
	 */
	double ink[] = {0, 255, 0};
	int n_ink = VIPS_NUMBER(ink);
	int r = 100;
	gboolean fill = FALSE;

#ifdef DEBUG_VERBOSE
	printf("paintbox_drag_update: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	gboolean handled = FALSE;

	switch (paintbox->tool) {
	case PAINTBOX_TOOL_BRUSH:
		if (tilesource) {
			handled = TRUE;
			if (!tilesource_draw_circle(tilesource,
				ink, n_ink, rint(image_x), rint(image_y), r, fill))
				imagewindow_error(paintbox->win);
		}
		break;

	case PAINTBOX_TOOL_LINE:
		handled = TRUE;
		paintbox->x1 = rint(image_x);
		paintbox->y1 = rint(image_y);
		gtk_widget_queue_draw(paintbox->imagedisplay);
		break;

	default:
		break;
	}

	return handled;
}

static gboolean
paintbox_drag_end(Imageui *imageui,
	gdouble offset_x, gdouble offset_y, GtkGestureDrag *drag,
	gpointer user_data)
{
	Paintbox *paintbox = PAINTBOX(user_data);
	Tilesource *tilesource = imageui_get_tilesource(imageui);

	double gtk_x = paintbox->start_x + offset_x;
	double gtk_y = paintbox->start_y + offset_y;
	double image_x, image_y;
	imageui_gtk_to_image(imageui, gtk_x, gtk_y, &image_x, &image_y);

	/* FIXME fetch from widgets.
	 */
	double ink[] = {0, 255, 0};
	int n_ink = VIPS_NUMBER(ink);
	int r = 100;
	gboolean fill = TRUE;

#ifdef DEBUG_VERBOSE
	printf("paintbox_drag_end: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	gboolean handled = FALSE;

	switch (paintbox->tool) {
	case PAINTBOX_TOOL_BRUSH:
		handled = TRUE;

		if (tilesource &&
			!tilesource_draw_circle(tilesource, ink, n_ink,
				rint(image_x), rint(image_y), r, fill))
			imagewindow_error(paintbox->win);

		break;

	case PAINTBOX_TOOL_LINE:
		handled = TRUE;
		paintbox->rubber = PAINTBOX_RUBBER_NONE;
		gtk_widget_queue_draw(paintbox->imagedisplay);

		if (tilesource &&
			!tilesource_draw_line(tilesource, ink, n_ink,
				paintbox->x0, paintbox->y0, paintbox->x1, paintbox->y1))
			imagewindow_error(paintbox->win);

		break;

	default:
		break;
	}

	return handled;
}

static void
paintbox_set_tool(Paintbox *paintbox, PaintboxTool tool)
{
	if (paintbox->tool != tool) {
		if (tool != PAINTBOX_TOOL_POINTER) {
			Imageui *imageui = imagewindow_get_imageui(paintbox->win);

			if (!imageui_make_paintable(imageui)) {
				imagewindow_error(paintbox->win);
				return;
			}
		}

		paintbox->tool = tool;

		paintbox_refresh(paintbox);
	}
}

static void
paintbox_snapshot(Imagedisplay *imagedisplay,
	GtkSnapshot *snapshot, Paintbox *paintbox)
{
	Imageui *imageui = paintbox->imageui;

	double x0_gtk, y0_gtk;
	imageui_image_to_gtk(imageui, paintbox->x0, paintbox->y0, &x0_gtk, &y0_gtk);

	double x1_gtk, y1_gtk;
	imageui_image_to_gtk(imageui, paintbox->x1, paintbox->y1, &x1_gtk, &y1_gtk);

	VipsRect window = {
		0,
		0,
		gtk_widget_get_width(GTK_WIDGET(imageui)),
		gtk_widget_get_height(GTK_WIDGET(imageui))
	};

	GskStroke *stroke;
	int x0, y0;
	int x1, y1;

	switch (paintbox->rubber) {
		case PAINTBOX_RUBBER_LINE:

		if (line_clip(&window,
			x0_gtk, y0_gtk, x1_gtk, y1_gtk, &x0, &y0, &x1, &y1)) {
			GskPathBuilder *builder = gsk_path_builder_new();
			gsk_path_builder_move_to(builder, x0, y0);
			gsk_path_builder_line_to(builder, x1, y1);
			g_autoptr(GskPath) path = gsk_path_builder_free_to_path(builder);

			stroke = gsk_stroke_new(3);
			gsk_stroke_set_dash(stroke, (float[1]){ 10 }, 1);
			gtk_snapshot_append_stroke(snapshot,
				path, stroke, &paintbox_border);
			gsk_stroke_free(stroke);

			stroke = gsk_stroke_new(3);
			gsk_stroke_set_dash(stroke, (float[1]){ 10 }, 1);
			gsk_stroke_set_dash_offset(stroke, 10);
			gtk_snapshot_append_stroke(snapshot,
				path, stroke, &paintbox_shadow);
			gsk_stroke_free(stroke);
		}
		break;

	default:
		break;
	}
}

// win->imageui has changed
static void
paintbox_imagewindow_new_image(Imagewindow *win, Paintbox *paintbox)
{
#ifdef DEBUG
	printf("paintbox_imagewindow_new_image:\n");
#endif /*DEBUG*/

	paintbox_disconnect(paintbox);

	paintbox->imageui = imagewindow_get_imageui(win);
	paintbox->drag_begin_sid = g_signal_connect(paintbox->imageui,
		"drag-begin", G_CALLBACK(paintbox_drag_begin), paintbox);
	paintbox->drag_update_sid = g_signal_connect(paintbox->imageui,
		"drag-update", G_CALLBACK(paintbox_drag_update), paintbox);
	paintbox->drag_end_sid = g_signal_connect(paintbox->imageui,
		"drag-end", G_CALLBACK(paintbox_drag_end), paintbox);

	paintbox->imagedisplay = imageui_get_imagedisplay(paintbox->imageui);
	paintbox->snapshot_sid = g_signal_connect(paintbox->imagedisplay,
		"snapshot", G_CALLBACK(paintbox_snapshot), paintbox);

	// reset tool to SELECT, since the new imageui might not be paintable
	paintbox_set_tool(paintbox, PAINTBOX_TOOL_POINTER);
}

static void
paintbox_set_imagewindow(Paintbox *paintbox, Imagewindow *win)
{
	// only support set once
	g_assert(!paintbox->win);

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
		gtk_action_bar_set_revealed(GTK_ACTION_BAR(paintbox->action_bar),
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

	paintbox->tools[PAINTBOX_TOOL_POINTER] = paintbox->pointer;
	paintbox->tools[PAINTBOX_TOOL_BRUSH] = paintbox->brush;
	paintbox->tools[PAINTBOX_TOOL_LINE] = paintbox->line;
	paintbox->tools[PAINTBOX_TOOL_TEXT] = paintbox->text;
	paintbox->tools[PAINTBOX_TOOL_DROPPER] = paintbox->dropper;

	Tslider *width = TSLIDER(paintbox->width);
	width->from = 0;
	width->to = 100;
	width->value = 5;
	width->digits = 1;
	tslider_changed(width);

	paintbox_refresh(paintbox);
}

static void
paintbox_toggled(GtkToggleButton *button, Paintbox *paintbox)
{
	const char *id = gtk_buildable_get_buildable_id(GTK_BUILDABLE(button));

#ifdef DEBUG
	printf("paintbox_toggled: id = %s\n", id);
#endif /*DEBUG*/

	if (gtk_toggle_button_get_active(button)) {
		int value =
			vips_enum_from_nick("paintbox_toggled", PAINTBOX_TOOL_TYPE, id);

		if (value < 0) {
			printf("paintbox_toggled: unknown id\n");
			return;
		}

		paintbox_set_tool(paintbox, value);
	}
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
	BIND_VARIABLE(Paintbox, pointer);
	BIND_VARIABLE(Paintbox, brush);
	BIND_VARIABLE(Paintbox, line);
	BIND_VARIABLE(Paintbox, text);
	BIND_VARIABLE(Paintbox, dropper);
	BIND_VARIABLE(Paintbox, ink);
	BIND_VARIABLE(Paintbox, fill);
	BIND_VARIABLE(Paintbox, width);
	BIND_VARIABLE(Paintbox, text_string);

	BIND_CALLBACK(paintbox_toggled);

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
