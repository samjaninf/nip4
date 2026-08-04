/* attaches to imageui and handles region interactions
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

/*
#define DEBUG_VERBOSE
#define DEBUG
 */

/* Snap if closer than this.
 */
const int imageui_snap_threshold = 10;

/* Drag state machine.
 */
typedef enum {
	IMAGEUI_WAIT,	/* Waiting for left down */
	IMAGEUI_SELECT, /* Manipulating a selected region */
	IMAGEUI_CREATE, /* Dragging out a new region */
} ImageuiState;

struct _Imageui {
	GtkWidget parent_instance;

	Imageui *imageui;

	/* Interaction state.
	 */
	ImageuiState state;

	/* All the regionviews we manage. True references.
	 */
	GSList *regionviews;

	/* Currently grabbed regionview.
	 */
	Regionview *grabbed;

	/* We use a floating regionview (no symbol) during eg. region create.
	 */
	Regionview *floating;

};

G_DEFINE_TYPE(Imageui, imageui, GTK_TYPE_WIDGET);

enum {
	PROP_IMAGEUI,

	PROP_LAST
};

void
imageui_add_regionview(Imageui *imageui, Regionview *regionview)
{
	g_assert(!g_slist_find(imageui->regionviews, regionview));
	g_assert(!regionview->imageui);

	imageui->regionviews = g_slist_prepend(imageui->regionviews, regionview);
	g_object_ref_sink(regionview);
	regionview->imageui = imageui;

	imageui_queue_draw(imageui);
}

void
imageui_remove_regionview(Imageui *imageui, Regionview *regionview)
{
	g_assert(g_slist_find(imageui->regionviews, regionview));

	imageui->regionviews = g_slist_remove(imageui->regionviews, regionview);
	regionview->imageui = NULL;
	g_object_unref(regionview);

	imageui_queue_draw(imageui);
}

static void
imageui_floating_remove(Imageui *imageui)
{
	if (imageui->floating) {
		imageui_remove_regionview(imageui, imageui->floating);
		imageui->floating = NULL;
	}
}

static void
imageui_floating_add(Imageui *imageui, int x, int y)
{
	imageui_floating_remove(imageui);

	Regionview *floating = regionview_new(NULL);
	imageui_add_regionview(imageui, floating);
	imageui->floating = floating;

	floating->type = REGIONVIEW_MARK;
	floating->our_area = (VipsRect){ x, y, 0, 0 };
	floating->draw_type = floating->type;
	floating->draw_area = floating->our_area;
	floating->start_area = floating->our_area;
	floating->resize = REGIONVIEW_RESIZE_BOTTOMRIGHT;
	floating->frozen = FALSE;
}

static void
imageui_dispose(GObject *object)
{
	Imageui *imageui = (Imageui *) object;

#ifdef DEBUG
	printf("imageui_dispose:\n");
#endif /*DEBUG*/

	while (imageui->regionviews) {
		Regionview *regionview = REGIONVIEW(imageui->regionviews->data);

		imageui_remove_regionview(imageui, regionview);
	}

	imageui_floating_remove(imageui);

	G_OBJECT_CLASS(imageui_parent_class)->dispose(object);
}

static void
imageui_set_property(GObject *object,
	guint prop_id, const GValue *value, GParamSpec *pspec)
{
	Imageui *imageui = (Imageui *) object;

	double zoom;

	switch (prop_id) {
	case PROP_IMAGEUI:
		imageui->imageui = TILESOURCE(g_value_get_object(value));
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
imageui_get_property(GObject *object,
	guint prop_id, GValue *value, GParamSpec *pspec)
{
	Imageui *imageui = IMAGEUI(object);

	double zoom;

	switch (prop_id) {
	case PROP_IMAGEUI:
		g_value_set_object(value, imageui->imageui);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

/* Track this during a snap.
 */
typedef struct {
	Imageui *imageui;

	int x;			/* Start point */
	int y;
	int off_x;		/* Current snap offset */
	int off_y;
	int best_x;		/* 'Closeness' of best snap so far */
	int best_y;
} ImageuiSnap;

static void *
imageui_snap_sub(Regionview *regionview, ImageuiSnap *snap, gboolean *snapped)
{
	/* Only static h/v guides.
	 */
	if (regionview->type != REGIONVIEW_HGUIDE &&
		regionview->type != REGIONVIEW_VGUIDE)
		return NULL;

	if (regionview->type == REGIONVIEW_HGUIDE) {
		int y = regionview->our_area.top;
		int score = abs(y - snap->y);

		if (score < snap->best_y) {
			snap->off_y = y - snap->y;
			snap->best_y = score;
			*snapped = TRUE;
		}
	}
	else {
		int x = regionview->our_area.left;
		int score = abs(x - snap->x);

		if (score < snap->best_x) {
			snap->off_x = x - snap->x;
			snap->best_x = score;
			*snapped = TRUE;
		}
	}

	return NULL;
}

static gboolean
imageui_snap(Imageui *imageui, ImageuiSnap *snap)
{
	gboolean snapped;

	// scale the snap threshold by the zoom factor
	snap->imageui = imageui;
	snap->off_x = 0;
	snap->off_y = 0;
	snap->best_x =
		VIPS_MAX(1, imageui_snap_threshold / imageui_get_zoom(imageui));
	snap->best_y =
		VIPS_MAX(1, imageui_snap_threshold / imageui_get_zoom(imageui));

	snapped = FALSE;
	slist_map2(imageui->regionviews,
		(SListMap2Fn) imageui_snap_sub, snap, &snapped);

	return snapped;
}

gboolean
imageui_snap_point(Imageui *imageui, int x, int y, int *sx, int *sy)
{
	ImageuiSnap snap;
	gboolean snapped;

	snap.x = x;
	snap.y = y;
	snapped = imageui_snap(imageui, &snap);

	*sx = x + snap.off_x;
	*sy = y + snap.off_y;

	return snapped;
}

gboolean
imageui_snap_rect(Imageui *imageui, VipsRect *in, VipsRect *out)
{
	/* Snap the corners plus the edge centres, take the best score.
	 */
	ImageuiSnap snap[8];
	snap[0].x = in->left;
	snap[0].y = in->top;
	snap[1].x = in->left + in->width;
	snap[1].y = in->top;
	snap[2].x = in->left + in->width;
	snap[2].y = in->top + in->height;
	snap[3].x = in->left;
	snap[3].y = in->top + in->height;
	snap[4].x = in->left + in->width / 2;
	snap[4].y = in->top;
	snap[5].x = in->left + in->width;
	snap[5].y = in->top + in->height / 2;
	snap[6].x = in->left + in->width / 2;
	snap[6].y = in->top + in->height;
	snap[7].x = in->left;
	snap[7].y = in->top + in->height / 2;

	gboolean snapped;
	snapped = FALSE;
	for (int i = 0; i < 8; i++)
		snapped |= imageui_snap(imageui, &snap[i]);

	int best;
	int best_score;
	best = 0;
	best_score = snap[0].best_x;
	for (int i = 1; i < 7; i++)
		if (snap[i].best_x < best_score) {
			best = i;
			best_score = snap[i].best_x;
		}
	out->left = in->left + snap[best].off_x;

	best = 0;
	best_score = snap[0].best_y;
	for (int i = 1; i < 7; i++)
		if (snap[i].best_y < best_score) {
			best = i;
			best_score = snap[i].best_y;
		}
	out->top = in->top + snap[best].off_y;

	out->width = in->width;
	out->height = in->height;

	return snapped;
}

static gboolean
imageui_key_pressed(GtkEventControllerKey *self,
	guint keyval, guint keycode, GdkModifierType state, gpointer user_data)
{
	Imageui *imageui = IMAGEUI(user_data);
	GtkScrolledWindow *scrolled_window =
		GTK_SCROLLED_WINDOW(imageui->scrolled_window);

	gboolean handled;
	double zoom_x;
	double zoom_y;
	gboolean ret;

#ifdef DEBUG_VERBOSE
	printf("imageui_key_pressed: keyval = %d, state = %d\n",
		keyval, state);
#endif /*DEBUG_VERBOSE*/

	handled = FALSE;

	switch (keyval) {
	case GDK_KEY_plus:
		imageui_magin(imageui);
		handled = TRUE;
		break;

	case GDK_KEY_minus:
		imageui_magout(imageui);
		handled = TRUE;
		break;

	case GDK_KEY_0:
		imageui_bestfit(imageui);
		handled = TRUE;
		break;

	case GDK_KEY_i:
		imageui_get_mouse_position(imageui, &zoom_x, &zoom_y);
		imageui_zoom_continuous(imageui, 1.5 * ZOOM_STEP, zoom_x, zoom_y);
		handled = TRUE;
		break;

	case GDK_KEY_o:
		imageui_get_mouse_position(imageui, &zoom_x, &zoom_y);
		imageui_zoom_continuous(imageui, 0.2 * ZOOM_STEP, zoom_x, zoom_y);
		handled = TRUE;
		break;

	case GDK_KEY_Left:
		if (state & GDK_SHIFT_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_PAGE_BACKWARD, TRUE, &ret);
		else if (state & GDK_CONTROL_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_START, TRUE, &ret);
		else
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_STEP_LEFT, TRUE, &ret);
		handled = TRUE;
		break;

	case GDK_KEY_Right:
		if (state & GDK_SHIFT_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_PAGE_FORWARD, TRUE, &ret);
		else if (state & GDK_CONTROL_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_END, TRUE, &ret);
		else
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_STEP_RIGHT, TRUE, &ret);
		handled = TRUE;
		break;

	case GDK_KEY_Up:
		if (state & GDK_SHIFT_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_PAGE_UP, FALSE, &ret);
		else if (state & GDK_CONTROL_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_START, FALSE, &ret);
		else
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_STEP_UP, FALSE, &ret);
		handled = TRUE;
		break;

	case GDK_KEY_Down:
		if (state & GDK_SHIFT_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_PAGE_DOWN, FALSE, &ret);
		else if (state & GDK_CONTROL_MASK)
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_END, FALSE, &ret);
		else
			g_signal_emit_by_name(scrolled_window, "scroll-child",
				GTK_SCROLL_STEP_DOWN, FALSE, &ret);
		handled = TRUE;
		break;

	case GDK_KEY_d:
		imageui_toggle_debug(imageui);
		handled = TRUE;
		break;

	default:
		break;
	}

	if (!handled) {
		int i;

		for (i = 0; i < VIPS_NUMBER(magnify_keys); i++)
			if (magnify_keys[i].keyval == keyval) {
				double zoom;

				zoom = magnify_keys[i].zoom;
				if (state & GDK_CONTROL_MASK)
					zoom = 1.0 / zoom;

				imageui_zoom_to_eased(imageui,
					zoom * imageui_get_pixel_size(imageui));

				handled = TRUE;
				break;
			}
	}

	return handled;
}

static gboolean
imageui_key_released(GtkEventControllerKey *self,
	guint keyval, guint keycode, GdkModifierType state, gpointer user_data)
{
	Imageui *imageui = IMAGEUI(user_data);

	gboolean handled;

	handled = FALSE;

	switch (keyval) {
	case GDK_KEY_i:
	case GDK_KEY_o:
		imageui->zoom_rate = 1.0;
		handled = TRUE;
		break;

	default:
		break;
	}

	if (handled)
		imageui_stop_animation(imageui);

	return handled;
}

// (x, y) in gtk cods
Regionview *
imageui_pick_regionview(Imageui *imageui, int x, int y)
{
	for (GSList *p = imageui->regionviews; p; p = p->next) {
		Regionview *regionview = REGIONVIEW(p->data);
		RegionviewResize resize = regionview_hit(regionview, x, y);

		if (resize != REGIONVIEW_RESIZE_NONE)
			return regionview;
	}

	return NULL;
}

static void
imageui_drag_begin(GtkEventControllerMotion *self,
	gdouble start_x, gdouble start_y, gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(self);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Imageui *imageui = IMAGEUI(user_data);

	Regionview *regionview;

#ifdef DEBUG_VERBOSE
	printf("imageui_drag_begin: start_x = %g, start_y = %g\n",
		start_x, start_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageui->state) {
	case IMAGEUI_WAIT:
		regionview = imageui_pick_regionview(imageui, start_x, start_y);

		if (regionview) {
			imageui->state = IMAGEUI_SELECT;
			regionview->resize = regionview_hit(regionview, start_x, start_y);
			imageui->grabbed = regionview;
			g_object_ref(regionview);
			regionview->start_area = regionview->our_area;
		}
		else if (modifiers & GDK_CONTROL_MASK) {
			imageui->state = IMAGEUI_CREATE;
			double left;
			double top;
			imageui_gtk_to_image(imageui, start_x, start_y, &left, &top);
			imageui_floating_add(imageui, left, top);
		}
		else {
			int window_left;
			int window_top;
			int window_width;
			int window_height;
			imageui_get_position(imageui,
				&window_left, &window_top, &window_width, &window_height);
			imageui->window_left = window_left;
			imageui->window_top = window_top;
			imageui->start_x = start_x;
			imageui->start_y = start_y;
		}

		break;

	case IMAGEUI_SELECT:
		break;

	case IMAGEUI_SCROLL:
		break;

	case IMAGEUI_CREATE:
		break;

	default:
		break;
	}
}

static void
imageui_regionview_update(Imageui *imageui, Regionview *regionview)
{
	regionview->draw_area = regionview->our_area;
	regionview->draw_type = regionview->type;
	imageui_queue_draw(imageui);
}

static void
imageui_drag_update(GtkEventControllerMotion *self,
	gdouble offset_x, gdouble offset_y, gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(self);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Imageui *imageui = IMAGEUI(user_data);
	double zoom = imageui_get_zoom(imageui);

#ifdef DEBUG_VERBOSE
	printf("imageui_drag_update: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageui->state) {
	case IMAGEUI_WAIT:
		if (fabs(offset_x) > 5 ||
			fabs(offset_y) > 5)
			imageui->state = IMAGEUI_SCROLL;
		break;

	case IMAGEUI_SELECT:
		regionview_resize(imageui->grabbed, modifiers,
			imageui->tilesource->image_width, imageui->tilesource->image_height,
			offset_x / zoom, offset_y / zoom);

		/* Refresh immediately .. gives immediate feedback during drag in large
		 * workspaces, especially on windows.
		 */
		imageui_regionview_update(imageui, imageui->grabbed);

		/* And nudge background recomp.
		 */
		regionview_model_update(imageui->grabbed);

		break;

	case IMAGEUI_CREATE:
		regionview_resize(imageui->floating, modifiers,
			imageui->tilesource->image_width, imageui->tilesource->image_height,
			offset_x / zoom, offset_y / zoom);
		imageui_regionview_update(imageui, imageui->floating);
		break;

	case IMAGEUI_SCROLL:
		imageui_set_position(imageui,
			imageui->window_left - offset_x, imageui->window_top - offset_y);
		break;

	default:
		break;
	}
}

static void
imageui_region_new(Imageui *imageui, RegionviewType type, VipsRect *rect)
{
	Row *row = imageui->iimage ? HEAPMODEL(imageui->iimage)->row : NULL;

	if (row) {
		char txt[MAX_STRSIZE];
		VipsBuf buf = VIPS_BUF_STATIC(txt);
		Symbol *sym;

		switch (type) {
		case REGIONVIEW_MARK:
			vips_buf_appendf(&buf, "%s ", CLASS_MARK);
			row_qualified_name(row, &buf);
			vips_buf_appendd(&buf, rect->left);
			vips_buf_appendd(&buf, rect->top);
			break;

		case REGIONVIEW_REGION:
			vips_buf_appendf(&buf, "%s ", CLASS_REGION);
			row_qualified_name(row, &buf);
			vips_buf_appendd(&buf, rect->left);
			vips_buf_appendd(&buf, rect->top);
			vips_buf_appendd(&buf, rect->width);
			vips_buf_appendd(&buf, rect->height);
			break;

		case REGIONVIEW_ARROW:
			vips_buf_appendf(&buf, "%s ", CLASS_ARROW);
			row_qualified_name(row, &buf);
			vips_buf_appendd(&buf, rect->left);
			vips_buf_appendd(&buf, rect->top);
			vips_buf_appendd(&buf, rect->width);
			vips_buf_appendd(&buf, rect->height);
			break;

		case REGIONVIEW_HGUIDE:
			vips_buf_appendf(&buf, "%s ", CLASS_HGUIDE);
			row_qualified_name(row, &buf);
			vips_buf_appendd(&buf, VIPS_RECT_BOTTOM(rect));
			break;

		case REGIONVIEW_VGUIDE:
			vips_buf_appendf(&buf, "%s ", CLASS_VGUIDE);
			row_qualified_name(row, &buf);
			vips_buf_appendd(&buf, VIPS_RECT_RIGHT(rect));
			break;

		default:
			g_assert_not_reached();
		}

		if (!(sym = workspace_add_def_recalc(row->ws, vips_buf_all(&buf)))) {
			GtkWindow *window =
				GTK_WINDOW(gtk_widget_get_root(GTK_WIDGET(imageui)));

			error_alert(window);
		}

		workspace_deselect_all(row->ws);
	}
}

static void
imageui_drag_end(GtkEventControllerMotion *self,
	gdouble offset_x, gdouble offset_y, gpointer user_data)
{
	Imageui *imageui = IMAGEUI(user_data);

#ifdef DEBUG_VERBOSE
	printf("imageui_drag_end: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageui->state) {
	case IMAGEUI_WAIT:
		break;

	case IMAGEUI_SELECT:
		regionview_model_update(imageui->grabbed);
		VIPS_UNREF(imageui->grabbed);
		break;

	case IMAGEUI_CREATE:
		if (imageui->floating) {
			imageui_region_new(imageui,
				imageui->floating->type, &imageui->floating->our_area);
			imageui_floating_remove(imageui);
		}

		imageui_queue_draw(imageui);

		break;

	case IMAGEUI_SCROLL:
		break;

	default:
		break;
	}

	imageui->state = IMAGEUI_WAIT;
}

static void
imageui_set_cursor(Imageui *imageui)
{
	RegionviewResize resize;

	resize = REGIONVIEW_RESIZE_NONE;

	if (imageui->grabbed)
		resize = imageui->grabbed->resize;
	else {
		int x = imageui->last_x_gtk;
		int y = imageui->last_y_gtk;

		Regionview *regionview;

		if ((regionview = imageui_pick_regionview(imageui, x, y)))
			resize = regionview_hit(regionview, x, y);
	}

	GdkCursor *cursor;
	cursor = NULL;
	if (resize != REGIONVIEW_RESIZE_NONE)
		cursor = imageui_cursors[resize];
	gtk_widget_set_cursor(GTK_WIDGET(imageui), cursor);
}

static void
imageui_motion(GtkEventControllerMotion *self,
	gdouble x, gdouble y, gpointer user_data)
{
	Imageui *imageui = IMAGEUI(user_data);

#ifdef DEBUG_VERBOSE
	printf("imageui_motion: x = %g, y = %g\n", x, y);
#endif /*DEBUG_VERBOSE*/

	imageui->last_x_gtk = x;
	imageui->last_y_gtk = y;

	imageui_set_cursor(imageui);
	imageui_changed(imageui);
}

static gboolean
imageui_scroll(GtkEventControllerMotion *self,
	double dx, double dy, gpointer user_data)
{
	Imageui *imageui = IMAGEUI(user_data);

	double x_image;
	double y_image;

#ifdef DEBUG_VERBOSE
	printf("imageui_scroll: dx = %g, dy = %g\n", dx, dy);
#endif /*DEBUG_VERBOSE*/

	imageui_get_mouse_position(imageui, &x_image, &y_image);

	if (dy < 0)
		imageui_set_zoom_position(imageui,
			ZOOM_STEP * imageui_get_zoom(imageui), x_image, y_image);
	else
		imageui_set_zoom_position(imageui,
			(1.0 / ZOOM_STEP) * imageui_get_zoom(imageui), x_image, y_image);

	return TRUE;
}

// from the imagedisplay snapshot method: draw any visible regions
static void
imageui_overlay_snapshot(Imagedisplay *imagedisplay,
	GtkSnapshot *snapshot, Imageui *imageui)
{
	for (GSList *p = imageui->regionviews; p; p = p->next) {
		Regionview *regionview = REGIONVIEW(p->data);

		regionview_draw(regionview, snapshot);
	}
}

static void
imageui_init(Imageui *imageui)
{
#ifdef DEBUG
	printf("imageui_init:\n");
#endif /*DEBUG*/

	gtk_widget_init_template(GTK_WIDGET(imageui));

	imageui->zoom_rate = 1.0;

	g_signal_connect_object(G_OBJECT(imageui->imagedisplay), "snapshot",
		G_CALLBACK(imageui_overlay_snapshot), imageui, 0);

	/* Uncomment to test our animation disable
	g_object_set(gtk_widget_get_settings(GTK_WIDGET(win)),
		"gtk-enable-animations", FALSE, NULL);
	 */

	// read the gtk animation setting preference
	imageui->should_animate = widget_should_animate(GTK_WIDGET(imageui));
}

static void
imageui_class_init(ImageuiClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

#ifdef DEBUG
	printf("imageui_class_init:\n");
#endif /*DEBUG*/

	BIND_RESOURCE("imageui.ui");
	BIND_LAYOUT();

	BIND_VARIABLE(Imageui, scrolled_window);
	BIND_VARIABLE(Imageui, imagedisplay);

	BIND_CALLBACK(imageui_drag_begin);
	BIND_CALLBACK(imageui_drag_update);
	BIND_CALLBACK(imageui_drag_end);
	BIND_CALLBACK(imageui_key_pressed);
	BIND_CALLBACK(imageui_key_released);
	BIND_CALLBACK(imageui_motion);
	BIND_CALLBACK(imageui_scroll);

	gobject_class->dispose = imageui_dispose;
	gobject_class->set_property = imageui_set_property;
	gobject_class->get_property = imageui_get_property;

	g_object_class_install_property(gobject_class, PROP_TILESOURCE,
		g_param_spec_object("tilesource",
			_("Tile source"),
			_("The tile source we display"),
			TILESOURCE_TYPE,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_IIMAGE,
		g_param_spec_object("iimage",
			_("iImage"),
			_("The model we represent"),
			IIMAGE_TYPE,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_BACKGROUND,
		g_param_spec_int("background",
			_("Background"),
			_("Background mode"),
			0, TILECACHE_BACKGROUND_LAST - 1,
			TILECACHE_BACKGROUND_CHECKERBOARD,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_ZOOM,
		g_param_spec_double("zoom",
			_("Zoom"),
			_("Zoom of viewport"),
			-VIPS_MAX_COORD, VIPS_MAX_COORD, 0,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_X,
		g_param_spec_double("x",
			_("x"),
			_("Horizontal position of viewport"),
			-VIPS_MAX_COORD, VIPS_MAX_COORD, 0,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_Y,
		g_param_spec_double("y",
			_("y"),
			_("Vertical position of viewport"),
			-VIPS_MAX_COORD, VIPS_MAX_COORD, 0,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_PIXEL_SIZE,
		g_param_spec_double("pixel_size",
			_("Pixel size"),
			_("Size of hardware display pixels in gtk coordinates"),
			0.0, 10.0, 0.0,
			G_PARAM_READWRITE));

	imageui_signals[SIG_CHANGED] = g_signal_new("changed",
		G_TYPE_FROM_CLASS(class),
		G_SIGNAL_RUN_LAST,
		0,
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);

	for (int i = 0; i < REGIONVIEW_RESIZE_LAST; i++)
		imageui_cursors[i] =
			gdk_cursor_new_from_name(imageui_cursor_names[i], NULL);
}

Imageui *
imageui_new(Tilesource *tilesource, iImage *iimage)
{
	Imageui *imageui;

#ifdef DEBUG
	printf("imageui_new:\n");
#endif /*DEBUG*/

	imageui = g_object_new(IMAGEUI_TYPE,
		"tilesource", tilesource,
		"iimage", iimage,
		NULL);

	return imageui;
}

Imageui *
imageui_duplicate(Tilesource *tilesource, Imageui *old_imageui)
{
	Imageui *new_imageui = imageui_new(tilesource, old_imageui->iimage);

	/* We want to copy position and zoom, so no bestfit.
	 */
	g_object_set(new_imageui->imagedisplay,
		"bestfit", FALSE,
		NULL);

	double zoom = imageui_get_zoom(old_imageui);
	imageui_set_zoom(new_imageui, zoom);

	// this won't work until imagedisplay has had a layout :( think about this
	// again
	int left, top, width, height;
	imageui_get_position(old_imageui, &left, &top, &width, &height);
	imageui_set_position(new_imageui, left, top);

	return new_imageui;
}

void
imageui_image_to_gtk(Imageui *imageui,
	double x_image, double y_image, double *x_gtk, double *y_gtk)
{
	imagedisplay_image_to_gtk(IMAGEDISPLAY(imageui->imagedisplay),
		x_image, y_image, x_gtk, y_gtk);
}

void
imageui_gtk_to_image(Imageui *imageui,
	double x_gtk, double y_gtk, double *x_image, double *y_image)
{
	imagedisplay_gtk_to_image(IMAGEDISPLAY(imageui->imagedisplay),
		x_gtk, y_gtk, x_image, y_image);
}

void
imageui_image_to_gtk_rect(Imageui *imageui, VipsRect *in, VipsRect *out)
{
	VipsRect rect;
	double x_gtk;
	double y_gtk;

	imageui_image_to_gtk(imageui, in->left, in->top, &x_gtk, &y_gtk);
	rect.left = x_gtk;
	rect.top = y_gtk;

	imageui_image_to_gtk(imageui,
		VIPS_RECT_RIGHT(in), VIPS_RECT_BOTTOM(in), &x_gtk, &y_gtk);
	rect.width = ceil(x_gtk) - rect.left;
	rect.height = ceil(y_gtk) - rect.top;

	*out = rect;
}

void
imageui_gtk_to_image_rect(Imageui *imageui, VipsRect *in, VipsRect *out)
{
	VipsRect rect;
	double x_image;
	double y_image;

	imageui_gtk_to_image(imageui, in->left, in->top, &x_image, &y_image);
	rect.left = x_image;
	rect.top = y_image;

	imageui_gtk_to_image(imageui,
		VIPS_RECT_RIGHT(in), VIPS_RECT_BOTTOM(in), &x_image, &y_image);
	rect.width = ceil(x_image) - rect.left;
	rect.height = ceil(y_image) - rect.top;

	*out = rect;
}
