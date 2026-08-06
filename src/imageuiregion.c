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
const int imageuiregion_snap_threshold = 10;

/* Drag state machine.
 */
typedef enum {
	IMAGEUIREGION_STATE_WAIT,	/* Waiting for left down */
	IMAGEUIREGION_STATE_SELECT, /* Manipulating a selected region */
	IMAGEUIREGION_STATE_CREATE, /* Dragging out a new region */
} ImageuiregionState;

struct _Imageuiregion {
	GObject parent_instance;

	Imageui *imageui;

	/* Interaction state.
	 */
	ImageuiregionState state;

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

G_DEFINE_TYPE(Imageuiregion, imageuiregion, G_TYPE_OBJECT);

enum {
	PROP_IMAGEUI,

	PROP_LAST
};

void
imageuiregion_add_regionview(Imageuiregion *imageuiregion,
	Regionview *regionview)
{
	g_assert(!g_slist_find(imageuiregion->regionviews, regionview));
	g_assert(!regionview->imageuiregion);

	imageuiregion->regionviews =
		g_slist_prepend(imageuiregion->regionviews, regionview);
	g_object_ref_sink(regionview);
	regionview->imageuiregion = imageuiregion;

	imageui_queue_draw(imageuiregion->imageui);
}

void
imageuiregion_remove_regionview(Imageuiregion *imageuiregion,
	Regionview *regionview)
{
	g_assert(g_slist_find(imageuiregion->regionviews, regionview));

	imageuiregion->regionviews =
		g_slist_remove(imageuiregion->regionviews, regionview);
	regionview->imageuiregion = NULL;
	g_object_unref(regionview);

	imageui_queue_draw(imageuiregion->imageui);
}

static void
imageuiregion_floating_remove(Imageuiregion *imageuiregion)
{
	if (imageuiregion->floating) {
		imageuiregion_remove_regionview(imageuiregion, imageuiregion->floating);
		imageuiregion->floating = NULL;
	}
}

static void
imageuiregion_floating_add(Imageuiregion *imageuiregion, int x, int y)
{
	imageuiregion_floating_remove(imageuiregion);

	Regionview *floating = regionview_new(NULL);
	imageuiregion_add_regionview(imageuiregion, floating);
	imageuiregion->floating = floating;

	floating->type = REGIONVIEW_MARK;
	floating->our_area = (VipsRect){ x, y, 0, 0 };
	floating->draw_type = floating->type;
	floating->draw_area = floating->our_area;
	floating->start_area = floating->our_area;
	floating->resize = REGIONVIEW_RESIZE_BOTTOMRIGHT;
	floating->frozen = FALSE;
}

static void
imageuiregion_dispose(GObject *object)
{
	Imageuiregion *imageuiregion = (Imageuiregion *) object;

#ifdef DEBUG
	printf("imageuiregion_dispose:\n");
#endif /*DEBUG*/

	while (imageuiregion->regionviews) {
		Regionview *regionview = REGIONVIEW(imageuiregion->regionviews->data);

		imageuiregion_remove_regionview(imageuiregion, regionview);
	}

	imageuiregion_floating_remove(imageuiregion);

	G_OBJECT_CLASS(imageuiregion_parent_class)->dispose(object);
}

static void
imageuiregion_set_property(GObject *object,
	guint prop_id, const GValue *value, GParamSpec *pspec)
{
	Imageuiregion *imageuiregion = (Imageuiregion *) object;

	switch (prop_id) {
	case PROP_IMAGEUI:
		imageuiregion->imageui = IMAGEUI(g_value_get_object(value));
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
imageuiregion_get_property(GObject *object,
	guint prop_id, GValue *value, GParamSpec *pspec)
{
	Imageuiregion *imageuiregion = IMAGEUIREGION(object);

	switch (prop_id) {
	case PROP_IMAGEUI:
		g_value_set_object(value, imageuiregion->imageui);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

/* Track this during a snap.
 */
typedef struct {
	Imageuiregion *imageuiregion;

	int x;			/* Start point */
	int y;
	int off_x;		/* Current snap offset */
	int off_y;
	int best_x;		/* 'Closeness' of best snap so far */
	int best_y;
} ImageuiregionSnap;

static void *
imageuiregion_snap_sub(Regionview *regionview,
	ImageuiregionSnap *snap, gboolean *snapped)
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
imageuiregion_snap(Imageuiregion *imageuiregion, ImageuiregionSnap *snap)
{
	Imageui *imageui = imageuiregion->imageui;

	gboolean snapped;

	// scale the snap threshold by the zoom factor
	snap->imageuiregion = imageuiregion;
	snap->off_x = 0;
	snap->off_y = 0;
	snap->best_x =
		VIPS_MAX(1, imageuiregion_snap_threshold / imageui_get_zoom(imageui));
	snap->best_y =
		VIPS_MAX(1, imageuiregion_snap_threshold / imageui_get_zoom(imageui));

	snapped = FALSE;
	slist_map2(imageuiregion->regionviews,
		(SListMap2Fn) imageuiregion_snap_sub, snap, &snapped);

	return snapped;
}

gboolean
imageuiregion_snap_point(Imageuiregion *imageuiregion,
	int x, int y, int *sx, int *sy)
{
	ImageuiregionSnap snap;
	gboolean snapped;

	snap.x = x;
	snap.y = y;
	snapped = imageuiregion_snap(imageuiregion, &snap);

	*sx = x + snap.off_x;
	*sy = y + snap.off_y;

	return snapped;
}

gboolean
imageuiregion_snap_rect(Imageuiregion *imageuiregion,
	VipsRect *in, VipsRect *out)
{
	/* Snap the corners plus the edge centres, take the best score.
	 */
	ImageuiregionSnap snap[8];
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
		snapped |= imageuiregion_snap(imageuiregion, &snap[i]);

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

// (x, y) in gtk cods
Regionview *
imageuiregion_pick_regionview(Imageuiregion *imageuiregion, int x, int y)
{
	for (GSList *p = imageuiregion->regionviews; p; p = p->next) {
		Regionview *regionview = REGIONVIEW(p->data);
		RegionviewResize resize = regionview_hit(regionview, x, y);

		if (resize != REGIONVIEW_RESIZE_NONE)
			return regionview;
	}

	return NULL;
}

static void
imageuiregion_drag_begin(GtkGestureDrag *self,
	gdouble start_x, gdouble start_y, gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(self);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Imageuiregion *imageuiregion = IMAGEUIREGION(user_data);
	Imageui *imageui = imageuiregion->imageui;

	Regionview *regionview;

#ifdef DEBUG_VERBOSE
	printf("imageuiregion_drag_begin: start_x = %g, start_y = %g\n",
		start_x, start_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageuiregion->state) {
	case IMAGEUIREGION_STATE_WAIT:
		regionview =
			imageuiregion_pick_regionview(imageuiregion, start_x, start_y);

		if (regionview) {
			imageuiregion->state = IMAGEUIREGION_STATE_SELECT;
			regionview->resize = regionview_hit(regionview, start_x, start_y);
			imageuiregion->grabbed = regionview;
			g_object_ref(regionview);
			regionview->start_area = regionview->our_area;
		}
		else if (modifiers & GDK_CONTROL_MASK) {
			imageuiregion->state = IMAGEUIREGION_STATE_CREATE;
			double left;
			double top;
			imageui_gtk_to_image(imageui, start_x, start_y, &left, &top);
			imageuiregion_floating_add(imageuiregion, left, top);
		}

		break;

	case IMAGEUIREGION_STATE_SELECT:
		break;

	case IMAGEUIREGION_STATE_CREATE:
		break;

	default:
		break;
	}
}

static void
imageuiregion_regionview_update(Imageuiregion *imageuiregion,
	Regionview *regionview)
{
	regionview->draw_area = regionview->our_area;
	regionview->draw_type = regionview->type;
	imageui_queue_draw(imageuiregion->imageui);
}

static void
imageuiregion_drag_update(GtkGestureDrag *self,
	gdouble offset_x, gdouble offset_y, gpointer user_data)
{
	GtkEventController *controller = GTK_EVENT_CONTROLLER(self);
	GdkModifierType modifiers =
		gtk_event_controller_get_current_event_state(controller);
	Imageuiregion *imageuiregion = IMAGEUIREGION(user_data);
	Imageui *imageui = imageuiregion->imageui;
	Tilesource *tilesource  = imageui_get_tilesource(imageui);

	double zoom = imageui_get_zoom(imageui);

#ifdef DEBUG_VERBOSE
	printf("imageuiregion_drag_update: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageuiregion->state) {
	case IMAGEUIREGION_STATE_SELECT:
		regionview_resize(imageuiregion->grabbed, modifiers,
			tilesource->image_width, tilesource->image_height,
			offset_x / zoom, offset_y / zoom);

		/* Refresh immediately .. gives immediate feedback during drag in large
		 * workspaces, especially on windows.
		 */
		imageuiregion_regionview_update(imageuiregion, imageuiregion->grabbed);

		/* And nudge background recomp.
		 */
		regionview_model_update(imageuiregion->grabbed);

		break;

	case IMAGEUIREGION_STATE_CREATE:
		regionview_resize(imageuiregion->floating, modifiers,
			tilesource->image_width, tilesource->image_height,
			offset_x / zoom, offset_y / zoom);
		imageuiregion_regionview_update(imageuiregion, imageuiregion->floating);
		break;

	default:
		break;
	}
}

static void
imageuiregion_region_new(Imageuiregion *imageuiregion,
	RegionviewType type, VipsRect *rect)
{
	Imageui *imageui = imageuiregion->imageui;
	iImage *iimage = imageui_get_iimage(imageui);
	Row *row = iimage ? HEAPMODEL(iimage)->row : NULL;

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
imageuiregion_drag_end(GtkGestureDrag *self,
	gdouble offset_x, gdouble offset_y, gpointer user_data)
{
	Imageuiregion *imageuiregion = IMAGEUIREGION(user_data);
	Imageui *imageui = imageuiregion->imageui;

#ifdef DEBUG_VERBOSE
	printf("imageuiregion_drag_end: offset_x = %g, offset_y = %g\n",
		offset_x, offset_y);
#endif /*DEBUG_VERBOSE*/

	switch (imageuiregion->state) {
	case IMAGEUIREGION_STATE_WAIT:
		break;

	case IMAGEUIREGION_STATE_SELECT:
		regionview_model_update(imageuiregion->grabbed);
		VIPS_UNREF(imageuiregion->grabbed);
		break;

	case IMAGEUIREGION_STATE_CREATE:
		if (imageuiregion->floating) {
			imageuiregion_region_new(imageuiregion,
				imageuiregion->floating->type,
				&imageuiregion->floating->our_area);
			imageuiregion_floating_remove(imageuiregion);
		}

		imageui_queue_draw(imageui);

		break;

	default:
		break;
	}

	imageuiregion->state = IMAGEUIREGION_STATE_WAIT;
}

static void
imageuiregion_set_cursor(Imageuiregion *imageuiregion)
{
	Imageui *imageui = imageuiregion->imageui;

	RegionviewResize resize;

	resize = REGIONVIEW_RESIZE_NONE;

	if (imageuiregion->grabbed)
		resize = imageuiregion->grabbed->resize;
	else {
		double x_gtk, y_gtk;
		imageui_get_mouse_position_gtk(imageui, &x_gtk, &y_gtk);

		Regionview *regionview =
			imageuiregion_pick_regionview(imageuiregion, x_gtk, y_gtk);
		if (regionview)
			resize = regionview_hit(regionview, x_gtk, y_gtk);
	}

	imageui_set_cursor(imageui, resize);
}

static void
imageuiregion_motion(GtkEventControllerMotion *self,
	gdouble x, gdouble y, gpointer user_data)
{
	Imageuiregion *imageuiregion = IMAGEUIREGION(user_data);

#ifdef DEBUG_VERBOSE
	printf("imageui_motion: x = %g, y = %g\n", x, y);
#endif /*DEBUG_VERBOSE*/

	imageuiregion_set_cursor(imageuiregion);
}

// from the imagedisplay snapshot method: draw any visible regions
static void
imageuiregion_overlay_snapshot(Imagedisplay *imagedisplay,
	GtkSnapshot *snapshot, Imageuiregion *imageuiregion)
{
	for (GSList *p = imageuiregion->regionviews; p; p = p->next) {
		Regionview *regionview = REGIONVIEW(p->data);

		regionview_draw(regionview, snapshot);
	}
}

static void
imageuiregion_init(Imageuiregion *imageuiregion)
{
	GtkWidget *imagedisplay = imageui_get_imagedisplay(imageuiregion->imageui);

#ifdef DEBUG
	printf("imageuiregion_init:\n");
#endif /*DEBUG*/

	g_signal_connect_object(G_OBJECT(imagedisplay), "snapshot",
		G_CALLBACK(imageuiregion_overlay_snapshot), imageuiregion, 0);
}

static void
imageuiregion_class_init(ImageuiregionClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

#ifdef DEBUG
	printf("imageuiregion_class_init:\n");
#endif /*DEBUG*/

	gobject_class->dispose = imageuiregion_dispose;
	gobject_class->set_property = imageuiregion_set_property;
	gobject_class->get_property = imageuiregion_get_property;

	g_object_class_install_property(gobject_class, PROP_IMAGEUI,
		g_param_spec_object("imageui",
			_("Imageui"),
			_("The imageui we paint on"),
			IMAGEUI_TYPE,
			G_PARAM_READWRITE));

}

Imageuiregion *
imageuiregion_new(Imageui *imageui)
{
	Imageuiregion *imageuiregion;

#ifdef DEBUG
	printf("imageuiregion_new:\n");
#endif /*DEBUG*/

	imageuiregion = g_object_new(IMAGEUIREGION_TYPE,
		"imageui", imageui,
		NULL);

	return imageuiregion;
}

