/* display an plot in a drawingarea
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

#include <goffice/goffice.h>

#include <goffice/app/go-plugin.h>
#include <goffice/app/go-plugin-loader-module.h>

#include <goffice/data/go-data-simple.h>

#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-label.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-plot.h>
#include <goffice/graph/gog-series.h>
#include <goffice/graph/gog-grid.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-legend.h>
#include <goffice/graph/gog-chart-map.h>

#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marker.h>

/*
 */
#define DEBUG_VERBOSE
#define DEBUG

#define GOG_UNREF(X) \
    G_STMT_START { \
		if(X) { \
			gog_object_clear_parent(GOG_OBJECT(X)); \
			g_object_unref(G_OBJECT(X)); \
			(X) = NULL; \
		} \
    } G_STMT_END

/* Choose line colours with this. RGB first, then mostly random. We can't use
 * goffice's default colours because we really want the first three to be: red,
 * green, blue.
 */

static GOColor default_colour[] = {
	GO_COLOR_FROM_RGB(255, 0, 0),
	GO_COLOR_FROM_RGB(0, 255, 0),
	GO_COLOR_FROM_RGB(0, 0, 255),
	GO_COLOR_FROM_RGB(100, 0, 102),
	GO_COLOR_FROM_RGB(17, 0, 102),
	GO_COLOR_FROM_RGB(0, 0, 180),
	GO_COLOR_FROM_RGB(0, 53, 255),
	GO_COLOR_FROM_RGB(0, 104, 234),
	GO_COLOR_FROM_RGB(0, 150, 188),
	GO_COLOR_FROM_RGB(0, 205, 170),
	GO_COLOR_FROM_RGB(0, 255, 139),
	GO_COLOR_FROM_RGB(0, 255, 55),
	GO_COLOR_FROM_RGB(40, 255, 40),
	GO_COLOR_FROM_RGB(106, 255, 74),
	GO_COLOR_FROM_RGB(155, 255, 48),
	GO_COLOR_FROM_RGB(209, 255, 21),
	GO_COLOR_FROM_RGB(239, 255, 7),
	GO_COLOR_FROM_RGB(255, 176, 0),
	GO_COLOR_FROM_RGB(255, 110, 0),
	GO_COLOR_FROM_RGB(255, 50, 0),
	GO_COLOR_FROM_RGB(196, 0, 0)
};

/* Build a GogPlot from a Plot.
 */
static GogPlot *
plot_new_gplot(Plot *plot)
{
	GogPlot *gplot;

	if (plot->style == PLOT_STYLE_BAR)
		gplot = gog_plot_new_by_name("GogHistogramPlot");
	else
		gplot = gog_plot_new_by_name("GogXYPlot");

	switch (plot->style) {
	case PLOT_STYLE_POINT:
		g_object_set(gplot, "default-style-has-lines", FALSE, NULL);
		break;

	case PLOT_STYLE_LINE:
		g_object_set(gplot, "default-style-has-markers", FALSE, NULL);
		break;

	case PLOT_STYLE_SPLINE:
		g_object_set(gplot, "default-style-has-markers", FALSE, NULL);
		g_object_set(gplot, "use-splines", TRUE, NULL);
		break;

	case PLOT_STYLE_BAR:
		break;

	default:
		g_assert(FALSE);
	}

	for (int i = 0; i < plot->columns; i++) {
		GogSeries *series;
		GOData *data;
		GError *error;
		char *caption;

		series = gog_plot_new_series(gplot);
		data = go_data_vector_val_new(plot->xcolumn[i], plot->rows, NULL);
		gog_series_set_dim(series, 0, data, &error);
		data = go_data_vector_val_new(plot->ycolumn[i], plot->rows, NULL);
		gog_series_set_dim(series, 1, data, &error);

		if ((caption = (char *) g_slist_nth_data(plot->series_captions, i)))
			caption = g_strdup(caption);
		else
			caption = g_strdup_printf("Band %d", i);
		data = go_data_scalar_str_new(caption, TRUE);
		gog_series_set_name(series, (GODataScalar *) data, &error);

		if (i < VIPS_NUMBER(default_colour)) {
			GOStyle *style;

			style = go_styled_object_get_style(GO_STYLED_OBJECT(series));

			style->line.color = default_colour[i];
			style->line.auto_color = FALSE;

			go_marker_set_fill_color(style->marker.mark,
				default_colour[i]);
			style->marker.auto_fill_color = FALSE;

			/* Could match fill, but black everywhere looks nicer.
			 */
			go_marker_set_outline_color(style->marker.mark,
				GO_COLOR_FROM_RGB(0, 0, 0));
			style->marker.auto_outline_color = FALSE;

			gog_object_request_update(GOG_OBJECT(series));
		}
	}

	return gplot;
}

static void
plot_grid_add(GogAxis *axis)
{
	GogGridLine *ggl;

	if (!gog_object_get_child_by_name(GOG_OBJECT(axis), "MajorGrid")) {
		ggl = g_object_new(GOG_TYPE_GRID_LINE, "is-minor", FALSE, NULL);
		gog_object_add_by_name(GOG_OBJECT(axis), "MajorGrid", GOG_OBJECT(ggl));
	}

	if (!gog_object_get_child_by_name(GOG_OBJECT(axis), "MinorGrid")) {
		ggl = g_object_new(GOG_TYPE_GRID_LINE, "is-minor", TRUE, NULL);
		gog_object_add_by_name(GOG_OBJECT(axis), "MinorGrid", GOG_OBJECT(ggl));
	}

	g_object_set(axis, "pos", GOG_AXIS_CROSS, NULL);
}

static void
plot_set_title(GogObject *thing, const char *role, const char *text)
{
	GogObject *title;

	title = gog_object_get_child_by_name(thing, role);
	if (text && !title) {
		title = g_object_new(GOG_TYPE_LABEL, NULL);
		gog_object_add_by_name(thing, role, title);
	}
	else if (!text && title) {
		gog_object_clear_parent(title);
		VIPS_UNREF(title);
	}

	if (text && title) {
		GOData *data;

		data = go_data_scalar_str_new(text, FALSE);
		gog_dataset_set_dim(GOG_DATASET(title), 0, data, NULL);
	}
}

static void
plot_style_window(Plot *plot, GogChart *gchart)
{
	GSList *axes;
	GogAxis *axis;
	GogObject *legend;

	axes = gog_chart_get_axes(gchart, GOG_AXIS_X);
	axis = GOG_AXIS(axes->data);
	g_slist_free(axes);

	gog_axis_set_bounds(axis, plot->xmin, plot->xmax);
	plot_set_title(GOG_OBJECT(axis), "Label", plot->xcaption);
	plot_grid_add(axis);

	axes = gog_chart_get_axes(gchart, GOG_AXIS_Y);
	axis = GOG_AXIS(axes->data);
	g_slist_free(axes);

	gog_axis_set_bounds(axis, plot->ymin, plot->ymax);
	plot_set_title(GOG_OBJECT(axis), "Label", plot->ycaption);
	plot_grid_add(axis);

	legend = gog_object_get_child_by_name(GOG_OBJECT(gchart), "Legend");
	if (plot->columns > 1 &&
		!legend) {
		legend = g_object_new(GOG_TYPE_LEGEND, NULL);
		gog_object_add_by_name(GOG_OBJECT(gchart),
			"Legend", GOG_OBJECT(legend));
	}
	else if (plot->columns == 1 &&
		legend) {
		gog_object_clear_parent(legend);
		VIPS_UNREF(legend);
	}

	plot_set_title(GOG_OBJECT(gchart), "Title", plot->caption);
}

static void
plot_style_thumbnail(Plot *plot, GogChart *gchart)
{
	GSList *axes;
	GogAxis *axis;

	axes = gog_chart_get_axes(gchart, GOG_AXIS_X);
	axis = GOG_AXIS(axes->data);
	g_slist_free(axes);

	g_object_set(axis,
		"major-tick-labeled", FALSE,
		"major-tick-size-pts", 0,
		"pos", GOG_AXIS_CROSS,
		NULL);
	gog_axis_set_bounds(axis, plot->xmin, plot->xmax);

	axes = gog_chart_get_axes(gchart, GOG_AXIS_Y);
	axis = GOG_AXIS(axes->data);
	g_slist_free(axes);

	g_object_set(axis,
		"major-tick-labeled", FALSE,
		"major-tick-size-pts", 0,
		"pos", GOG_AXIS_CROSS,
		NULL);
	gog_axis_set_bounds(axis, plot->ymin, plot->ymax);
}

/*
Imageinfo *
plot_to_image(Plot *plot, Reduce *rc, double dpi)
{
	GogGraph *ggraph;
	GogChart *gchart;
	GogPlot *gplot;
	GogRenderer *renderer;
	GdkPixbuf *pixbuf;
	double width_in_pts, height_in_pts;
	Imageinfo *ii;

	ggraph = g_object_new(GOG_TYPE_GRAPH, NULL);

	gchart = g_object_new(GOG_TYPE_CHART, NULL);
	gog_object_add_by_name(GOG_OBJECT(ggraph),
		"Chart", GOG_OBJECT(gchart));

	gplot = plot_new_gplot(plot);
	gog_object_add_by_name(GOG_OBJECT(gchart), "Plot", GOG_OBJECT(gplot));

	plot_style_main(plot, gchart);

	renderer = gog_renderer_new(ggraph);

	gog_graph_force_update(ggraph);

	gog_graph_get_size(ggraph, &width_in_pts, &height_in_pts);

	gog_renderer_update(renderer,
		width_in_pts * dpi / 72.0, height_in_pts * dpi / 72.0);

	pixbuf = gog_renderer_get_pixbuf(renderer);

	if (!(ii = imageinfo_new_from_pixbuf(main_imageinfogroup, rc->heap,
			  pixbuf))) {
		VIPS_UNREF(renderer);
		VIPS_UNREF(ggraph);

		return NULL;
	}

	VIPS_UNREF(renderer);
	VIPS_UNREF(ggraph);

	return ii;
}
 */

struct _Plotdisplay {
	GtkDrawingArea parent_instance;

	/* The Plot model we draw. We hold a ref to this.
	 */
	Plot *plot;

	/* Draw in thumbnail (low detail) mode.
	 */
	gboolean thumbnail;

	GogGraph *ggraph;
	GogChart *gchart;
	GogPlot *gplot;
};

G_DEFINE_TYPE(Plotdisplay, plotdisplay, GTK_TYPE_DRAWING_AREA);

enum {
	/* Set the plot model we display.
	 */
	PROP_PLOT = 1,

	/* Draw in low-detail mode.
	 */
	PROP_THUMBNAIL,

};

static void
plotdisplay_dispose(GObject *object)
{
	Plotdisplay *plotdisplay = (Plotdisplay *) object;

#ifdef DEBUG
	printf("plotdisplay_dispose:\n");
#endif /*DEBUG*/

	VIPS_UNREF(plotdisplay->plot);
	GOG_UNREF(plotdisplay->gplot);

	G_OBJECT_CLASS(plotdisplay_parent_class)->dispose(object);
}

static void
plotdisplay_plot_changed(Plot *plot, Plotdisplay *plotdisplay)
{
#ifdef DEBUG
	printf("plotdisplay_tilecache_changed:\n");
#endif /*DEBUG*/

	if (plotdisplay->plot) {
		GOG_UNREF(plotdisplay->gplot);

		plotdisplay->gplot = plot_new_gplot(plot);
		gog_object_add_by_name(GOG_OBJECT(plotdisplay->gchart),
			"Plot", GOG_OBJECT(plotdisplay->gplot));

		if (plotdisplay->thumbnail)
			plot_style_thumbnail(plot, plotdisplay->gchart);
		else
			plot_style_window(plot, plotdisplay->gchart);

		gtk_widget_queue_draw(GTK_WIDGET(plotdisplay));
	}
}

static void
plotdisplay_set_plot(Plotdisplay *plotdisplay, Plot *plot)
{
	if (plotdisplay->plot != plot) {
		VIPS_UNREF(plotdisplay->plot);

		if (plot) {
			plotdisplay->plot = plot;
			g_object_ref(plotdisplay->plot);

			g_signal_connect_object(plot, "changed",
				G_CALLBACK(plotdisplay_plot_changed),
				plotdisplay, 0);

			/* Do initial change to init.
			 */
			plotdisplay_plot_changed(plot, plotdisplay);
		}
	}
}

#ifdef DEBUG
static const char *
plotdisplay_property_name(guint prop_id)
{
	switch (prop_id) {
	case PROP_PLOT:
		return "PLOT";
		break;

	case PROP_THUMBNAIL:
		return "THUMBNAIL";
		break;

	default:
		return "<unknown>";
	}
}
#endif /*DEBUG*/

static void
plotdisplay_set_property(GObject *object,
	guint prop_id, const GValue *value, GParamSpec *pspec)
{
	Plotdisplay *plotdisplay = (Plotdisplay *) object;

#ifdef DEBUG
	{
		g_autofree char *str = g_strdup_value_contents(value);
		printf("plotdisplay_set_property: %s = %s\n",
			plotdisplay_property_name(prop_id), str);
	}
#endif /*DEBUG*/

	switch (prop_id) {
	case PROP_PLOT:
		plotdisplay_set_plot(plotdisplay, g_value_get_object(value));
		break;

	case PROP_THUMBNAIL:
		plotdisplay->thumbnail = g_value_get_boolean(value);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
plotdisplay_get_property(GObject *object,
	guint prop_id, GValue *value, GParamSpec *pspec)
{
	Plotdisplay *plotdisplay = (Plotdisplay *) object;

	switch (prop_id) {
	case PROP_PLOT:
		g_value_set_object(value, plotdisplay->plot);
		break;

	case PROP_THUMBNAIL:
		g_value_set_boolean(value, plotdisplay->thumbnail);
		break;

	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
		break;
	}
}

static void
plotdisplay_draw_function(GtkDrawingArea *area,
	cairo_t *cr, int width, int height, gpointer user_data)
{
	Plotdisplay *plotdisplay = (Plotdisplay *) user_data;

#ifdef DEBUG_VERBOSE
	printf("plotdisplay_draw_function:\n");
#endif /*DEBUG_VERBOSE*/

    if (plotdisplay->plot) {
		GogRenderer *renderer = gog_renderer_new(plotdisplay->ggraph);
		// do we need this?
		//gog_graph_force_update(plotdisplay->ggraph);
		gog_renderer_update(renderer, width, height);

		cairo_surface_t *surface = gog_renderer_get_cairo_surface (renderer);
        cairo_rectangle(cr, 0, 0, width, height);
        cairo_clip(cr);
        cairo_set_source_surface(cr, surface, 0, 0);
        cairo_paint(cr);

		VIPS_UNREF(renderer);
    }
}

static void
plotdisplay_init(Plotdisplay *plotdisplay)
{
#ifdef DEBUG
	printf("plotdisplay_init:\n");
#endif /*DEBUG*/

	gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(plotdisplay),
		plotdisplay_draw_function, plotdisplay, NULL);

	// a graph holding a plot ... our chart goes into the plot
	plotdisplay->ggraph = g_object_new(GOG_TYPE_GRAPH, NULL);
	plotdisplay->gchart = g_object_new(GOG_TYPE_CHART, NULL);
	gog_object_add_by_name(GOG_OBJECT(plotdisplay->ggraph),
		"Chart", GOG_OBJECT(plotdisplay->gchart));
}

static void
plotdisplay_class_init(PlotdisplayClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

#ifdef DEBUG
	printf("plotdisplay_class_init:\n");
#endif /*DEBUG*/

	gobject_class->dispose = plotdisplay_dispose;
	gobject_class->set_property = plotdisplay_set_property;
	gobject_class->get_property = plotdisplay_get_property;

	g_object_class_install_property(gobject_class, PROP_PLOT,
		g_param_spec_object("plot",
			_("Plot"),
			_("Plot model to draw"),
			PLOT_TYPE,
			G_PARAM_READWRITE));

	g_object_class_install_property(gobject_class, PROP_THUMBNAIL,
		g_param_spec_boolean("thumbnail",
			_("Thumbnail"),
			_("Display a plot in thumbnail mode"),
			FALSE,
			G_PARAM_READWRITE));

    libgoffice_init();
    go_plugins_init(NULL, NULL, NULL, NULL, TRUE, GO_TYPE_PLUGIN_LOADER_MODULE);
}

Plotdisplay *
plotdisplay_new(Plot *plot)
{
	Plotdisplay *plotdisplay;

#ifdef DEBUG
	printf("plotdisplay_new:\n");
#endif /*DEBUG*/

	plotdisplay = g_object_new(plotdisplay_get_type(),
		"plot", plot,
		NULL);

	return plotdisplay;
}
