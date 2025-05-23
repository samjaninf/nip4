// watch an iobject and call _refresh on idle if it changes ... the base class
// for all of our view wisgets

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
#define DEBUG
 */

/* Time each refresh
#define DEBUG_TIME
 */

#include "nip4.h"

G_DEFINE_TYPE(vObject, vobject, GTK_TYPE_WIDGET)

static Queue *vobject_dirty = NULL;

static gint vobject_refresh_timeout = 0;

/* Remove from refresh queue.
 */
static void
vobject_refresh_dequeue(vObject *vobject)
{
	if (vobject->dirty) {
#ifdef DEBUG
		printf("vobject_refresh_dequeue: \"%s\"\n",
			G_OBJECT_TYPE_NAME(vobject));
#endif /*DEBUG*/

		vobject->dirty = FALSE;
		queue_remove(vobject_dirty, vobject);
	}
}

#ifdef DEBUG_TIME

/* Refresh all vobjects at once and time them.
 */
static gboolean
vobject_refresh_timeout_cb(gpointer user_data)
{
	static GTimer *refresh_timer = NULL;
	double last_elapsed;
	double worst_time = 0.0;
	int worst_index = -1;
	void *data;
	int n;

	vobject_refresh_timeout = 0;

	if (!refresh_timer)
		refresh_timer = g_timer_new();

	g_timer_reset(refresh_timer);

	printf("vobject_idle_refresh: starting ...\n");

	for (n = 0; (data = queue_head(vobject_dirty)); n++) {
		vObject *vobject = VOBJECT(data);

		double elapsed;

		vobject->dirty = FALSE;
		vobject_refresh(vobject);
		elapsed = g_timer_elapsed(refresh_timer, NULL);

		if (elapsed - last_elapsed > worst_time) {
			worst_time = elapsed - last_elapsed;
			worst_index = n;
		}

		last_elapsed = elapsed;
	}

	printf("vobject_idle_refresh: done after %gs (%d refreshes)\n",
		g_timer_elapsed(refresh_timer, NULL), n);

	printf("vobject_idle_refresh: worst %gs (refresh %d)\n",
		worst_time, worst_index);

	return FALSE;
}

#else /*DEBUG_TIME*/

/* Refresh stuff off the dirty list.
 */
static gboolean
vobject_refresh_timeout_cb(gpointer user_data)
{
	void *data;

#ifdef DEBUG
	printf("vobject_refresh_timeout_cb:\n");
#endif /*DEBUG*/

	vobject_refresh_timeout = 0;

	while ((data = queue_head(vobject_dirty))) {
		vObject *vobject = VOBJECT(data);

#ifdef DEBUG
		printf("vobject_refresh_timeout_cb: starting \"%s\" (%p)\n",
			G_OBJECT_TYPE_NAME(vobject), vobject);
#endif /*DEBUG*/

		/* We must clear dirty before we _refresh() so that if the
		 * _refresh() indirectly triggers another update, we will
		 * _refresh() again.
		 */
		vobject->dirty = FALSE;
		vobject_refresh(vobject);
	}

	return FALSE;
}

#endif /*DEBUG_TIME*/

/* Mark something for refresh. We seldom call this directly ... just change
 * the iobject and all vobjects will have a refresh queued.
 */
void *
vobject_refresh_queue(vObject *vobject)
{
	if (!vobject->dirty) {
#ifdef DEBUG
		printf("vobject_refresh_queue: %s (%p)",
			G_OBJECT_TYPE_NAME(vobject), vobject);
		if (vobject->iobject)
			printf(", iobject %s \"%s\"",
				G_OBJECT_TYPE_NAME(vobject->iobject), vobject->iobject->name);
		printf("\n");
#endif /*DEBUG*/

		vobject->dirty = TRUE;
		queue_add(vobject_dirty, vobject);

		VIPS_FREEF(g_source_remove, vobject_refresh_timeout);
		vobject_refresh_timeout = g_timeout_add(1,
			(GSourceFunc) vobject_refresh_timeout_cb, NULL);
	}

	return NULL;
}

/* Called for iobject changed signal ... queue a refresh.
 */
static void
vobject_iobject_changed(iObject *iobject, vObject *vobject)
{
#ifdef DEBUG
	printf("vobject_iobject_changed: %s %s \"%s\"\n",
		G_OBJECT_TYPE_NAME(vobject),
		G_OBJECT_TYPE_NAME(iobject),
		iobject->name);
#endif /*DEBUG*/

	vobject_refresh_queue(vobject);
}

/* Called for iobject destroy signal ... kill the vobject too.
 */
static void
vobject_iobject_destroy(iObject *iobject, vObject *vobject)
{
#ifdef DEBUG
	printf("vobject_iobject_destroy:\n"
		   "\tmodel %s \"%s\"\n"
		   "\tview %s\n",
		G_OBJECT_TYPE_NAME(iobject), iobject->name,
		G_OBJECT_TYPE_NAME(vobject));
#endif /*DEBUG*/

	view_child_remove(VIEW(vobject));

	// it's a weakref going away
	g_assert(!vobject->iobject || vobject->iobject == iobject);
	vobject->iobject = NULL;
}

/* Link to iobject.
 */
void
vobject_link(vObject *vobject, iObject *iobject)
{
	vObjectClass *vobject_class = VOBJECT_GET_CLASS(vobject);

	g_assert(!vobject->iobject);

	if (vobject_class->link)
		vobject_class->link(vobject, iobject);

	/* Queue a refresh ... we always need at least one.
	 */
	vobject_refresh_queue(vobject);
}

static void
vobject_dispose(GObject *object)
{
	vObject *vobject;

	g_return_if_fail(object != NULL);
	g_return_if_fail(IS_VOBJECT(object));

	vobject = VOBJECT(object);

#ifdef DEBUG
	printf("vobject_dispose: \"%s\"\n", G_OBJECT_TYPE_NAME(object));
#endif /*DEBUG*/

	vobject_refresh_dequeue(vobject);

	G_OBJECT_CLASS(vobject_parent_class)->dispose(object);
}

static void
vobject_finalize(GObject *gobject)
{
#ifdef DEBUG
	printf("vobject_finalize: \"%s\"\n", G_OBJECT_TYPE_NAME(gobject));
#endif /*DEBUG*/

	G_OBJECT_CLASS(vobject_parent_class)->finalize(gobject);
}

static void
vobject_real_refresh(vObject *vobject)
{
#ifdef DEBUG
	printf("vobject_real_refresh: %p %s\n",
		vobject, G_OBJECT_TYPE_NAME(vobject));
#endif /*DEBUG*/
}

static void
vobject_real_link(vObject *vobject, iObject *iobject)
{
	g_assert(!vobject->iobject);

	// a weak ref ... we break the link when the model is destroyed
	vobject->iobject = iobject;
	g_signal_connect_object(iobject, "destroy",
		G_CALLBACK(vobject_iobject_destroy), vobject, 0);

	g_signal_connect_object(iobject, "changed",
		G_CALLBACK(vobject_iobject_changed), vobject, G_CONNECT_DEFAULT);
}

static void
vobject_class_init(vObjectClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS(class);

	gobject_class->finalize = vobject_finalize;
	gobject_class->dispose = vobject_dispose;

	class->refresh = vobject_real_refresh;
	class->link = vobject_real_link;

	/* Static init.
	 */
	vobject_dirty = queue_new();
}

static void
vobject_init(vObject *vobject)
{
	/* All new vobjects will need refreshing.
	 */
	vobject_refresh_queue(vobject);
}

/* Trigger the refresh method for a vobject immediately ... we usually queue
 * and wait for idle, but this can be better for interactive stuff.
 */
void *
vobject_refresh(vObject *vobject)
{
	vObjectClass *vobject_class = VOBJECT_GET_CLASS(vobject);

	// don't refresh if there's no model
	if (vobject->iobject &&
		vobject_class->refresh)
		vobject_class->refresh(vobject);

	return NULL;
}
