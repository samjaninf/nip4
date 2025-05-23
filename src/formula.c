/* display a caption/value label pair, on a click display the formula in an
 * entry widget
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

G_DEFINE_TYPE(Formula, formula, GTK_TYPE_WIDGET)

/* Our signals.
 */
enum {
	EDIT,
	CHANGED,
	ACTIVATE,
	LAST_SIGNAL
};

static guint formula_signals[LAST_SIGNAL] = { 0 };

/* Formula needing a refresh.
 */
static GSList *formula_refresh_all = NULL;

/* The idle we add if there are any formula needing a refresh.
 */
static gint formula_refresh_idle = 0;

/* Unqueue a refresh.
 */
static void
formula_refresh_unqueue(Formula *formula)
{
	if (formula->refresh_queued) {
		formula_refresh_all = g_slist_remove(formula_refresh_all, formula);
		formula->refresh_queued = FALSE;

		if (!formula_refresh_all)
			VIPS_FREEF(g_source_remove, formula_refresh_idle);
	}
}

static void
formula_refresh(Formula *formula)
{
#ifdef DEBUG
	printf("formula_refresh\n");
#endif /*DEBUG*/

	/* Set edit mode.
	 */
	if (formula->edit &&
		!gtk_widget_is_visible(formula->entry)) {
		/* Tell everyone we are going into edit mode ... used to add to
		 * resettable, for example.
		 */
		g_signal_emit(G_OBJECT(formula), formula_signals[EDIT], 0);

		gtk_widget_set_visible(formula->entry, TRUE);
		gtk_widget_set_visible(formula->right_label, FALSE);
		formula->changed = FALSE;
	}
	else if (!formula->edit &&
		gtk_widget_is_visible(formula->entry)) {
		gtk_widget_set_visible(formula->right_label, TRUE);
		gtk_widget_set_visible(formula->entry, FALSE);
	}

	/* Don't update the formula display if the user has edited the text ...
	 * we shouldn't destroy their work.
	 */
	if (formula->expr &&
		!formula->changed)
		g_object_set(formula->entry, "text", formula->expr, NULL);

	if (formula->caption) {
		set_glabel(formula->left_label, _("%s:"), formula->caption);
		gtk_widget_set_visible(formula->left_label, TRUE);
	}
	else
		gtk_widget_set_visible(formula->left_label, FALSE);

	if (formula->value)
		/* Just display the first line of the formula ... it can be
		 * mutiline for class members, for example.
		 */
		set_glabel1(formula->right_label, "%s", formula->value);

	if (formula->edit &&
		formula->needs_focus) {
		// will select text automatically
		ientry_grab_focus(IENTRY(formula->entry));
		formula->needs_focus = FALSE;
	}
}

static gboolean
formula_refresh_idle_cb(void *user_data)
{
	formula_refresh_idle = 0;

	while (formula_refresh_all) {
		Formula *formula = FORMULA(formula_refresh_all->data);

		formula_refresh_unqueue(formula);
		formula_refresh(formula);
	}

	return FALSE;
}

static void
formula_refresh_queue(Formula *formula)
{
	if (!formula->refresh_queued) {
		formula_refresh_all = g_slist_prepend(formula_refresh_all, formula);
		formula->refresh_queued = TRUE;

		if (!formula_refresh_idle)
			formula_refresh_idle = g_idle_add(formula_refresh_idle_cb, NULL);
	}
}

static void
formula_dispose(GObject *object)
{
	Formula *formula;

#ifdef DEBUG
	printf("formula_dispose\n");
#endif /*DEBUG*/

	g_return_if_fail(object != NULL);
	g_return_if_fail(IS_FORMULA(object));

	/* My instance destroy stuff.
	 */
	formula = FORMULA(object);

	formula_refresh_unqueue(formula);
	gtk_widget_dispose_template(GTK_WIDGET(formula), FORMULA_TYPE);

	G_OBJECT_CLASS(formula_parent_class)->dispose(object);
}

/* Change edit mode.
 */
void
formula_set_edit(Formula *formula, gboolean edit)
{
#ifdef DEBUG
	printf("formula_set_edit: %d\n", edit);
#endif /*DEBUG*/

	if (formula->edit != edit) {
		formula->edit = edit;
		formula_refresh_queue(formula);
	}

	/* Can't have been edited yet, whichever way we're turning edit.
	 */
	formula->changed = FALSE;
}

/* Grab focus on next refresh.
 */
void
formula_set_needs_focus(Formula *formula, gboolean needs_focus)
{
#ifdef DEBUG
	printf("formula_set_needs_focus: %d\n", needs_focus);
#endif /*DEBUG*/

	if (formula->needs_focus != needs_focus) {
		formula->needs_focus = needs_focus;
		formula_refresh_queue(formula);
	}
}

/* Change sensitive mode.
 */
void
formula_set_sensitive(Formula *formula, gboolean sensitive)
{
#ifdef DEBUG
	printf("formula_set_sensitive: %d\n", sensitive);
#endif /*DEBUG*/

	if (formula->sensitive != sensitive) {
		formula->sensitive = sensitive;

		if (!formula->sensitive)
			formula_set_edit(formula, FALSE);

		formula_refresh_queue(formula);
	}
}

/* Re-read the text. TRUE if we saw a change.
 */
gboolean
formula_scan(Formula *formula)
{
	gboolean changed;

#ifdef DEBUG
	printf("formula_scan\n");
#endif /*DEBUG*/

	changed = FALSE;

	/* Should be in edit mode.
	 */
	if (formula->edit &&
		gtk_widget_is_visible(formula->entry)) {
		/* There should be some text.
		 */
		g_autofree char *text = NULL;
		g_object_get(formula->entry, "text", &text, NULL);
		if (text &&
			strspn(text, WHITESPACE) != strlen(text)) {
			VIPS_SETSTR(formula->expr, text);
			changed = TRUE;
		}

		formula_set_edit(formula, FALSE);
	}

	return changed;
}

static void
formula_changed(GtkEntry *self, Formula *formula)
{
#ifdef DEBUG
	printf("formula_changed:\n");
#endif /*DEBUG*/

	// there has been a keypress ... we will probably need scanning
	g_signal_emit(G_OBJECT(formula), formula_signals[CHANGED], 0);
}

static void
formula_cancel(GtkEntry *self, Formula *formula)
{
#ifdef DEBUG
	printf("formula_cancel:\n");
#endif /*DEBUG*/

	formula_set_edit(formula, FALSE);
}

/* Activated!
 */
static void
formula_activate(GtkEntry *self, Formula *formula)
{
#ifdef DEBUG
	printf("formula_activate:\n");
#endif /*DEBUG*/

	g_signal_emit(G_OBJECT(formula), formula_signals[ACTIVATE], 0);
}

static void
formula_pressed(GtkGestureClick *gesture,
	guint n_press, double x, double y, Formula *formula)
{
#ifdef DEBUG
	printf("formula_pressed:\n");
#endif /*DEBUG*/

	if (formula->sensitive &&
		!formula->edit) {
		formula_set_edit(formula, TRUE);
		formula_set_needs_focus(formula, TRUE);
	}
}

static void
formula_real_changed(Formula *formula)
{
#ifdef DEBUG
	printf("formula_real_changed:\n");
#endif /*DEBUG*/

	formula->changed = TRUE;
}

static void
formula_class_init(FormulaClass *class)
{
	GObjectClass *gobject_class = (GObjectClass *) class;

	BIND_RESOURCE("formula.ui");
	BIND_LAYOUT();

	BIND_CALLBACK(formula_changed);
	BIND_CALLBACK(formula_cancel);
	BIND_CALLBACK(formula_pressed);
	BIND_CALLBACK(formula_activate);

	BIND_VARIABLE(Formula, hbox);
	BIND_VARIABLE(Formula, left_label);
	BIND_VARIABLE(Formula, right_label);
	BIND_VARIABLE(Formula, entry);

	gobject_class->dispose = formula_dispose;

	/* Create signals.
	 */
	formula_signals[EDIT] = g_signal_new("edit",
		G_OBJECT_CLASS_TYPE(gobject_class),
		G_SIGNAL_RUN_FIRST,
		G_STRUCT_OFFSET(FormulaClass, changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	formula_signals[CHANGED] = g_signal_new("changed",
		G_OBJECT_CLASS_TYPE(gobject_class),
		G_SIGNAL_RUN_FIRST,
		G_STRUCT_OFFSET(FormulaClass, changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	formula_signals[ACTIVATE] = g_signal_new("activate",
		G_OBJECT_CLASS_TYPE(gobject_class),
		G_SIGNAL_RUN_FIRST,
		G_STRUCT_OFFSET(FormulaClass, activate),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);

	/* Init methods.
	 */
	class->changed = formula_real_changed;
}

static void
formula_init(Formula *formula)
{
	gtk_widget_init_template(GTK_WIDGET(formula));
}

Formula *
formula_new(void)
{
	Formula *formula = g_object_new(FORMULA_TYPE, NULL);

	return formula;
}

void
formula_set_caption(Formula *formula, const char *caption)
{
	if (!caption &&
		formula->caption) {
		VIPS_FREE(formula->caption);
		formula_refresh_queue(formula);
	}
	else if (caption &&
		(!formula->caption ||
 		 !g_str_equal(caption, formula->caption))) {
		VIPS_SETSTR(formula->caption, caption);
		formula_refresh_queue(formula);
	}
}

void
formula_set_value_expr(Formula *formula, const char *value, const char *expr)
{
#ifdef DEBUG
	printf("formula_set_value_expr: value=\"%s\", expr=\"%s\"\n",
		value, expr);
#endif /*DEBUG*/

	if (value &&
		(!formula->value ||
		 !g_str_equal(value, formula->value))) {
		VIPS_SETSTR(formula->value, value);
		formula_refresh_queue(formula);
	}

	if (expr &&
		(!formula->expr ||
		 !g_str_equal(expr, formula->expr))) {
		VIPS_SETSTR(formula->expr, expr);
		formula_refresh_queue(formula);
	}
}
