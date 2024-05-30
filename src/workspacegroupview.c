/* main processing window
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

#include "nip4.h"

/*
#define DEBUG
 */

G_DEFINE_TYPE(Workspacegroupview, workspacegroupview, VIEW_TYPE)

static void
workspacegroupview_dispose(GObject *object)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(object);

#ifdef DEBUG
	printf("workspacegroupview_dispose:\n");
#endif /*DEBUG*/

	gtk_widget_dispose_template(GTK_WIDGET(wsgview), WORKSPACEGROUPVIEW_TYPE);

	G_OBJECT_CLASS(workspacegroupview_parent_class)->dispose(object);
}

static void
workspacegroupview_realize(GtkWidget *widget)
{
#ifdef DEBUG
	printf("workspacegroupview_realize\n");
#endif /*DEBUG*/

	GTK_WIDGET_CLASS(workspacegroupview_parent_class)->realize(widget);

	/* Mark us as a symbol drag-to widget.
	 */
	set_symbol_drag_type(widget);
}

static void
workspacegroupview_child_add(View *parent, View *child)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(parent);
	Workspaceview *wview = WORKSPACEVIEW(child);
	Workspace *ws = WORKSPACE(VOBJECT(child)->iobject);

	printf("workspacegroupview_child_add:\n");

	VIEW_CLASS(workspacegroupview_parent_class)->child_add(parent, child);

	gtk_notebook_insert_page(GTK_NOTEBOOK(wsgview->notebook),
		GTK_WIDGET(wview), GTK_WIDGET(wview->label), ICONTAINER(ws)->pos);

	gtk_notebook_set_tab_reorderable(GTK_NOTEBOOK(wsgview->notebook),
		GTK_WIDGET(wview), TRUE);
	gtk_notebook_set_tab_detachable(GTK_NOTEBOOK(wsgview->notebook),
		GTK_WIDGET(wview), TRUE);
}

static void
workspacegroupview_child_remove(View *parent, View *child)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(parent);
	Workspace *ws = WORKSPACE(VOBJECT(child)->iobject);

	printf("workspacegroupview_child_remove:\n");

	VIEW_CLASS(workspacegroupview_parent_class)->child_remove(parent, child);

	// must be at the end since this will unref the child
	gtk_notebook_remove_page(GTK_NOTEBOOK(wsgview->notebook),
		ICONTAINER(ws)->pos);
}

static void
workspacegroupview_child_position(View *parent, View *child)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(parent);
	Workspaceview *wview = WORKSPACEVIEW(child);

	printf("workspacegroupview_child_position:\n");

	gtk_notebook_reorder_child(GTK_NOTEBOOK(wsgview->notebook),
		GTK_WIDGET(wview), ICONTAINER(wview)->pos);

	VIEW_CLASS(workspacegroupview_parent_class)->child_position(parent, child);
}

static void
workspacegroupview_child_front(View *parent, View *child)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(parent);
	Workspaceview *wview = WORKSPACEVIEW(child);

	int page;
	GtkWidget *current_front;

	printf("workspacegroupview_child_front:\n");

	page = gtk_notebook_get_current_page(GTK_NOTEBOOK(wsgview->notebook));
	current_front =
		gtk_notebook_get_nth_page(GTK_NOTEBOOK(wsgview->notebook), page);

	if (current_front != GTK_WIDGET(wview)) {
		page = gtk_notebook_page_num(GTK_NOTEBOOK(wsgview->notebook),
			GTK_WIDGET(wview));
		gtk_notebook_set_current_page(GTK_NOTEBOOK(wsgview->notebook), page);
	}
}

static void
workspacegroupview_new_tab_clicked(GtkButton *button, void *user_data)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(user_data);
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);

	if (!workspace_new_blank(wsg))
		mainwindow_error(MAINWINDOW(view_get_window(VIEW(wsgview))));
}

static void
workspacegroupview_class_init(WorkspacegroupviewClass *class)
{
	GtkWidgetClass *widget_class = (GtkWidgetClass *) class;
	ViewClass *view_class = (ViewClass *) class;

	BIND_RESOURCE("workspacegroupview.ui");

	gtk_widget_class_set_layout_manager_type(GTK_WIDGET_CLASS(class),
		GTK_TYPE_BIN_LAYOUT);

	BIND_VARIABLE(Workspacegroupview, notebook);

	BIND_CALLBACK(workspacegroupview_new_tab_clicked);

	G_OBJECT_CLASS(class)->dispose = workspacegroupview_dispose;

	widget_class->realize = workspacegroupview_realize;

	view_class->child_add = workspacegroupview_child_add;
	view_class->child_remove = workspacegroupview_child_remove;
	view_class->child_position = workspacegroupview_child_position;
	view_class->child_front = workspacegroupview_child_front;
}

static Workspaceview *
notebookpage_get_workspaceview(GtkWidget *page)
{
	return WORKSPACEVIEW(page);
}

// Called for switching the current page, and for page drags between
// notebooks.
static void
workspacegroupview_switch_page_cb(GtkNotebook *notebook,
	GtkWidget *page, guint page_num, gpointer user_data)
{
	Workspaceview *wview = notebookpage_get_workspaceview(page);
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);
	Workspacegroup *old_wsg = WORKSPACEGROUP(ICONTAINER(ws)->parent);
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(user_data);
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);

	printf("workspacegroupview_switch_page_cb:\n");

	// we can come here during gtk_notebook_page_remove
	if (!old_wsg)
		return;

	if (old_wsg != wsg) {
		icontainer_reparent(ICONTAINER(wsg), ICONTAINER(ws), -1);
		filemodel_set_modified(FILEMODEL(wsg), TRUE);
		filemodel_set_modified(FILEMODEL(old_wsg), TRUE);

		// the old mainwindow might now be empty
		mainwindow_cull();
	}

	icontainer_current(ICONTAINER(wsg), ICONTAINER(ws));

	if (ws->compat_major) {
		error_top(_("Compatibility mode"));
		error_sub(_("This workspace was created by version %d.%d. "
					"A set of compatibility menus have been loaded "
					"for this window."),
			ws->compat_major,
			ws->compat_minor);
		mainwindow_error(MAINWINDOW(view_get_window(VIEW(wview))));
	}

    /* How bizarre, pages sometimes fail to set up correctly. Force a
     * resize to get everything to init.
    if (wsgview &&
        wsgview->notebook)
        gtk_widget_queue_resize(GTK_WIDGET(wsgview->notebook));
     */

}

static void
workspacegroupview_page_added_cb(GtkNotebook *notebook,
	GtkWidget *page, guint page_num, gpointer user_data)
{
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(user_data);
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);
	Mainwindow *main = MAINWINDOW(gtk_widget_get_root(GTK_WIDGET(notebook)));

	filemodel_set_window_hint(FILEMODEL(wsg), GTK_WINDOW(main));
}

static GtkNotebook *
workspacegroupview_create_window_cb(GtkNotebook *notebook,
	GtkWidget *page, gpointer user_data)
{
	Workspaceview *wview = WORKSPACEVIEW(page);
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);
	Workspacegroup *wsg = WORKSPACEGROUP(ICONTAINER(ws)->parent);
	Workspaceroot *wsr = wsg->wsr;

	Mainwindow *new_main;
	Workspacegroup *new_wsg;
	char name[256];

	printf("workspacegroupview_create_window_cb: wsg = %s, ws = %s\n",
		IOBJECT(wsg)->name, IOBJECT(ws)->name);

	workspaceroot_name_new(wsr, name);
	new_wsg = workspacegroup_new(wsr);

	printf("workspacegroupview_create_window_cb: new wsg = %s\n", name);

	iobject_set(IOBJECT(new_wsg), name, NULL);

	Mainwindow *main = MAINWINDOW(view_get_window(VIEW(wview)));
	GtkApplication *app = gtk_window_get_application(GTK_WINDOW(main));
	new_main = mainwindow_new(app);
	mainwindow_set_wsg(new_main, new_wsg);
	gtk_window_present(GTK_WINDOW(new_main));
	Workspacegroupview *new_wsgview =
		mainwindow_get_workspacegroupview(new_main);

	return GTK_NOTEBOOK(new_wsgview->notebook);
}

static void
workspacegroupview_page_reordered_cb(GtkNotebook *notebook,
	GtkWidget *page, guint page_num, gpointer user_data)
{
	Workspaceview *wview = WORKSPACEVIEW(page);
	Workspacegroupview *wsgview = WORKSPACEGROUPVIEW(VIEW(wview)->parent);
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);

	int i;
	gboolean changed;

	changed = FALSE;

	for (i = 0; i < gtk_notebook_get_n_pages(notebook); i++) {
		GtkWidget *page_n = gtk_notebook_get_nth_page(notebook, i);
		Workspaceview *wview = WORKSPACEVIEW(page_n);
		Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);

		if (ICONTAINER(ws)->pos != i) {
			ICONTAINER(ws)->pos = i;
			changed = TRUE;
		}
	}

	if (changed) {
		icontainer_pos_sort(ICONTAINER(wsg));
		filemodel_set_modified(FILEMODEL(wsg), TRUE);
	}
}

/*
static void
workspacegroupview_tab_double_cb(GtkNotebook *notebook, GdkEvent *event,
	Workspacegroupview *wsgview)
{
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);

	int i;
	GtkWidget *page;
	GtkWidget *tab;

	i = gtk_notebook_get_n_pages(notebook);
	page = gtk_notebook_get_nth_page(notebook, i - 1);
	tab = gtk_notebook_get_tab_label(notebook, page);

	if (event->button.x > tab->allocation.x + tab->allocation.width &&
		!workspace_new_blank(wsg))
		error_alert(GTK_WIDGET(wsgview));
}

static void
workspacegroupview_add_workspace_cb(GtkWidget *wid,
	Workspacegroupview *wsgview)
{
	Workspacegroup *wsg = WORKSPACEGROUP(VOBJECT(wsgview)->iobject);

	if (!workspace_new_blank(wsg))
		error_alert(GTK_WIDGET(wsgview));
}

static void
workspacegroupview_add_workspace_cb2(GtkWidget *wid, GtkWidget *host,
	Workspacegroupview *wsgview)
{
	workspacegroupview_add_workspace_cb(wid, wsgview);
}

static void
workspacegroupview_load_workspace_cb2(GtkWidget *wid, GtkWidget *host,
	Workspacegroupview *wsgview)
{
	Mainw *mainw = MAINW(iwindow_get_root(GTK_WIDGET(wsgview)));

	mainw_workspace_merge(mainw);
}

static void
workspacegroupview_select_all_cb(GtkWidget *wid, GtkWidget *host,
	Workspaceview *wview)
{
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);

	if (!ws->locked)
		workspace_select_all(ws);
}

static void
workspacegroupview_merge_sub(iWindow *iwnd,
	void *client, iWindowNotifyFn nfn, void *sys)
{
	Filesel *filesel = FILESEL(iwnd);
	Workspace *ws = WORKSPACE(client);
	Workspacegroup *wsg = workspace_get_workspacegroup(ws);

	char *filename;
	Column *col;

	if ((filename = filesel_get_filename(filesel))) {
		icontainer_current(ICONTAINER(wsg), ICONTAINER(ws));

		progress_begin();

		column_clear_last_new();

		if (!workspace_merge_file(ws, filename))
			nfn(sys, IWINDOW_ERROR);
		else {
			symbol_recalculate_all();
			nfn(sys, IWINDOW_YES);
		}

		if ((col = column_get_last_new()))
			column_scrollto(col, MODEL_SCROLL_TOP);

		progress_end();

		g_free(filename);
	}
	else
		nfn(sys, IWINDOW_ERROR);
}

static void
workspacegroupview_merge_cb(GtkWidget *wid, GtkWidget *host,
	Workspaceview *wview)
{
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);
	GtkWindow *window = view_get_window(VIEW(wview));
	GtkWidget *filesel = filesel_new();

	if (ws->locked)
		return;

	iwindow_set_title(IWINDOW(filesel),
		_("Merge Into Tab \"%s\""), IOBJECT(ws)->name);
	filesel_set_flags(FILESEL(filesel), FALSE, FALSE);
	filesel_set_filetype(FILESEL(filesel), filesel_type_workspace, 0);
	iwindow_set_parent(IWINDOW(filesel), GTK_WIDGET(iwnd));
	idialog_set_iobject(IDIALOG(filesel), IOBJECT(ws));
	filesel_set_done(FILESEL(filesel),
		workspacegroupview_merge_sub, ws);
	iwindow_build(IWINDOW(filesel));

	gtk_widget_show(GTK_WIDGET(filesel));
}

static void
workspacegroupview_save_as_sub(iWindow *iwnd,
	void *client, iWindowNotifyFn nfn, void *sys)
{
	Filesel *filesel = FILESEL(iwnd);
	Workspace *ws = WORKSPACE(client);
	Workspacegroup *wsg = workspace_get_workspacegroup(ws);

	char *filename;

	if ((filename = filesel_get_filename(filesel))) {
		icontainer_current(ICONTAINER(wsg), ICONTAINER(ws));
		if (!workspacegroup_save_current(wsg, filename))
			nfn(sys, IWINDOW_ERROR);
		else
			nfn(sys, IWINDOW_YES);

		g_free(filename);
	}
	else
		nfn(sys, IWINDOW_ERROR);
}

static void
workspacegroupview_save_as_cb(GtkWidget *wid, GtkWidget *host,
	Workspaceview *wview)
{
	Workspace *ws = WORKSPACE(VOBJECT(wview)->iobject);
	GtkWindow *window = view_get_window(VIEW(wview));
	GtkWidget *filesel = filesel_new();

	iwindow_set_title(IWINDOW(filesel),
		_("Save Tab \"%s\""), IOBJECT(ws)->name);
	filesel_set_flags(FILESEL(filesel), FALSE, TRUE);
	filesel_set_filetype(FILESEL(filesel), filesel_type_workspace, 0);
	iwindow_set_parent(IWINDOW(filesel), GTK_WIDGET(iwnd));
	idialog_set_iobject(IDIALOG(filesel), IOBJECT(ws));
	filesel_set_done(FILESEL(filesel),
		workspacegroupview_save_as_sub, ws);
	iwindow_build(IWINDOW(filesel));

	gtk_widget_show(GTK_WIDGET(filesel));
}
 */

static void
workspacegroupview_init(Workspacegroupview *wsgview)
{
	printf("workspacegroupview_init:\n");

	gtk_widget_init_template(GTK_WIDGET(wsgview));

	g_signal_connect(wsgview->notebook, "switch_page",
		G_CALLBACK(workspacegroupview_switch_page_cb), wsgview);
	g_signal_connect(wsgview->notebook, "page_added",
		G_CALLBACK(workspacegroupview_page_added_cb), wsgview);
	g_signal_connect(wsgview->notebook, "page_reordered",
		G_CALLBACK(workspacegroupview_page_reordered_cb), wsgview);
	g_signal_connect(wsgview->notebook, "create_window",
		G_CALLBACK(workspacegroupview_create_window_cb), wsgview);

	printf("workspacegroupview_init: FIXME .. implement tab doubleclick\n");
	/*
	doubleclick_add(wsgview->notebook, FALSE,
		NULL, NULL,
		DOUBLECLICK_FUNC(workspacegroupview_tab_double_cb),
		wsgview);
	 */

	printf("workspacegroupview_init: FIXME .. implement tab gutter menu\n");
	/*
	wsgview->gutter_menu = popup_build(_("Tab gutter menu"));
	popup_add_but(wsgview->gutter_menu, _("New Tab"),
		POPUP_FUNC(workspacegroupview_add_workspace_cb2));
	popup_add_but(wsgview->gutter_menu, _("Merge Into Workspace"),
		POPUP_FUNC(workspacegroupview_load_workspace_cb2));
	popup_attach(wsgview->notebook, wsgview->gutter_menu, wsgview);
	 */

	/*
	GtkWidget *but;
	GtkWidget *icon;

	but = gtk_button_new();
	gtk_button_set_relief(GTK_BUTTON(but), GTK_RELIEF_NONE);
	set_tooltip(but, _("Add a workspace"));
	icon = gtk_image_new_from_stock(GTK_STOCK_ADD, GTK_ICON_SIZE_MENU);
	gtk_container_add(GTK_CONTAINER(but), icon);
	gtk_widget_show(icon);
	gtk_widget_show(but);
	gtk_notebook_set_action_widget(GTK_NOTEBOOK(wsgview->notebook),
		but, GTK_PACK_END);
	g_signal_connect(but, "clicked",
		G_CALLBACK(workspacegroupview_add_workspace_cb), wsgview);

	gtk_box_pack_start(GTK_BOX(wsgview), wsgview->notebook, TRUE, TRUE, 0);
	gtk_widget_show(wsgview->notebook);
	 */

	printf("workspacegroupview_init: add tab menu\n");
	/*
	wsgview->tab_menu = popup_build(_("Tab menu"));
	popup_add_but(wsgview->tab_menu, _("Rename"),
		POPUP_FUNC(workspacegroupview_rename_cb));
	popup_add_but(wsgview->tab_menu, _("Select All"),
		POPUP_FUNC(workspacegroupview_select_all_cb));
	popup_add_but(wsgview->tab_menu, STOCK_DUPLICATE,
		POPUP_FUNC(workspacegroupview_duplicate_cb));
	popup_add_but(wsgview->tab_menu, _("Merge Into Tab"),
		POPUP_FUNC(workspacegroupview_merge_cb));
	popup_add_but(wsgview->tab_menu, GTK_STOCK_SAVE_AS,
		POPUP_FUNC(workspacegroupview_save_as_cb));
	menu_add_sep(wsgview->tab_menu);
	popup_add_but(wsgview->tab_menu, GTK_STOCK_DELETE,
		POPUP_FUNC(workspacegroupview_delete_cb));
	 */
}

View *
workspacegroupview_new(void)
{
	Workspacegroupview *wsgview = g_object_new(WORKSPACEGROUPVIEW_TYPE, NULL);

	return VIEW(wsgview);
}
