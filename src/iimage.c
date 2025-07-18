/* an image class object in a workspace
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

G_DEFINE_TYPE(iImage, iimage, CLASSMODEL_TYPE)

static void
iimage_dispose(GObject *gobject)
{
	iImage *iimage;

#ifdef DEBUG
	printf("iimage_dispose %p\n", gobject);
#endif /*DEBUG*/

	g_return_if_fail(gobject != NULL);
	g_return_if_fail(IS_IIMAGE(gobject));

	iimage = IIMAGE(gobject);

	slist_map(iimage->classmodels,
		(SListMapFn) classmodel_iimage_unlink, iimage);
	g_assert(!iimage->classmodels);

	G_OBJECT_CLASS(iimage_parent_class)->dispose(gobject);
}

static void
iimage_finalize(GObject *gobject)
{
	iImage *iimage;

#ifdef DEBUG
	printf("iimage_finalize\n");
#endif /*DEBUG*/

	g_return_if_fail(gobject != NULL);
	g_return_if_fail(IS_IIMAGE(gobject));

	iimage = IIMAGE(gobject);

	image_value_destroy(&iimage->value);
	VIPS_FREEF(g_slist_free, iimage->views);
	vips_buf_destroy(&iimage->caption_buffer);

	G_OBJECT_CLASS(iimage_parent_class)->finalize(gobject);
}

/* Return the main caption.
 */
static const char *
iimage_generate_caption(iObject *iobject)
{
	iImage *iimage = IIMAGE(iobject);
	Imageinfo *ii = iimage->value.ii;
	VipsBuf *buf = &iimage->caption_buffer;

	vips_buf_rewind(buf);

	image_value_caption(&iimage->value, buf);

	if (ii) {
		vips_buf_appends(buf, ", ");
		iobject_info(IOBJECT(iimage->value.ii), buf);
	}

	return vips_buf_all(buf);
}

static void
iimage_info(iObject *iobject, VipsBuf *buf)
{
	iImage *iimage = IIMAGE(iobject);
	Imageinfo *ii = iimage->value.ii;
	VipsImage *image;

	if (ii &&
		(image = ii->image)) {
		const char *filename;

		if (vips_image_get_typeof(image, ORIGINAL_FILENAME) != 0) {
			if (!vips_image_get_string(image, ORIGINAL_FILENAME, &filename)) {
				vips_buf_appends(buf, _("Original filename"));
				vips_buf_appendf(buf, ": %s\n", filename);
			}
		}
	}
}

static View *
iimage_view_new(Model *model, View *parent)
{
	return iimageview_new();
}

static void
iimage_edit(Model *model, GtkWindow *window)
{
	iImage *iimage = IIMAGE(model);
	GtkApplication *app = gtk_window_get_application(window);
	Imagewindow *win = imagewindow_new(APP(app));

	imagewindow_set_iimage(win, iimage);

	gtk_window_present(GTK_WINDOW(win));
}

static xmlNode *
iimage_save(Model *model, xmlNode *xnode)
{
	iImage *iimage = IIMAGE(model);
	xmlNode *xthis;

	if (!(xthis = MODEL_CLASS(iimage_parent_class)->save(model, xnode)))
		return NULL;

	/* We always rebuild the value from the expr ... don't save.
	 */
	if (!set_iprop(xthis, "image_left", iimage->image_left) ||
		!set_iprop(xthis, "image_top", iimage->image_top) ||
		!set_iprop(xthis, "image_mag", iimage->image_mag) ||
		!set_sprop(xthis, "show_status", bool_to_char(iimage->show_status)) ||
		!set_sprop(xthis, "show_convert", bool_to_char(iimage->show_convert)))
		return NULL;

	if (iimage->view_settings.valid &&
		 (!set_dprop(xthis, "scale", iimage->view_settings.scale) ||
		  !set_dprop(xthis, "offset", iimage->view_settings.offset) ||
		  !set_iprop(xthis, "page", iimage->view_settings.page) ||
		  !set_sprop(xthis, "falsecolour",
			bool_to_char(iimage->view_settings.falsecolour)) ||
		  !set_sprop(xthis, "mode",
			vips_enum_nick(TILESOURCE_MODE_TYPE, iimage->view_settings.mode))))
		return NULL;

	return xthis;
}

static gboolean
iimage_load(Model *model,
	ModelLoadState *state, Model *parent, xmlNode *xnode)
{
	iImage *iimage = IIMAGE(model);

	g_assert(IS_RHS(parent));

	(void) get_iprop(xnode, "image_left", &iimage->image_left);
	(void) get_iprop(xnode, "image_top", &iimage->image_top);
	(void) get_iprop(xnode, "image_mag", &iimage->image_mag);
	(void) get_bprop(xnode, "show_status", &iimage->show_status);
	(void) get_bprop(xnode, "show_paintbox", &iimage->show_paintbox);
	(void) get_bprop(xnode, "show_convert", &iimage->show_convert);

	char mode[64];
	if (get_dprop(xnode, "scale", &iimage->view_settings.scale) &&
		get_dprop(xnode, "offset", &iimage->view_settings.offset) &&
		get_iprop(xnode, "page", &iimage->view_settings.page) &&
		get_bprop(xnode, "falsecolour", &iimage->view_settings.falsecolour) &&
		get_sprop(xnode, "mode", mode, sizeof(mode))) {
		iimage->view_settings.mode =
			vips_enum_from_nick("nip4", TILESOURCE_MODE_TYPE, mode);
		iimage->view_settings.valid = TRUE;
	}

	return MODEL_CLASS(iimage_parent_class)->load(model, state, parent, xnode);
}

/* Need to implement _update_heap(), as not all model fields are directly
 * editable ... some are set only from expr. See also iregion.c.
 */
static void *
iimage_update_heap(Heapmodel *heapmodel)
{
	iImage *iimage = IIMAGE(heapmodel);
	ImageValue *value = &iimage->value;

#ifdef DEBUG
	printf("iimage_update_heap: ");
	row_name_print(HEAPMODEL(iimage)->row);
	printf("\n");
#endif /*DEBUG*/

	VIPS_FREE(CLASSMODEL(iimage)->filename);

	if (value->ii && imageinfo_is_from_file(value->ii))
		VIPS_SETSTR(CLASSMODEL(iimage)->filename, IOBJECT(value->ii)->name);

	/* Classmodel _update_heap() will do _instance_new() from the fixed up
	 * model.
	 */
	return HEAPMODEL_CLASS(iimage_parent_class)->update_heap(heapmodel);
}

/* Update iImage from heap.
 */
static gboolean
iimage_class_get(Classmodel *classmodel, PElement *root)
{
	iImage *iimage = IIMAGE(classmodel);
	ImageValue *value = &iimage->value;

	Imageinfo *ii;

#ifdef DEBUG
	printf("iimage_class_get: ");
	row_name_print(HEAPMODEL(iimage)->row);
	printf("\n");
#endif /*DEBUG*/

	if (!class_get_member_image(root, MEMBER_VALUE, &ii))
		return FALSE;
	image_value_set(value, ii);

	/* Try to update the filename for this row ... get from the meta if we
	 * can.
	 */
	VIPS_FREE(classmodel->filename);
	if (ii) {
		VipsImage *image;
		const char *filename;

		if ((image = ii->image) &&
			vips_image_get_typeof(image, ORIGINAL_FILENAME) != 0) {
			if (vips_image_get_string(image, ORIGINAL_FILENAME, &filename))
				return FALSE;
		}
		else if (imageinfo_is_from_file(ii))
			filename = IOBJECT(ii)->name;
		else
			filename = NULL;

		VIPS_SETSTR(classmodel->filename, filename);
	}

	return CLASSMODEL_CLASS(iimage_parent_class)->class_get(classmodel, root);
}

/* Make a new "fn value" application.
 */
static gboolean
iimage_class_new(Classmodel *classmodel, PElement *fn, PElement *out)
{
	Heap *heap = reduce_context->heap;
	iImage *iimage = IIMAGE(classmodel);
	ImageValue *value = &iimage->value;

	PElement rhs;

#ifdef DEBUG
	printf("iimage_class_new: ");
	row_name_print(HEAPMODEL(iimage)->row);
	printf("\n");
#endif /*DEBUG*/

	/* Make application nodes.
	 */
	heap_appl_init(out, fn);
	if (!heap_appl_add(heap, out, &rhs))
		return FALSE;

	PEPUTP(&rhs, ELEMENT_MANAGED, value->ii);

	return TRUE;
}

static gboolean
iimage_graphic_save(Classmodel *classmodel,
	GtkWindow *window, const char *filename)
{
	iImage *iimage = IIMAGE(classmodel);
	VipsImage *image = iimage->value.ii->image;

	if (image) {
		char buf[VIPS_PATH_MAX];

		/* We don't want $VAR etc. in the filename we pass down to the file
		 * ops.
		 */
		g_strlcpy(buf, filename, VIPS_PATH_MAX);
		path_expand(buf);

		SaveOptions *options = save_options_new(window, image, buf);
		if (!options)
			return FALSE;

		gtk_window_present(GTK_WINDOW(options));
	}

	return TRUE;
}

gboolean
iimage_replace(iImage *iimage, const char *filename)
{
	Row *row = HEAPMODEL(iimage)->row;
	iText *itext = ITEXT(HEAPMODEL(iimage)->rhs->itext);
	char txt[MAX_STRSIZE];
	VipsBuf buf = VIPS_BUF_STATIC(txt);

	vips_buf_appends(&buf, "Image_file \"");
	vips_buf_appendsc(&buf, TRUE, filename);
	vips_buf_appends(&buf, "\"");

	if (itext_set_formula(itext, vips_buf_all(&buf))) {
		itext_set_edited(itext, TRUE);
		workspace_set_modified(row->ws, TRUE);
		(void) expr_dirty(row->expr, link_serial_new());
	}

	return TRUE;
}

gboolean
iimage_replace_imageinfo(iImage *iimage, Imageinfo *ii)
{
	image_value_set(&iimage->value, ii);
	classmodel_update(CLASSMODEL(iimage));

	return TRUE;
}

static gboolean
iimage_graphic_replace(Classmodel *classmodel, GtkWindow *window,
	const char *filename)
{
	return iimage_replace(IIMAGE(classmodel), filename);
}

static void
iimage_class_init(iImageClass *class)
{
	GObjectClass *gobject_class = (GObjectClass *) class;
	iObjectClass *iobject_class = (iObjectClass *) class;
	ModelClass *model_class = (ModelClass *) class;
	HeapmodelClass *heapmodel_class = (HeapmodelClass *) class;
	ClassmodelClass *classmodel_class = (ClassmodelClass *) class;

	/* Create signals.
	 */

	/* Init methods.
	 */
	gobject_class->dispose = iimage_dispose;
	gobject_class->finalize = iimage_finalize;

	iobject_class->user_name = _("Image");
	iobject_class->generate_caption = iimage_generate_caption;
	iobject_class->info = iimage_info;

	model_class->view_new = iimage_view_new;
	model_class->edit = iimage_edit;
	model_class->save = iimage_save;
	model_class->load = iimage_load;

	heapmodel_class->update_heap = iimage_update_heap;

	classmodel_class->class_get = iimage_class_get;
	classmodel_class->class_new = iimage_class_new;

	classmodel_class->graphic_save = iimage_graphic_save;
	classmodel_class->graphic_filter_save = imageinfo_filter_save_new;
	classmodel_class->graphic_replace = iimage_graphic_replace;
	classmodel_class->graphic_filter_replace = imageinfo_filter_load_new;

	/* Static init.
	 */
	model_register_loadable(MODEL_CLASS(class));
}

static void
iimage_init(iImage *iimage)
{
	image_value_init(&iimage->value, CLASSMODEL(iimage));

	iimage->classmodels = NULL;

	iimage->views = NULL;

	iimage->image_left = 0;
	iimage->image_top = 0;
	iimage->image_mag = 0;

	iimage->show_status = FALSE;
	iimage->show_paintbox = FALSE;
	iimage->show_convert = FALSE;

	vips_buf_init_dynamic(&iimage->caption_buffer, MAX_LINELENGTH);

	iobject_set(IOBJECT(iimage), CLASS_IMAGE, NULL);
}

void
iimage_update_view_settings(iImage *iimage, ViewSettings *view_settings)
{
	iimage->view_settings = *view_settings;
	iobject_changed(IOBJECT(iimage));
}
