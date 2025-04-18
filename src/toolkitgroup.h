/* Declarations for toolkitgroup.c
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

#define TOOLKITGROUP_TYPE (toolkitgroup_get_type())
#define TOOLKITGROUP(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), TOOLKITGROUP_TYPE, Toolkitgroup))
#define TOOLKITGROUP_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), TOOLKITGROUP_TYPE, ToolkitgroupClass))
#define IS_TOOLKITGROUP(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), TOOLKITGROUP_TYPE))
#define IS_TOOLKITGROUP_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), TOOLKITGROUP_TYPE))
#define TOOLKITGROUP_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), TOOLKITGROUP_TYPE, ToolkitgroupClass))

/* A toolkitgroup.
 */
struct _Toolkitgroup {
	Model parent_class;

	/* We may load some compat definitions to support this workspace, if it
	 * was written by an older version.
	 *
	 * The version number of the compat stuff we loaded. Zero for no compat
	 * stuff loaded.
	 */
	int compat_major;
	int compat_minor;

	/* Defs in toolkits in this group are created as locals of this
	 * symbol. This is symbol_root for the main toolkitgroup, but can be
	 * local to a workspace if we are loading a set of compatibility defs.
	 */
	Symbol *root;
};

typedef struct _ToolkitgroupClass {
	ModelClass parent_class;

	/* Methods.
	 */
} ToolkitgroupClass;

Toolkit *toolkitgroup_map(Toolkitgroup *kitg,
	toolkit_map_fn fn, void *a, void *b);

GType toolkitgroup_get_type(void);

Toolkitgroup *toolkitgroup_new(Symbol *root);

void toolkitgroup_sort(Toolkitgroup *kitg);
