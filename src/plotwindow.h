/* a plot display window
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

#ifndef __PLOTWINDOW_H
#define __PLOTWINDOW_H

#define PLOTWINDOW_TYPE (plotwindow_get_type())
#define PLOTWINDOW(obj) \
	(G_TYPE_CHECK_INSTANCE_CAST((obj), PLOTWINDOW_TYPE, Plotwindow))
#define PLOTWINDOW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_CAST((klass), PLOTWINDOW_TYPE, PlotwindowClass))
#define IS_PLOTWINDOW(obj) \
	(G_TYPE_CHECK_INSTANCE_TYPE((obj), PLOTWINDOW_TYPE))
#define IS_PLOTWINDOW_CLASS(klass) \
	(G_TYPE_CHECK_CLASS_TYPE((klass), PLOTWINDOW_TYPE))
#define PLOTWINDOW_GET_CLASS(obj) \
	(G_TYPE_INSTANCE_GET_CLASS((obj), PLOTWINDOW_TYPE, PlotwindowClass))

G_DECLARE_FINAL_TYPE(Plotwindow, plotwindow,
	NIP4, PLOTWINDOW, GtkApplicationWindow)

Plotwindow *plotwindow_new(App *app);

#endif /* __PLOTWINDOW_H */
