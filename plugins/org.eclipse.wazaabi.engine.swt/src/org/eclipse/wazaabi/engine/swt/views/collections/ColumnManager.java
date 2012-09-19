/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.views.collections;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.wazaabi.mm.core.extras.CellEditor;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;

public class ColumnManager {

	private final SWTCollectionView collectionView;
	private List<ViewerColumn> viewerColumns = new ArrayList<ViewerColumn>();

	private Hashtable<EClass, org.eclipse.jface.viewers.CellEditor> cellEditors = new Hashtable<EClass, org.eclipse.jface.viewers.CellEditor>();

	protected ColumnManager(SWTCollectionView collectionView) {
		this.collectionView = collectionView;

	}

	protected void disposeAllColumns(final org.eclipse.swt.widgets.Widget w) {
		if (w instanceof org.eclipse.swt.widgets.Tree)
			for (org.eclipse.swt.widgets.TreeColumn column : ((org.eclipse.swt.widgets.Tree) w)
					.getColumns())
				column.dispose();
		else if (w instanceof org.eclipse.swt.widgets.Table)
			for (org.eclipse.swt.widgets.TableColumn column : ((org.eclipse.swt.widgets.Table) w)
					.getColumns())
				column.dispose();
	}

	protected org.eclipse.swt.widgets.Widget getSWTWidget() {
		return collectionView.getSWTWidget();
	}

	protected void setHeaderVisible(final org.eclipse.swt.widgets.Widget w) {
		if (w instanceof org.eclipse.swt.widgets.Tree)
			((org.eclipse.swt.widgets.Tree) w).setHeaderVisible(true);
		else if (w instanceof org.eclipse.swt.widgets.Table)
			((org.eclipse.swt.widgets.Table) w).setHeaderVisible(true);
	}

	public void update(List<StyleRule> rules) {

		final org.eclipse.swt.widgets.Widget w = getSWTWidget();

		if (w == null || w.isDisposed() || collectionView.getViewer() == null)
			return;

		disposeAllColumns(w);
		viewerColumns.clear();
		disposeAllCellEditors();
		cellEditors.clear();

		// TODO : we need to check whether the style is on or not
		setHeaderVisible(w);

		int columnIndex = 0;
		for (StyleRule rule : rules)
			createViewerColumn(w, (ColumnDescriptor) rule, columnIndex++);
	}

	protected void createViewerColumn(final org.eclipse.swt.widgets.Widget w,
			final ColumnDescriptor columnDescriptor, final int columnIndex) {

		ViewerColumn viewerColumn = null;
		if (w instanceof org.eclipse.swt.widgets.Tree) {
			viewerColumn = new TreeViewerColumn(
					(TreeViewer) collectionView.getViewer(), SWT.NONE);

			// TODO : not supported yet
			// viewerColumn.getColumn().setMoveable(true);

			((TreeViewerColumn) viewerColumn).getColumn().setText(
					columnDescriptor.getLabel() != null ? columnDescriptor
							.getLabel() : "");//$NON-NLS-1$

			((TreeViewerColumn) viewerColumn).getColumn().setWidth(
					columnDescriptor.getMinimumWidth());
		} else if (w instanceof org.eclipse.swt.widgets.Table) {
			viewerColumn = new TableViewerColumn(
					(TableViewer) collectionView.getViewer(), SWT.NONE);

			// TODO : not supported yet
			// viewerColumn.getColumn().setMoveable(true);

			((TableViewerColumn) viewerColumn).getColumn().setText(
					columnDescriptor.getLabel() != null ? columnDescriptor
							.getLabel() : "");//$NON-NLS-1$

			((TableViewerColumn) viewerColumn).getColumn().setWidth(
					columnDescriptor.getMinimumWidth());

		}
		if (viewerColumn != null) {
			viewerColumn.setLabelProvider(new ColumnLabelProvider() {

				public String getText(Object element) {
					return collectionView.getLabelProvider().getColumnText(
							element, columnIndex);
				}

			});

			if (columnDescriptor.getEditingSupport() != null) {

				final org.eclipse.jface.viewers.CellEditor cellEditor = getCellEditor(columnDescriptor
						.getCellEditor());
				viewerColumn.setEditingSupport(new EditingSupport(
						(ColumnViewer) collectionView.getViewer()) {

					protected boolean canEdit(Object element) {
						return true;
					}

					protected org.eclipse.jface.viewers.CellEditor getCellEditor(
							Object element) {
						return cellEditor;
					}

					protected Object getValue(Object element) {
						return "hello";
					}

					protected void setValue(Object element, Object value) {
						// ((MyModel) element).counter =
						// Integer.parseInt(value.toString());
						// v.update(element, null);
					}
				});

			}
		}

	}

	protected org.eclipse.jface.viewers.CellEditor getCellEditor(
			CellEditor cellEditor) {
		if (cellEditor != null) {
			org.eclipse.jface.viewers.CellEditor swtCellEditor = cellEditors
					.get(cellEditor.eClass());
			if (swtCellEditor != null)
				return swtCellEditor;
			swtCellEditor = CellEditorFactory.getInstance().getCellEditor(
					cellEditor);
			if (swtCellEditor != null) {
				swtCellEditor
						.create((org.eclipse.swt.widgets.Composite) collectionView
								.getSWTWidget());
				// TODO : implement this
				// swtCellEditor.setStyle(style);
				cellEditors.put(cellEditor.eClass(), swtCellEditor);
				return swtCellEditor;
			}
		}
		return null;
	}

	protected void disposeAllCellEditors() {
		for (org.eclipse.jface.viewers.CellEditor cellEditor : cellEditors
				.values())
			cellEditor.dispose();
	}

	public void dispose() {
		disposeAllCellEditors();
	}
}
