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

	private Hashtable<EClass, org.eclipse.jface.viewers.CellEditor> modelCellEditors = new Hashtable<EClass, org.eclipse.jface.viewers.CellEditor>();

	private Hashtable<ColumnDescriptor, DynamicEditingSupport> dynamicEditingSupports = new Hashtable<ColumnDescriptor, DynamicEditingSupport>();

	protected ColumnManager(SWTCollectionView collectionView) {
		this.collectionView = collectionView;

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
				DynamicEditingSupport dynamicEditingSupport = new DynamicEditingSupport(
						this, columnDescriptor);
				dynamicEditingSupports.put(columnDescriptor,
						dynamicEditingSupport);
				viewerColumn.setEditingSupport(dynamicEditingSupport);
			}
		}

	}

	public void dispose() {
		disposeAllModelCellEditors();
		for (DynamicEditingSupport dynamicEditingSupport : dynamicEditingSupports
				.values())
			dynamicEditingSupport.dispose();
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

	protected void disposeAllModelCellEditors() {
		for (org.eclipse.jface.viewers.CellEditor cellEditor : modelCellEditors
				.values())
			cellEditor.dispose();
	}

	public SWTCollectionView getCollectionView() {
		return collectionView;
	}

	protected org.eclipse.jface.viewers.CellEditor getModelCellEditor(
			CellEditor cellEditor) {
		if (cellEditor != null) {
			org.eclipse.jface.viewers.CellEditor swtCellEditor = modelCellEditors
					.get(cellEditor.eClass());
			if (swtCellEditor != null)
				return swtCellEditor;
			swtCellEditor = CellEditorFactory.getInstance().getCellEditor(
					cellEditor);
			if (swtCellEditor != null && swtCellEditor.getControl() == null) {
				swtCellEditor
						.create((org.eclipse.swt.widgets.Composite) collectionView
								.getSWTWidget());
				// TODO : implement this
				// swtCellEditor.setStyle(style);
				modelCellEditors.put(cellEditor.eClass(), swtCellEditor);
				return swtCellEditor;
			}
		}
		return null;
	}

	protected org.eclipse.swt.widgets.Widget getSWTWidget() {
		return collectionView.getSWTWidget();
	}

	public void update(List<StyleRule> rules) {

		final org.eclipse.swt.widgets.Widget w = getSWTWidget();

		if (w == null || w.isDisposed() || collectionView.getViewer() == null)
			return;

		disposeAllColumns(w);
		viewerColumns.clear();
		disposeAllModelCellEditors();
		modelCellEditors.clear();

		// // TODO : we need to check whether the style is on or not
		// setHeaderVisible(w);

		int columnIndex = 0;
		for (StyleRule rule : rules)
			createViewerColumn(w, (ColumnDescriptor) rule, columnIndex++);
	}
}
