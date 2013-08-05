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

package org.eclipse.wazaabi.engine.swt.commons.views.collections;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.jface.layout.AbstractColumnLayout;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnLayoutData;
import org.eclipse.jface.viewers.ColumnPixelData;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.wazaabi.mm.core.Alignment;
import org.eclipse.wazaabi.mm.core.extras.CellEditor;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.collections.AbstractColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;

public class ColumnManager {

	private final SWTCollectionView collectionView;

	private List<ViewerColumn> viewerColumns = new ArrayList<ViewerColumn>();

	private Hashtable<EClass, org.eclipse.jface.viewers.CellEditor> modelCellEditors = new Hashtable<EClass, org.eclipse.jface.viewers.CellEditor>();

	private Hashtable<AbstractColumnDescriptor, DynamicEditingSupport> dynamicEditingSupports = new Hashtable<AbstractColumnDescriptor, DynamicEditingSupport>();

	protected ColumnManager(SWTCollectionView collectionView) {
		this.collectionView = collectionView;

	}

	protected void createViewerColumn(final org.eclipse.swt.widgets.Widget w,
			final AbstractColumnDescriptor columnDescriptor,
			final int columnIndex) {

		ViewerColumn viewerColumn = null;
		if (w instanceof org.eclipse.swt.widgets.Tree) {
			viewerColumn = new TreeViewerColumn(
					(TreeViewer) collectionView.getViewer(),
					getSWTColumnHeaderStyle(columnDescriptor));

			// TODO : not supported yet
			// viewerColumn.getColumn().setMoveable(true);

			final TreeColumn column = ((TreeViewerColumn) viewerColumn)
					.getColumn();

			column.setText(columnDescriptor.getLabel() != null ? columnDescriptor
					.getLabel() : "");//$NON-NLS-1$

			if (getSWTWidget() instanceof org.eclipse.swt.widgets.Composite
					&& ((org.eclipse.swt.widgets.Composite) getSWTWidget())
							.getLayout() instanceof AbstractColumnLayout) {
				ColumnLayoutData columnLayoutData = null;
				if (columnDescriptor.eClass() == CoreCollectionsStylesPackage.Literals.COLUMN_DESCRIPTOR)
					columnLayoutData = new ColumnPixelData(
							((ColumnDescriptor) columnDescriptor).getWidth(),
							columnDescriptor.isResizable());
				else if (columnDescriptor.eClass() == CoreCollectionsStylesPackage.Literals.WEIGHTED_COLUMN_DESCRIPTOR)
					columnLayoutData = new ColumnWeightData(
							((WeightedColumnDescriptor) columnDescriptor)
									.getWeight(),
							((WeightedColumnDescriptor) columnDescriptor)
									.getMinimumWidth(), columnDescriptor
									.isResizable());
				((AbstractColumnLayout) ((org.eclipse.swt.widgets.Composite) getSWTWidget())
						.getLayout()).setColumnData(column, columnLayoutData);
			}

		} else if (w instanceof org.eclipse.swt.widgets.Table) {
			viewerColumn = new TableViewerColumn(
					(TableViewer) collectionView.getViewer(),
					getSWTColumnHeaderStyle(columnDescriptor));

			// TODO : not supported yet
			// viewerColumn.getColumn().setMoveable(true);

			((TableViewerColumn) viewerColumn).getColumn().setText(
					columnDescriptor.getLabel() != null ? columnDescriptor
							.getLabel() : "");//$NON-NLS-1$

			final TableColumn column = ((TableViewerColumn) viewerColumn)
					.getColumn();

			column.setText(columnDescriptor.getLabel() != null ? columnDescriptor
					.getLabel() : "");//$NON-NLS-1$

			if (getSWTWidget() instanceof org.eclipse.swt.widgets.Composite
					&& ((org.eclipse.swt.widgets.Composite) getSWTWidget())
							.getLayout() instanceof AbstractColumnLayout) {
				ColumnLayoutData columnLayoutData = null;
				if (columnDescriptor.eClass() == CoreCollectionsStylesPackage.Literals.COLUMN_DESCRIPTOR)
					columnLayoutData = new ColumnPixelData(
							((ColumnDescriptor) columnDescriptor).getWidth(),
							columnDescriptor.isResizable());
				else if (columnDescriptor.eClass() == CoreCollectionsStylesPackage.Literals.WEIGHTED_COLUMN_DESCRIPTOR)
					columnLayoutData = new ColumnWeightData(
							((WeightedColumnDescriptor) columnDescriptor)
									.getWeight(),
							((WeightedColumnDescriptor) columnDescriptor)
									.getMinimumWidth(), columnDescriptor
									.isResizable());
				((AbstractColumnLayout) ((org.eclipse.swt.widgets.Composite) getSWTWidget())
						.getLayout()).setColumnData(column, columnLayoutData);
			}

		}
		if (viewerColumn != null) {
			if (collectionView.getLabelProvider() instanceof PathSelectorLabelProvider) {
				final PathSelectorLabelProvider labelProvider = (PathSelectorLabelProvider) collectionView
						.getLabelProvider();
				viewerColumn.setLabelProvider(new ColumnLabelProvider() {

					public String getText(Object element) {
						return labelProvider
								.getColumnText(element, columnIndex);
					}

					public Image getImage(Object element) {
						return labelProvider.getColumnImage(element,
								columnIndex);
					}

				});
			} else if (collectionView.getLabelProvider() instanceof DynamicLabelProvider) {
				final DynamicLabelProvider labelProvider = (DynamicLabelProvider) collectionView
						.getLabelProvider();
				viewerColumn.setLabelProvider(new StyledCellLabelProvider() {

					@Override
					public void update(ViewerCell cell) {
						final Object element = cell.getElement();
						final int columnIndex = cell.getColumnIndex();
						final Display display = cell.getControl().getDisplay();
						cell.setText(labelProvider.getColumnText(element,
								columnIndex));
						cell.setImage(labelProvider.getColumnImage(element,
								columnIndex));
						final Color foreground = labelProvider
								.getForegroundColor(element, columnIndex,
										display);
						if (foreground != null)
							cell.setForeground(foreground);
						final Color background = labelProvider
								.getBackgroundColor(element, columnIndex,
										display);
						if (background != null)
							cell.setBackground(background);
						final Font font = labelProvider.getFont(element,
								columnIndex, display, cell.getFont());
						if (font != null)
							cell.setFont(font);
						super.update(cell);
					}
				});
			} else
				viewerColumn.setLabelProvider(new ColumnLabelProvider() {

					public String getText(Object element) {
						return element != null ? element.toString() : ""; //$NON-NLS-1$
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

	protected int getSWTColumnHeaderStyle(
			AbstractColumnDescriptor columnDescriptor) {
		switch (columnDescriptor.getHeaderAlignment().getValue()) {
		case Alignment.LEAD_VALUE:
			return SWT.LEAD;
		case Alignment.CENTER_VALUE:
			return SWT.CENTER;
		case Alignment.TRAIL_VALUE:
			return SWT.TRAIL;
		}
		return SWT.None;
	}

	public void dispose() {
		disposeAllModelCellEditors();
		disposeAllDynamicEditingSupports();
	}

	protected void disposeAllDynamicEditingSupports() {
		for (int i = 0; i < dynamicEditingSupports.values().size(); i++) {
			if (dynamicEditingSupports.get(i) != null) {
				// we try to avoid to dispose more than one time every
				// EditingSupport
				boolean isUnique = true;
				for (int j = i + 1; j < dynamicEditingSupports.values().size(); j++)
					if (dynamicEditingSupports.get(j) == dynamicEditingSupports
							.get(i)) {
						isUnique = false;
						break;
					}
				if (isUnique)
					dynamicEditingSupports.get(i).dispose();
			}
		}
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

			Object candidate = getCollectionView().getHost().getViewer()
					.createComponent(this, cellEditor, null, CellEditor.class);
			if (candidate instanceof org.eclipse.jface.viewers.CellEditor)
				swtCellEditor = (org.eclipse.jface.viewers.CellEditor) candidate;
			if (swtCellEditor.getControl() == null) {
				swtCellEditor
						.create((org.eclipse.swt.widgets.Composite) collectionView
								.getSWTCollectionControl());
				// TODO : implement this
				// swtCellEditor.setStyle(style);
				modelCellEditors.put(cellEditor.eClass(), swtCellEditor);
				return swtCellEditor;
			}
		}
		return null;
	}

	protected org.eclipse.swt.widgets.Control getSWTCollectionControl() {
		return collectionView.getSWTCollectionControl();
	}

	protected org.eclipse.swt.widgets.Widget getSWTWidget() {
		return collectionView.getSWTWidget();
	}

	public void update(List<StyleRule> rules) {

		final org.eclipse.swt.widgets.Control ctl = getSWTCollectionControl();

		if (ctl == null || ctl.isDisposed()
				|| collectionView.getViewer() == null)
			return;

		// TODO : at the moment this method recreates more then it updates
		disposeAllColumns(ctl);
		viewerColumns.clear();
		disposeAllModelCellEditors();
		modelCellEditors.clear();

		// // TODO : we need to check whether the style is on or not
		// setHeaderVisible(w);

		int columnIndex = 0;
		for (StyleRule rule : rules)
			createViewerColumn(ctl, (AbstractColumnDescriptor) rule,
					columnIndex++);

		// Since we re create all the columns, we need to re layout
		collectionView.revalidate();
	}
}
