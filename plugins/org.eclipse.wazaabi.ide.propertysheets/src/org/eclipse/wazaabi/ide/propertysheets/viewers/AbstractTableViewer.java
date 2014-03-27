/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.viewers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.OwnerDrawLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.PropertySection;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeService;
import org.eclipse.wazaabi.ide.propertysheets.complexcelleditors.InPlaceCellEditor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.graphicalhelpers.GraphicalHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.viewers.DescriptorLabelColumn.LabelPrinter;

public abstract class AbstractTableViewer implements TargetChangeListener,
		PropertySection {

	private final List<TargetChangeListener> listeners = new ArrayList<TargetChangeListener>();
	private GraphicalHelperFactory graphicalHelperFactory = new GraphicalHelperFactory();
	private Composite container = null;
	private final StackLayout stackLayout = new StackLayout();
	private TableViewer tableViewer = null;
	private AbstractDescriptorFactory descriptorFactory = null;
	private EditingHelperFactory editingHelperFactory = null;
	private DescriptorLabelColumn labelColumn = null;

	private InPlaceCellEditor currentInPlaceCellEditor = null;

	public void createControls(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		container.setLayout(stackLayout);

		tableViewer = createViewer(container);
		stackLayout.topControl = tableViewer.getControl();
		createColumns();
		getViewer().setContentProvider(getContentProvider());
		applyTableStyle();

		tableViewer.getControl().addKeyListener(new KeyAdapter() {

			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == '\u001A')
					System.out.println("UNDO");
				else if (e.character == '\u0019')
					System.out.println("REDO");
			}

		});
		tableViewer.getControl().addMouseListener(new MouseListener() {

			public void mouseDoubleClick(MouseEvent e) {
			}

			public void mouseDown(MouseEvent e) {
				Point pt = new Point(e.x, e.y);
				Table table = ((Table) getViewer().getControl());
				TableItem item = table.getItem(pt);
				if (item == null)
					return;
				for (int i = 0; i < table.getColumnCount(); i++) {
					Rectangle rect = item.getBounds(i);
					if (rect.contains(pt)) {

						if (i == 1) {
							Object element = getViewer().getElementAt(
									table.indexOf(item));
							AbstractEditingHelper editingHelper = getEditingHelperFactory()
									.getEditingHelper((EObject) element);
							if (!editingHelper.canEdit(element)) {
								currentInPlaceCellEditor = (InPlaceCellEditor) editingHelper
										.getCellEditor(table.getParent(),
												element);
								if (currentInPlaceCellEditor != null
										&& currentInPlaceCellEditor
												.getControl() != null) {
									((TargetChangeService) currentInPlaceCellEditor)
											.addTargetChangeListener(AbstractTableViewer.this);
									stackLayout.topControl = currentInPlaceCellEditor
											.getControl();
									container.layout(true, true);
									currentInPlaceCellEditor.getControl()
											.addDisposeListener(
													new DisposeListener() {

														public void widgetDisposed(
																DisposeEvent e) {
															((TargetChangeService) currentInPlaceCellEditor)
																	.removeTargetChangeListener(AbstractTableViewer.this);
															currentInPlaceCellEditor = null;
															stackLayout.topControl = getViewer()
																	.getControl();
															if (!container
																	.isDisposed())
																container
																		.layout();
														}
													});
									currentInPlaceCellEditor.setValue(element);
								}
							}
						}
					}
				}
			}

			public void mouseUp(MouseEvent e) {
			}
		});

		tableViewer.getControl().addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if ((e.stateMask & SWT.CTRL) == SWT.CTRL) {
					if (e.keyCode == 'Z' || e.keyCode == 'z')
						fireUndo();
					else if (e.keyCode == 'Y' || e.keyCode == 'y')
						fireRedo();
				}
			}
		});
	}

	public void addTargetChangeListener(TargetChangeListener listener) {
		listeners.add(listener);
	}

	protected void applyTableStyle() {
		getViewer().getTable().setHeaderVisible(true);
		getViewer().getTable().setLinesVisible(true);
	}

	protected abstract String getLabelColumnLabel();

	protected void createColumns() {
		labelColumn = new DescriptorLabelColumn(getViewer(), this,
				getDescriptorFactory(), getBlankRow(), getLabelPrinter(),
				getLabelColumnLabel());
		createValuesColumn();
	}

	protected EditingHelperFactory createEditingHelperFactory() {
		return new EditingHelperFactory();
	}

	protected void createValuesColumn() {

		TableViewerColumn valueCol = new TableViewerColumn(getViewer(),
				SWT.NONE);
		valueCol.setLabelProvider(new OwnerDrawLabelProvider() {
			@Override
			protected void erase(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((EObject) element)
						.erase(event, element, 1);
			}

			@Override
			protected void measure(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((EObject) element)
						.measure(event, element, 1);
			}

			@Override
			protected void paint(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((EObject) element)
						.paint(event, element, 1);
			}

		});
		valueCol.setEditingSupport(new EditingSupport(getViewer()) {

			@Override
			protected boolean canEdit(Object element) {
				return getEditingHelperFactory().getEditingHelper(
						(EObject) element).canEdit(element);
			}

			@Override
			protected CellEditor getCellEditor(Object element) {
				CellEditor editor = getEditingHelperFactory().getEditingHelper(
						(EObject) element).getCellEditor(
						getViewer().getControl(), element);
				if (editor instanceof TargetChangeService)
					((TargetChangeService) editor)
							.addTargetChangeListener(AbstractTableViewer.this);
				return editor;
			}

			@Override
			protected Object getValue(Object element) {
				return getEditingHelperFactory().getEditingHelper(
						(EObject) element).getValue(element);
			}

			@Override
			protected void setValue(Object element, Object value) {
				getEditingHelperFactory().getEditingHelper((EObject) element)
						.setValue(element, value, AbstractTableViewer.this);
			}
		});
		valueCol.getColumn().setText("Value");
		valueCol.getColumn().setWidth(400);

	}

	protected TableViewer createViewer(Composite parent) {
		return new TableViewer(parent, getCreationStyle()) {

			@Override
			protected ColumnViewerEditor createViewerEditor() {
				ColumnViewerEditor o = super.createViewerEditor();
				return o;
			}

		};
	}

	protected int getCreationStyle() {
		return SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION
				| SWT.BORDER;
	}

	protected EditingHelperFactory getEditingHelperFactory() {
		if (editingHelperFactory == null)
			editingHelperFactory = createEditingHelperFactory();
		return editingHelperFactory;
	}

	public Object getInput() {
		return getViewer().getInput();
	}

	protected int getTableViewerStyle() {
		return SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION
				| SWT.BORDER;
	}

	public TableViewer getViewer() {
		return tableViewer;
	}

	public void refresh() {
		labelColumn.refresh();
		getViewer().refresh();
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void refresh(boolean updateLabels) {
		labelColumn.refresh();
		getViewer().refresh(updateLabels);
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void refresh(boolean updateLabels, boolean reveal) {
		labelColumn.refresh();
		getViewer().refresh(updateLabels, reveal);
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void refresh(Object element) {
		labelColumn.refresh();
		getViewer().refresh(element);
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void refresh(Object element, boolean updateLabels) {
		labelColumn.refresh();
		getViewer().refresh(element, updateLabels);
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void refresh(Object element, boolean updateLabels, boolean reveal) {
		labelColumn.refresh();
		getViewer().refresh(element, updateLabels, reveal);
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.refresh();
	}

	public void removeTargetChangeListener(TargetChangeListener listener) {
		listeners.remove(listener);
	}

	public void setInput(Object input) {
		if (currentInPlaceCellEditor != null)
			currentInPlaceCellEditor.dispose();
		getViewer().setInput(input);
	}

	public void targetAdded(EObject container, EObject target, int position) {
		fireRowAdded(container, target, position);
	}

	public void targetModified(EObject target, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		fireRowModified(target, feature, position, oldValue, newValue);
	}

	public void targetMultipleModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		fireRowModified(target, features, positions, oldValues, newValues);

	}

	public void targetRemoved(EObject container, EObject target) {
		fireRowRemoved(container, target);
	}

	public Control getControl() {
		return container;
	}

	public void dispose() {
		listeners.clear();
		if (getControl() != null && !getControl().isDisposed())
			getControl().dispose();
	}

	public abstract String getLabel();

	protected abstract AbstractDescriptorFactory createAbstractDescriptorFactory();

	protected abstract EObject getBlankRow();

	protected abstract IContentProvider getContentProvider();

	protected AbstractDescriptorFactory getDescriptorFactory() {
		if (descriptorFactory == null)
			descriptorFactory = createAbstractDescriptorFactory();
		return descriptorFactory;
	}

	protected void fireRowAdded(EObject container, EObject row, int position) {
		for (TargetChangeListener listener : listeners)
			listener.targetAdded(container, row, position);
	}

	protected void fireRowModified(EObject row, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		for (TargetChangeListener listener : listeners)
			listener.targetModified(row, feature, position, oldValue, newValue);
	}

	protected void fireRowModified(EObject row,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		for (TargetChangeListener listener : listeners)
			listener.targetMultipleModified(row, features, positions,
					oldValues, newValues);
	}

	protected void fireRowRemoved(EObject container, EObject row) {
		for (TargetChangeListener listener : listeners)
			listener.targetRemoved(container, row);
	}

	abstract protected LabelPrinter getLabelPrinter();

	protected void fireUndo() {
		for (TargetChangeListener listener : listeners)
			listener.undo();
	}

	protected void fireRedo() {
		for (TargetChangeListener listener : listeners)
			listener.redo();
	}

	public void undo() {
		fireUndo();
	}

	public void redo() {
		fireRedo();
	}

}