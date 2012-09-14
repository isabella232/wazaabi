/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.core.views.CollectionView;
import org.eclipse.wazaabi.engine.edp.PathException;
import org.eclipse.wazaabi.engine.edp.locationpaths.IPointersEvaluator;
import org.eclipse.wazaabi.engine.swt.views.SWTControlView;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.swt.descriptors.SWTDescriptorsPackage;

public class SWTCollectionView extends SWTControlView implements CollectionView {

	private boolean selectionChangedListenerBlocked = false;

	private ISelectionChangedListener selectionChangedListener = new ISelectionChangedListener() {

		@SuppressWarnings("unchecked")
		public void selectionChanged(SelectionChangedEvent event) {
			if (!selectionChangedListenerBlocked
					&& event.getSelection() instanceof StructuredSelection) {
				((CollectionEditPart) getHost()).blockSelectionListening();
				try {
					merge(((Collection) getHost().getModel()).getSelection(),
							((StructuredSelection) event.getSelection())
									.toList());
				} finally {
					((CollectionEditPart) getHost()).blockSelectionListening();

				}
			}
		}

	};

	protected static void merge(EList<Object> previousList, List<Object> newList) {
		List<Object> toRemove = new ArrayList<Object>();
		for (Object previous : previousList)
			if (!newList.contains(previous))
				toRemove.add(previous);
		for (Object item : toRemove)
			previousList.remove(item);
		for (int i = 0; i < newList.size(); i++)
			if (i >= previousList.size())
				previousList.add(i, newList.get(i));
			else {
				int idx = previousList.indexOf(newList.get(i));
				if (idx != -1) {
					if (idx != i)
						previousList.move(i, idx);
				} else
					previousList.add(i, newList.get(i));
			}
	}

	public EClass getWidgetViewEClass() {
		return SWTDescriptorsPackage.Literals.COLLECTION;
	}

	@Override
	public boolean needReCreateWidgetView(StyleRule rule) {
		if (rule instanceof LookAndFeelRule
				&& CollectionEditPart.LOOK_AND_FEEL_PROPERTY_NAME.equals(rule
						.getPropertyName()))
			return !isLookAndFeelCorrect(((LookAndFeelRule) rule).getValue());
		else
			return super.needReCreateWidgetView(rule);
	}

	/**
	 * Returns the LookAndFell associated to the model, null otherwise.
	 * 
	 * @return A LookAndFeel if found, null otherwise.
	 */
	protected LookAndFeel getLookAndFeel() {
		for (StyleRule rule : ((StyledElement) getHost().getModel())
				.getStyleRules())
			if (CollectionEditPart.LOOK_AND_FEEL_PROPERTY_NAME.equals(rule
					.getPropertyName()) && rule instanceof LookAndFeelRule)
				return ((LookAndFeelRule) rule).getValue();
		return null;
	}

	/**
	 * given a LookAndFeel, returns whether the SWT Widget is an instance of the
	 * corresponding class.
	 * 
	 * @param rule
	 * @return
	 */
	protected boolean isLookAndFeelCorrect(LookAndFeel lookAndFeel) {
		final org.eclipse.swt.widgets.Widget widget = getSWTWidget();
		switch (lookAndFeel.getValue()) {
		case LookAndFeel.COMBOBOX_VALUE:
			return widget instanceof org.eclipse.swt.custom.CCombo;
		case LookAndFeel.TABLE_VALUE:
			return widget instanceof org.eclipse.swt.widgets.Table;
		case LookAndFeel.TREE_VALUE:
			return widget instanceof org.eclipse.swt.widgets.Tree;
		}
		return false;
	}

	// @Override
	// protected int computeSWTCreationStyle(StyleRule rule) {
	// final String propertyName = rule.getPropertyName();
	// if (CollectionEditPart.READ_ONLY_PROPERTY_NAME.equals(propertyName)
	// && ((BooleanRule) rule).isValue())
	// return SWT.READ_ONLY;
	// return super.computeSWTCreationStyle(rule);
	// }

	protected Widget createSWTWidget(Widget parent, int swtStyle, int index) {
		int style = computeSWTCreationStyle(getHost());

		LookAndFeel lookAndFeel = getLookAndFeel();
		if (lookAndFeel == null)
			lookAndFeel = LookAndFeel.TABLE;

		switch (lookAndFeel.getValue()) {
		case LookAndFeel.COMBOBOX_VALUE:
			comboViewer = new ComboViewer(
					(org.eclipse.swt.widgets.Composite) parent, style
							| SWT.READ_ONLY);
			comboViewer
					.addSelectionChangedListener(getSelectionChangedListener());
			return comboViewer.getCombo();
		case LookAndFeel.TREE_VALUE:
			treeViewer = new TreeViewer(
					(org.eclipse.swt.widgets.Composite) parent, style);
			treeViewer
					.addSelectionChangedListener(getSelectionChangedListener());
			return treeViewer.getControl();
		case LookAndFeel.TABLE_VALUE:
			tableViewer = new TableViewer(
					(org.eclipse.swt.widgets.Composite) parent, style);
			tableViewer
					.addSelectionChangedListener(getSelectionChangedListener());
			return tableViewer.getControl();
		}
		throw new RuntimeException("Invalid LookAndFeel value"); //$NON-NLS-1$
	}

	public void setInput(Object input) {
		System.out.println("setInput:" + input);
		if (!getSWTControl().isDisposed())
			if (getSWTControl() instanceof org.eclipse.swt.widgets.Tree) {
				if (treeViewer != null
						&& treeViewer.getContentProvider() != null)
					treeViewer.setInput(input);
			} else if (getSWTControl() instanceof org.eclipse.swt.widgets.Table) {
				if (tableViewer != null
						&& tableViewer.getContentProvider() != null)
					tableViewer.setInput(input);
			} else if (getSWTControl() instanceof org.eclipse.swt.widgets.Combo
					|| getSWTControl() instanceof org.eclipse.swt.custom.CCombo) {
				if (comboViewer != null
						&& comboViewer.getContentProvider() != null)
					comboViewer.setInput(input);
			}
	}

	// TODO : instead 3 viewer, we should have only one
	private TreeViewer treeViewer = null;
	private TableViewer tableViewer = null;
	private ComboViewer comboViewer = null;

	public void updateSameStyleRules(List<StyleRule> rules) {
		if (CollectionEditPart.COLUMN_DESCRIPTOR_PROPERTY_NAME.equals(rules
				.get(0).getPropertyName()))
			updateColumns(rules);
		else if (CollectionEditPart.CONTENT_PROVIDER_PROPERTY_NAME.equals(rules
				.get(0).getPropertyName()))
			updateContentProvider(rules);
		else if (CollectionEditPart.LABEL_RENDERER_PROPERTY_NAME.equals(rules
				.get(0).getPropertyName()))
			updateLabelRenderer(rules);
		else if (CollectionEditPart.DYNAMIC_PROVIDER_PROPERTY_NAME.equals(rules
				.get(0).getPropertyName()))
			updateDynamicProviders(rules);
	}

	protected void updateDynamicProviders(List<StyleRule> rules) {
		if (!rules.isEmpty()) {
			List<String> uris = new ArrayList<String>();
			for (StyleRule rule : rules)
				if (!uris.contains(((DynamicProvider) rule).getUri()))
					uris.add(((DynamicProvider) rule).getUri());

			if (getSWTWidget() instanceof org.eclipse.swt.widgets.Table
					&& tableViewer != null) {
				if (!(tableViewer.getContentProvider() instanceof DynamicContentProvider)) {
					if (tableViewer.getContentProvider() != null)
						tableViewer.getContentProvider().dispose();
					tableViewer
							.setContentProvider(new DynamicContentProvider());
				}
				if (!(tableViewer.getLabelProvider() instanceof DynamicLabelProvider)) {
					if (tableViewer.getLabelProvider() != null)
						tableViewer.getLabelProvider().dispose();
					tableViewer.setLabelProvider(new DynamicLabelProvider());
				}

				((DynamicContentProvider) tableViewer.getContentProvider())
						.updateDynamicProviderURIs(uris);
				((DynamicLabelProvider) tableViewer.getLabelProvider())
						.updateDynamicProviderURIs(uris);

			} else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Combo
					|| getSWTWidget() instanceof org.eclipse.swt.custom.CCombo
					&& comboViewer != null) {
				if (!(comboViewer.getContentProvider() instanceof DynamicContentProvider)) {
					if (comboViewer.getContentProvider() != null)
						comboViewer.getContentProvider().dispose();
					comboViewer
							.setContentProvider(new DynamicContentProvider());
				}
				if (!(comboViewer.getLabelProvider() instanceof DynamicLabelProvider)) {
					if (comboViewer.getLabelProvider() != null)
						comboViewer.getLabelProvider().dispose();
					comboViewer.setLabelProvider(new DynamicLabelProvider());
				}
				((DynamicContentProvider) comboViewer.getContentProvider())
						.updateDynamicProviderURIs(uris);
				((DynamicLabelProvider) comboViewer.getLabelProvider())
						.updateDynamicProviderURIs(uris);
			}
		}
	}

	protected void updateContentProvider(List<StyleRule> rules) {
		final Hashtable<String, List<String>> selectors = getSelectors(rules);
		if (getSWTWidget() instanceof org.eclipse.swt.widgets.Tree
				&& treeViewer != null)
			treeViewer.setContentProvider(new PathSelectorContentProvider(this,
					selectors));
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Table
				&& tableViewer != null)
			tableViewer.setContentProvider(new PathSelectorContentProvider(
					this, selectors));
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Combo
				|| getSWTWidget() instanceof org.eclipse.swt.custom.CCombo
				&& comboViewer != null)
			comboViewer.setContentProvider(new PathSelectorContentProvider(
					this, selectors));

	}

	protected Hashtable<String, List<String>> getSelectors(List<StyleRule> rules) {
		Hashtable<String, List<String>> selectors = new Hashtable<String, List<String>>();
		for (StyleRule rule : rules) {
			if (rule instanceof PathSelector) {
				PathSelector pathSelector = (PathSelector) rule;
				if (pathSelector.getEClassifierName() == null
						|| "".equals(pathSelector.getEClassifierName()) || pathSelector.getPaths().isEmpty()) //$NON-NLS-1$ 
					continue;
				List<String> paths = selectors.get(pathSelector
						.getEClassifierName());
				if (paths == null) {
					paths = new ArrayList<String>();
					selectors.put(pathSelector.getEClassifierName(), paths);
				}
				for (String path : pathSelector.getPaths())
					paths.add(path);
			}
		}
		return selectors;
	}

	protected void updateLabelRenderer(List<StyleRule> rules) {
		final Hashtable<String, List<String>> selectors = getSelectors(rules);
		if (getSWTWidget() instanceof org.eclipse.swt.widgets.Tree
				&& treeViewer != null)
			treeViewer
					.setLabelProvider(new org.eclipse.wazaabi.engine.swt.views.collections.PathSelectorLabelProvider(
							this, selectors));
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Table
				&& tableViewer != null)
			tableViewer
					.setLabelProvider(new org.eclipse.wazaabi.engine.swt.views.collections.PathSelectorLabelProvider(
							this, selectors));
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Combo
				|| getSWTWidget() instanceof org.eclipse.swt.custom.CCombo
				&& comboViewer != null) {
			comboViewer
					.setLabelProvider(new org.eclipse.wazaabi.engine.swt.views.collections.PathSelectorLabelProvider(
							this, selectors));
		}
	}

	protected void updateColumns(List<StyleRule> rules) {
		if (!getSWTWidget().isDisposed()
				&& getSWTWidget() instanceof org.eclipse.swt.widgets.Tree
				&& treeViewer != null)
			;
		else if (!getSWTWidget().isDisposed()
				&& getSWTWidget() instanceof org.eclipse.swt.widgets.Table
				&& tableViewer != null) {
			for (TableColumn column : ((org.eclipse.swt.widgets.Table) getSWTWidget())
					.getColumns())
				column.dispose();
			((org.eclipse.swt.widgets.Table) getSWTWidget())
					.setHeaderVisible(true);

			for (StyleRule rule : rules) {
				final ColumnDescriptor columnDescriptor = (ColumnDescriptor) rule;

				TableColumn tableColumn = null;
				if (columnDescriptor.getEditingSupport() != null
						&& !"".equals(columnDescriptor.getEditingSupport())) {
					final TableViewerColumn column = new TableViewerColumn(
							tableViewer, SWT.NONE);
					tableColumn = column.getColumn();
					final int columnIndex = tableViewer.getTable().indexOf(
							tableColumn);
					column.setLabelProvider(new ColumnLabelProvider() {
						@Override
						public String getText(Object element) {
							return ((ITableLabelProvider) tableViewer
									.getLabelProvider()).getColumnText(element,
									columnIndex);
						}
					});

					// by default, the cellEditor is a TextCellEditor
					// even if it is not specified
					CellEditor cellEditor = new org.eclipse.jface.viewers.TextCellEditor(
							((org.eclipse.swt.widgets.Table) getSWTWidget()));
					// if (columnDescriptor.getCellEditor() instanceof
					// TextCellEditor)
					// cellEditor = new
					// org.eclipse.jface.viewers.TextCellEditor(
					// ((org.eclipse.swt.widgets.Table) getSWTWidget()));

					column.setEditingSupport(new DynamicEditingSupport(
							tableViewer, cellEditor));
				} else
					tableColumn = new TableColumn(
							((org.eclipse.swt.widgets.Table) getSWTWidget()),
							SWT.NONE);
				tableColumn
						.setText(columnDescriptor.getLabel() != null ? columnDescriptor
								.getLabel() : "");//$NON-NLS-1$
				tableColumn.setWidth(columnDescriptor.getMinimumWidth());

			}
		}

	}

	protected List<ColumnDescriptor> getColumnDescriptors() {
		List<ColumnDescriptor> columnDescriptors = new ArrayList<ColumnDescriptor>();
		for (StyleRule rule : ((StyledElement) getHost().getModel())
				.getStyleRules())
			if (rule instanceof ColumnDescriptor
					&& CollectionEditPart.COLUMN_DESCRIPTOR_PROPERTY_NAME
							.equals(rule.getPropertyName()))
				columnDescriptors.add((ColumnDescriptor) rule);
		return columnDescriptors;
	}

	protected ISelectionChangedListener getSelectionChangedListener() {
		return selectionChangedListener;
	}

	public void refresh() {
		if (getSWTWidget().isDisposed())
			return;
		if (getSWTWidget() instanceof org.eclipse.swt.widgets.Tree
				&& treeViewer != null)
			treeViewer.refresh();
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Table
				&& tableViewer != null)
			tableViewer.refresh();
		else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Combo
				|| getSWTWidget() instanceof org.eclipse.swt.custom.CCombo
				&& comboViewer != null)
			comboViewer.refresh();
	}

	public void setSelection(List<Object> newSelection) {
		if (getSWTWidget().isDisposed())
			return;
		IStructuredSelection selection = new StructuredSelection(newSelection);
		selectionChangedListenerBlocked = true;
		try {
			if (getSWTWidget() instanceof org.eclipse.swt.widgets.Tree
					&& treeViewer != null
					&& treeViewer.getSelection() instanceof IStructuredSelection)
				treeViewer.setSelection(selection);
			else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Table
					&& tableViewer != null
					&& tableViewer.getSelection() instanceof IStructuredSelection)
				tableViewer.setSelection(selection);
			else if (getSWTWidget() instanceof org.eclipse.swt.widgets.Combo
					|| getSWTWidget() instanceof org.eclipse.swt.custom.CCombo
					&& comboViewer != null
					&& comboViewer.getSelection() instanceof IStructuredSelection)
				comboViewer.setSelection(selection);
		} finally {
			selectionChangedListenerBlocked = false;
		}
	}

	protected Object[] getElements(Object inputElement,
			Hashtable<String, List<String>> selectors) {
		if (inputElement instanceof EObject) {
			String eClassName = ((EObject) inputElement).eClass().getName();
			List<Object> result = new ArrayList<Object>();
			IPointersEvaluator pointersEvaluator = getHost().getViewer()
					.getPointersEvaluator();
			List<String> paths = selectors.get(eClassName);
			for (String path : paths) {
				try {
					List<?> pointers = pointersEvaluator.selectPointers(
							inputElement, path);
					for (Object pointer : pointers) {
						Object value = pointersEvaluator.getValue(pointer);
						if (value instanceof List)
							result.addAll((List<?>) value);
						else
							result.add(value);
					}
				} catch (PathException e) {
					System.err.println(e.getMessage()); // TODO : log that
				}

			}
			return result.toArray();
		}
		return new Object[] {};
	}
}
