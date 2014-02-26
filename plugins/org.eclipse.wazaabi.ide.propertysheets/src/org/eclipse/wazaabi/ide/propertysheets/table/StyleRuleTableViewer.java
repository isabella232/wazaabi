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

package org.eclipse.wazaabi.ide.propertysheets.table;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.bindings.keys.ParseException;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewerEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.OwnerDrawLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.ImageUtils;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.AbstractEditingHelper;
import org.eclipse.wazaabi.ide.propertysheets.editinghelpers.EditingHelperFactory;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.PropertyNameContentProposalProvider;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.table.graphicalhelpers.GraphicalHelperFactory;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.impl.BlankRuleImpl;

public class StyleRuleTableViewer implements TargetChangeListener {

	private final List<TargetChangeListener> listeners = new ArrayList<TargetChangeListener>();
	private GraphicalHelperFactory graphicalHelperFactory = new GraphicalHelperFactory();
	int hoverIndex = -1;
	Button deleteButton = null;
	private Composite container = null;
	private final StackLayout stackLayout = new StackLayout();
	private TableViewer tableViewer = null;

	private StyleRuleDescriptorFactory styleRuleDescriptorFactory = new StyleRuleDescriptorFactory();
	private EditingHelperFactory editingHelperFactory = null;

	private Image deleteIcon = null;

	public static final BlankRuleImpl RULE_FOR_INSERTION = new BlankRuleImpl() {
	};

	public StyleRuleTableViewer(Composite parent) {
		container = new Composite(parent, SWT.NONE);
		container.setLayout(stackLayout);
		container.addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				if (deleteIcon != null && deleteIcon.isDisposed())
					deleteIcon.dispose();
			}
		});
		deleteIcon = ImageUtils.getImage("icons/delete.gif", getClass());

		tableViewer = createViewer(container);
		stackLayout.topControl = tableViewer.getControl();
		createColumns();
		getViewer().setContentProvider(new ContentProvider());
		applyTableStyle();

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
									.getEditingHelper((StyleRule) element);
							if (!editingHelper.canEdit(element)) {
								final CellEditor editor = editingHelper
										.getCellEditor(table.getParent(),
												element);
								if (editor != null
										&& editor.getControl() != null) {
									((TargetChangeService) editor)
											.addTargetChangeListener(StyleRuleTableViewer.this);
									stackLayout.topControl = editor
											.getControl();
									container.layout();
									editor.getControl().addDisposeListener(
											new DisposeListener() {

												public void widgetDisposed(
														DisposeEvent e) {
													((TargetChangeService) editor)
															.removeTargetChangeListener(StyleRuleTableViewer.this);
													stackLayout.topControl = getViewer()
															.getControl();
													if (!container.isDisposed())
														container.layout();
												}
											});
									editor.setValue(element);
								}
							}
						}
					}
				}
			}

			public void mouseUp(MouseEvent e) {
			}
		});

		deleteButton = new Button((Table) getViewer().getControl(), SWT.PUSH
				| SWT.FLAT);
		deleteButton.setVisible(false);
		deleteButton.setImage(deleteIcon);

		deleteButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Object selected = getViewer().getElementAt(hoverIndex);
				StyledElement styledElement = (StyledElement) getViewer()
						.getInput();
				if (selected instanceof StyleRule)
					fireStyleRuleRemoved(styledElement, (StyleRule) selected);
			}
		});
		getViewer().getControl().addMouseMoveListener(new MouseMoveListener() {

			public void mouseMove(MouseEvent e) {
				TableItem item = ((Table) getViewer().getControl())
						.getItem(new Point(e.x, e.y));
				if (item != null) {
					hoverIndex = ((Table) getViewer().getControl())
							.indexOf(item);
					if (getViewer().getElementAt(hoverIndex) != RULE_FOR_INSERTION)
						showHoverButtons(item, e.x, e.y);
				} else {
					hoverIndex = -1;
					hideHoverButtons();
				}
			}
		});
		getViewer().getControl().addMouseTrackListener(
				new MouseTrackListener() {

					public void mouseEnter(MouseEvent e) {
					}

					public void mouseExit(MouseEvent e) {
						TableItem item = ((Table) getViewer().getControl())
								.getItem(new Point(e.x, e.y));
						if (item != null) {
							hoverIndex = ((Table) getViewer().getControl())
									.indexOf(item);
							showHoverButtons(item, e.x, e.y);
						} else {
							hoverIndex = -1;
							hideHoverButtons();
						}

					}

					public void mouseHover(MouseEvent e) {

					}
				});
		//
		// int operations = DND.DROP_MOVE;
		// Transfer[] transferTypes = new Transfer[] {
		// TextTransfer.getInstance() };
		// getViewer().addDropSupport(operations, transferTypes,
		// new DropTargetListener1(getViewer()));
		// getViewer().addDragSupport(operations, transferTypes,
		// new DragSourceListener1());
	}

	public void addTargetChangeListener(TargetChangeListener listener) {
		listeners.add(listener);
	}

	protected void applyTableStyle() {
		getViewer().getTable().setHeaderVisible(true);
		getViewer().getTable().setLinesVisible(true);
	}

	protected void createColumns() {
		createPropertyNameColumn();
		createStyleValueColumn();
	}

	protected EditingHelperFactory createEditingHelperFactory() {
		return new EditingHelperFactory();
	}

	protected void createPropertyNameColumn() {
		TableViewerColumn propertyNameCol = new TableViewerColumn(getViewer(),
				SWT.NONE);

		propertyNameCol.getColumn().setText("Property name");
		propertyNameCol.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				if (element instanceof StyleRule)
					return ((StyleRule) element).getPropertyName();
				return "";
			}
		});

		propertyNameCol.setEditingSupport(new EditingSupport(getViewer()) {

			@Override
			protected boolean canEdit(Object element) {
				return true;
			}

			@Override
			protected CellEditor getCellEditor(Object element) {
				IContentProposalProvider contentProposalProvider = new PropertyNameContentProposalProvider(
						(StyledElement) getViewer().getInput());
				// TODO : move that somewhere else
				KeyStroke keyStroke = null;
				try {
					keyStroke = KeyStroke.getInstance("Ctrl+Space");
				} catch (ParseException e) {
					e.printStackTrace();
				}
				return new TextCellEditorWithContentProposal(
						(Composite) getViewer().getControl(),
						contentProposalProvider, keyStroke, null);
			}

			@Override
			protected Object getValue(Object element) {
				if (element == RULE_FOR_INSERTION)
					return "";
				else
					return ((StyleRule) element).getPropertyName();
			}

			@Override
			protected void setValue(Object element, Object value) {
				int position = -1;
				if (element != RULE_FOR_INSERTION
						&& ((StyleRule) element).getPropertyName()
								.equals(value))
					return; // DO NOTHING, SAME PROPERTY NAME
				StyledElement styledElement = (StyledElement) getViewer()
						.getInput();
				StyleRuleDescriptor descriptor = styleRuleDescriptorFactory
						.findDescriptor(styledElement, (String) value);
				if (descriptor != null) {
					StyleRule newStyleRule = descriptor.createNewStyleRule();
					if (newStyleRule != null) {
						if (element != RULE_FOR_INSERTION)
							fireStyleRuleRemoved(styledElement,
									(StyleRule) element);
						fireStyleRuleAdded(styledElement, newStyleRule,
								position);
					}
				}
			}
		});

		propertyNameCol.getColumn().setWidth(150);
	}

	protected void createStyleValueColumn() {

		TableViewerColumn valueCol = new TableViewerColumn(getViewer(),
				SWT.NONE);
		valueCol.setLabelProvider(new OwnerDrawLabelProvider() {
			@Override
			protected void erase(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((StyleRule) element)
						.erase(event, element, 1);
			}

			@Override
			protected void measure(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((StyleRule) element)
						.measure(event, element, 1);
			}

			@Override
			protected void paint(Event event, Object element) {
				graphicalHelperFactory.getGraphicalHelper((StyleRule) element)
						.paint(event, element, 1);
			}

		});
		valueCol.setEditingSupport(new EditingSupport(getViewer()) {

			@Override
			protected boolean canEdit(Object element) {
				return getEditingHelperFactory().getEditingHelper(
						(StyleRule) element).canEdit(element);
			}

			@Override
			protected CellEditor getCellEditor(Object element) {
				CellEditor editor = getEditingHelperFactory().getEditingHelper(
						(StyleRule) element).getCellEditor(
						getViewer().getControl(), element);
				if (editor instanceof TargetChangeService)
					((TargetChangeService) editor)
							.addTargetChangeListener(StyleRuleTableViewer.this);
				return editor;
			}

			@Override
			protected Object getValue(Object element) {
				return getEditingHelperFactory().getEditingHelper(
						(StyleRule) element).getValue(element);
			}

			@Override
			protected void setValue(Object element, Object value) {
				getEditingHelperFactory().getEditingHelper((StyleRule) element)
						.setValue(element, value, StyleRuleTableViewer.this);
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

	protected void fireStyleRuleAdded(StyledElement styledElement,
			StyleRule styleRule, int position) {
		for (TargetChangeListener listener : listeners)
			listener.targetAdded(styledElement, styleRule, position);
	}

	protected void fireStyleRuleModified(StyleRule styleRule,
			EStructuralFeature feature, int position, Object oldValue,
			Object newValue) {
		for (TargetChangeListener listener : listeners)
			listener.targetModified(styleRule, feature, position, oldValue,
					newValue);
	}

	protected void fireStyleRuleModified(StyleRule styleRule,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		for (TargetChangeListener listener : listeners)
			listener.targetMultipleModified(styleRule, features, positions,
					oldValues, newValues);
	}

	protected void fireStyleRuleRemoved(StyledElement styleElement,
			StyleRule styleRule) {
		for (TargetChangeListener listener : listeners)
			listener.targetRemoved(styleElement, styleRule);
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

	protected void hideHoverButtons() {
		if (deleteButton != null && !deleteButton.isDisposed())
			deleteButton.setVisible(false);
	}

	public void refresh() {
		hideHoverButtons();
		getViewer().refresh();
	}

	public void refresh(boolean updateLabels) {
		hideHoverButtons();
		getViewer().refresh(updateLabels);
	}

	public void refresh(boolean updateLabels, boolean reveal) {
		hideHoverButtons();
		getViewer().refresh(updateLabels, reveal);
	}

	public void refresh(Object element) {
		hideHoverButtons();
		getViewer().refresh(element);
	}

	public void refresh(Object element, boolean updateLabels) {
		hideHoverButtons();
		getViewer().refresh(element, updateLabels);
	}

	public void refresh(Object element, boolean updateLabels, boolean reveal) {
		hideHoverButtons();
		getViewer().refresh(element, updateLabels, reveal);
	}

	public void removeTargetChangeListener(TargetChangeListener listener) {
		listeners.remove(listener);
	}

	public void setInput(Object input) {
		getViewer().setInput(input);
	}

	protected void showHoverButtons(TableItem item, int x, int y) {
		final Rectangle itemBounds = item.getBounds(1);
		if (hoverIndex != -1 && deleteButton != null
				&& !deleteButton.isDisposed()) {
			deleteButton.setBounds(itemBounds.x + itemBounds.width
					- itemBounds.height, itemBounds.y, itemBounds.height,
					itemBounds.height);
			deleteButton.setVisible(true);
		}
	}

	public void targetAdded(EObject container, EObject target, int position) {
		if (target instanceof StyleRule)
			fireStyleRuleAdded((StyledElement) container, (StyleRule) target,
					position);
	}

	public void targetModified(EObject target, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		if (target instanceof StyleRule)
			fireStyleRuleModified((StyleRule) target, feature, position,
					oldValue, newValue);
	}

	public void targetMultipleModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		if (target instanceof StyleRule)
			fireStyleRuleModified((StyleRule) target, features, positions,
					oldValues, newValues);

	}

	public void targetRemoved(EObject container, EObject target) {
		if (target instanceof StyleRule)
			fireStyleRuleRemoved((StyledElement) container, (StyleRule) target);
	}

	public Control getControl() {
		return container;
	}
}