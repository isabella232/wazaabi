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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.bindings.keys.ParseException;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.tabbed.ImageUtils;

public class AbstractLabelColumn {

	private int hoverIndex = -1;
	private Button deleteButton = null;
	private Image deleteIcon = null;

	public static abstract class LabelPrinter {

		abstract public String getLabel(EObject item);
	};

	public AbstractLabelColumn(final ColumnViewer viewer,
			final TargetChangeListener listener, final Object blankRow,
			final String columnLabel) {
		TableViewerColumn labelsCol = new TableViewerColumn(
				(TableViewer) viewer, SWT.NONE);

		labelsCol.getColumn().setText(columnLabel != null ? columnLabel : "");// $NON-NLQS-1$
		labelsCol.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(Object element) {
				if (element instanceof EObject)
					if (blankRow.equals(element))
						return ""; //$NON-NLS-1$
					else
						return printer.getLabel((EObject) element);
				return "";
			}
		});

		labelsCol.setEditingSupport(new EditingSupport(viewer) {

			@Override
			protected boolean canEdit(Object element) {
				return true;
			}

			@Override
			protected CellEditor getCellEditor(Object element) {
				IContentProposalProvider contentProposalProvider = new LabelContentProposalProvider(
						(EObject) viewer.getInput(), descriptorFactory);
				// TODO : move that somewhere else
				KeyStroke keyStroke = null;
				try {
					keyStroke = KeyStroke.getInstance("Ctrl+Space");
				} catch (ParseException e) {
					e.printStackTrace();
				}
				return new TextCellEditorWithContentProposal((Composite) viewer
						.getControl(), contentProposalProvider, keyStroke, null);
			}

			@Override
			protected Object getValue(Object element) {
				if (blankRow.equals(element))
					return "";
				else
					return printer.getLabel((EObject) element);
			}

			@Override
			protected void setValue(Object element, Object value) {
				int position = -1;
				if (!blankRow.equals(element)
						&& printer.getLabel((EObject) element).equals(value))
					return; // DO NOTHING, SAME PROPERTY NAME
				EObject container = (EObject) viewer.getInput();
				AbstractDescriptor descriptor = descriptorFactory
						.findDescriptor(container, (String) value);
				EObject newRow = null;
				if (descriptor != null)
					newRow = descriptor.createNewInstance();
				else
					newRow = createRowWithoutDescriptor(element, value);
				if (newRow != null) {
					if (!blankRow.equals(element))
						listener.targetRemoved(container, (EObject) element);
					listener.targetAdded(container, newRow, position);
				}
			}
		});

		labelsCol.getColumn().setWidth(150);

		deleteIcon = new Image(viewer.getControl().getDisplay(),
				ImageUtils.getImageData("icons/delete.gif",
						AbstractTableViewer.class));

		viewer.getControl().addDisposeListener(new DisposeListener() {

			public void widgetDisposed(DisposeEvent e) {
				if (deleteIcon != null && deleteIcon.isDisposed())
					deleteIcon.dispose();
			}
		});

		deleteButton = new Button((Table) viewer.getControl(), SWT.PUSH
				| SWT.FLAT);
		deleteButton.setVisible(false);
		deleteButton.setImage(deleteIcon);

		deleteButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				Object selected = ((TableViewer) viewer)
						.getElementAt(hoverIndex);
				if (viewer.getInput() instanceof EObject
						&& selected instanceof EObject)
					listener.targetRemoved((EObject) viewer.getInput(),
							(EObject) selected);
			}
		});
		viewer.getControl().addMouseMoveListener(new MouseMoveListener() {

			public void mouseMove(MouseEvent e) {
				TableItem item = ((Table) viewer.getControl())
						.getItem(new Point(e.x, e.y));
				if (item != null) {
					hoverIndex = ((Table) viewer.getControl()).indexOf(item);
					if (!blankRow.equals(((TableViewer) viewer)
							.getElementAt(hoverIndex)))
						showHoverButtons(item, e.x, e.y);
				} else {
					hoverIndex = -1;
					hideHoverButtons();
				}
			}
		});
		viewer.getControl().addMouseTrackListener(new MouseTrackListener() {

			public void mouseEnter(MouseEvent e) {
			}

			public void mouseExit(MouseEvent e) {
				TableItem item = ((Table) viewer.getControl())
						.getItem(new Point(e.x, e.y));
				if (item != null) {
					hoverIndex = ((Table) viewer.getControl()).indexOf(item);
					showHoverButtons(item, e.x, e.y);
				} else {
					hoverIndex = -1;
					hideHoverButtons();
				}

			}

			public void mouseHover(MouseEvent e) {

			}
		});

	}

	protected void hideHoverButtons() {
		if (deleteButton != null && !deleteButton.isDisposed())
			deleteButton.setVisible(false);
	}

	protected void showHoverButtons(TableItem item, int x, int y) {
		final Rectangle itemBounds = item.getBounds(0);
		if (hoverIndex != -1 && deleteButton != null
				&& !deleteButton.isDisposed()) {
			deleteButton.setBounds(itemBounds.x + itemBounds.width
					- itemBounds.height, itemBounds.y, itemBounds.height,
					itemBounds.height);
			deleteButton.setVisible(true);
		}
	}

	public void refresh() {
		hideHoverButtons();
	}

	protected EObject createRowWithoutDescriptor(Object element, Object value) {
		return null;
	}
}
