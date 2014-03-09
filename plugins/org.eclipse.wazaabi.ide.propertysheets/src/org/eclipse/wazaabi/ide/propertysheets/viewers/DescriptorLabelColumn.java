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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;

public class DescriptorLabelColumn {

	public static abstract class LabelPrinter {

		abstract public String getLabel(EObject item);
	};

	public DescriptorLabelColumn(final ColumnViewer viewer,
			final TargetChangeListener listener,
			final AbstractDescriptorFactory descriptorFactory,
			final EObject blankRow, final LabelPrinter printer) {
		TableViewerColumn labelsCol = new TableViewerColumn(
				(TableViewer) viewer, SWT.NONE);

		labelsCol.getColumn().setText("Property name");
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
						(EObject) getViewer().getInput(), descriptorFactory);
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
				EObject container = (EObject) getViewer().getInput();
				AbstractDescriptor descriptor = descriptorFactory
						.findDescriptor(container, (String) value);
				if (descriptor != null) {
					EObject newRow = descriptor.createNewInstance();
					if (newRow != null) {
						if (!blankRow.equals(element))
							listener.targetRemoved(container, (EObject) element);
						listener.targetAdded(container, newRow, position);
					}
				}
			}
		});

		labelsCol.getColumn().setWidth(150);
	}

}
