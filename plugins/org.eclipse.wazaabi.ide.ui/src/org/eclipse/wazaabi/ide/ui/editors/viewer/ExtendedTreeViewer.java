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

package org.eclipse.wazaabi.ide.ui.editors.viewer;

import org.eclipse.gef.ui.parts.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.wazaabi.ide.mapping.rules.MappingRuleManager;
import org.eclipse.wazaabi.ide.ui.editparts.AbstractComponentTreeEditPart;

public class ExtendedTreeViewer extends TreeViewer {

	public static final String BINDING_INPUT_NAME = "BINDING_INPUT_NAME"; //$NON-NLS-1$
	public static final String DEFAULT_INPUT_VARIABLE_NAME = "input"; //$NON-NLS-1$

	private MappingRuleManager mappingRuleManager;

	static class OwnerDrawListener implements Listener {

		public void handleEvent(Event event) {
			Object ep = event.item.getData();
			if (ep instanceof AbstractComponentTreeEditPart) {
				switch (event.type) {
				case SWT.MeasureItem:
					((AbstractComponentTreeEditPart) ep).measureWidget(event);
					break;
				case SWT.PaintItem:
					((AbstractComponentTreeEditPart) ep).paintWidget(event);
					break;
				case SWT.EraseItem:
					((AbstractComponentTreeEditPart) ep).eraseWidget(event);
					break;
				}
			}
		}
	};

	public ExtendedTreeViewer() {
		super();
		addDropTargetListener(new LocalTransferDropTargetListener(this));
		addDragSourceListener(new TreeViewerTransferDragListener(this));
	}

	public void setMappingRuleManager(MappingRuleManager mappingRuleManager) {
		this.mappingRuleManager = mappingRuleManager;
	}

	@Override
	public Control createControl(Composite parent) {
		Tree tree = new Tree(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		tree.setHeaderVisible(true);
		createTreeColumns(tree);
		setControl(tree);
		tree.addListener(SWT.MeasureItem, new OwnerDrawListener());
		tree.addListener(SWT.PaintItem, new OwnerDrawListener());
		tree.addListener(SWT.EraseItem, new OwnerDrawListener());
		return tree;
	}

	protected void createTreeColumns(Tree tree) {
		TreeColumn column = new TreeColumn(tree, SWT.LEFT);
		column.setWidth(200);
		column = new TreeColumn(tree, SWT.LEFT);
		column.setWidth(100);
	}

	public MappingRuleManager getMappingRuleManager() {
		return mappingRuleManager;
	}
}