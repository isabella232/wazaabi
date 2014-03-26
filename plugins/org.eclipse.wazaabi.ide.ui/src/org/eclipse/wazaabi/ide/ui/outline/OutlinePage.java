/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.ui.outline;

import org.eclipse.gef.commands.CommandStackEvent;
import org.eclipse.gef.commands.CommandStackEventListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;

public class OutlinePage extends AbstractOutlinePage {

	private final WazaabiTreeEditor editor;

	private CommandStackEventListener commandStackListener = new CommandStackEventListener() {
		public void stackChanged(CommandStackEvent event) {
			setViewerContents(getVisibleContents(getEditorModel()));
			// TODO : re - apply editor's selection
		}
	};

	public OutlinePage(WazaabiTreeEditor editor) {
		super(editor.getViewer());
		this.editor = editor;
	}

	protected WazaabiTreeEditor getEditor() {
		return editor;
	}

	@Override
	public void dispose() {
		getEditor().getEditDomain().getCommandStack()
				.removeCommandStackEventListener(commandStackListener);
		super.dispose();
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);
		getEditor().getEditDomain().getCommandStack()
				.addCommandStackEventListener(commandStackListener);
	}

}
