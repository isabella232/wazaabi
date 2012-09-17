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

package org.eclipse.wazaabi.ide.ui.editors.actions;

import org.eclipse.gef.ui.actions.EditorPartAction;
import org.eclipse.ui.IEditorPart;
import org.eclipse.wazaabi.ide.ui.editors.WazaabiTreeEditor;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ExtendedTreeViewer;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class HideLayoutInfoAction extends EditorPartAction {

	public static final String HIDE_LAYOUT_ACTION_ID = "HideLayoutAction";

	public HideLayoutInfoAction(IEditorPart editor, int style) {
		super(editor, style);
	}

	public void run() {
		if (getViewer() != null)
			getViewer().setDisplayLayoutInfo(!isViewerDisplayingLayoutInfos());
	}

	@Override
	protected boolean calculateEnabled() {
		return getViewer() != null;
	}

	@Override
	protected void init() {
		super.init();
		// setToolTipText(MessageFormat.format(GEFMessages.UndoAction_Tooltip,
		//				new Object[] { "" }).trim()); //$NON-NLS-1$
		setText("Hide layout infos"); //$NON-NLS-1$
		setId(HIDE_LAYOUT_ACTION_ID);

		setImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor("filter"));
		setDisabledImageDescriptor(Activator.getDefault().getImageRegistry()
				.getDescriptor("filter_disabled"));
	}

	@Override
	public boolean isChecked() {
		return !isViewerDisplayingLayoutInfos();
	}

	protected boolean isViewerDisplayingLayoutInfos() {
		if (getViewer() != null)
			return getViewer().isDisplayingLayoutInfo();
		return true;
	}

	protected ExtendedTreeViewer getViewer() {
		if (getEditorPart() instanceof WazaabiTreeEditor
				&& ((WazaabiTreeEditor) getEditorPart()).getViewer() instanceof ExtendedTreeViewer)
			return ((ExtendedTreeViewer) ((WazaabiTreeEditor) getEditorPart())
					.getViewer());
		return null;
	}

}
