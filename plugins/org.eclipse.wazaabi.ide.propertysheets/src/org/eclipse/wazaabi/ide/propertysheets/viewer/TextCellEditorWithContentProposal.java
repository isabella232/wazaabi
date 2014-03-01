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

package org.eclipse.wazaabi.ide.propertysheets.viewer;

import org.eclipse.jface.bindings.keys.KeyStroke;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposalListener2;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.fieldassist.TextContentAdapter;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

public class TextCellEditorWithContentProposal extends TextCellEditor {

    private ContentProposalAdapter contentProposalAdapter;
    private boolean popupOpen = false;

    public TextCellEditorWithContentProposal(Composite parent,
            IContentProposalProvider contentProposalProvider,
            KeyStroke keyStroke, char[] autoActivationCharacters) {
        super(parent);
        enableContentProposal(contentProposalProvider, keyStroke,
                autoActivationCharacters);
    }

    protected boolean dependsOnExternalFocusListener() {
        return false;
    }

    private void enableContentProposal(
            IContentProposalProvider contentProposalProvider,
            KeyStroke keyStroke, char[] autoActivationCharacters) {
        contentProposalAdapter = new ContentProposalAdapter(text,
                new TextContentAdapter() {

                    @Override
                    public void insertControlContents(Control control,
                            String text, int cursorPosition) {
                        ((Text) control).setText(text);
                        ((Text) control).setSelection(text.length());
                    }

                }, contentProposalProvider, keyStroke, null);

        contentProposalAdapter
                .addContentProposalListener(new IContentProposalListener2() {

                    public void proposalPopupClosed(
                            ContentProposalAdapter adapter) {
                        popupOpen = false;
                    }

                    public void proposalPopupOpened(
                            ContentProposalAdapter adapter) {
                        popupOpen = true;
                    }
                });
    }

    protected void focusLost() {
        if (!popupOpen)
            super.focusLost();
    }

    /**
     * Return the {@link ContentProposalAdapter} of this cell editor.
     *
     * @return the {@link ContentProposalAdapter}
     */
    public ContentProposalAdapter getContentProposalAdapter() {
        return contentProposalAdapter;
    }
}