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

package org.eclipse.wazaabi.ide.propertysheets.forms.complexcelleditors;

import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class SearchDeferredURIDialog extends TitleAreaDialog {
	private TableViewer viewer;
	private final String initialUri;

	public SearchDeferredURIDialog(Shell parentShell, String initialUri) {
		super(parentShell);
		this.initialUri = initialUri;
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite comp = (Composite) super.createDialogArea(parent);

		getShell().setText("Event Handler Locator");
		setTitle("Locate Event Handler");

		Composite container = new Composite(comp, SWT.NONE);
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		container.setLayout(new GridLayout(2, false));

		Label l = new Label(container, SWT.NONE);
		l.setText("URI");

		final Text uri = new Text(container, SWT.BORDER | SWT.SEARCH
				| SWT.ICON_SEARCH);
		uri.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		if (initialUri != null)
			uri.setText(initialUri);

		new Label(container, SWT.NONE);

		viewer = new TableViewer(container);
		GridData gd = new GridData(GridData.FILL_BOTH);
		viewer.getControl().setLayoutData(gd);
		viewer.setContentProvider(new ObservableListContentProvider());
		viewer.setLabelProvider(new StyledCellLabelProvider() {
			@Override
			public void update(ViewerCell cell) {
				// ContributionData data = (ContributionData) cell.getElement();
				// StyledString styledString = new
				// StyledString(data.resourceName,
				// null);

				// if (data.bundleName != null) {
				// styledString
				//							.append(" - " + data.bundleName, StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
				// }

				// if (data.sourceType != null) {
				//					styledString.append(" - ", StyledString.DECORATIONS_STYLER); //$NON-NLS-1$
				// styledString.append(
				//							data.sourceType + "", StyledString.COUNTER_STYLER); //$NON-NLS-1$
				// }
				//
				// if (data.iconPath == null) {
				// cell.setImage(javaClassImage);
				// }
				//
				// cell.setText(styledString.getString());
				// cell.setStyleRanges(styledString.getStyleRanges());
			}
		});
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				okPressed();
			}
		});

		// final WritableList list = new WritableList();
		// viewer.setInput(list);

		// final WazaabiUriContributionCollector collector = getCollector();

		// t.addModifyListener(new ModifyListener() {
		// private ContributionResultHandlerImpl currentResultHandler;
		//
		// public void modifyText(ModifyEvent e) {
		// if (currentResultHandler != null) {
		// currentResultHandler.cancled = true;
		// }
		// list.clear();
		// currentResultHandler = new ContributionResultHandlerImpl(list);
		// IWazaabiURIContributionProvider.Filter filter = new
		// IWazaabiURIContributionProvider.Filter(
		// project, t.getText());
		// collector.findContributions(filter, currentResultHandler);
		// t.addKeyListener(new KeyAdapter() {
		// public void keyPressed(KeyEvent e) {
		// if (e.keyCode == SWT.ARROW_DOWN) {
		// if (viewer.getTable().getItemCount() > 0) {
		// viewer.getTable().setFocus();
		// viewer.getTable().select(0);
		// }
		// }
		// }
		// });
		// viewer.getTable().addKeyListener(new KeyAdapter() {
		// @Override
		// public void keyPressed(KeyEvent e) {
		// super.keyPressed(e);
		// if ((e.keyCode == SWT.ARROW_UP)
		// && (viewer.getTable().getSelectionIndex() == 0)) {
		// t.setFocus();
		// }
		// }
		// });
		// }
		// });

		return comp;

	}

	@Override
	protected void okPressed() {
		IStructuredSelection s = (IStructuredSelection) viewer.getSelection();
		if (!s.isEmpty()) {
			// ContributionData cd = (ContributionData) s.getFirstElement();
			//			String uri = "bundleclass://" + cd.bundleName + "/" + cd.resourceName; //$NON-NLS-1$ //$NON-NLS-2$
			// Command cmd = SetCommand.create(editingDomain, contribution,
			// feature, uri);
			// if (cmd.canExecute()) {
			// editingDomain.getCommandStack().execute(cmd);
			// super.okPressed();
		}
	}
}
