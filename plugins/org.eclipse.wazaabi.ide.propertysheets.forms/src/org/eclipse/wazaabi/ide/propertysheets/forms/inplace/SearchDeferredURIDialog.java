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

package org.eclipse.wazaabi.ide.propertysheets.forms.inplace;

import java.util.List;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wazaabi.ide.propertysheets.MethodLocator;

public class SearchDeferredURIDialog extends TitleAreaDialog {
	private TableViewer viewer;
	private final String initialUri;
	private final MethodLocator methodLocator;
	private final String methodName;
	private final int argsCount;

	public SearchDeferredURIDialog(Shell parentShell,
			MethodLocator methodLocator, String methodName, int argsCount,
			String initialUri) {
		super(parentShell);
		this.initialUri = initialUri;
		this.methodLocator = methodLocator;
		this.methodName = methodName;
		this.argsCount = argsCount;
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
		viewer.setContentProvider(new IStructuredContentProvider() {

			public void inputChanged(Viewer viewer, Object oldInput,
					Object newInput) {
			}

			public void dispose() {
			}

			public Object[] getElements(Object inputElement) {
				if (inputElement instanceof List<?>)
					return ((List<?>) inputElement).toArray();
				return null;
			}
		});
		viewer.setLabelProvider(new ILabelProvider() {

			public void removeListener(ILabelProviderListener listener) {
			}

			public boolean isLabelProperty(Object element, String property) {
				return false;
			}

			public void dispose() {
			}

			public void addListener(ILabelProviderListener listener) {
			}

			public String getText(Object element) {
				return element.toString();
			}

			public Image getImage(Object element) {
				return null;
			}
		});
		viewer.addDoubleClickListener(new IDoubleClickListener() {

			public void doubleClick(DoubleClickEvent event) {
				okPressed();
			}
		});

		if (methodLocator != null)
			viewer.setInput(methodLocator.getURIs(methodName, argsCount));

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
			selected = (String) s.getFirstElement();
			super.okPressed();
		}
	}

	private String selected = null;

	public String getSelected() {
		return selected;
	}
}
