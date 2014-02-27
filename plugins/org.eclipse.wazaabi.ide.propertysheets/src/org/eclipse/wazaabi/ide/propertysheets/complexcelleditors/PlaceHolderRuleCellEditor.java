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

package org.eclipse.wazaabi.ide.propertysheets.complexcelleditors;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.AbstractListViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.wazaabi.ide.propertysheets.ImageUtils;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.ide.propertysheets.styleruledescriptors.StyleRuleDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details.AbstractUIContentsDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.table.editinghelpers.complexcelleditors.details.UIContentsDescriptorFactory;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;

public abstract class PlaceHolderRuleCellEditor extends InPlaceCellEditor {

	protected class CloseAction extends Action {

		public CloseAction() {
			super();
		}

		public CloseAction(String text) {
			super(text);
		}

		public CloseAction(String text, ImageDescriptor image) {
			super(text, image);
		}

		public CloseAction(String text, int style) {
			super(text, style);
		}

		@Override
		public void run() {
			if (getControl() != null && !getControl().isDisposed())
				getControl().dispose();
		}

	};

	private AbstractListViewer ruleSelectionViewer;
	private AbstractUIContentsDescriptor currentContentsDescriptor = null;
	private UIContentsDescriptorFactory uiContentsDescriptorFactory = null;

	private ImageDescriptor closeImageDescriptor = null;

	protected static final StyleRuleDescriptor EMPTY_STYLE_RULE_DESCRIPTOR = new StyleRuleDescriptor(
			"", "", "", "", "");

	public PlaceHolderRuleCellEditor(Composite parent) {
		super(parent);
	}

	abstract protected Composite createEmptyDetailsContents(Composite parent);

	protected UIContentsDescriptorFactory createUIContentsDescriptorFactory() {
		return new UIContentsDescriptorFactory();
	}

	protected void fireInputChanged(Object oldInput, Object newInput) {
		StyledElement styledElement = (StyledElement) ((StyleRule) oldInput)
				.eContainer();
		int position = styledElement.getStyleRules().indexOf(oldInput);
		fireTargetRemoved(styledElement, (EObject) oldInput);
		fireTargetAdded(styledElement, (EObject) newInput, position);
	}

	protected void fireSelectionChanged(SelectionChangedEvent event) {

		AbstractUIContentsDescriptor contentsDescriptor = getUIContentsDescriptor(((IStructuredSelection) event
				.getSelection()).getFirstElement());
		if (contentsDescriptor == null && currentContentsDescriptor == null)
			return;
		if (contentsDescriptor == null
				|| (contentsDescriptor != null && !contentsDescriptor
						.equals(currentContentsDescriptor))) {
			refreshDetails(contentsDescriptor);
			currentContentsDescriptor = contentsDescriptor;

			Object oldInput = getInput();
			StyleRuleDescriptor newDescriptor = (StyleRuleDescriptor) ((IStructuredSelection) event
					.getSelection()).getFirstElement();

			if (!newDescriptor.getEClassName().equals(
					((EObject) oldInput).eClass().getName())
					|| !newDescriptor.getPackageURI().equals(
							((EObject) oldInput).eClass().getEPackage()
									.getNsURI())) {

				Object newInput = null;
				if (newDescriptor == EMPTY_STYLE_RULE_DESCRIPTOR)
					newInput = new PlaceHolderRule(
							(StyleRuleDescriptor) getSelectionViewer()
									.getInput());
				else
					newInput = newDescriptor.createNewStyleRule();

				if ((newInput != null && !newInput.equals(oldInput))
						|| (newInput == null && oldInput != null)) {
					setInput(newInput);
					fireInputChanged(oldInput, newInput);
				}

			}
			if (contentsDescriptor != null)
				contentsDescriptor.setInput(getDetailsSection(),
						(StyleRule) getInput());
			updateDetails();
		}
	}

	abstract protected Control getDetailsSection();

	protected EClass getRelatedEClass(StyleRuleDescriptor descriptor) {
		if (descriptor instanceof StyleRuleDescriptor) {
			EPackage ePackage = EPackage.Registry.INSTANCE
					.getEPackage(((StyleRuleDescriptor) descriptor)
							.getPackageURI());
			if (ePackage != null)
				return (EClass) ePackage
						.getEClassifier(((StyleRuleDescriptor) descriptor)
								.getEClassName());
		}
		return null;
	}

	protected AbstractListViewer getSelectionViewer() {
		return ruleSelectionViewer;
	}

	protected AbstractUIContentsDescriptor getUIContentsDescriptor(
			Object newSelection) {
		if (newSelection instanceof StyleRuleDescriptor)
			return getUIContentsDescriptorFactory().getUIContentsDescriptor(
					getRelatedEClass(((StyleRuleDescriptor) newSelection)));
		return null;
	}

	public UIContentsDescriptorFactory getUIContentsDescriptorFactory() {
		if (uiContentsDescriptorFactory == null)
			uiContentsDescriptorFactory = createUIContentsDescriptorFactory();
		return uiContentsDescriptorFactory;
	}

	@Override
	public void refresh() {
		ISelection selection = null;
		if (getInput() instanceof PlaceHolderRule) {
			getSelectionViewer().setInput(
					((PlaceHolderRule) getInput()).getStyleRuleDescriptor());
			selection = new StructuredSelection(EMPTY_STYLE_RULE_DESCRIPTOR);
		} else if (getInput() instanceof StyleRule) {
			StyleRuleDescriptor descriptor = new StyleRuleDescriptorFactory()
					.getDescriptor((StyleRule) getInput());
			getSelectionViewer().setInput(descriptor.getContainer());
			selection = new StructuredSelection(descriptor);
		}
		getSelectionViewer().setSelection(selection);
	}

	abstract protected void refreshDetails(
			AbstractUIContentsDescriptor contentsDescriptor);

	protected void setSelectionViewer(AbstractListViewer ruleSelectionViewer) {
		this.ruleSelectionViewer = ruleSelectionViewer;
	}

	protected void updateDetails() {
		if (currentContentsDescriptor != null)
			currentContentsDescriptor.refresh(getDetailsSection());
	}

	@Override
	protected Control createDetailsSection(Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Layout createLayout() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Composite createMainSection(Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Control createSelectorSection(Composite parent) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String getHeaderTitle() {
		// TODO Auto-generated method stub
		return null;
	}

	protected CloseAction createCloseAction() {
		if (closeImageDescriptor == null)
			closeImageDescriptor = ImageUtils.getImageDescriptor(
					"icons/delete.gif", PlaceHolderRuleCellEditor.class); //$NON-NLS-1$

		CloseAction closeAction = new CloseAction(null, closeImageDescriptor);
		// closeAction.setImageDescriptor(closeImageDescriptor);
		return closeAction;
	}
}
