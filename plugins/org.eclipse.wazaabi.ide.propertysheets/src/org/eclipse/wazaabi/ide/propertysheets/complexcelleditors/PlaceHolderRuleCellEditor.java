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
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor.PlaceHolderRule;
import org.eclipse.wazaabi.ide.propertysheets.tabbed.ImageUtils;

public abstract class PlaceHolderRuleCellEditor extends InPlaceCellEditor {
	private Control selectorControl;
	private Control detailControl;

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

	@Override
	protected Control createControl(Composite parent) {
		Composite mainSection = createMainSection(parent);
		selectorControl = createSelectorSection(mainSection);
		detailControl = createDetailsSection(mainSection);
		return mainSection;
	}

	abstract protected Control createDetailsSection(Composite parent);

	abstract protected Layout createLayout();

	abstract protected Composite createMainSection(Composite parent);

	abstract protected Control createSelectorSection(Composite parent);

	abstract protected Composite createEmptyDetailsContents(Composite parent);

	protected UIContentsDescriptorFactory createUIContentsDescriptorFactory() {
		return new UIContentsDescriptorFactory();
	}

	protected void fireInputChanged(Object oldInput, Object newInput) {
		EObject styledElement = ((EObject) oldInput).eContainer();
		fireTargetRemoved(styledElement, (EObject) oldInput);
		fireTargetAdded(styledElement, (EObject) newInput,
				getPosition(styledElement, (EObject) oldInput));
	}

	protected abstract int getPosition(EObject container, EObject element);

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
					newInput = newDescriptor.createNewInstance();

				if ((newInput != null && !newInput.equals(oldInput))
						|| (newInput == null && oldInput != null)) {
					setInput(newInput);
					fireInputChanged(oldInput, newInput);
				}

			}
			if (contentsDescriptor != null)
				contentsDescriptor.setInput(getDetailsSection(),
						(EObject) getInput());
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
		} else if (getInput() instanceof EObject) {
			AbstractDescriptor descriptor = getDescriptorFactory()
					.getDescriptor((EObject) getInput());
			getSelectionViewer().setInput(descriptor.getContainer());
			selection = new StructuredSelection(descriptor);
		}
		getSelectionViewer().setSelection(selection);
	}

	private AbstractDescriptorFactory descriptorFactory = null;

	protected AbstractDescriptorFactory getDescriptorFactory() {
		if (descriptorFactory == null)
			descriptorFactory = createAbstractDescriptorFactory();
		return descriptorFactory;
	}

	abstract protected AbstractDescriptorFactory createAbstractDescriptorFactory();

	abstract protected void refreshDetails(
			AbstractUIContentsDescriptor contentsDescriptor);

	protected void setSelectionViewer(AbstractListViewer ruleSelectionViewer) {
		this.ruleSelectionViewer = ruleSelectionViewer;
	}

	protected void updateDetails() {
		if (currentContentsDescriptor != null)
			currentContentsDescriptor.refresh(getDetailsSection());
	}

	abstract protected String getHeaderTitle();

	protected CloseAction createCloseAction() {
		if (closeImageDescriptor == null)
			closeImageDescriptor = ImageUtils.getImageDescriptor(
					"icons/delete.gif", PlaceHolderRuleCellEditor.class); //$NON-NLS-1$

		CloseAction closeAction = new CloseAction(null, closeImageDescriptor);
		// closeAction.setImageDescriptor(closeImageDescriptor);
		return closeAction;
	}

	public Control getSelectorControl() {
		return selectorControl;
	}

	public Control getDetailControl() {
		return detailControl;
	}
}
