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
import org.eclipse.jface.viewers.AbstractListViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.AbstractDescriptorFactory;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor;
import org.eclipse.wazaabi.ide.propertysheets.descriptors.StyleRuleDescriptor.PlaceHolderRule;

public abstract class PlaceHolderRuleCellEditor extends InPlaceCellEditor {
	private Control selectorControl;
	private Control detailControl;
	private AbstractListViewer ruleSelectionViewer;
	private AbstractUIContentsDescriptor currentContentsDescriptor = null;
	private UIContentsDescriptorFactory uiContentsDescriptorFactory = null;

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
		setSectionsLayoutData(mainSection, selectorControl, detailControl);
		return mainSection;
	}

	abstract protected void setSectionsLayoutData(Control mainSection,
			Control selectorControl, Control detailControl);

	abstract protected Control createDetailsSection(Composite parent);

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
			createDetailsContents(contentsDescriptor);
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
			refreshDetails();
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
	protected void setInput(Object input) {
		super.setInput(input);
		if (input instanceof PlaceHolderRule)
			getSelectionViewer().setInput(
					((PlaceHolderRule) input).getStyleRuleDescriptor());
		else if (input instanceof EObject) {
			AbstractDescriptor descriptor = getDescriptorFactory()
					.getDescriptor((EObject) input);
			getSelectionViewer().setInput(descriptor.getContainer());
		}
	}

	@Override
	public void refresh() {
		IStructuredSelection newSelection = null;
		AbstractDescriptor currentDescriptor = null;
		if (getSelectionViewer().getSelection() instanceof IStructuredSelection)
			currentDescriptor = (AbstractDescriptor) ((IStructuredSelection) getSelectionViewer()
					.getSelection()).getFirstElement();
		
		if (getInput() instanceof PlaceHolderRule)
			newSelection = new StructuredSelection(EMPTY_STYLE_RULE_DESCRIPTOR);
		else if (getInput() instanceof EObject) {
			AbstractDescriptor descriptor = getDescriptorFactory()
					.getDescriptor((EObject) getInput());
			newSelection = new StructuredSelection(descriptor);
		}
		if (currentDescriptor == null) {
			if (newSelection.getFirstElement() == null)
				return;
		} else if (currentDescriptor.equals(newSelection.getFirstElement())) {
			refreshDetails();
			return;
		}
		getSelectionViewer().setSelection(newSelection);
	}

	private AbstractDescriptorFactory descriptorFactory = null;

	protected AbstractDescriptorFactory getDescriptorFactory() {
		if (descriptorFactory == null)
			descriptorFactory = createAbstractDescriptorFactory();
		return descriptorFactory;
	}

	abstract protected AbstractDescriptorFactory createAbstractDescriptorFactory();

	abstract protected void createDetailsContents(
			AbstractUIContentsDescriptor contentsDescriptor);

	protected void setSelectionViewer(AbstractListViewer ruleSelectionViewer) {
		this.ruleSelectionViewer = ruleSelectionViewer;
	}

	protected void refreshDetails() {
		if (currentContentsDescriptor != null)
			currentContentsDescriptor.refresh(getDetailsSection());
	}

	abstract protected String getHeaderTitle();

	public Control getSelectorControl() {
		return selectorControl;
	}

	public Control getDetailControl() {
		return detailControl;
	}

}
