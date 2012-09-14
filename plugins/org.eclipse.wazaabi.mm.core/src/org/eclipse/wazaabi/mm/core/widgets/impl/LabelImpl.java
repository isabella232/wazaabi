/**
 *  Copyright (c) 2008 Olivier Moises
 * 
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the Eclipse Public License v1.0
 *  which accompanies this distribution, and is available at
 *  http://www.eclipse.org/legal/epl-v10.html
 *  
 *  Contributors:
 *    Olivier Moises- initial API and implementation
 */
package org.eclipse.wazaabi.mm.core.widgets.impl;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.core.widgets.Label;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Label</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * </p>
 *
 * @generated
 */
public class LabelImpl extends AbstractComponentImpl implements Label {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected LabelImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreWidgetsPackage.Literals.LABEL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getImage() {
		org.eclipse.wazaabi.mm.core.styles.ImageRule rule = (org.eclipse.wazaabi.mm.core.styles.ImageRule) getFirstStyleRule(
				"image", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.IMAGE_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getText() {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
				"text", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
		if (rule != null)
			return rule.getValue();
		return null;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setImage(String imageUri) {
		org.eclipse.wazaabi.mm.core.styles.ImageRule rule = (org.eclipse.wazaabi.mm.core.styles.ImageRule) getFirstStyleRule(
				"image", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.IMAGE_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createImageRule();
			rule.setPropertyName("image"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(imageUri);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setText(String text) {
		org.eclipse.wazaabi.mm.core.styles.StringRule rule = (org.eclipse.wazaabi.mm.core.styles.StringRule) getFirstStyleRule(
				"text", //$NON-NLS-1$
				org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage.Literals.STRING_RULE);
		if (rule == null) {
			rule = org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory.eINSTANCE
					.createStringRule();
			rule.setPropertyName("text"); //$NON-NLS-1$
			getStyleRules().add(rule);
		}
		rule.setValue(text);
	}

} //LabelImpl
