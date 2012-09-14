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
package org.eclipse.wazaabi.mm.core.styles.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.TabRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Tab Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl#getImage <em>Image</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.TabRuleImpl#isClosable <em>Closable</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class TabRuleImpl extends LayoutDataRuleImpl implements TabRule {
	/**
	 * The default value of the '{@link #getLabel() <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected static final String LABEL_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLabel() <em>Label</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLabel()
	 * @generated
	 * @ordered
	 */
	protected String label = LABEL_EDEFAULT;

	/**
	 * The default value of the '{@link #getImage() <em>Image</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getImage()
	 * @generated
	 * @ordered
	 */
	protected static final String IMAGE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getImage() <em>Image</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getImage()
	 * @generated
	 * @ordered
	 */
	protected String image = IMAGE_EDEFAULT;

	/**
	 * The default value of the '{@link #isClosable() <em>Closable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isClosable()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CLOSABLE_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isClosable() <em>Closable</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isClosable()
	 * @generated
	 * @ordered
	 */
	protected boolean closable = CLOSABLE_EDEFAULT;

	/**
	 * This is true if the Closable attribute has been set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	protected boolean closableESet;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected TabRuleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CoreStylesPackage.Literals.TAB_RULE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setLabel(String newLabel) {
		String oldLabel = label;
		label = newLabel;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TAB_RULE__LABEL, oldLabel, label));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getImage() {
		return image;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setImage(String newImage) {
		String oldImage = image;
		image = newImage;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TAB_RULE__IMAGE, oldImage, image));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isClosable() {
		return closable;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setClosable(boolean newClosable) {
		boolean oldClosable = closable;
		closable = newClosable;
		boolean oldClosableESet = closableESet;
		closableESet = true;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.TAB_RULE__CLOSABLE, oldClosable, closable, !oldClosableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void unsetClosable() {
		boolean oldClosable = closable;
		boolean oldClosableESet = closableESet;
		closable = CLOSABLE_EDEFAULT;
		closableESet = false;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.TAB_RULE__CLOSABLE, oldClosable, CLOSABLE_EDEFAULT, oldClosableESet));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isSetClosable() {
		return closableESet;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CoreStylesPackage.TAB_RULE__LABEL:
				return getLabel();
			case CoreStylesPackage.TAB_RULE__IMAGE:
				return getImage();
			case CoreStylesPackage.TAB_RULE__CLOSABLE:
				return isClosable();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CoreStylesPackage.TAB_RULE__LABEL:
				setLabel((String)newValue);
				return;
			case CoreStylesPackage.TAB_RULE__IMAGE:
				setImage((String)newValue);
				return;
			case CoreStylesPackage.TAB_RULE__CLOSABLE:
				setClosable((Boolean)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case CoreStylesPackage.TAB_RULE__LABEL:
				setLabel(LABEL_EDEFAULT);
				return;
			case CoreStylesPackage.TAB_RULE__IMAGE:
				setImage(IMAGE_EDEFAULT);
				return;
			case CoreStylesPackage.TAB_RULE__CLOSABLE:
				unsetClosable();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case CoreStylesPackage.TAB_RULE__LABEL:
				return LABEL_EDEFAULT == null ? label != null : !LABEL_EDEFAULT.equals(label);
			case CoreStylesPackage.TAB_RULE__IMAGE:
				return IMAGE_EDEFAULT == null ? image != null : !IMAGE_EDEFAULT.equals(image);
			case CoreStylesPackage.TAB_RULE__CLOSABLE:
				return isSetClosable();
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (label: ");
		result.append(label);
		result.append(", image: ");
		result.append(image);
		result.append(", closable: ");
		if (closableESet) result.append(closable); else result.append("<unset>");
		result.append(')');
		return result.toString();
	}

} //TabRuleImpl
