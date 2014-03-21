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
import org.eclipse.wazaabi.mm.core.styles.ExpandRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Expand Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl#getLabel <em>Label</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl#isExpanded <em>Expanded</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.ExpandRuleImpl#getImage <em>Image</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExpandRuleImpl extends LayoutDataRuleImpl implements ExpandRule {
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
     * The default value of the '{@link #isExpanded() <em>Expanded</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isExpanded()
     * @generated
     * @ordered
     */
	protected static final boolean EXPANDED_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isExpanded() <em>Expanded</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isExpanded()
     * @generated
     * @ordered
     */
	protected boolean expanded = EXPANDED_EDEFAULT;

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
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected ExpandRuleImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreStylesPackage.Literals.EXPAND_RULE;
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.EXPAND_RULE__LABEL, oldLabel, label));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isExpanded() {
        return expanded;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setExpanded(boolean newExpanded) {
        boolean oldExpanded = expanded;
        expanded = newExpanded;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.EXPAND_RULE__EXPANDED, oldExpanded, expanded));
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
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.EXPAND_RULE__IMAGE, oldImage, image));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreStylesPackage.EXPAND_RULE__LABEL:
                return getLabel();
            case CoreStylesPackage.EXPAND_RULE__EXPANDED:
                return isExpanded();
            case CoreStylesPackage.EXPAND_RULE__IMAGE:
                return getImage();
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
            case CoreStylesPackage.EXPAND_RULE__LABEL:
                setLabel((String)newValue);
                return;
            case CoreStylesPackage.EXPAND_RULE__EXPANDED:
                setExpanded((Boolean)newValue);
                return;
            case CoreStylesPackage.EXPAND_RULE__IMAGE:
                setImage((String)newValue);
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
            case CoreStylesPackage.EXPAND_RULE__LABEL:
                setLabel(LABEL_EDEFAULT);
                return;
            case CoreStylesPackage.EXPAND_RULE__EXPANDED:
                setExpanded(EXPANDED_EDEFAULT);
                return;
            case CoreStylesPackage.EXPAND_RULE__IMAGE:
                setImage(IMAGE_EDEFAULT);
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
            case CoreStylesPackage.EXPAND_RULE__LABEL:
                return LABEL_EDEFAULT == null ? label != null : !LABEL_EDEFAULT.equals(label);
            case CoreStylesPackage.EXPAND_RULE__EXPANDED:
                return expanded != EXPANDED_EDEFAULT;
            case CoreStylesPackage.EXPAND_RULE__IMAGE:
                return IMAGE_EDEFAULT == null ? image != null : !IMAGE_EDEFAULT.equals(image);
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
        result.append(", expanded: ");
        result.append(expanded);
        result.append(", image: ");
        result.append(image);
        result.append(')');
        return result.toString();
    }

} //ExpandRuleImpl
