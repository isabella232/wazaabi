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
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.FontRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Font Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl#getPropertyName <em>Property Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl#getName <em>Name</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl#getHeight <em>Height</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl#isItalic <em>Italic</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.core.styles.impl.FontRuleImpl#isBold <em>Bold</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class FontRuleImpl extends EObjectImpl implements FontRule {
	/**
     * The default value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected static final String PROPERTY_NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getPropertyName() <em>Property Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getPropertyName()
     * @generated
     * @ordered
     */
	protected String propertyName = PROPERTY_NAME_EDEFAULT;

	/**
     * The default value of the '{@link #getName() <em>Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getName()
     * @generated
     * @ordered
     */
	protected static final String NAME_EDEFAULT = null;

	/**
     * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getName()
     * @generated
     * @ordered
     */
	protected String name = NAME_EDEFAULT;

	/**
     * This is true if the Name attribute has been set.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	protected boolean nameESet;

	/**
     * The default value of the '{@link #getHeight() <em>Height</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeight()
     * @generated
     * @ordered
     */
	protected static final int HEIGHT_EDEFAULT = 0;

	/**
     * The cached value of the '{@link #getHeight() <em>Height</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #getHeight()
     * @generated
     * @ordered
     */
	protected int height = HEIGHT_EDEFAULT;

	/**
     * This is true if the Height attribute has been set.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	protected boolean heightESet;

	/**
     * The default value of the '{@link #isItalic() <em>Italic</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isItalic()
     * @generated
     * @ordered
     */
	protected static final boolean ITALIC_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isItalic() <em>Italic</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isItalic()
     * @generated
     * @ordered
     */
	protected boolean italic = ITALIC_EDEFAULT;

	/**
     * This is true if the Italic attribute has been set.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	protected boolean italicESet;

	/**
     * The default value of the '{@link #isBold() <em>Bold</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isBold()
     * @generated
     * @ordered
     */
	protected static final boolean BOLD_EDEFAULT = false;

	/**
     * The cached value of the '{@link #isBold() <em>Bold</em>}' attribute.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #isBold()
     * @generated
     * @ordered
     */
	protected boolean bold = BOLD_EDEFAULT;

	/**
     * This is true if the Bold attribute has been set.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     * @ordered
     */
	protected boolean boldESet;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	protected FontRuleImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	protected EClass eStaticClass() {
        return CoreStylesPackage.Literals.FONT_RULE;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getPropertyName() {
        return propertyName;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setPropertyName(String newPropertyName) {
        String oldPropertyName = propertyName;
        propertyName = newPropertyName;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.FONT_RULE__PROPERTY_NAME, oldPropertyName, propertyName));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String getName() {
        return name;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setName(String newName) {
        String oldName = name;
        name = newName;
        boolean oldNameESet = nameESet;
        nameESet = true;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.FONT_RULE__NAME, oldName, name, !oldNameESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void unsetName() {
        String oldName = name;
        boolean oldNameESet = nameESet;
        name = NAME_EDEFAULT;
        nameESet = false;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.FONT_RULE__NAME, oldName, NAME_EDEFAULT, oldNameESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isSetName() {
        return nameESet;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public int getHeight() {
        return height;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setHeight(int newHeight) {
        int oldHeight = height;
        height = newHeight;
        boolean oldHeightESet = heightESet;
        heightESet = true;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.FONT_RULE__HEIGHT, oldHeight, height, !oldHeightESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void unsetHeight() {
        int oldHeight = height;
        boolean oldHeightESet = heightESet;
        height = HEIGHT_EDEFAULT;
        heightESet = false;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.FONT_RULE__HEIGHT, oldHeight, HEIGHT_EDEFAULT, oldHeightESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isSetHeight() {
        return heightESet;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isItalic() {
        return italic;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setItalic(boolean newItalic) {
        boolean oldItalic = italic;
        italic = newItalic;
        boolean oldItalicESet = italicESet;
        italicESet = true;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.FONT_RULE__ITALIC, oldItalic, italic, !oldItalicESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void unsetItalic() {
        boolean oldItalic = italic;
        boolean oldItalicESet = italicESet;
        italic = ITALIC_EDEFAULT;
        italicESet = false;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.FONT_RULE__ITALIC, oldItalic, ITALIC_EDEFAULT, oldItalicESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isSetItalic() {
        return italicESet;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isBold() {
        return bold;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void setBold(boolean newBold) {
        boolean oldBold = bold;
        bold = newBold;
        boolean oldBoldESet = boldESet;
        boldESet = true;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, CoreStylesPackage.FONT_RULE__BOLD, oldBold, bold, !oldBoldESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void unsetBold() {
        boolean oldBold = bold;
        boolean oldBoldESet = boldESet;
        bold = BOLD_EDEFAULT;
        boldESet = false;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.UNSET, CoreStylesPackage.FONT_RULE__BOLD, oldBold, BOLD_EDEFAULT, oldBoldESet));
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public boolean isSetBold() {
        return boldESet;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case CoreStylesPackage.FONT_RULE__PROPERTY_NAME:
                return getPropertyName();
            case CoreStylesPackage.FONT_RULE__NAME:
                return getName();
            case CoreStylesPackage.FONT_RULE__HEIGHT:
                return getHeight();
            case CoreStylesPackage.FONT_RULE__ITALIC:
                return isItalic();
            case CoreStylesPackage.FONT_RULE__BOLD:
                return isBold();
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
            case CoreStylesPackage.FONT_RULE__PROPERTY_NAME:
                setPropertyName((String)newValue);
                return;
            case CoreStylesPackage.FONT_RULE__NAME:
                setName((String)newValue);
                return;
            case CoreStylesPackage.FONT_RULE__HEIGHT:
                setHeight((Integer)newValue);
                return;
            case CoreStylesPackage.FONT_RULE__ITALIC:
                setItalic((Boolean)newValue);
                return;
            case CoreStylesPackage.FONT_RULE__BOLD:
                setBold((Boolean)newValue);
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
            case CoreStylesPackage.FONT_RULE__PROPERTY_NAME:
                setPropertyName(PROPERTY_NAME_EDEFAULT);
                return;
            case CoreStylesPackage.FONT_RULE__NAME:
                unsetName();
                return;
            case CoreStylesPackage.FONT_RULE__HEIGHT:
                unsetHeight();
                return;
            case CoreStylesPackage.FONT_RULE__ITALIC:
                unsetItalic();
                return;
            case CoreStylesPackage.FONT_RULE__BOLD:
                unsetBold();
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
            case CoreStylesPackage.FONT_RULE__PROPERTY_NAME:
                return PROPERTY_NAME_EDEFAULT == null ? propertyName != null : !PROPERTY_NAME_EDEFAULT.equals(propertyName);
            case CoreStylesPackage.FONT_RULE__NAME:
                return isSetName();
            case CoreStylesPackage.FONT_RULE__HEIGHT:
                return isSetHeight();
            case CoreStylesPackage.FONT_RULE__ITALIC:
                return isSetItalic();
            case CoreStylesPackage.FONT_RULE__BOLD:
                return isSetBold();
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
        result.append(" (propertyName: ");
        result.append(propertyName);
        result.append(", name: ");
        if (nameESet) result.append(name); else result.append("<unset>");
        result.append(", height: ");
        if (heightESet) result.append(height); else result.append("<unset>");
        result.append(", italic: ");
        if (italicESet) result.append(italic); else result.append("<unset>");
        result.append(", bold: ");
        if (boldESet) result.append(bold); else result.append("<unset>");
        result.append(')');
        return result.toString();
    }

} //FontRuleImpl
