/**
 */
package org.eclipse.wazaabi.mm.fx.styles.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.wazaabi.mm.core.styles.impl.LayoutDataRuleImpl;

import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData;
import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutPosition;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Border Layout Data</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutDataImpl#getPosition <em>Position</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BorderLayoutDataImpl extends LayoutDataRuleImpl implements BorderLayoutData {
    /**
     * The default value of the '{@link #getPosition() <em>Position</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getPosition()
     * @generated
     * @ordered
     */
    protected static final BorderLayoutPosition POSITION_EDEFAULT = BorderLayoutPosition.CENTER;

    /**
     * The cached value of the '{@link #getPosition() <em>Position</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getPosition()
     * @generated
     * @ordered
     */
    protected BorderLayoutPosition position = POSITION_EDEFAULT;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected BorderLayoutDataImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return FXStylesPackage.Literals.BORDER_LAYOUT_DATA;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public BorderLayoutPosition getPosition() {
        return position;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setPosition(BorderLayoutPosition newPosition) {
        BorderLayoutPosition oldPosition = position;
        position = newPosition == null ? POSITION_EDEFAULT : newPosition;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, FXStylesPackage.BORDER_LAYOUT_DATA__POSITION, oldPosition, position));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case FXStylesPackage.BORDER_LAYOUT_DATA__POSITION:
                return getPosition();
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
            case FXStylesPackage.BORDER_LAYOUT_DATA__POSITION:
                setPosition((BorderLayoutPosition)newValue);
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
            case FXStylesPackage.BORDER_LAYOUT_DATA__POSITION:
                setPosition(POSITION_EDEFAULT);
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
            case FXStylesPackage.BORDER_LAYOUT_DATA__POSITION:
                return position != POSITION_EDEFAULT;
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
        result.append(" (position: ");
        result.append(position);
        result.append(')');
        return result.toString();
    }

} //BorderLayoutDataImpl
