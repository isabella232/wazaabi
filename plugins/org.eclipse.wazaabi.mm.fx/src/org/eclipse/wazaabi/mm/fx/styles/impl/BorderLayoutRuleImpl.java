/**
 */
package org.eclipse.wazaabi.mm.fx.styles.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl;
import org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Border Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.impl.BorderLayoutRuleImpl#getMargin <em>Margin</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BorderLayoutRuleImpl extends LayoutRuleImpl implements BorderLayoutRule {
    /**
     * The default value of the '{@link #getMargin() <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMargin()
     * @generated
     * @ordered
     */
    protected static final int MARGIN_EDEFAULT = 10;

    /**
     * The cached value of the '{@link #getMargin() <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getMargin()
     * @generated
     * @ordered
     */
    protected int margin = MARGIN_EDEFAULT;

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected BorderLayoutRuleImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return FXStylesPackage.Literals.BORDER_LAYOUT_RULE;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public int getMargin() {
        return margin;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setMargin(int newMargin) {
        int oldMargin = margin;
        margin = newMargin;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, FXStylesPackage.BORDER_LAYOUT_RULE__MARGIN, oldMargin, margin));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case FXStylesPackage.BORDER_LAYOUT_RULE__MARGIN:
                return getMargin();
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
            case FXStylesPackage.BORDER_LAYOUT_RULE__MARGIN:
                setMargin((Integer)newValue);
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
            case FXStylesPackage.BORDER_LAYOUT_RULE__MARGIN:
                setMargin(MARGIN_EDEFAULT);
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
            case FXStylesPackage.BORDER_LAYOUT_RULE__MARGIN:
                return margin != MARGIN_EDEFAULT;
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
        result.append(" (margin: ");
        result.append(margin);
        result.append(')');
        return result.toString();
    }

} //BorderLayoutRuleImpl
