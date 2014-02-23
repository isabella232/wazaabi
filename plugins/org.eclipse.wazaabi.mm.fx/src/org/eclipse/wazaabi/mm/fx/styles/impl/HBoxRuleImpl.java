/**
 */
package org.eclipse.wazaabi.mm.fx.styles.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.wazaabi.mm.core.styles.impl.LayoutRuleImpl;
import org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage;
import org.eclipse.wazaabi.mm.fx.styles.HBoxRule;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>HBox Rule</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl#getSpacing <em>Spacing</em>}</li>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.impl.HBoxRuleImpl#getMargin <em>Margin</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class HBoxRuleImpl extends LayoutRuleImpl implements HBoxRule {
    /**
     * The default value of the '{@link #getSpacing() <em>Spacing</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSpacing()
     * @generated
     * @ordered
     */
    protected static final int SPACING_EDEFAULT = 10;

    /**
     * The cached value of the '{@link #getSpacing() <em>Spacing</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @see #getSpacing()
     * @generated
     * @ordered
     */
    protected int spacing = SPACING_EDEFAULT;

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
    protected HBoxRuleImpl() {
        super();
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    protected EClass eStaticClass() {
        return FXStylesPackage.Literals.HBOX_RULE;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public int getSpacing() {
        return spacing;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public void setSpacing(int newSpacing) {
        int oldSpacing = spacing;
        spacing = newSpacing;
        if (eNotificationRequired())
            eNotify(new ENotificationImpl(this, Notification.SET, FXStylesPackage.HBOX_RULE__SPACING, oldSpacing, spacing));
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
            eNotify(new ENotificationImpl(this, Notification.SET, FXStylesPackage.HBOX_RULE__MARGIN, oldMargin, margin));
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    @Override
    public Object eGet(int featureID, boolean resolve, boolean coreType) {
        switch (featureID) {
            case FXStylesPackage.HBOX_RULE__SPACING:
                return getSpacing();
            case FXStylesPackage.HBOX_RULE__MARGIN:
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
            case FXStylesPackage.HBOX_RULE__SPACING:
                setSpacing((Integer)newValue);
                return;
            case FXStylesPackage.HBOX_RULE__MARGIN:
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
            case FXStylesPackage.HBOX_RULE__SPACING:
                setSpacing(SPACING_EDEFAULT);
                return;
            case FXStylesPackage.HBOX_RULE__MARGIN:
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
            case FXStylesPackage.HBOX_RULE__SPACING:
                return spacing != SPACING_EDEFAULT;
            case FXStylesPackage.HBOX_RULE__MARGIN:
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
        result.append(" (spacing: ");
        result.append(spacing);
        result.append(", margin: ");
        result.append(margin);
        result.append(')');
        return result.toString();
    }

} //HBoxRuleImpl
