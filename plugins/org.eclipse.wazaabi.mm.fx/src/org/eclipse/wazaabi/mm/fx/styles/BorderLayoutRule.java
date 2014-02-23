/**
 */
package org.eclipse.wazaabi.mm.fx.styles;

import org.eclipse.wazaabi.mm.core.styles.LayoutRule;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Border Layout Rule</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule#getMargin <em>Margin</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage#getBorderLayoutRule()
 * @model
 * @generated
 */
public interface BorderLayoutRule extends LayoutRule {
    /**
     * Returns the value of the '<em><b>Margin</b></em>' attribute.
     * The default value is <code>"10"</code>.
     * <!-- begin-user-doc -->
     * <p>
     * If the meaning of the '<em>Margin</em>' attribute isn't clear,
     * there really should be more of a description here...
     * </p>
     * <!-- end-user-doc -->
     * @return the value of the '<em>Margin</em>' attribute.
     * @see #setMargin(int)
     * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage#getBorderLayoutRule_Margin()
     * @model default="10"
     * @generated
     */
    int getMargin();

    /**
     * Sets the value of the '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule#getMargin <em>Margin</em>}' attribute.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param value the new value of the '<em>Margin</em>' attribute.
     * @see #getMargin()
     * @generated
     */
    void setMargin(int value);

} // BorderLayoutRule
