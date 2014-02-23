/**
 */
package org.eclipse.wazaabi.mm.fx.styles.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.fx.styles.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.fx.styles.FXStylesPackage
 * @generated
 */
public class FXStylesAdapterFactory extends AdapterFactoryImpl {
    /**
     * The cached model package.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected static FXStylesPackage modelPackage;

    /**
     * Creates an instance of the adapter factory.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public FXStylesAdapterFactory() {
        if (modelPackage == null) {
            modelPackage = FXStylesPackage.eINSTANCE;
        }
    }

    /**
     * Returns whether this factory is applicable for the type of the object.
     * <!-- begin-user-doc -->
     * This implementation returns <code>true</code> if the object is either the model's package or is an instance object of the model.
     * <!-- end-user-doc -->
     * @return whether this factory is applicable for the type of the object.
     * @generated
     */
    @Override
    public boolean isFactoryForType(Object object) {
        if (object == modelPackage) {
            return true;
        }
        if (object instanceof EObject) {
            return ((EObject)object).eClass().getEPackage() == modelPackage;
        }
        return false;
    }

    /**
     * The switch that delegates to the <code>createXXX</code> methods.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    protected FXStylesSwitch<Adapter> modelSwitch =
        new FXStylesSwitch<Adapter>() {
            @Override
            public Adapter caseBorderLayoutRule(BorderLayoutRule object) {
                return createBorderLayoutRuleAdapter();
            }
            @Override
            public Adapter caseHBoxRule(HBoxRule object) {
                return createHBoxRuleAdapter();
            }
            @Override
            public Adapter caseVBoxRule(VBoxRule object) {
                return createVBoxRuleAdapter();
            }
            @Override
            public Adapter caseBorderLayoutData(BorderLayoutData object) {
                return createBorderLayoutDataAdapter();
            }
            @Override
            public Adapter caseStyleRule(StyleRule object) {
                return createStyleRuleAdapter();
            }
            @Override
            public Adapter caseLayoutRule(LayoutRule object) {
                return createLayoutRuleAdapter();
            }
            @Override
            public Adapter caseLayoutDataRule(LayoutDataRule object) {
                return createLayoutDataRuleAdapter();
            }
            @Override
            public Adapter defaultCase(EObject object) {
                return createEObjectAdapter();
            }
        };

    /**
     * Creates an adapter for the <code>target</code>.
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @param target the object to adapt.
     * @return the adapter for the <code>target</code>.
     * @generated
     */
    @Override
    public Adapter createAdapter(Notifier target) {
        return modelSwitch.doSwitch((EObject)target);
    }


    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule <em>Border Layout Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutRule
     * @generated
     */
    public Adapter createBorderLayoutRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.fx.styles.HBoxRule <em>HBox Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.fx.styles.HBoxRule
     * @generated
     */
    public Adapter createHBoxRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.fx.styles.VBoxRule <em>VBox Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.fx.styles.VBoxRule
     * @generated
     */
    public Adapter createVBoxRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData <em>Border Layout Data</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.fx.styles.BorderLayoutData
     * @generated
     */
    public Adapter createBorderLayoutDataAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StyleRule <em>Style Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.core.styles.StyleRule
     * @generated
     */
    public Adapter createStyleRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutRule <em>Layout Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.core.styles.LayoutRule
     * @generated
     */
    public Adapter createLayoutRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.LayoutDataRule <em>Layout Data Rule</em>}'.
     * <!-- begin-user-doc -->
     * This default implementation returns null so that we can easily ignore cases;
     * it's useful to ignore a case when inheritance will catch all the cases anyway.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @see org.eclipse.wazaabi.mm.core.styles.LayoutDataRule
     * @generated
     */
    public Adapter createLayoutDataRuleAdapter() {
        return null;
    }

    /**
     * Creates a new adapter for the default case.
     * <!-- begin-user-doc -->
     * This default implementation returns null.
     * <!-- end-user-doc -->
     * @return the new adapter.
     * @generated
     */
    public Adapter createEObjectAdapter() {
        return null;
    }

} //FXStylesAdapterFactory
