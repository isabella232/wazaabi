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
package org.eclipse.wazaabi.mm.core.styles.collections.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.core.styles.collections.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreCollectionsStylesFactoryImpl extends EFactoryImpl implements CoreCollectionsStylesFactory {
	/**
     * Creates the default factory implementation.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public static CoreCollectionsStylesFactory init() {
        try {
            CoreCollectionsStylesFactory theCoreCollectionsStylesFactory = (CoreCollectionsStylesFactory)EPackage.Registry.INSTANCE.getEFactory(CoreCollectionsStylesPackage.eNS_URI);
            if (theCoreCollectionsStylesFactory != null) {
                return theCoreCollectionsStylesFactory;
            }
        }
        catch (Exception exception) {
            EcorePlugin.INSTANCE.log(exception);
        }
        return new CoreCollectionsStylesFactoryImpl();
    }

	/**
     * Creates an instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CoreCollectionsStylesFactoryImpl() {
        super();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public EObject create(EClass eClass) {
        switch (eClass.getClassifierID()) {
            case CoreCollectionsStylesPackage.LOOK_AND_FEEL_RULE: return createLookAndFeelRule();
            case CoreCollectionsStylesPackage.PATH_SELECTOR: return createPathSelector();
            case CoreCollectionsStylesPackage.DYNAMIC_PROVIDER: return createDynamicProvider();
            case CoreCollectionsStylesPackage.COLUMN_DESCRIPTOR: return createColumnDescriptor();
            case CoreCollectionsStylesPackage.WEIGHTED_COLUMN_DESCRIPTOR: return createWeightedColumnDescriptor();
            default:
                throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
        }
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
        switch (eDataType.getClassifierID()) {
            case CoreCollectionsStylesPackage.LOOK_AND_FEEL:
                return createLookAndFeelFromString(eDataType, initialValue);
            default:
                throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
        }
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
        switch (eDataType.getClassifierID()) {
            case CoreCollectionsStylesPackage.LOOK_AND_FEEL:
                return convertLookAndFeelToString(eDataType, instanceValue);
            default:
                throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
        }
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public LookAndFeelRule createLookAndFeelRule() {
        LookAndFeelRuleImpl lookAndFeelRule = new LookAndFeelRuleImpl();
        return lookAndFeelRule;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public PathSelector createPathSelector() {
        PathSelectorImpl pathSelector = new PathSelectorImpl();
        return pathSelector;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public DynamicProvider createDynamicProvider() {
        DynamicProviderImpl dynamicProvider = new DynamicProviderImpl();
        return dynamicProvider;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public ColumnDescriptor createColumnDescriptor() {
        ColumnDescriptorImpl columnDescriptor = new ColumnDescriptorImpl();
        return columnDescriptor;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public WeightedColumnDescriptor createWeightedColumnDescriptor() {
        WeightedColumnDescriptorImpl weightedColumnDescriptor = new WeightedColumnDescriptorImpl();
        return weightedColumnDescriptor;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public LookAndFeel createLookAndFeelFromString(EDataType eDataType, String initialValue) {
        LookAndFeel result = LookAndFeel.get(initialValue);
        if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
        return result;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public String convertLookAndFeelToString(EDataType eDataType, Object instanceValue) {
        return instanceValue == null ? null : instanceValue.toString();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CoreCollectionsStylesPackage getCoreCollectionsStylesPackage() {
        return (CoreCollectionsStylesPackage)getEPackage();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @deprecated
     * @generated
     */
	@Deprecated
	public static CoreCollectionsStylesPackage getPackage() {
        return CoreCollectionsStylesPackage.eINSTANCE;
    }

} //CoreCollectionsStylesFactoryImpl
