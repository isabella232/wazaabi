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
package org.eclipse.wazaabi.mm.core.widgets;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage
 * @generated
 */
public interface CoreWidgetsFactory extends EFactory {
	/**
     * The singleton instance of the factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	CoreWidgetsFactory eINSTANCE = org.eclipse.wazaabi.mm.core.widgets.impl.CoreWidgetsFactoryImpl.init();

	/**
     * Returns a new object of class '<em>Progress Bar</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Progress Bar</em>'.
     * @generated
     */
	ProgressBar createProgressBar();

	/**
     * Returns a new object of class '<em>Container</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Container</em>'.
     * @generated
     */
	Container createContainer();

	/**
     * Returns a new object of class '<em>Text Component</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Text Component</em>'.
     * @generated
     */
	TextComponent createTextComponent();

	/**
     * Returns a new object of class '<em>Push Button</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Push Button</em>'.
     * @generated
     */
	PushButton createPushButton();

	/**
     * Returns a new object of class '<em>Label</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Label</em>'.
     * @generated
     */
	Label createLabel();

	/**
     * Returns a new object of class '<em>Radio Button</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Radio Button</em>'.
     * @generated
     */
	RadioButton createRadioButton();

	/**
     * Returns a new object of class '<em>Check Box</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Check Box</em>'.
     * @generated
     */
	CheckBox createCheckBox();

	/**
     * Returns a new object of class '<em>Slider</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Slider</em>'.
     * @generated
     */
	Slider createSlider();

	/**
     * Returns a new object of class '<em>Spinner</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Spinner</em>'.
     * @generated
     */
	Spinner createSpinner();

	/**
     * Returns a new object of class '<em>Scale</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Scale</em>'.
     * @generated
     */
	Scale createScale();

	/**
     * Returns a new object of class '<em>Collection</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Collection</em>'.
     * @generated
     */
	Collection createCollection();

	/**
     * Returns a new object of class '<em>Menu Component</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Menu Component</em>'.
     * @generated
     */
	MenuComponent createMenuComponent();

	/**
     * Returns a new object of class '<em>Separator</em>'.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return a new object of class '<em>Separator</em>'.
     * @generated
     */
	Separator createSeparator();

	/**
     * Returns the package supported by this factory.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @return the package supported by this factory.
     * @generated
     */
	CoreWidgetsPackage getCoreWidgetsPackage();

} //CoreWidgetsFactory
