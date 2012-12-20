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
package org.eclipse.wazaabi.mm.core.styles;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage
 * @generated
 */
public interface CoreStylesFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CoreStylesFactory eINSTANCE = org.eclipse.wazaabi.mm.core.styles.impl.CoreStylesFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Color Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Color Rule</em>'.
	 * @generated
	 */
	ColorRule createColorRule();

	/**
	 * Returns a new object of class '<em>String Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>String Rule</em>'.
	 * @generated
	 */
	StringRule createStringRule();

	/**
	 * Returns a new object of class '<em>Orientation Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Orientation Rule</em>'.
	 * @generated
	 */
	OrientationRule createOrientationRule();

	/**
	 * Returns a new object of class '<em>Boolean Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Boolean Rule</em>'.
	 * @generated
	 */
	BooleanRule createBooleanRule();

	/**
	 * Returns a new object of class '<em>Int Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Int Rule</em>'.
	 * @generated
	 */
	IntRule createIntRule();

	/**
	 * Returns a new object of class '<em>Font Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Font Rule</em>'.
	 * @generated
	 */
	FontRule createFontRule();

	/**
	 * Returns a new object of class '<em>Stack Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Stack Layout Rule</em>'.
	 * @generated
	 */
	StackLayoutRule createStackLayoutRule();

	/**
	 * Returns a new object of class '<em>Direction Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Direction Rule</em>'.
	 * @generated
	 */
	DirectionRule createDirectionRule();

	/**
	 * Returns a new object of class '<em>Marker</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Marker</em>'.
	 * @generated
	 */
	Marker createMarker();

	/**
	 * Returns a new object of class '<em>Image Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Image Rule</em>'.
	 * @generated
	 */
	ImageRule createImageRule();

	/**
	 * Returns a new object of class '<em>Tabbed Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Tabbed Layout Rule</em>'.
	 * @generated
	 */
	TabbedLayoutRule createTabbedLayoutRule();

	/**
	 * Returns a new object of class '<em>Tab Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Tab Rule</em>'.
	 * @generated
	 */
	TabRule createTabRule();

	/**
	 * Returns a new object of class '<em>Bar Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Bar Layout Rule</em>'.
	 * @generated
	 */
	BarLayoutRule createBarLayoutRule();

	/**
	 * Returns a new object of class '<em>Expand Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Expand Rule</em>'.
	 * @generated
	 */
	ExpandRule createExpandRule();

	/**
	 * Returns a new object of class '<em>Expand Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Expand Layout Rule</em>'.
	 * @generated
	 */
	ExpandLayoutRule createExpandLayoutRule();

	/**
	 * Returns a new object of class '<em>Sash Form Layout Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Sash Form Layout Rule</em>'.
	 * @generated
	 */
	SashFormLayoutRule createSashFormLayoutRule();

	/**
	 * Returns a new object of class '<em>Hyperlink Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Hyperlink Rule</em>'.
	 * @generated
	 */
	HyperlinkRule createHyperlinkRule();

	/**
	 * Returns a new object of class '<em>Sash Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Sash Rule</em>'.
	 * @generated
	 */
	SashRule createSashRule();

	/**
	 * Returns a new object of class '<em>Scroll Bar Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Scroll Bar Rule</em>'.
	 * @generated
	 */
	ScrollBarRule createScrollBarRule();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	CoreStylesPackage getCoreStylesPackage();

} //CoreStylesFactory
