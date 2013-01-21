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

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import org.eclipse.wazaabi.mm.core.styles.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreStylesFactoryImpl extends EFactoryImpl implements CoreStylesFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static CoreStylesFactory init() {
		try {
			CoreStylesFactory theCoreStylesFactory = (CoreStylesFactory)EPackage.Registry.INSTANCE.getEFactory("http://www.wazaabi.org/core/styles"); 
			if (theCoreStylesFactory != null) {
				return theCoreStylesFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new CoreStylesFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreStylesFactoryImpl() {
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
			case CoreStylesPackage.COLOR_RULE: return createColorRule();
			case CoreStylesPackage.STRING_RULE: return createStringRule();
			case CoreStylesPackage.ORIENTATION_RULE: return createOrientationRule();
			case CoreStylesPackage.BOOLEAN_RULE: return createBooleanRule();
			case CoreStylesPackage.INT_RULE: return createIntRule();
			case CoreStylesPackage.FONT_RULE: return createFontRule();
			case CoreStylesPackage.STACK_LAYOUT_RULE: return createStackLayoutRule();
			case CoreStylesPackage.DIRECTION_RULE: return createDirectionRule();
			case CoreStylesPackage.MARKER: return createMarker();
			case CoreStylesPackage.IMAGE_RULE: return createImageRule();
			case CoreStylesPackage.TABBED_LAYOUT_RULE: return createTabbedLayoutRule();
			case CoreStylesPackage.TAB_RULE: return createTabRule();
			case CoreStylesPackage.BAR_LAYOUT_RULE: return createBarLayoutRule();
			case CoreStylesPackage.EXPAND_RULE: return createExpandRule();
			case CoreStylesPackage.EXPAND_LAYOUT_RULE: return createExpandLayoutRule();
			case CoreStylesPackage.SASH_FORM_LAYOUT_RULE: return createSashFormLayoutRule();
			case CoreStylesPackage.HYPERLINK_RULE: return createHyperlinkRule();
			case CoreStylesPackage.SASH_RULE: return createSashRule();
			case CoreStylesPackage.SCROLL_BAR_RULE: return createScrollBarRule();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ColorRule createColorRule() {
		ColorRuleImpl colorRule = new ColorRuleImpl();
		return colorRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public StringRule createStringRule() {
		StringRuleImpl stringRule = new StringRuleImpl();
		return stringRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public OrientationRule createOrientationRule() {
		OrientationRuleImpl orientationRule = new OrientationRuleImpl();
		return orientationRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BooleanRule createBooleanRule() {
		BooleanRuleImpl booleanRule = new BooleanRuleImpl();
		return booleanRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public IntRule createIntRule() {
		IntRuleImpl intRule = new IntRuleImpl();
		return intRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public FontRule createFontRule() {
		FontRuleImpl fontRule = new FontRuleImpl();
		return fontRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public StackLayoutRule createStackLayoutRule() {
		StackLayoutRuleImpl stackLayoutRule = new StackLayoutRuleImpl();
		return stackLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DirectionRule createDirectionRule() {
		DirectionRuleImpl directionRule = new DirectionRuleImpl();
		return directionRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Marker createMarker() {
		MarkerImpl marker = new MarkerImpl();
		return marker;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ImageRule createImageRule() {
		ImageRuleImpl imageRule = new ImageRuleImpl();
		return imageRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TabbedLayoutRule createTabbedLayoutRule() {
		TabbedLayoutRuleImpl tabbedLayoutRule = new TabbedLayoutRuleImpl();
		return tabbedLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public TabRule createTabRule() {
		TabRuleImpl tabRule = new TabRuleImpl();
		return tabRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public BarLayoutRule createBarLayoutRule() {
		BarLayoutRuleImpl barLayoutRule = new BarLayoutRuleImpl();
		return barLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExpandRule createExpandRule() {
		ExpandRuleImpl expandRule = new ExpandRuleImpl();
		return expandRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExpandLayoutRule createExpandLayoutRule() {
		ExpandLayoutRuleImpl expandLayoutRule = new ExpandLayoutRuleImpl();
		return expandLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SashFormLayoutRule createSashFormLayoutRule() {
		SashFormLayoutRuleImpl sashFormLayoutRule = new SashFormLayoutRuleImpl();
		return sashFormLayoutRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public HyperlinkRule createHyperlinkRule() {
		HyperlinkRuleImpl hyperlinkRule = new HyperlinkRuleImpl();
		return hyperlinkRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SashRule createSashRule() {
		SashRuleImpl sashRule = new SashRuleImpl();
		return sashRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ScrollBarRule createScrollBarRule() {
		ScrollBarRuleImpl scrollBarRule = new ScrollBarRuleImpl();
		return scrollBarRule;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreStylesPackage getCoreStylesPackage() {
		return (CoreStylesPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static CoreStylesPackage getPackage() {
		return CoreStylesPackage.eINSTANCE;
	}

} //CoreStylesFactoryImpl
