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
package org.eclipse.wazaabi.mm.core.styles.util;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;

import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.wazaabi.mm.core.styles.*;

/**
 * <!-- begin-user-doc -->
 * The <b>Adapter Factory</b> for the model.
 * It provides an adapter <code>createXXX</code> method for each class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage
 * @generated
 */
public class CoreStylesAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static CoreStylesPackage modelPackage;

	/**
	 * Creates an instance of the adapter factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CoreStylesAdapterFactory() {
		if (modelPackage == null) {
			modelPackage = CoreStylesPackage.eINSTANCE;
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
	protected CoreStylesSwitch<Adapter> modelSwitch =
		new CoreStylesSwitch<Adapter>() {
			@Override
			public Adapter caseStyledElement(StyledElement object) {
				return createStyledElementAdapter();
			}
			@Override
			public Adapter caseStyleRule(StyleRule object) {
				return createStyleRuleAdapter();
			}
			@Override
			public Adapter caseColorRule(ColorRule object) {
				return createColorRuleAdapter();
			}
			@Override
			public Adapter caseStringRule(StringRule object) {
				return createStringRuleAdapter();
			}
			@Override
			public Adapter caseOrientationRule(OrientationRule object) {
				return createOrientationRuleAdapter();
			}
			@Override
			public Adapter caseBooleanRule(BooleanRule object) {
				return createBooleanRuleAdapter();
			}
			@Override
			public Adapter caseIntRule(IntRule object) {
				return createIntRuleAdapter();
			}
			@Override
			public Adapter caseBlankRule(BlankRule object) {
				return createBlankRuleAdapter();
			}
			@Override
			public Adapter caseFontRule(FontRule object) {
				return createFontRuleAdapter();
			}
			@Override
			public Adapter caseLayoutRule(LayoutRule object) {
				return createLayoutRuleAdapter();
			}
			@Override
			public Adapter caseStackLayoutRule(StackLayoutRule object) {
				return createStackLayoutRuleAdapter();
			}
			@Override
			public Adapter caseLayoutDataRule(LayoutDataRule object) {
				return createLayoutDataRuleAdapter();
			}
			@Override
			public Adapter caseDirectionRule(DirectionRule object) {
				return createDirectionRuleAdapter();
			}
			@Override
			public Adapter caseMarker(Marker object) {
				return createMarkerAdapter();
			}
			@Override
			public Adapter caseImageRule(ImageRule object) {
				return createImageRuleAdapter();
			}
			@Override
			public Adapter caseTabbedLayoutRule(TabbedLayoutRule object) {
				return createTabbedLayoutRuleAdapter();
			}
			@Override
			public Adapter caseTabRule(TabRule object) {
				return createTabRuleAdapter();
			}
			@Override
			public Adapter caseBarLayoutRule(BarLayoutRule object) {
				return createBarLayoutRuleAdapter();
			}
			@Override
			public Adapter caseExpandRule(ExpandRule object) {
				return createExpandRuleAdapter();
			}
			@Override
			public Adapter caseExpandLayoutRule(ExpandLayoutRule object) {
				return createExpandLayoutRuleAdapter();
			}
			@Override
			public Adapter caseSashFormLayoutRule(SashFormLayoutRule object) {
				return createSashFormLayoutRuleAdapter();
			}
			@Override
			public Adapter caseHyperlinkRule(HyperlinkRule object) {
				return createHyperlinkRuleAdapter();
			}
			@Override
			public Adapter caseSashRule(SashRule object) {
				return createSashRuleAdapter();
			}
			@Override
			public Adapter caseScrollBarRule(ScrollBarRule object) {
				return createScrollBarRuleAdapter();
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
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StyledElement <em>Styled Element</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.StyledElement
	 * @generated
	 */
	public Adapter createStyledElementAdapter() {
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
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.ColorRule <em>Color Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.ColorRule
	 * @generated
	 */
	public Adapter createColorRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StringRule <em>String Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.StringRule
	 * @generated
	 */
	public Adapter createStringRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.OrientationRule <em>Orientation Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.OrientationRule
	 * @generated
	 */
	public Adapter createOrientationRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.BooleanRule <em>Boolean Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.BooleanRule
	 * @generated
	 */
	public Adapter createBooleanRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.IntRule <em>Int Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.IntRule
	 * @generated
	 */
	public Adapter createIntRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.BlankRule <em>Blank Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.BlankRule
	 * @generated
	 */
	public Adapter createBlankRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.FontRule <em>Font Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.FontRule
	 * @generated
	 */
	public Adapter createFontRuleAdapter() {
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
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.StackLayoutRule <em>Stack Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.StackLayoutRule
	 * @generated
	 */
	public Adapter createStackLayoutRuleAdapter() {
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
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.DirectionRule <em>Direction Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.DirectionRule
	 * @generated
	 */
	public Adapter createDirectionRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.Marker <em>Marker</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.Marker
	 * @generated
	 */
	public Adapter createMarkerAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.ImageRule <em>Image Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.ImageRule
	 * @generated
	 */
	public Adapter createImageRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule <em>Tabbed Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule
	 * @generated
	 */
	public Adapter createTabbedLayoutRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.TabRule <em>Tab Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.TabRule
	 * @generated
	 */
	public Adapter createTabRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.BarLayoutRule <em>Bar Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.BarLayoutRule
	 * @generated
	 */
	public Adapter createBarLayoutRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.ExpandRule <em>Expand Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandRule
	 * @generated
	 */
	public Adapter createExpandRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule <em>Expand Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule
	 * @generated
	 */
	public Adapter createExpandLayoutRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule <em>Sash Form Layout Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule
	 * @generated
	 */
	public Adapter createSashFormLayoutRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.HyperlinkRule <em>Hyperlink Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.HyperlinkRule
	 * @generated
	 */
	public Adapter createHyperlinkRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.SashRule <em>Sash Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.SashRule
	 * @generated
	 */
	public Adapter createSashRuleAdapter() {
		return null;
	}

	/**
	 * Creates a new adapter for an object of class '{@link org.eclipse.wazaabi.mm.core.styles.ScrollBarRule <em>Scroll Bar Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * This default implementation returns null so that we can easily ignore cases;
	 * it's useful to ignore a case when inheritance will catch all the cases anyway.
	 * <!-- end-user-doc -->
	 * @return the new adapter.
	 * @see org.eclipse.wazaabi.mm.core.styles.ScrollBarRule
	 * @generated
	 */
	public Adapter createScrollBarRuleAdapter() {
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

} //CoreStylesAdapterFactory
