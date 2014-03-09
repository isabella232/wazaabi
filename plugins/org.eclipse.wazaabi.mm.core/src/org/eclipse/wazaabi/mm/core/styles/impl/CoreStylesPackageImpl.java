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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.wazaabi.mm.core.CorePackage;

import org.eclipse.wazaabi.mm.core.annotations.CoreAnnotationsPackage;

import org.eclipse.wazaabi.mm.core.annotations.impl.CoreAnnotationsPackageImpl;

import org.eclipse.wazaabi.mm.core.extras.CoreExtrasPackage;

import org.eclipse.wazaabi.mm.core.extras.impl.CoreExtrasPackageImpl;

import org.eclipse.wazaabi.mm.core.handlers.CoreHandlersPackage;

import org.eclipse.wazaabi.mm.core.handlers.impl.CoreHandlersPackageImpl;

import org.eclipse.wazaabi.mm.core.impl.CorePackageImpl;

import org.eclipse.wazaabi.mm.core.styles.BarLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.BlankRule;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.BoxLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.styles.DirectionRule;
import org.eclipse.wazaabi.mm.core.styles.ExpandLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.ExpandRule;
import org.eclipse.wazaabi.mm.core.styles.FontRule;
import org.eclipse.wazaabi.mm.core.styles.HyperlinkRule;
import org.eclipse.wazaabi.mm.core.styles.ImageRule;
import org.eclipse.wazaabi.mm.core.styles.IntRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutDataRule;
import org.eclipse.wazaabi.mm.core.styles.LayoutRule;
import org.eclipse.wazaabi.mm.core.styles.Marker;
import org.eclipse.wazaabi.mm.core.styles.OrientationRule;
import org.eclipse.wazaabi.mm.core.styles.SashFormLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.SashRule;
import org.eclipse.wazaabi.mm.core.styles.ScrollBarRule;
import org.eclipse.wazaabi.mm.core.styles.StackLayoutRule;
import org.eclipse.wazaabi.mm.core.styles.StringRule;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.core.styles.TabRule;
import org.eclipse.wazaabi.mm.core.styles.TabbedLayoutRule;

import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesPackage;

import org.eclipse.wazaabi.mm.core.styles.collections.impl.CoreCollectionsStylesPackageImpl;

import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;

import org.eclipse.wazaabi.mm.core.widgets.impl.CoreWidgetsPackageImpl;

import org.eclipse.wazaabi.mm.edp.EdpPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CoreStylesPackageImpl extends EPackageImpl implements CoreStylesPackage {
	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass styledElementEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass styleRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass colorRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass stringRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass orientationRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass booleanRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass intRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass blankRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass fontRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass layoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass stackLayoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass layoutDataRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass directionRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass markerEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass imageRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass tabbedLayoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass tabRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass barLayoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass expandRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass expandLayoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass sashFormLayoutRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass hyperlinkRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass sashRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private EClass scrollBarRuleEClass = null;

	/**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    private EClass boxLayoutRuleEClass = null;

    /**
     * Creates an instance of the model <b>Package</b>, registered with
     * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
     * package URI value.
     * <p>Note: the correct way to create the package is via the static
     * factory method {@link #init init()}, which also performs
     * initialization of the package, or returns the registered package,
     * if one already exists.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see org.eclipse.emf.ecore.EPackage.Registry
     * @see org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage#eNS_URI
     * @see #init()
     * @generated
     */
	private CoreStylesPackageImpl() {
        super(eNS_URI, CoreStylesFactory.eINSTANCE);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private static boolean isInited = false;

	/**
     * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
     * 
     * <p>This method is used to initialize {@link CoreStylesPackage#eINSTANCE} when that field is accessed.
     * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @see #eNS_URI
     * @see #createPackageContents()
     * @see #initializePackageContents()
     * @generated
     */
	public static CoreStylesPackage init() {
        if (isInited) return (CoreStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreStylesPackage.eNS_URI);

        // Obtain or create and register package
        CoreStylesPackageImpl theCoreStylesPackage = (CoreStylesPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CoreStylesPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CoreStylesPackageImpl());

        isInited = true;

        // Initialize simple dependencies
        EdpPackage.eINSTANCE.eClass();

        // Obtain or create and register interdependencies
        CorePackageImpl theCorePackage = (CorePackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) instanceof CorePackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI) : CorePackage.eINSTANCE);
        CoreWidgetsPackageImpl theCoreWidgetsPackage = (CoreWidgetsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) instanceof CoreWidgetsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreWidgetsPackage.eNS_URI) : CoreWidgetsPackage.eINSTANCE);
        CoreCollectionsStylesPackageImpl theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) instanceof CoreCollectionsStylesPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI) : CoreCollectionsStylesPackage.eINSTANCE);
        CoreAnnotationsPackageImpl theCoreAnnotationsPackage = (CoreAnnotationsPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) instanceof CoreAnnotationsPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreAnnotationsPackage.eNS_URI) : CoreAnnotationsPackage.eINSTANCE);
        CoreHandlersPackageImpl theCoreHandlersPackage = (CoreHandlersPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) instanceof CoreHandlersPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreHandlersPackage.eNS_URI) : CoreHandlersPackage.eINSTANCE);
        CoreExtrasPackageImpl theCoreExtrasPackage = (CoreExtrasPackageImpl)(EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) instanceof CoreExtrasPackageImpl ? EPackage.Registry.INSTANCE.getEPackage(CoreExtrasPackage.eNS_URI) : CoreExtrasPackage.eINSTANCE);

        // Create package meta-data objects
        theCoreStylesPackage.createPackageContents();
        theCorePackage.createPackageContents();
        theCoreWidgetsPackage.createPackageContents();
        theCoreCollectionsStylesPackage.createPackageContents();
        theCoreAnnotationsPackage.createPackageContents();
        theCoreHandlersPackage.createPackageContents();
        theCoreExtrasPackage.createPackageContents();

        // Initialize created meta-data
        theCoreStylesPackage.initializePackageContents();
        theCorePackage.initializePackageContents();
        theCoreWidgetsPackage.initializePackageContents();
        theCoreCollectionsStylesPackage.initializePackageContents();
        theCoreAnnotationsPackage.initializePackageContents();
        theCoreHandlersPackage.initializePackageContents();
        theCoreExtrasPackage.initializePackageContents();

        // Mark meta-data to indicate it can't be changed
        theCoreStylesPackage.freeze();

  
        // Update the registry and return the package
        EPackage.Registry.INSTANCE.put(CoreStylesPackage.eNS_URI, theCoreStylesPackage);
        return theCoreStylesPackage;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getStyledElement() {
        return styledElementEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EReference getStyledElement_StyleRules() {
        return (EReference)styledElementEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getStyleRule() {
        return styleRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getStyleRule_PropertyName() {
        return (EAttribute)styleRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getColorRule() {
        return colorRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getColorRule_Red() {
        return (EAttribute)colorRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getColorRule_Green() {
        return (EAttribute)colorRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getColorRule_Blue() {
        return (EAttribute)colorRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getStringRule() {
        return stringRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getStringRule_Value() {
        return (EAttribute)stringRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getOrientationRule() {
        return orientationRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getOrientationRule_Value() {
        return (EAttribute)orientationRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getBooleanRule() {
        return booleanRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getBooleanRule_Value() {
        return (EAttribute)booleanRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getIntRule() {
        return intRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getIntRule_Value() {
        return (EAttribute)intRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getBlankRule() {
        return blankRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getFontRule() {
        return fontRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFontRule_Name() {
        return (EAttribute)fontRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFontRule_Height() {
        return (EAttribute)fontRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFontRule_Italic() {
        return (EAttribute)fontRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getFontRule_Bold() {
        return (EAttribute)fontRuleEClass.getEStructuralFeatures().get(3);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getLayoutRule() {
        return layoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getStackLayoutRule() {
        return stackLayoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getStackLayoutRule_MarginHeight() {
        return (EAttribute)stackLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getStackLayoutRule_MarginWidth() {
        return (EAttribute)stackLayoutRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getStackLayoutRule_Top() {
        return (EAttribute)stackLayoutRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getLayoutDataRule() {
        return layoutDataRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getDirectionRule() {
        return directionRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getDirectionRule_Value() {
        return (EAttribute)directionRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getMarker() {
        return markerEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getImageRule() {
        return imageRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getTabbedLayoutRule() {
        return tabbedLayoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabbedLayoutRule_MaximizeVisible() {
        return (EAttribute)tabbedLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabbedLayoutRule_MinimizeVisible() {
        return (EAttribute)tabbedLayoutRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabbedLayoutRule_Position() {
        return (EAttribute)tabbedLayoutRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getTabRule() {
        return tabRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabRule_Label() {
        return (EAttribute)tabRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabRule_Image() {
        return (EAttribute)tabRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getTabRule_Closable() {
        return (EAttribute)tabRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getBarLayoutRule() {
        return barLayoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getBarLayoutRule_Draggable() {
        return (EAttribute)barLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getExpandRule() {
        return expandRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getExpandRule_Label() {
        return (EAttribute)expandRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getExpandRule_Expanded() {
        return (EAttribute)expandRuleEClass.getEStructuralFeatures().get(1);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getExpandRule_Image() {
        return (EAttribute)expandRuleEClass.getEStructuralFeatures().get(2);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getExpandLayoutRule() {
        return expandLayoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getSashFormLayoutRule() {
        return sashFormLayoutRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getSashFormLayoutRule_Orientation() {
        return (EAttribute)sashFormLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getHyperlinkRule() {
        return hyperlinkRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getSashRule() {
        return sashRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EAttribute getSashRule_Weight() {
        return (EAttribute)sashRuleEClass.getEStructuralFeatures().get(0);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public EClass getScrollBarRule() {
        return scrollBarRuleEClass;
    }

	/**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EClass getBoxLayoutRule() {
        return boxLayoutRuleEClass;
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getBoxLayoutRule_Orientation() {
        return (EAttribute)boxLayoutRuleEClass.getEStructuralFeatures().get(0);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getBoxLayoutRule_Margin() {
        return (EAttribute)boxLayoutRuleEClass.getEStructuralFeatures().get(1);
    }

    /**
     * <!-- begin-user-doc -->
     * <!-- end-user-doc -->
     * @generated
     */
    public EAttribute getBoxLayoutRule_Spacing() {
        return (EAttribute)boxLayoutRuleEClass.getEStructuralFeatures().get(2);
    }

    /**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public CoreStylesFactory getCoreStylesFactory() {
        return (CoreStylesFactory)getEFactoryInstance();
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private boolean isCreated = false;

	/**
     * Creates the meta-model objects for the package.  This method is
     * guarded to have no affect on any invocation but its first.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void createPackageContents() {
        if (isCreated) return;
        isCreated = true;

        // Create classes and their features
        styledElementEClass = createEClass(STYLED_ELEMENT);
        createEReference(styledElementEClass, STYLED_ELEMENT__STYLE_RULES);

        styleRuleEClass = createEClass(STYLE_RULE);
        createEAttribute(styleRuleEClass, STYLE_RULE__PROPERTY_NAME);

        colorRuleEClass = createEClass(COLOR_RULE);
        createEAttribute(colorRuleEClass, COLOR_RULE__RED);
        createEAttribute(colorRuleEClass, COLOR_RULE__GREEN);
        createEAttribute(colorRuleEClass, COLOR_RULE__BLUE);

        stringRuleEClass = createEClass(STRING_RULE);
        createEAttribute(stringRuleEClass, STRING_RULE__VALUE);

        orientationRuleEClass = createEClass(ORIENTATION_RULE);
        createEAttribute(orientationRuleEClass, ORIENTATION_RULE__VALUE);

        booleanRuleEClass = createEClass(BOOLEAN_RULE);
        createEAttribute(booleanRuleEClass, BOOLEAN_RULE__VALUE);

        intRuleEClass = createEClass(INT_RULE);
        createEAttribute(intRuleEClass, INT_RULE__VALUE);

        blankRuleEClass = createEClass(BLANK_RULE);

        fontRuleEClass = createEClass(FONT_RULE);
        createEAttribute(fontRuleEClass, FONT_RULE__NAME);
        createEAttribute(fontRuleEClass, FONT_RULE__HEIGHT);
        createEAttribute(fontRuleEClass, FONT_RULE__ITALIC);
        createEAttribute(fontRuleEClass, FONT_RULE__BOLD);

        layoutRuleEClass = createEClass(LAYOUT_RULE);

        stackLayoutRuleEClass = createEClass(STACK_LAYOUT_RULE);
        createEAttribute(stackLayoutRuleEClass, STACK_LAYOUT_RULE__MARGIN_HEIGHT);
        createEAttribute(stackLayoutRuleEClass, STACK_LAYOUT_RULE__MARGIN_WIDTH);
        createEAttribute(stackLayoutRuleEClass, STACK_LAYOUT_RULE__TOP);

        layoutDataRuleEClass = createEClass(LAYOUT_DATA_RULE);

        directionRuleEClass = createEClass(DIRECTION_RULE);
        createEAttribute(directionRuleEClass, DIRECTION_RULE__VALUE);

        markerEClass = createEClass(MARKER);

        imageRuleEClass = createEClass(IMAGE_RULE);

        tabbedLayoutRuleEClass = createEClass(TABBED_LAYOUT_RULE);
        createEAttribute(tabbedLayoutRuleEClass, TABBED_LAYOUT_RULE__MAXIMIZE_VISIBLE);
        createEAttribute(tabbedLayoutRuleEClass, TABBED_LAYOUT_RULE__MINIMIZE_VISIBLE);
        createEAttribute(tabbedLayoutRuleEClass, TABBED_LAYOUT_RULE__POSITION);

        tabRuleEClass = createEClass(TAB_RULE);
        createEAttribute(tabRuleEClass, TAB_RULE__LABEL);
        createEAttribute(tabRuleEClass, TAB_RULE__IMAGE);
        createEAttribute(tabRuleEClass, TAB_RULE__CLOSABLE);

        barLayoutRuleEClass = createEClass(BAR_LAYOUT_RULE);
        createEAttribute(barLayoutRuleEClass, BAR_LAYOUT_RULE__DRAGGABLE);

        expandRuleEClass = createEClass(EXPAND_RULE);
        createEAttribute(expandRuleEClass, EXPAND_RULE__LABEL);
        createEAttribute(expandRuleEClass, EXPAND_RULE__EXPANDED);
        createEAttribute(expandRuleEClass, EXPAND_RULE__IMAGE);

        expandLayoutRuleEClass = createEClass(EXPAND_LAYOUT_RULE);

        sashFormLayoutRuleEClass = createEClass(SASH_FORM_LAYOUT_RULE);
        createEAttribute(sashFormLayoutRuleEClass, SASH_FORM_LAYOUT_RULE__ORIENTATION);

        hyperlinkRuleEClass = createEClass(HYPERLINK_RULE);

        sashRuleEClass = createEClass(SASH_RULE);
        createEAttribute(sashRuleEClass, SASH_RULE__WEIGHT);

        scrollBarRuleEClass = createEClass(SCROLL_BAR_RULE);

        boxLayoutRuleEClass = createEClass(BOX_LAYOUT_RULE);
        createEAttribute(boxLayoutRuleEClass, BOX_LAYOUT_RULE__ORIENTATION);
        createEAttribute(boxLayoutRuleEClass, BOX_LAYOUT_RULE__MARGIN);
        createEAttribute(boxLayoutRuleEClass, BOX_LAYOUT_RULE__SPACING);
    }

	/**
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	private boolean isInitialized = false;

	/**
     * Complete the initialization of the package and its meta-model.  This
     * method is guarded to have no affect on any invocation but its first.
     * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
     * @generated
     */
	public void initializePackageContents() {
        if (isInitialized) return;
        isInitialized = true;

        // Initialize package
        setName(eNAME);
        setNsPrefix(eNS_PREFIX);
        setNsURI(eNS_URI);

        // Obtain other dependent packages
        CoreCollectionsStylesPackage theCoreCollectionsStylesPackage = (CoreCollectionsStylesPackage)EPackage.Registry.INSTANCE.getEPackage(CoreCollectionsStylesPackage.eNS_URI);
        CorePackage theCorePackage = (CorePackage)EPackage.Registry.INSTANCE.getEPackage(CorePackage.eNS_URI);

        // Add subpackages
        getESubpackages().add(theCoreCollectionsStylesPackage);

        // Create type parameters

        // Set bounds for type parameters

        // Add supertypes to classes
        colorRuleEClass.getESuperTypes().add(this.getStyleRule());
        stringRuleEClass.getESuperTypes().add(this.getStyleRule());
        orientationRuleEClass.getESuperTypes().add(this.getStyleRule());
        booleanRuleEClass.getESuperTypes().add(this.getStyleRule());
        intRuleEClass.getESuperTypes().add(this.getStyleRule());
        blankRuleEClass.getESuperTypes().add(this.getStyleRule());
        fontRuleEClass.getESuperTypes().add(this.getStyleRule());
        layoutRuleEClass.getESuperTypes().add(this.getStyleRule());
        stackLayoutRuleEClass.getESuperTypes().add(this.getLayoutRule());
        layoutDataRuleEClass.getESuperTypes().add(this.getStyleRule());
        directionRuleEClass.getESuperTypes().add(this.getStyleRule());
        markerEClass.getESuperTypes().add(this.getStyleRule());
        imageRuleEClass.getESuperTypes().add(this.getStringRule());
        tabbedLayoutRuleEClass.getESuperTypes().add(this.getStackLayoutRule());
        tabRuleEClass.getESuperTypes().add(this.getLayoutDataRule());
        barLayoutRuleEClass.getESuperTypes().add(this.getLayoutRule());
        expandRuleEClass.getESuperTypes().add(this.getLayoutDataRule());
        expandLayoutRuleEClass.getESuperTypes().add(this.getLayoutRule());
        sashFormLayoutRuleEClass.getESuperTypes().add(this.getLayoutRule());
        hyperlinkRuleEClass.getESuperTypes().add(this.getLayoutRule());
        sashRuleEClass.getESuperTypes().add(this.getLayoutDataRule());
        scrollBarRuleEClass.getESuperTypes().add(this.getStyleRule());
        boxLayoutRuleEClass.getESuperTypes().add(this.getLayoutRule());

        // Initialize classes and features; add operations and parameters
        initEClass(styledElementEClass, StyledElement.class, "StyledElement", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEReference(getStyledElement_StyleRules(), this.getStyleRule(), null, "styleRules", null, 0, -1, StyledElement.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        EOperation op = addEOperation(styledElementEClass, this.getStyleRule(), "getFirstStyleRule", 0, 1, IS_UNIQUE, IS_ORDERED);
        addEParameter(op, ecorePackage.getEString(), "propertyName", 0, 1, IS_UNIQUE, IS_ORDERED);
        addEParameter(op, ecorePackage.getEClass(), "eClass", 0, 1, IS_UNIQUE, IS_ORDERED);

        op = addEOperation(styledElementEClass, null, "removeFirstStyleRule", 0, 1, IS_UNIQUE, IS_ORDERED);
        addEParameter(op, ecorePackage.getEString(), "propertyName", 0, 1, IS_UNIQUE, IS_ORDERED);
        addEParameter(op, ecorePackage.getEClass(), "eClass", 0, 1, IS_UNIQUE, IS_ORDERED);

        initEClass(styleRuleEClass, StyleRule.class, "StyleRule", IS_ABSTRACT, IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getStyleRule_PropertyName(), ecorePackage.getEString(), "propertyName", null, 0, 1, StyleRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(colorRuleEClass, ColorRule.class, "ColorRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getColorRule_Red(), ecorePackage.getEInt(), "red", null, 0, 1, ColorRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getColorRule_Green(), ecorePackage.getEInt(), "green", null, 0, 1, ColorRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getColorRule_Blue(), ecorePackage.getEInt(), "blue", null, 0, 1, ColorRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(stringRuleEClass, StringRule.class, "StringRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getStringRule_Value(), ecorePackage.getEString(), "value", null, 0, 1, StringRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(orientationRuleEClass, OrientationRule.class, "OrientationRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getOrientationRule_Value(), theCorePackage.getOrientation(), "value", "HORIZONTAL", 0, 1, OrientationRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(booleanRuleEClass, BooleanRule.class, "BooleanRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getBooleanRule_Value(), ecorePackage.getEBoolean(), "value", "false", 0, 1, BooleanRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(intRuleEClass, IntRule.class, "IntRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getIntRule_Value(), ecorePackage.getEInt(), "value", null, 0, 1, IntRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(blankRuleEClass, BlankRule.class, "BlankRule", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(fontRuleEClass, FontRule.class, "FontRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getFontRule_Name(), ecorePackage.getEString(), "name", null, 0, 1, FontRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getFontRule_Height(), ecorePackage.getEInt(), "height", null, 0, 1, FontRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getFontRule_Italic(), ecorePackage.getEBoolean(), "italic", null, 0, 1, FontRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getFontRule_Bold(), ecorePackage.getEBoolean(), "bold", null, 0, 1, FontRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(layoutRuleEClass, LayoutRule.class, "LayoutRule", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(stackLayoutRuleEClass, StackLayoutRule.class, "StackLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getStackLayoutRule_MarginHeight(), ecorePackage.getEInt(), "marginHeight", "0", 0, 1, StackLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getStackLayoutRule_MarginWidth(), ecorePackage.getEInt(), "marginWidth", "0", 0, 1, StackLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getStackLayoutRule_Top(), ecorePackage.getEInt(), "top", null, 0, 1, StackLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(layoutDataRuleEClass, LayoutDataRule.class, "LayoutDataRule", IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(directionRuleEClass, DirectionRule.class, "DirectionRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getDirectionRule_Value(), theCorePackage.getDirection(), "value", null, 0, 1, DirectionRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(markerEClass, Marker.class, "Marker", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(imageRuleEClass, ImageRule.class, "ImageRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(tabbedLayoutRuleEClass, TabbedLayoutRule.class, "TabbedLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getTabbedLayoutRule_MaximizeVisible(), ecorePackage.getEBoolean(), "maximizeVisible", null, 1, 1, TabbedLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTabbedLayoutRule_MinimizeVisible(), ecorePackage.getEBoolean(), "minimizeVisible", null, 1, 1, TabbedLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTabbedLayoutRule_Position(), theCorePackage.getPosition(), "position", "TOP", 0, 1, TabbedLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(tabRuleEClass, TabRule.class, "TabRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getTabRule_Label(), ecorePackage.getEString(), "label", null, 0, 1, TabRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTabRule_Image(), ecorePackage.getEString(), "image", null, 0, 1, TabRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getTabRule_Closable(), ecorePackage.getEBoolean(), "closable", null, 1, 1, TabRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(barLayoutRuleEClass, BarLayoutRule.class, "BarLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getBarLayoutRule_Draggable(), ecorePackage.getEBoolean(), "draggable", null, 1, 1, BarLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(expandRuleEClass, ExpandRule.class, "ExpandRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getExpandRule_Label(), ecorePackage.getEString(), "label", null, 0, 1, ExpandRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getExpandRule_Expanded(), ecorePackage.getEBoolean(), "expanded", "false", 0, 1, ExpandRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getExpandRule_Image(), ecorePackage.getEString(), "image", null, 0, 1, ExpandRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(expandLayoutRuleEClass, ExpandLayoutRule.class, "ExpandLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(sashFormLayoutRuleEClass, SashFormLayoutRule.class, "SashFormLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getSashFormLayoutRule_Orientation(), theCorePackage.getOrientation(), "orientation", "VERTICAL", 0, 1, SashFormLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(hyperlinkRuleEClass, HyperlinkRule.class, "HyperlinkRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(sashRuleEClass, SashRule.class, "SashRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getSashRule_Weight(), ecorePackage.getEInt(), "weight", null, 0, 1, SashRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

        initEClass(scrollBarRuleEClass, ScrollBarRule.class, "ScrollBarRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);

        initEClass(boxLayoutRuleEClass, BoxLayoutRule.class, "BoxLayoutRule", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
        initEAttribute(getBoxLayoutRule_Orientation(), theCorePackage.getOrientation(), "orientation", null, 0, 1, BoxLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getBoxLayoutRule_Margin(), ecorePackage.getEInt(), "margin", null, 0, 1, BoxLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
        initEAttribute(getBoxLayoutRule_Spacing(), ecorePackage.getEInt(), "spacing", null, 0, 1, BoxLayoutRule.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
    }

} //CoreStylesPackageImpl
