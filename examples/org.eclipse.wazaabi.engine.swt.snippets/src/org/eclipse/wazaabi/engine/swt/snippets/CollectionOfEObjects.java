/*******************************************************************************
 * Copyright (c) 2008 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.snippets;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.engine.core.editparts.CollectionEditPart;
import org.eclipse.wazaabi.engine.swt.nonosgi.SWTHelper;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.locationpaths.nonosgi.LocationPathsHelper;
import org.eclipse.wazaabi.locator.urn.java.nonosgi.URNJavaLocatorHelper;
import org.eclipse.wazaabi.mm.core.Alignment;
import org.eclipse.wazaabi.mm.core.styles.BooleanRule;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.ScrollBarRule;
import org.eclipse.wazaabi.mm.core.styles.collections.ColumnDescriptor;
import org.eclipse.wazaabi.mm.core.styles.collections.CoreCollectionsStylesFactory;
import org.eclipse.wazaabi.mm.core.styles.collections.DynamicProvider;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeel;
import org.eclipse.wazaabi.mm.core.styles.collections.LookAndFeelRule;
import org.eclipse.wazaabi.mm.core.styles.collections.PathSelector;
import org.eclipse.wazaabi.mm.core.styles.collections.WeightedColumnDescriptor;
import org.eclipse.wazaabi.mm.core.widgets.Collection;
import org.eclipse.wazaabi.mm.core.widgets.Container;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.swt.styles.FillLayoutRule;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CollectionOfEObjects {

	private static final Logger logger = LoggerFactory
			.getLogger(CollectionOfEObjects.class);

	public static void main(String[] args) {

		// create the shell
		Display display = new Display();
		Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
		mainShell.setLayout(new FillLayout());
		mainShell.setSize(400, 400);

		// create the viewer
		SWTControlViewer viewer = new SWTControlViewer(mainShell);
		// init SWT Engine in standalone mode
		SWTHelper.init(viewer);
		// initialize the locationPaths processor
		LocationPathsHelper.init(viewer);

		URNJavaLocatorHelper.init(viewer);

		// create a container and set its layout
		Container container = CoreWidgetsFactory.eINSTANCE.createContainer();
		FillLayoutRule layoutRule = SWTStylesFactory.eINSTANCE
				.createFillLayoutRule();
		layoutRule.setPropertyName("layout");
		container.getStyleRules().add(layoutRule);

		// create a collection

		final Collection collection = CoreWidgetsFactory.eINSTANCE
				.createCollection();

		BooleanRule booleanRule = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		booleanRule.setValue(true);
		booleanRule.setPropertyName("header-visible");
		collection.getStyleRules().add(booleanRule);

		BooleanRule multiselect = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		multiselect
				.setPropertyName(CollectionEditPart.MULTIPLE_SELECTION_PROPERTY_NAME);
		multiselect.setValue(true);
		collection.getStyleRules().add(multiselect);

		LookAndFeelRule lookAndFeelRule = CoreCollectionsStylesFactory.eINSTANCE
				.createLookAndFeelRule();
		lookAndFeelRule.setPropertyName("lookandfeel"); //$NON-NLS-1$
		lookAndFeelRule.setValue(LookAndFeel.TREE);
		collection.getStyleRules().add(lookAndFeelRule);

		BooleanRule isCheckable = CoreStylesFactory.eINSTANCE
				.createBooleanRule();
		isCheckable.setPropertyName("checkable");
		collection.getStyleRules().add(isCheckable);
		isCheckable.setValue(true);

		ColorRule colorRule = CoreStylesFactory.eINSTANCE.createColorRule();
		colorRule.setPropertyName("background-color");
		colorRule.setBlue(200);
		colorRule.setGreen(255);
		colorRule.setRed(255);
		collection.getStyleRules().add(colorRule);
		PathSelector pathSelector1 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector1.setPropertyName("content-provider");
		pathSelector1.setEClassifierName("EPackage");
		pathSelector1.getPaths().add("&eSubpackages");
		PathSelector pathSelector2 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector2.setPropertyName("content-provider");
		pathSelector2.setEClassifierName("EPackage");
		pathSelector2.getPaths().add("&eClassifiers");
		PathSelector pathSelector3 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector3.setPropertyName("label-renderer");
		pathSelector3.setEClassifierName("EPackage");
		pathSelector3.getPaths().add("@name");
		pathSelector3.getPaths().add("@nsPrefix");
		pathSelector3.getPaths().add("@nsURI");
		PathSelector pathSelector4 = CoreCollectionsStylesFactory.eINSTANCE
				.createPathSelector();
		pathSelector4.setPropertyName("label-renderer");
		pathSelector4.setEClassifierName("EClass");
		pathSelector4.getPaths().add("@name");

		collection.getStyleRules().add(pathSelector1);
		collection.getStyleRules().add(pathSelector2);
		collection.getStyleRules().add(pathSelector3);
		collection.getStyleRules().add(pathSelector4);

		ColumnDescriptor columnDescriptor1 = CoreCollectionsStylesFactory.eINSTANCE
				.createColumnDescriptor();
		columnDescriptor1.setLabel("test1");
		columnDescriptor1.setPropertyName("column-descriptor");
		columnDescriptor1.setWidth(100);
		columnDescriptor1.setHeaderAlignment(Alignment.CENTER);
		WeightedColumnDescriptor columnDescriptor2 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor2.setLabel("test2");
		columnDescriptor2.setPropertyName("column-descriptor");
		columnDescriptor2.setWeight(50);
		columnDescriptor2.setHeaderAlignment(Alignment.LEAD);

		WeightedColumnDescriptor columnDescriptor3 = CoreCollectionsStylesFactory.eINSTANCE
				.createWeightedColumnDescriptor();
		columnDescriptor3.setLabel("test3");
		columnDescriptor3.setPropertyName("column-descriptor");
		columnDescriptor3.setWeight(200);
		columnDescriptor3.setHeaderAlignment(Alignment.TRAIL);

		ScrollBarRule vscroll = CoreStylesFactory.eINSTANCE
				.createScrollBarRule();
		vscroll.setPropertyName("vertical-scrollbar");
		collection.getStyleRules().add(vscroll);

		collection.getStyleRules().add(columnDescriptor1);
		collection.getStyleRules().add(columnDescriptor2);
		collection.getStyleRules().add(columnDescriptor3);

		EPackage rootPackage = createDomainObject();
		// collection.setInput(rootPackage);
		// append the collection to the container's children list.
		container.getChildren().add(collection);

		collection.eAdapters().add(new AdapterImpl() {

			@Override
			public void notifyChanged(Notification msg) {
				switch (msg.getFeatureID(Collection.class)) {
				case CoreWidgetsPackage.COLLECTION__SELECTION:
					if (msg.getEventType() == Notification.ADD)
						logger.debug("add:" + msg.getNewValue());
					if (msg.getEventType() == Notification.REMOVE)
						logger.debug("remove:" + msg.getOldValue());
					break;
				case CoreWidgetsPackage.COLLECTION__CHECKED_ELEMENTS:
					if (msg.getEventType() == Notification.ADD)
						logger.debug("checked:" + msg.getNewValue());
					if (msg.getEventType() == Notification.REMOVE)
						logger.debug("unchecked:" + msg.getOldValue());
					break;
				}
			}
		});
		collection.getCheckedElements().add(
				rootPackage.getESubpackages().get(0));
		collection.getSelection().add(rootPackage.getESubpackages().get(1));

		DynamicProvider sorter = CoreCollectionsStylesFactory.eINSTANCE
				.createDynamicProvider();
		sorter.setPropertyName("comparator");
		sorter.setUri("urn:java:org.eclipse.wazaabi.engine.swt.snippets.providers.PackagesReverseComparator");
		collection.getStyleRules().add(sorter);

		// inject the container into the viewer
		viewer.setContents(container);
		// collection.getSelection().set(0,
		// rootPackage.getESubpackages().get(0));
		// collection.getSelection().set(0,
		// rootPackage.getESubpackages().get(1));
		//
		// collection.getCheckedElements().remove(
		// rootPackage.getESubpackages().get(0));
		// collection.getCheckedElements().add(
		// rootPackage.getESubpackages().get(1));

		// collection.getSelection().clear();

		// collection.getStyleRules().remove(sorter);

		CoreUtils.refreshContent(collection);

		Button b = new Button(mainShell, SWT.PUSH);

		b.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				collection.setInput(createDomainObject());

			}

		});
		mainShell.open();

		while (!mainShell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		display.dispose();
	}

	public static EPackage createDomainObject() {
		EPackage ePackage1 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage1.setName("package1");
		ePackage1.setNsPrefix("p1");
		ePackage1.setNsURI("urn:p1");
		EPackage ePackage2 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage2.setName("package2");
		ePackage2.setNsPrefix("p2");
		ePackage2.setNsURI("urn:p2");
		ePackage1.getESubpackages().add(ePackage2);
		EPackage ePackage3 = EcoreFactory.eINSTANCE.createEPackage();
		ePackage3.setName("package3");
		ePackage3.setNsPrefix("p3");
		ePackage3.setNsURI("urn:p3");
		ePackage1.getESubpackages().add(ePackage3);
		EPackage subPackage1 = EcoreFactory.eINSTANCE.createEPackage();
		subPackage1.setName("subPackage1");
		subPackage1.setNsPrefix("subP1");
		subPackage1.setNsURI("urn:subp&");
		ePackage3.getESubpackages().add(subPackage1);

		EClass eClass1 = EcoreFactory.eINSTANCE.createEClass();
		eClass1.setName("class1");
		ePackage1.getEClassifiers().add(eClass1);

		return ePackage1;
	}
}
