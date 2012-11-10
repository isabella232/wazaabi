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

package org.eclipse.wazaabi.ide.ui.editors;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.wazaabi.ide.ui.editors.actions.wizards.SelectECoreElementWizard;
import org.eclipse.wazaabi.ide.ui.internal.Activator;
import org.eclipse.wazaabi.mm.core.styles.CoreStylesPackage;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsPackage;
import org.eclipse.wazaabi.mm.edp.events.EDPEventsPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.swt.styles.SWTStylesPackage;

public class ImagesUtils {

	/**
	 * This method registers all the images used in this plugin.
	 * 
	 * @param registry
	 */
	public static void initializeImageRegistry(ImageRegistry registry) {
		registerImageData(registry, "filter_disabled",
				"icons/toolbar/dlcl16/filter_ps.gif");
		registerImageData(registry, "filter",
				"icons/toolbar/ecl16/filter_ps.gif");

		registerImageData(registry,
				CoreWidgetsPackage.Literals.CONTAINER.getName(),
				"icons/widgets/Composite.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.PUSH_BUTTON.getName(),
				"icons/widgets/Button.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.TEXT_COMPONENT.getName(),
				"icons/widgets/Text.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.CHECK_BOX.getName(),
				"icons/widgets/Button_check.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.RADIO_BUTTON.getName(),
				"icons/widgets/Button_radio.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.LABEL.getName(),
				"icons/widgets/Label.gif");

		registerImageData(registry,
				CoreWidgetsPackage.Literals.SCALE.getName(),
				"icons/widgets/Scale.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.SLIDER.getName(),
				"icons/widgets/Slider.gif");
		registerImageData(registry,
				CoreWidgetsPackage.Literals.SPINNER.getName(),
				"icons/widgets/Spinner.gif");

		registerImageData(registry,
				SWTStylesPackage.Literals.FILL_LAYOUT_RULE.getName(),
				"icons/layout/FillLayout.gif");
		registerImageData(registry,
				SWTStylesPackage.Literals.ROW_LAYOUT_RULE.getName(),
				"icons/layout/RowLayout.gif");
		registerImageData(registry,
				SWTStylesPackage.Literals.ROW_DATA_RULE.getName(),
				"icons/layout/RowData.gif");
		registerImageData(registry,
				CoreStylesPackage.Literals.STACK_LAYOUT_RULE.getName(),
				"icons/custom/StackLayout.png");
		registerImageData(registry,
				SWTStylesPackage.Literals.GRID_LAYOUT_RULE.getName(),
				"icons/layout/GridLayout.gif");
		registerImageData(registry,
				SWTStylesPackage.Literals.GRID_DATA_RULE.getName(),
				"icons/layout/GridData.gif");

		registerImageData(registry, SelectECoreElementWizard.EPACKAGE_IMG_NAME,
				"icons/ecore/" + SelectECoreElementWizard.EPACKAGE_IMG_NAME);
		registerImageData(registry, SelectECoreElementWizard.CLASS_IMG_NAME,
				"icons/ecore/" + SelectECoreElementWizard.CLASS_IMG_NAME);
		registerImageData(registry,
				SelectECoreElementWizard.EATTRIBUTE_IMG_NAME, "icons/ecore/"
						+ SelectECoreElementWizard.EATTRIBUTE_IMG_NAME);
		registerImageData(registry,
				SelectECoreElementWizard.EREFERENCE_IMG_NAME, "icons/ecore/"
						+ SelectECoreElementWizard.EREFERENCE_IMG_NAME);

		registerImageData(registry,
				EDPHandlersPackage.Literals.BINDING.getName(),
				"icons/binding/paperclip.png");

		registerImageData(registry,
				EDPHandlersPackage.Literals.EVENT_HANDLER.getName(),
				"icons/binding/listener_method.gif");

		registerImageData(registry,
				EDPEventsPackage.Literals.PROPERTY_CHANGED_EVENT.getName(),
				"icons/binding/propertyChangedEvent.png");
		registerImageData(registry,
				EDPEventsPackage.Literals.CONTENT_CHANGED_EVENT.getName(),
				"icons/binding/contentChangedEvent.png");
		registerImageData(registry, EDPEventsPackage.Literals.EVENT.getName(),
				"icons/binding/UIEvent.png");

	}

	/*
	 * Registers with the given key the <code>ImageData</code> found using the
	 * path in this plugin. Does nothing if no <code>ImageData</code> has been
	 * found.
	 */
	public static void registerImageData(ImageRegistry registry, String key,
			String path) {
		ImageData data = getImageData(path);
		if (data != null)
			registry.put(key, ImageDescriptor.createFromImageData(data));
	}

	/**
	 * Reads the <code>ImageData</code> using the given path applied in this
	 * plugin.
	 * 
	 * @param path
	 * @return
	 */
	public static ImageData getImageData(String path) {
		URL url = Activator.getDefault().getBundle().getEntry(path);
		if (url != null) {
			try {
				InputStream in = url.openStream();
				ImageData data = new ImageData(in);
				in.close();
				return data;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return null;
	}
}
