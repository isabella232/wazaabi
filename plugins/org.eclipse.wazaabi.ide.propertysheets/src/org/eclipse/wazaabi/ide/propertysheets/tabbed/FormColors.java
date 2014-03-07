/*******************************************************************************
 * Copyright (c) 2000, 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.tabbed;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.resource.LocalResourceManager;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;

public class FormColors {

	protected Map<String, Color> colorRegistry = new HashMap<String, Color>(10);

	private LocalResourceManager resources;

	/**
	 * Blends c1 and c2 based in the provided ratio.
	 * 
	 * @param c1
	 *            first color
	 * @param c2
	 *            second color
	 * @param ratio
	 *            percentage of the first color in the blend (0-100)
	 * @return the RGB value of the blended color
	 * @since 3.1
	 */
	public static RGB blend(RGB c1, RGB c2, int ratio) {
		int r = blend(c1.red, c2.red, ratio);
		int g = blend(c1.green, c2.green, ratio);
		int b = blend(c1.blue, c2.blue, ratio);
		return new RGB(r, g, b);
	}

	/**
	 * Blends two primary color components based on the provided ratio.
	 * 
	 * @param v1
	 *            first component
	 * @param v2
	 *            second component
	 * @param ratio
	 *            percentage of the first component in the blend
	 * @return
	 */
	private static int blend(int v1, int v2, int ratio) {
		int b = (ratio * v1 + (100 - ratio) * v2) / 100;
		return Math.min(255, b);
	}

	/**
	 * Creates the color for the specified key using the provided RGB object.
	 * The color object will be returned and also put into the registry. When
	 * the class is disposed, the color will be disposed with it.
	 * 
	 * @param key
	 *            the unique color key
	 * @param rgb
	 *            the RGB object
	 * @return the allocated color object
	 */
	public Color createColor(String key, RGB rgb) {
		Color c = getResourceManager().createColor(rgb);
		Color prevC = (Color) colorRegistry.get(key);
		if (prevC != null && !prevC.isDisposed())
			getResourceManager().destroyColor(prevC.getRGB());
		colorRegistry.put(key, c);
		return c;
	}

	private LocalResourceManager getResourceManager() {
		if (resources == null)
			resources = new LocalResourceManager(JFaceResources.getResources());
		return resources;
	}
}
