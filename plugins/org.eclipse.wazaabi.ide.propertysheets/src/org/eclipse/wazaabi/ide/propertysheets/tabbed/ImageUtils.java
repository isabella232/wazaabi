/*******************************************************************************
 * Copyright (c) 2014 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.ide.propertysheets.tabbed;

import java.io.IOException;
import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.ImageData;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

public class ImageUtils {

	private static final String PREFIX = "icons/";

	public static ImageData getImageData(String path, Class<?> clazz) {
		URL url = null;
		Bundle bundle = FrameworkUtil.getBundle(clazz);
		if (bundle != null) {
			url = bundle.getEntry(path);
		} else {
			if (path.startsWith(PREFIX))
				path = path.substring(PREFIX.length());
			url = clazz.getClassLoader().getResource(path);
		}
		try {
			return new ImageData(url.openStream());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static ImageDescriptor getImageDescriptor(String path, Class<?> clazz) {
		URL url = null;
		Bundle bundle = FrameworkUtil.getBundle(clazz);
		if (bundle != null) {
			url = bundle.getEntry(path);
		} else {
			if (path.startsWith(PREFIX))
				path = path.substring(PREFIX.length());
			url = clazz.getClassLoader().getResource(path);
		}
		return ImageDescriptor.createFromURL(url);
	}
}
