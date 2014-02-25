package org.eclipse.wazaabi.ide.propertysheets;

import java.net.URL;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;
import org.osgi.framework.FrameworkUtil;

public class ImageUtils {

	private static final String PREFIX = "icons/";

	public static Image getImage(String file, Class<?> clazz) {
		URL url = null;
		Bundle bundle = FrameworkUtil.getBundle(clazz);
		if (bundle != null) {
			url = bundle.getEntry(file);
		} else {
			if (file.startsWith(PREFIX))
				file = file.substring(PREFIX.length());
			url = clazz.getClassLoader().getResource(file);
		}
		ImageDescriptor image = ImageDescriptor.createFromURL(url);
		return image.createImage();
	}
}
