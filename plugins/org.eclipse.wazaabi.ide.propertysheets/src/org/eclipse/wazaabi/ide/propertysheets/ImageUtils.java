package org.eclipse.wazaabi.ide.propertysheets;

import java.io.IOException;
import java.net.URL;

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
}
