package org.eclipse.wazaabi.engine.swt.snippets.providers;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class FileListProvider {

	public List<?> getChildren(Object parent) {
		if (parent instanceof File && ((File) parent).isDirectory())
			return Arrays.asList(((File) parent).listFiles());
		return null;
	}

	public String getText(Object element) {
		if (element instanceof File)
			return ((File) element).getName();
		return null;
	}

	public String getText(Object element, int columnIndex) {
		if (columnIndex == 1)
			return "hello"; //$NON-NLS-1$
		if (element instanceof File)
			return ((File) element).getName();
		return null;
	}

	public Boolean hasChildren(Object element) {
		if (element instanceof File)
			return ((File) element).isDirectory();
		return false;
	}

}
