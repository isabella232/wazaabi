package org.eclipse.wazaabi.engine.swt.snippets.providers;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class FileListProvider {

	private Image folderImage = null;
	private Image fileImage = null;

	public FileListProvider() {
		System.out.println("okokok");
		folderImage = new Image(Display.getCurrent(), getClass()
				.getClassLoader().getResourceAsStream("fldr_obj.gif"));
		fileImage = new Image(Display.getCurrent(), getClass().getClassLoader()
				.getResourceAsStream("file_obj.gif"));
	}

	public List<?> getChildren(Object parent) {
		if (parent instanceof File && ((File) parent).isDirectory()
				&& ((File) parent).listFiles() != null)
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

	public Image getImage(Object element, int columnIndex) {
		if (columnIndex == 0 && element instanceof File)
			if (((File) element).isDirectory())
				return folderImage;
			else
				return fileImage;
		return null;
	}

	public void dispose() {
		if (folderImage != null && !folderImage.isDisposed()) {
			System.out.println("disposing folderImage");
			folderImage.dispose();
		}
		if (fileImage != null && !fileImage.isDisposed()) {
			System.out.println("disposing fileImage");
			fileImage.dispose();
		}
	}
}
