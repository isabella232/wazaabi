/*******************************************************************************
 * Copyright (c) 2012 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.engine.swt.snippets.providers;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.wazaabi.engine.core.CoreUtils;
import org.eclipse.wazaabi.mm.core.styles.ColorRule;
import org.eclipse.wazaabi.mm.core.styles.FontRule;

public class FileListProvider {

	private Image folderImage = null;
	private Image fileImage = null;

	public FileListProvider() {
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

	public ColorRule getBackgroundColor(Object element, int index) {
		if (element instanceof File && ((File) element).isDirectory()
				&& index == 1)
			return CoreUtils.createColorRule(100, 100, 100);
		return null;
	}

	public ColorRule getForegroundColor(Object element, int index) {
		if (element instanceof File && ((File) element).isDirectory()
				&& index == 0)
			return CoreUtils.createColorRule(100, 0, 0);
		return null;
	}

	public FontRule getFont(Object element, int index) {
		if (element instanceof File && ((File) element).isDirectory()
				&& index == 2) {
			return CoreUtils.createFontRule(null, 0, true, true);
		}
		return null;

	}
}
