package org.eclipse.wazaabi.engine.swt.snippets.providers;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FilenamesMoreThan3CharFilter {

	private static final Logger logger = LoggerFactory
			.getLogger(FilenamesMoreThan3CharFilter.class);

	public FilenamesMoreThan3CharFilter() {
		logger.debug("Creation of the Filter");
	}

	public boolean select(Object parentElement, Object element) {
		if (element instanceof File)
			if (((File) element).getName().length() > 3)
				return true;
		return false;
	}
}
