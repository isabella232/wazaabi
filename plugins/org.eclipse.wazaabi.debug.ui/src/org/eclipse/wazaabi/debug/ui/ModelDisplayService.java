/*******************************************************************************
 * Copyright (c) 2013 Olivier Moises
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   Olivier Moises- initial API and implementation
 *******************************************************************************/

package org.eclipse.wazaabi.debug.ui;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wazaabi.engine.swt.viewers.SWTControlViewer;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ModelDisplayService {

	private static class ViewerThread extends Thread {

		private Display display = null;
		private SWTControlViewer viewer = null;
		private Object contents = null;

		public Object getContents() {
			return contents;
		}

		@Override
		public void interrupt() {
			super.interrupt();
			if (display != null && !display.isDisposed()) {
				display.syncExec(new Runnable() {

					public void run() {
						display.dispose();
						display = null;
						viewer = null;
					}
				});
			}
			logger.debug("end of ViewerThread interrupt method");
		}

		@Override
		public void run() {
			display = new Display();
			Shell mainShell = new Shell(display, SWT.SHELL_TRIM);
			mainShell.setLayout(new FillLayout());
			mainShell.setSize(300, 300);

			viewer = new SWTControlViewer(mainShell);
			if (getContents() != null)
				viewer.setContents(getContents());
			mainShell.open();

			while (!mainShell.isDisposed()) {
				if (!display.readAndDispatch())
					display.sleep();
			}
			if (display != null && !display.isDisposed()) {
				display.dispose();
				display = null;
				viewer = null;
			}
			logger.debug("End of ViewerThread run method");
		}

		public void setContents(final Object contents) {
			this.contents = contents;
			// if the viewer is displayed, let's update its content
			if (viewer != null && display != null && !display.isDisposed())
				display.asyncExec(new Runnable() {

					public void run() {
						logger.debug("set viewer contents {}", getContents());
						viewer.setContents(getContents());
						if (viewer.getControl() != null
								&& !viewer.getControl().isDisposed()
								&& viewer.getControl().getParent() instanceof Composite)
							((Composite) viewer.getControl().getParent())
									.layout(true, true);
					}
				});
		}
	}

	private final static Logger logger = LoggerFactory
			.getLogger(ModelDisplayService.class);
	private boolean isActive = false;

	private final String modelPath;
	private final int port;

	private ViewerThread viewerThread = null;
	private Service service = null;

	public ModelDisplayService(String modelPath, int port) {
		this.modelPath = modelPath;
		this.port = port;
	}

	public int getPort() {
		return port;
	}

	public void activate() {
		if (isActive)
			return;
		openViewer(parseModel());
		isActive = true;
		service = new Service(this, getPort());
		service.start();
		logger.debug("Service Activated");
	}

	public void processCommand(String command) {
		if ("reload".equals(command)) {
			AbstractComponent model = parseModel();
			if (model != null) {
				setContents(model);
			}
		}

	}

	protected AbstractComponent parseModel() {
		XMIResource res = new XMIResourceImpl();
		FileInputStream fIn = null;
		try {
			fIn = new FileInputStream(getModelPath());
			res.load(fIn, null);
		} catch (FileNotFoundException e) {
			logger.error("Unable to find {}", getModelPath());
		} catch (IOException e) {
			logger.error("Unable to parse {}", getModelPath());
		} finally {
			if (fIn != null)
				try {
					fIn.close();
				} catch (IOException e) {
					logger.error("Unable to close {}", getModelPath());
				}
		}
		return !res.getContents().isEmpty()
				&& res.getContents().get(0) instanceof AbstractComponent ? ((AbstractComponent) res
				.getContents().get(0)) : null;
	}

	protected void closeViewer() {
		if (viewerThread != null && viewerThread.isAlive())
			viewerThread.interrupt();
		viewerThread = null;
	}

	public void deactivate() {
		if (!isActive)
			return;
		if (service != null) {
			service.interrupt();
			service = null;
		}
		isActive = false;
		logger.debug("Service deactivated");
	};

	public String getModelPath() {
		return modelPath;
	}

	protected boolean isActive() {
		return isActive;
	}

	protected void openViewer(Object contents) {
		if (viewerThread != null && viewerThread.isAlive())
			viewerThread.interrupt();
		viewerThread = new ViewerThread();
		if (contents != null)
			viewerThread.setContents(contents);
		viewerThread.start();
	}

	protected void setContents(Object contents) {
		if (!viewerThread.isInterrupted())
			viewerThread.setContents(contents);
	}
}
