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

package org.eclipse.wazaabi.ide.ui.editors.actions.wizards;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceImpl;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.wazaabi.ide.ui.editors.TreeEditorMessages;
import org.eclipse.wazaabi.ide.ui.internal.Activator;

public class SelectECoreElementWizard extends Wizard {

	private static final String SELECT_ECORE_PAGE = "SelectECorePage"; //$NON-NLS-1$
	private static final String SELECT_ELEMENT_PAGE = "SelectElementPage"; //$NON-NLS-1$

	private EObject result = null;

	public static final String EPACKAGE_IMG_NAME = "EPackage.gif"; //$NON-NLS-1$
	public static final String CLASS_IMG_NAME = "EClass.gif"; //$NON-NLS-1$
	public static final String EATTRIBUTE_IMG_NAME = "EAttribute.gif"; //$NON-NLS-1$
	public static final String EREFERENCE_IMG_NAME = "EReference.gif"; //$NON-NLS-1$

	private class SelectECorePage extends WizardPage {
		private TableViewer tableViewer = null;

		protected SelectECorePage(String pageName) {
			super(pageName);
			setTitle(TreeEditorMessages.SelectECoreElementWizard_SelectECorePage_Title);
			setPageComplete(false);
		}

		public void createControl(Composite parent) {
			tableViewer = new TableViewer(parent, SWT.NONE);

			tableViewer
					.addSelectionChangedListener(new ISelectionChangedListener() {

						public void selectionChanged(SelectionChangedEvent event) {
							SelectECorePage.this.getContainer().updateButtons();
							if (!tableViewer.getSelection().isEmpty()) {
								((SelectElementPage) SelectECoreElementWizard.this
										.getPage(SELECT_ELEMENT_PAGE))
										.setInput(((IFile) ((IStructuredSelection) tableViewer
												.getSelection())
												.getFirstElement()));
							}
						}
					});
			tableViewer.getControl().addMouseListener(new MouseAdapter() {

				public void mouseDoubleClick(MouseEvent e) {
					IWizardPage page = getNextPage();
					if (page == null) {
						// something must have happened getting the next page
						return;
					}
					if (SelectECoreElementWizard.this.getContainer() instanceof WizardDialog)
						((WizardDialog) SelectECoreElementWizard.this
								.getContainer()).showPage(getNextPage());
				}
			});
			tableViewer.setContentProvider(new IStructuredContentProvider() {

				public void inputChanged(Viewer viewer, Object oldInput,
						Object newInput) {
				}

				public void dispose() {
				}

				public Object[] getElements(Object inputElement) {
					if (inputElement instanceof IWorkspaceRoot) {
						final List<IFile> ecoreFiles = new ArrayList<IFile>();
						try {
							((IWorkspaceRoot) inputElement)
									.accept(new IResourceVisitor() {

										public boolean visit(IResource resource)
												throws CoreException {
											if (resource instanceof IFile
													&& ((IFile) resource)
															.getName()
															.endsWith(".ecore")) //$NON-NLS-1$
												ecoreFiles
														.add((IFile) resource);
											return true;
										}
									});
						} catch (CoreException e) {
							e.printStackTrace();
						}
						return ecoreFiles.toArray();
					}
					return new Object[] {};
				}
			});
			tableViewer.setLabelProvider(new ITableLabelProvider() {

				public void removeListener(ILabelProviderListener listener) {
				}

				public boolean isLabelProperty(Object element, String property) {
					return false;
				}

				public void dispose() {
				}

				public void addListener(ILabelProviderListener listener) {
				}

				public String getColumnText(Object element, int columnIndex) {
					if (element instanceof IResource)
						return ((IResource) element).getName()
								+ "  ["
								+ ((IResource) element).getLocation()
										.makeRelativeTo(
												ResourcesPlugin.getWorkspace()
														.getRoot()
														.getLocation()) + "]";
					return null;
				}

				public Image getColumnImage(Object element, int columnIndex) {
					return null;
				}
			});

			tableViewer.getControl().getDisplay().asyncExec(new Runnable() {

				public void run() {
					tableViewer.setInput(ResourcesPlugin.getWorkspace()
							.getRoot());
				}
			});
			setControl(tableViewer.getControl());
		}

		@Override
		public boolean isPageComplete() {
			return !tableViewer.getSelection().isEmpty();
		}

	}

	private class SelectElementPage extends WizardPage {

		private TreeViewer ecoreTreeViewer = null;

		protected SelectElementPage(String pageName) {
			super(pageName);
			setTitle(TreeEditorMessages.SelectECoreElementWizard_SelectElementPage_Title);
		}

		@Override
		public boolean isPageComplete() {
			EObject selectedElement = getSelectedElement();
			return selectedElement instanceof EClass
					|| selectedElement instanceof EStructuralFeature;
		}

		public EObject getSelectedElement() {
			if (!ecoreTreeViewer.getSelection().isEmpty())
				return (EObject) ((IStructuredSelection) ecoreTreeViewer
						.getSelection()).getFirstElement();
			return null;
		}

		public void createControl(final Composite parent) {
			ecoreTreeViewer = new TreeViewer(parent, SWT.NONE);

			ecoreTreeViewer
					.addSelectionChangedListener(new ISelectionChangedListener() {

						public void selectionChanged(SelectionChangedEvent event) {
							SelectElementPage.this.getContainer()
									.updateButtons();
						}
					});
			ecoreTreeViewer.getControl().addMouseListener(new MouseAdapter() {

				public void mouseDoubleClick(MouseEvent e) {
					if (SelectECoreElementWizard.this.getContainer() instanceof WizardDialog)
						if (isPageComplete() && performFinish())
							((WizardDialog) SelectECoreElementWizard.this
									.getContainer()).close();
				}
			});

			ecoreTreeViewer.setContentProvider(new ITreeContentProvider() {

				public void inputChanged(Viewer viewer, Object oldInput,
						Object newInput) {
				}

				public void dispose() {
				}

				public boolean hasChildren(Object element) {
					if (element instanceof EPackage)
						return !((EPackage) element).getEClassifiers()
								.isEmpty();
					else if (element instanceof EClass)
						return !((EClass) element).getEStructuralFeatures()
								.isEmpty();
					return false;
				}

				public Object getParent(Object element) {
					return null;
				}

				public Object[] getElements(Object inputElement) {
					if (inputElement instanceof IFile) {
						try {
							XMIResource resource = new XMIResourceImpl();
							resource.load(((IFile) inputElement).getContents(),
									null);
							return resource.getContents().toArray();
						} catch (IOException e) {
							e.printStackTrace();
						} catch (CoreException e) {
							e.printStackTrace();
						}
					}
					return new Object[] {};
				}

				public Object[] getChildren(Object parentElement) {
					if (parentElement instanceof EPackage)
						return ((EPackage) parentElement).eContents().toArray();
					else if (parentElement instanceof EClass)
						return ((EClass) parentElement)
								.getEStructuralFeatures().toArray();
					return new Object[] {};
				}
			});
			ecoreTreeViewer.setLabelProvider(new ILabelProvider() {

				private Image eClassImage = Activator.getDefault()
						.getImageRegistry().getDescriptor(CLASS_IMG_NAME)
						.createImage(true);
				private Image eReferenceImage = Activator.getDefault()
						.getImageRegistry().getDescriptor(EREFERENCE_IMG_NAME)
						.createImage(true);
				private Image eAttributeImage = Activator.getDefault()
						.getImageRegistry().getDescriptor(EATTRIBUTE_IMG_NAME)
						.createImage(true);
				private Image EPackageImage = Activator.getDefault()
						.getImageRegistry().getDescriptor(EPACKAGE_IMG_NAME)
						.createImage(true);

				public void removeListener(ILabelProviderListener listener) {

				}

				public boolean isLabelProperty(Object element, String property) {
					return false;
				}

				public void dispose() {
					if (eClassImage != null && !eClassImage.isDisposed())
						eClassImage.dispose();
					if (eClassImage != null && !eClassImage.isDisposed())
						eReferenceImage.dispose();
					if (eClassImage != null && !eClassImage.isDisposed())
						eAttributeImage.dispose();
					if (eClassImage != null && !eClassImage.isDisposed())
						EPackageImage.dispose();

				}

				public void addListener(ILabelProviderListener listener) {
				}

				public String getText(Object element) {
					if (element instanceof ENamedElement)
						return ((ENamedElement) element).getName();
					if (element != null)
						return element.toString();
					else
						return ""; //$NON-NLS-1$
				}

				public Image getImage(Object element) {
					if (element instanceof EClass)
						return eClassImage;
					else if (element instanceof EPackage)
						return EPackageImage;
					else if (element instanceof EAttribute)
						return eAttributeImage;
					else if (element instanceof EReference)
						return eReferenceImage;
					return null;
				}
			});
			setControl(ecoreTreeViewer.getControl());
		}

		public void setInput(final IFile file) {

			if (file.exists()) {
				SelectElementPage.this.getControl().getDisplay()
						.asyncExec(new Runnable() {
							public void run() {
								getEcoreTreeViewer().setInput(file);
							}
						});
			}
		}

		protected TreeViewer getEcoreTreeViewer() {
			return ecoreTreeViewer;
		}
	}

	public void addPages() {
		addPage(new SelectECorePage(SELECT_ECORE_PAGE));
		addPage(new SelectElementPage(SELECT_ELEMENT_PAGE));
	}

	public EObject getResult() {
		return result;
	}

	protected void setResult(EObject newResult) {
		this.result = newResult;
	}

	@Override
	public boolean performFinish() {
		setResult(((SelectElementPage) SelectECoreElementWizard.this
				.getPage(SELECT_ELEMENT_PAGE)).getSelectedElement());
		return getResult() != null;
	}

};
