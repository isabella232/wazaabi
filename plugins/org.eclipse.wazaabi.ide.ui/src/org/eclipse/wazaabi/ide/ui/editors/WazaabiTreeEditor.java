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

package org.eclipse.wazaabi.ide.ui.editors;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.EventObject;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.command.CommandStackListener;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.domain.IEditingDomainProvider;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.ContextMenuProvider;
import org.eclipse.gef.EditDomain;
import org.eclipse.gef.commands.CommandStack;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.ui.actions.ActionRegistry;
import org.eclipse.gef.ui.actions.DeleteAction;
import org.eclipse.gef.ui.actions.PrintAction;
import org.eclipse.gef.ui.actions.RedoAction;
import org.eclipse.gef.ui.actions.SaveAction;
import org.eclipse.gef.ui.actions.SelectAllAction;
import org.eclipse.gef.ui.actions.UndoAction;
import org.eclipse.gef.ui.actions.UpdateAction;
import org.eclipse.gef.ui.palette.PaletteViewer;
import org.eclipse.gef.ui.parts.SelectionSynchronizer;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.eclipse.ui.views.properties.IPropertySheetPage;
import org.eclipse.wazaabi.engine.edp.EDPSingletons;
import org.eclipse.wazaabi.ide.ui.editors.actions.HideLayoutInfoAction;
import org.eclipse.wazaabi.ide.ui.editors.actions.InsertECoreElementAction;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ExtendedTreeViewer;
import org.eclipse.wazaabi.ide.ui.editparts.RootTreeEditPartWithOneChild;
import org.eclipse.wazaabi.ide.ui.editparts.TreePartFactory;
import org.eclipse.wazaabi.ide.ui.outline.OutlinePage;
import org.eclipse.wazaabi.ide.ui.propertysheets.eventhandlers.AbstractStyleRuleAction;
import org.eclipse.wazaabi.mm.core.widgets.AbstractComponent;
import org.eclipse.wazaabi.mm.core.widgets.CoreWidgetsFactory;
import org.eclipse.wazaabi.ui.runtime.parts.TabbedPropertySheetPage;

public class WazaabiTreeEditor extends EditorPart implements
		IEditingDomainProvider, ISelectionProvider, IMenuListener,
		org.eclipse.gef.commands.CommandStackListener, ISelectionListener {

	private static final int PALETTE_SIZE = 125;

	private TransactionalEditingDomain editingDomain;
	private EditDomain editDomain;
	private ExtendedTreeViewer viewer;
	private PaletteViewer paletteViewer;
	private PaletteRoot root;
	private OutlinePage outlinePage;
	private ActionRegistry actionRegistry;
	private List<String> stackActions = new ArrayList<String>();
	private List<String> selectionActions = new ArrayList<String>();
	private List<String> propertyActions = new ArrayList<String>();
	private SelectionSynchronizer synchronizer;

	public WazaabiTreeEditor() {
		super();
		setEditDomain(new EditDomain());
		initializeEditingDomain();
	}

	@Override
	public void doSave(IProgressMonitor monitor) {
		// Save only resources that have actually changed.
		//
		final Map<Object, Object> saveOptions = new HashMap<Object, Object>();
		saveOptions.put(Resource.OPTION_SAVE_ONLY_IF_CHANGED,
				Resource.OPTION_SAVE_ONLY_IF_CHANGED_MEMORY_BUFFER);

		// Do the work within an operation because this is a long running
		// activity that modifies the workbench.
		//
		WorkspaceModifyOperation operation = new WorkspaceModifyOperation() {
			// This is the method that gets invoked when the operation runs.
			//
			@Override
			public void execute(IProgressMonitor monitor) {
				// Save the resources to the file system.
				//
				boolean first = true;
				for (Resource resource : editingDomain.getResourceSet()
						.getResources()) {
					if ((first || !resource.getContents().isEmpty() || isPersisted(resource))
							&& !editingDomain.isReadOnly(resource)) {
						try {
							// long timeStamp = resource.getTimeStamp();
							resource.save(saveOptions);
							// if (resource.getTimeStamp() != timeStamp) {
							// savedResources.add(resource);
							// }
						} catch (Exception exception) {
							// resourceToDiagnosticMap
							// .put(resource,
							// analyzeResourceProblems(resource,
							// exception));
						}
						first = false;
					}
				}
			}
		};

		// updateProblemIndication = false;
		try {
			// This runs the options, and shows progress.
			//
			new ProgressMonitorDialog(getSite().getShell()).run(true, false,
					operation);

			// Refresh the necessary state.
			//
			((BasicCommandStack) editingDomain.getCommandStack()).saveIsDone();
			firePropertyChange(IEditorPart.PROP_DIRTY);
		} catch (Exception exception) {
			// Something went wrong that shouldn't.
			//
			// MyEditorPlugin.INSTANCE.log(exception);
		}
		// updateProblemIndication = true;
		// updateProblemIndication();
	}

	protected boolean isPersisted(Resource resource) {
		boolean result = false;
		try {
			InputStream stream = editingDomain.getResourceSet()
					.getURIConverter().createInputStream(resource.getURI());
			if (stream != null) {
				result = true;
				stream.close();
			}
		} catch (IOException e) {
			// Ignore
		}
		return result;
	}

	@Override
	public void doSaveAs() {
		SaveAsDialog saveAsDialog = new SaveAsDialog(getSite().getShell());
		saveAsDialog.open();
		IPath path = saveAsDialog.getResult();
		if (path != null) {
			IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
			if (file != null) {
				doSaveAs(URI.createPlatformResourceURI(file.getFullPath()
						.toString(), true), new FileEditorInput(file));
			}
		}
	}

	protected void doSaveAs(URI uri, IEditorInput editorInput) {
		(editingDomain.getResourceSet().getResources().get(0)).setURI(uri);
		setInputWithNotify(editorInput);
		setPartName(editorInput.getName());
		IProgressMonitor progressMonitor = new NullProgressMonitor();
		doSave(progressMonitor);
	}

	@Override
	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		setSite(site);
		setInputWithNotify(input);
		getCommandStack().addCommandStackListener(this);
		setPartName(input.getName());
		site.setSelectionProvider(this);
		getSite().getWorkbenchWindow().getSelectionService()
				.addSelectionListener(this);
		// site.getPage().addPartListener(partListener);
		// ResourcesPlugin.getWorkspace().addResourceChangeListener(resourceChangeListener,
		// IResourceChangeEvent.POST_CHANGE);
		initializeActionRegistry();

	}

	protected void initializeActionRegistry() {
		createActions();
		updateActions(propertyActions);
		updateActions(stackActions);
	}

	protected void createActions() {
		ActionRegistry registry = getActionRegistry();
		IAction action;

		action = new HideLayoutInfoAction(this, Action.AS_CHECK_BOX);
		registry.registerAction(action);

		action = new UndoAction(this);
		registry.registerAction(action);
		getStackActions().add(action.getId());

		action = new RedoAction(this);
		registry.registerAction(action);
		getStackActions().add(action.getId());

		action = new SelectAllAction(this);
		registry.registerAction(action);

		action = new DeleteAction((IWorkbenchPart) this);
		registry.registerAction(action);
		getSelectionActions().add(action.getId());

		action = new SaveAction(this);
		registry.registerAction(action);
		getPropertyActions().add(action.getId());

		action = new InsertECoreElementAction((IWorkbenchPart) this);
		registry.registerAction(action);
		getSelectionActions().add(action.getId());

		registry.registerAction(new PrintAction(this));
	}

	protected List<String> getStackActions() {
		return stackActions;
	}

	protected List<String> getSelectionActions() {
		return selectionActions;
	}

	protected List<String> getPropertyActions() {
		return propertyActions;
	}

	@Override
	public boolean isDirty() {
		return ((BasicCommandStack) getEditingDomain().getCommandStack())
				.isSaveNeeded();
	}

	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	@Override
	public void createPartControl(Composite parent) {
		createModel();
		Splitter splitter = new Splitter(parent, SWT.HORIZONTAL);
		createPaletteViewer(splitter);
		viewer = new ExtendedTreeViewer();
		initializeViewer(splitter);

		// TODO : very very poor way to manage blank files.

		if (editingDomain.getResourceSet().getResources().get(0).getContents()
				.isEmpty())
			try {
				getEditingDomain()
						.getCommandStack()
						.execute(
								new RecordingCommand(
										(TransactionalEditingDomain) getEditingDomain()) {
									protected void doExecute() {
										editingDomain
												.getResourceSet()
												.getResources()
												.get(0)
												.getContents()
												.add(CoreWidgetsFactory.eINSTANCE
														.createContainer());
									}
								});

			} catch (Throwable t) {
				t.printStackTrace();
			}
		// TODO : manage multiple resources
		getViewer().setContents(
				editingDomain.getResourceSet().getResources().get(0)
						.getContents().get(0));

		splitter.maintainSize(getPaletteViewer().getControl());
		splitter.setFixedSize(getInitialPaletteSize());
		splitter.addFixedSizeChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				handlePaletteResized(((Splitter) evt.getSource())
						.getFixedSize());
			}
		});

		// CONFIGURE VIEWER
		ContextMenuProvider provider = new WazaabiTreeEditorContextMenuProvider(
				viewer, getActionRegistry());
		viewer.setContextMenu(provider);
		getSite().registerContextMenu(this.getClass() + ".contextmenu", //$NON-NLS-1$
				provider, viewer);

		hookGraphicalViewer();

		// viewer.setKeyHandler(new GraphicalViewerKeyHandler(viewer)
		// .setParent(getCommonKeyHandler()));

	}

	protected void initializeViewer(Composite parent) {
		getViewer().setRootEditPart(new RootTreeEditPartWithOneChild());
		getViewer().createControl(parent);
		getViewer().setEditDomain(editDomain);
		getViewer().setEditPartFactory(new TreePartFactory());
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	public void createModel() {

		IFile file = (IFile) getEditorInput().getAdapter(IFile.class);
		if (file != null) {
			URI resourceURI = URI.createPlatformResourceURI(file.getFullPath()
					.toString(), true);

			// Exception exception = null;
			// Resource resource = null;
			try {
				// Load the resource through the editing domain.
				//
				/* resource = */editingDomain.getResourceSet().getResource(
						resourceURI, true);
			} catch (Exception e) {
				// exception = e;
				/* resource = */editingDomain.getResourceSet().getResource(
						resourceURI, false);
				e.printStackTrace();
			}
		}

		// Diagnostic diagnostic = analyzeResourceProblems(resource, exception);
		// if (diagnostic.getSeverity() != Diagnostic.OK) {
		// resourceToDiagnosticMap.put(resource,
		// analyzeResourceProblems(resource, exception));
		// }
		// editingDomain.getResourceSet().eAdapters()
		// .add(problemIndicationAdapter);
	}

	public void menuAboutToShow(IMenuManager manager) {
		// TODO Auto-generated method stub

	}

	public void addSelectionChangedListener(ISelectionChangedListener listener) {
		// TODO Auto-generated method stub

	}

	public ISelection getSelection() {
		// TODO Auto-generated method stub
		return null;
	}

	public void removeSelectionChangedListener(
			ISelectionChangedListener listener) {
		// TODO Auto-generated method stub

	}

	public void setSelection(ISelection selection) {
		// TODO Auto-generated method stub

	}

	public EditingDomain getEditingDomain() {
		return editingDomain;
	}

	protected void initializeEditingDomain() {

		editingDomain = TransactionalEditingDomain.Factory.INSTANCE
				.createEditingDomain();

		// TODO : is it not a better way for this ?
		editingDomain.getResourceSet().getResourceFactoryRegistry()
				.getExtensionToFactoryMap()
				.put("ui", new XMIResourceFactoryImpl());

		editingDomain.getCommandStack().addCommandStackListener(
				new CommandStackListener() {
					public void commandStackChanged(final EventObject event) {
						getSite().getShell().getDisplay()
								.asyncExec(new Runnable() {
									public void run() {
										firePropertyChange(IEditorPart.PROP_DIRTY);

										// we do not call getOutlinePage()
										// because we don't want to instantiate
										// a new outline page at this point
										if (WazaabiTreeEditor.this.outlinePage != null)
											WazaabiTreeEditor.this.outlinePage
													.refreshSelection();

										// if (propertySheetPage != null
										// && !propertySheetPage.getControl()
										// .isDisposed()) {
										// propertySheetPage.refresh();
										// }
									}
								});
					}
				});

	}

	public ExtendedTreeViewer getViewer() {
		return viewer;
	}

	protected void createPaletteViewer(Composite parent) {
		PaletteViewer viewer = new PaletteViewer();
		setPaletteViewer(viewer);
		viewer.createControl(parent);
		configurePaletteViewer();
		hookPaletteViewer();
		initializePaletteViewer();
	}

	protected void setPaletteViewer(PaletteViewer paletteViewer) {
		this.paletteViewer = paletteViewer;
	}

	protected void configurePaletteViewer() {
	}

	protected void hookPaletteViewer() {
		getEditDomain().setPaletteViewer(paletteViewer);
	}

	protected void initializePaletteViewer() {
	}

	public EditDomain getEditDomain() {
		return editDomain;
	}

	protected PaletteViewer getPaletteViewer() {
		return paletteViewer;
	}

	protected int getInitialPaletteSize() {
		return PALETTE_SIZE;
	}

	protected void handlePaletteResized(int newSize) {
	}

	protected void setEditDomain(EditDomain ed) {
		this.editDomain = ed;
		getEditDomain().setPaletteRoot(getPaletteRoot());
	}

	protected PaletteRoot getPaletteRoot() {
		if (root == null) {
			root = new PaletteRoot();
			new PaletteFactory().createChildren(root);
		}
		return root;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Object getAdapter(Class type) {
		if (type.equals(IContentOutlinePage.class))
			return getOutlinePage();
		if (type.equals(ActionRegistry.class))
			return getActionRegistry();
		if (type.equals(CommandStack.class))
			return getCommandStack();
		if (type.equals(IPropertySheetPage.class))
			return getPropertySheetPage();

		return super.getAdapter(type);
	}

	protected ActionRegistry getActionRegistry() {
		if (actionRegistry == null)
			actionRegistry = new ActionRegistry();
		return actionRegistry;
	}

	public void commandStackChanged(EventObject event) {
		updateActions(stackActions);
	}

	protected void updateActions(List<String> actionIds) {
		ActionRegistry registry = getActionRegistry();
		Iterator<String> iter = actionIds.iterator();
		while (iter.hasNext()) {
			IAction action = registry.getAction(iter.next());
			if (action instanceof UpdateAction)
				((UpdateAction) action).update();
		}
	}

	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
		// If not the active editor, ignore selection changed.
		if (this.equals(getSite().getPage().getActiveEditor())) {
			updateActions(selectionActions);
			if (getOutlinePage() != null) {
				getOutlinePage().selectionChanged(
						new SelectionChangedEvent(this, selection));
			}
		}
	}

	protected void firePropertyChange(int property) {
		super.firePropertyChange(property);
		updateActions(propertyActions);
	}

	public void dispose() {

		// if (getActionBarContributor().getActiveEditor() == this) {
		// getActionBarContributor().setActiveEditor(null);
		// }

		if (outlinePage != null) {
			outlinePage.dispose();
		}

		getCommandStack().removeCommandStackListener(this);
		getSite().getWorkbenchWindow().getSelectionService()
				.removeSelectionListener(this);
		getEditDomain().setActiveTool(null);
		getActionRegistry().dispose();
		super.dispose();
	}

	protected CommandStack getCommandStack() {
		return getEditDomain().getCommandStack();
	}

	protected SelectionSynchronizer getSelectionSynchronizer() {
		if (synchronizer == null)
			synchronizer = new SelectionSynchronizer();
		return synchronizer;
	}

	protected void hookGraphicalViewer() {
		getSelectionSynchronizer().addViewer(getViewer());
		getSite().setSelectionProvider(getViewer());
	}

	protected IPropertySheetPage propertySheetPage;

	public IPropertySheetPage getPropertySheetPage() {
		if (propertySheetPage == null) {
			propertySheetPage = new TabbedPropertySheetPage(
					"platform:/plugin/org.eclipse.wazaabi.ide.ui/UIs/propertypage.ui") {

				@Override
				protected void doSetInput(AbstractComponent component,
						Object input) {
					component.set(AbstractStyleRuleAction.EDIT_DOMAIN_KEY,
							WazaabiTreeEditor.this.getEditDomain());
					component
							.set(AbstractStyleRuleAction.TRANSATIONAL_EDITING_DOMAIN_KEY,
									WazaabiTreeEditor.this.getEditingDomain());
					super.doSetInput(component, input);
				}

				@Override
				protected void createViewer(Composite parent) {
					super.createViewer(parent);
					getViewer()
							.setPointersEvaluator(
									EDPSingletons
											.getRegistry()
											.getPointersEvaluator(
													"org.eclipse.wazaabi.engine.locationpaths.emftransactions.PointersEvaluatorImpl")); //$NON-NLS-1$
				}

			};

		}
		return propertySheetPage;
	}

	protected OutlinePage getOutlinePage() {
		if (this.outlinePage == null)
			this.outlinePage = new OutlinePage(getViewer());
		return this.outlinePage;
	}
}
