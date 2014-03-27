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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.domain.IEditingDomainProvider;
import org.eclipse.emf.edit.ui.util.EditUIUtil;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gef.ContextMenuProvider;
import org.eclipse.gef.EditDomain;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.commands.CommandStack;
import org.eclipse.gef.commands.CommandStackEvent;
import org.eclipse.gef.commands.CommandStackEventListener;
import org.eclipse.gef.commands.CompoundCommand;
import org.eclipse.gef.editparts.RootTreeEditPart;
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
import org.eclipse.wazaabi.ide.mapping.rules.MappingRuleManager;
import org.eclipse.wazaabi.ide.propertysheets.TargetChangeListener;
import org.eclipse.wazaabi.ide.ui.PaletteContribution;
import org.eclipse.wazaabi.ide.ui.editors.actions.ChangeMappingAction;
import org.eclipse.wazaabi.ide.ui.editors.actions.InsertECoreElementAction;
import org.eclipse.wazaabi.ide.ui.editors.actions.RunInSeparateWindow;
import org.eclipse.wazaabi.ide.ui.editors.viewer.ExtendedTreeViewer;
import org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules.OnCollectionMappingRules;
import org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules.OnContainerMappingRules;
import org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules.OnJDTElementsMappingRules;
import org.eclipse.wazaabi.ide.ui.editors.viewer.bindingrules.OnTextComponentMapping;
import org.eclipse.wazaabi.ide.ui.editparts.TreePartFactory;
import org.eclipse.wazaabi.ide.ui.editparts.commands.binding.InsertNewBindingCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.binding.RemoveBindingCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers.InsertNewEventHandlerCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers.ModifyEventHandlerCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.eventhandlers.RemoveEventHandlerCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.events.InsertNewEventCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.events.ModifyEventCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.events.RemoveEventCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.parameters.InsertNewParameterCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.parameters.ModifyParameterCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.parameters.RemoveParameterCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules.InsertNewStyleRuleCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules.ModifyStyleRuleCommand;
import org.eclipse.wazaabi.ide.ui.editparts.commands.stylerules.RemoveStyleRuleCommand;
import org.eclipse.wazaabi.ide.ui.outline.AbstractOutlinePage;
import org.eclipse.wazaabi.ide.ui.outline.OutlinePage;
import org.eclipse.wazaabi.ide.ui.palette.ComponentsDrawerPaletteContribution;
import org.eclipse.wazaabi.ide.ui.palette.ControlGroupPaletteContribution;
import org.eclipse.wazaabi.ide.ui.propertysheetpage.PropertySheetPage;
import org.eclipse.wazaabi.mm.core.styles.StyleRule;
import org.eclipse.wazaabi.mm.core.styles.StyledElement;
import org.eclipse.wazaabi.mm.edp.EventDispatcher;
import org.eclipse.wazaabi.mm.edp.events.Event;
import org.eclipse.wazaabi.mm.edp.handlers.Binding;
import org.eclipse.wazaabi.mm.edp.handlers.EDPHandlersPackage;
import org.eclipse.wazaabi.mm.edp.handlers.EventHandler;
import org.eclipse.wazaabi.mm.edp.handlers.Parameter;
import org.eclipse.wazaabi.mm.edp.handlers.Parameterized;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WazaabiTreeEditor extends EditorPart implements
		IEditingDomainProvider, ISelectionProvider, IMenuListener,
		CommandStackEventListener, ISelectionListener, TargetChangeListener {

	private static final int PALETTE_SIZE = 125;
	final static Logger logger = LoggerFactory
			.getLogger(WazaabiTreeEditor.class);

	private TransactionalEditingDomain editingDomain;
	private EditDomain editDomain;
	private ExtendedTreeViewer viewer;
	private PaletteViewer paletteViewer;
	private PaletteRoot root;
	private AbstractOutlinePage outlinePage;
	private ActionRegistry actionRegistry;
	private List<String> stackActions = new ArrayList<String>();
	private List<String> selectionActions = new ArrayList<String>();
	private List<String> propertyActions = new ArrayList<String>();
	private SelectionSynchronizer synchronizer;
	private MappingRuleManager mappingRuleManager = null;

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
							exception.printStackTrace();
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
			getCommandStack().markSaveLocation();
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
		getCommandStack().addCommandStackEventListener(this);
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

		action = new ChangeMappingAction((IWorkbenchPart) this);
		registry.registerAction(action);
		getSelectionActions().add(action.getId());

		action = new RunInSeparateWindow(this);
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

	/**
	 * This is for implementing {@link IEditorPart} and simply tests the command
	 * stack. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public boolean isDirty() {
		return getCommandStack().isDirty();
	}

	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	@Override
	public void createPartControl(Composite parent) {
		Resource resource = createOrGetResource();

		Composite splitter = createEditorSplitter(parent);
		createPaletteViewer(splitter);
		initializeEditorSplitter(splitter);

		viewer = new ExtendedTreeViewer();
		viewer.setMappingRuleManager(getMappingRuleManager());
		initializeViewer(splitter);

		getViewer().setContents(resource);

		registContextMenu();

		hookGraphicalViewer();

		// viewer.setKeyHandler(new GraphicalViewerKeyHandler(viewer)
		// .setParent(getCommonKeyHandler()));

	}

	/**
	 * 
	 * create splitter for the editor.
	 * 
	 * @param parent
	 * @return
	 */
	protected Composite createEditorSplitter(Composite parent) {
		return new Splitter(parent, SWT.HORIZONTAL);
	}

	/**
	 * 
	 * create splitter for the editor.
	 * 
	 * @param parent
	 * @return
	 */
	protected void initializeEditorSplitter(Composite splitter) {
		if (splitter instanceof Splitter) {
			((Splitter) splitter).maintainSize(getPaletteViewer().getControl());
			((Splitter) splitter).setFixedSize(getInitialPaletteSize());
			((Splitter) splitter)
					.addFixedSizeChangeListener(new PropertyChangeListener() {
						public void propertyChange(PropertyChangeEvent evt) {
							handlePaletteResized(((Splitter) evt.getSource())
									.getFixedSize());
						}
					});
		}
	}

	/**
	 * 
	 * regist ContextMenu
	 */
	protected void registContextMenu() {
		// CONFIGURE VIEWER
		ContextMenuProvider provider = new WazaabiTreeEditorContextMenuProvider(
				viewer, getActionRegistry());
		viewer.setContextMenu(provider);
		getSite().registerContextMenu(this.getClass() + ".contextmenu", //$NON-NLS-1$
				provider, viewer);
	}

	protected void initializeViewer(Composite parent) {
		getViewer().setRootEditPart(new RootTreeEditPart());
		getViewer().createControl(parent);
		getViewer().setEditDomain(editDomain);
		getViewer().setEditPartFactory(new TreePartFactory());
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	public Resource createOrGetResource() {

		URI resourceURI = EditUIUtil.getURI(getEditorInput());
		editingDomain.setID(resourceURI.toString());
		TransactionalEditingDomain.Registry.INSTANCE.add(editingDomain.getID(),
				editingDomain);

		Resource resource = null;
		/*
		 * Exception exception = null; Resource resource = null;
		 */
		try {
			// Load the resource through the editing domain.
			//
			resource = editingDomain.getResourceSet().getResource(resourceURI,
					true);
		} catch (Exception e) {
			// exception = e;
			resource = editingDomain.getResourceSet().getResource(resourceURI,
					false);
			logger.error("{}\n{}", e.getMessage(), e.getCause());
		}

		// Diagnostic diagnostic = analyzeResourceProblems(resource, exception);
		// if (diagnostic.getSeverity() != Diagnostic.OK) {
		// resourceToDiagnosticMap.put(resource,
		// analyzeResourceProblems(resource, exception));
		// }
		// editingDomain.getResourceSet().eAdapters()
		// .add(problemIndicationAdapter);

		return resource;
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

		if (propertySheetPage != null
				&& !propertySheetPage.getControl().isDisposed()) {
			propertySheetPage.refresh();
		}

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
			root.setId(ContributionBasedPaletteFactory.PALETTE_ROOT_ID);
			ContributionBasedPaletteFactory paletteFactory = new ContributionBasedPaletteFactory();
			initializePalette(paletteFactory);
			paletteFactory.createChildren(root);
		}
		return root;
	}

	/**
	 * This methods set the content of the Palette with a PaletteContribution
	 * not provided by OSGI DS.
	 */
	protected void initializePalette(
			ContributionBasedPaletteFactory paletteFactory) {
		ContributionBasedPaletteFactory.addPaletteContributionsToContainer(
				root, new PaletteContribution[] {
						new ComponentsDrawerPaletteContribution(),
						new ControlGroupPaletteContribution() });

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

	public void stackChanged(CommandStackEvent event) {
		getSite().getShell().getDisplay().asyncExec(new Runnable() {
			public void run() {

				firePropertyChange(IEditorPart.PROP_DIRTY);

				// we do not call getOutlinePage()
				// because we don't want to instantiate
				// a new outline page at this point
				if (WazaabiTreeEditor.this.outlinePage != null)
					WazaabiTreeEditor.this.outlinePage.refreshSelection();

				if (propertySheetPage != null)
					propertySheetPage.refresh();
			}
		});

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

		getCommandStack().removeCommandStackEventListener(this);
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

	protected PropertySheetPage propertySheetPage;

	public PropertySheetPage getPropertySheetPage() {
		if (propertySheetPage == null) {
			propertySheetPage = new PropertySheetPage();
		}
		return propertySheetPage;
	}

	protected AbstractOutlinePage getOutlinePage() {
		if (this.outlinePage == null)
			this.outlinePage = new OutlinePage(this);
		return this.outlinePage;
	}

	protected void initializeMappingRuleManager() {
		mappingRuleManager
				.registerContainingInstance(new OnContainerMappingRules(
						mappingRuleManager));
		mappingRuleManager
				.registerContainingInstance(new OnTextComponentMapping());
		mappingRuleManager
				.registerContainingInstance(new OnJDTElementsMappingRules());
		mappingRuleManager
				.registerContainingInstance(new OnCollectionMappingRules());
	}

	public MappingRuleManager getMappingRuleManager() {
		if (mappingRuleManager == null) {
			mappingRuleManager = new MappingRuleManager();
			initializeMappingRuleManager();
		}
		return mappingRuleManager;
	}

	// TODO : improve the management of commands
	// By using inheritance, command selector (aka factory) etc...

	@Override
	public void targetAdded(EObject container, EObject target, int position) {
		Command cmd = null;
		if (container instanceof StyledElement && target instanceof StyleRule) {
			cmd = new InsertNewStyleRuleCommand();
			((InsertNewStyleRuleCommand) cmd)
					.setStyledElement((StyledElement) container);
			((InsertNewStyleRuleCommand) cmd).setIndex(position);
			((InsertNewStyleRuleCommand) cmd)
					.setNewStyleRule((StyleRule) target);
		} else if (container instanceof EventDispatcher
				&& target.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER) {
			cmd = new InsertNewEventHandlerCommand();
			((InsertNewEventHandlerCommand) cmd)
					.setEventDispatcher((EventDispatcher) container);
			((InsertNewEventHandlerCommand) cmd).setIndex(position);
			((InsertNewEventHandlerCommand) cmd)
					.setNewEventHandler((EventHandler) target);
		} else if (container instanceof EventHandler && target instanceof Event) {
			cmd = new InsertNewEventCommand();
			((InsertNewEventCommand) cmd)
					.setEventHandler((EventHandler) container);
			((InsertNewEventCommand) cmd).setIndex(position);
			((InsertNewEventCommand) cmd).setNewEvent((Event) target);
		} else if (container instanceof EventDispatcher
				&& target.eClass() == EDPHandlersPackage.Literals.BINDING) {
			cmd = new InsertNewBindingCommand();
			((InsertNewBindingCommand) cmd)
					.setEventDispatcher((EventDispatcher) container);
			((InsertNewBindingCommand) cmd).setIndex(position);
			((InsertNewBindingCommand) cmd).setNewBinding((Binding) target);
		} else if (container instanceof Parameterized
				&& target instanceof Parameter) {
			cmd = new InsertNewParameterCommand();
			((InsertNewParameterCommand) cmd)
					.setParameterized((Parameterized) container);
			((InsertNewParameterCommand) cmd).setIndex(position);
			((InsertNewParameterCommand) cmd)
					.setNewParameter((Parameter) target);
		}
		if (cmd != null && cmd.canExecute())
			getCommandStack().execute(cmd);
	}

	@Override
	public void targetModified(EObject target, EStructuralFeature feature,
			int position, Object oldValue, Object newValue) {
		Command cmd = null;
		if (target instanceof StyleRule) {
			cmd = new ModifyStyleRuleCommand();
			((ModifyStyleRuleCommand) cmd).setStyleRule((StyleRule) target);
			((ModifyStyleRuleCommand) cmd).setFeature(feature);
			((ModifyStyleRuleCommand) cmd).setIndex(position);
			((ModifyStyleRuleCommand) cmd).setNewValue(newValue);
		} else if (target.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER) {
			cmd = new ModifyEventHandlerCommand();
			((ModifyEventHandlerCommand) cmd)
					.setEventHandler((EventHandler) target);
			((ModifyEventHandlerCommand) cmd).setFeature(feature);
			((ModifyEventHandlerCommand) cmd).setIndex(position);
			((ModifyEventHandlerCommand) cmd).setNewValue(newValue);
		} else if (target instanceof Event) {
			cmd = new ModifyEventCommand();
			((ModifyEventCommand) cmd).setEvent((Event) target);
			((ModifyEventCommand) cmd).setFeature(feature);
			((ModifyEventCommand) cmd).setIndex(position);
			((ModifyEventCommand) cmd).setNewValue(newValue);
		} else if (target instanceof Parameter) {
			cmd = new ModifyParameterCommand();
			((ModifyParameterCommand) cmd).setParameter((Parameter) target);
			((ModifyParameterCommand) cmd).setFeature(feature);
			((ModifyParameterCommand) cmd).setIndex(position);
			((ModifyParameterCommand) cmd).setNewValue(newValue);
		}
		if (cmd != null && cmd.canExecute())
			getCommandStack().execute(cmd);
	}

	@Override
	public void targetMultipleModified(EObject target,
			List<EStructuralFeature> features, List<Integer> positions,
			List<Object> oldValues, List<Object> newValues) {
		CompoundCommand cmd = new CompoundCommand();
		for (int i = 0; i < features.size(); i++) {
			if (target instanceof StyleRule) {
				ModifyStyleRuleCommand modifyStyleRuleCommand = new ModifyStyleRuleCommand();
				modifyStyleRuleCommand.setStyleRule((StyleRule) target);
				modifyStyleRuleCommand.setFeature(features.get(i));
				modifyStyleRuleCommand.setIndex(positions.get(i));
				modifyStyleRuleCommand.setNewValue(newValues.get(i));
				cmd.add(modifyStyleRuleCommand);
			} else if (target.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER) {
				ModifyEventHandlerCommand modifyEventHandlerCommand = new ModifyEventHandlerCommand();
				((ModifyEventHandlerCommand) modifyEventHandlerCommand)
						.setEventHandler((EventHandler) target);
				((ModifyEventHandlerCommand) modifyEventHandlerCommand)
						.setFeature(features.get(i));
				((ModifyEventHandlerCommand) modifyEventHandlerCommand)
						.setIndex(positions.get(i));
				((ModifyEventHandlerCommand) modifyEventHandlerCommand)
						.setNewValue(newValues.get(i));
			} else if (target instanceof Event) {
				ModifyEventCommand modifyEventCommand = new ModifyEventCommand();
				((ModifyEventCommand) modifyEventCommand)
						.setEvent((Event) target);
				((ModifyEventCommand) modifyEventCommand).setFeature(features
						.get(i));
				((ModifyEventCommand) modifyEventCommand).setIndex(positions
						.get(i));
				((ModifyEventCommand) modifyEventCommand).setNewValue(newValues
						.get(i));
			} else if (target instanceof Parameter) {
				ModifyParameterCommand modifyParameterCommand = new ModifyParameterCommand();
				((ModifyParameterCommand) modifyParameterCommand)
						.setParameter((Parameter) target);
				((ModifyParameterCommand) modifyParameterCommand)
						.setFeature(features.get(i));
				((ModifyParameterCommand) modifyParameterCommand)
						.setIndex(positions.get(i));
				((ModifyParameterCommand) modifyParameterCommand)
						.setNewValue(newValues.get(i));
			}
		}
		if (!cmd.isEmpty() && cmd.canExecute())
			getCommandStack().execute(cmd);

	}

	@Override
	public void targetRemoved(EObject container, EObject target) {
		Command cmd = null;
		if (container instanceof StyledElement && target instanceof StyleRule) {
			cmd = new RemoveStyleRuleCommand();
			((RemoveStyleRuleCommand) cmd)
					.setStyledElement((StyledElement) container);
			((RemoveStyleRuleCommand) cmd).setStyleRule((StyleRule) target);
		} else if (container instanceof EventDispatcher
				&& target.eClass() == EDPHandlersPackage.Literals.EVENT_HANDLER) {
			cmd = new RemoveEventHandlerCommand();
			((RemoveEventHandlerCommand) cmd)
					.setEventDispatcher((EventDispatcher) container);
			((RemoveEventHandlerCommand) cmd)
					.setEventHandler((EventHandler) target);
		} else if (target instanceof Event) {
			cmd = new RemoveEventCommand();
			((RemoveEventCommand) cmd)
					.setEventHandler((EventHandler) container);
			((RemoveEventCommand) cmd).setEvent((Event) target);
		} else if (container instanceof EventDispatcher
				&& target.eClass() == EDPHandlersPackage.Literals.BINDING) {
			cmd = new RemoveBindingCommand();
			((RemoveBindingCommand) cmd)
					.setEventDispatcher((EventDispatcher) container);
			((RemoveBindingCommand) cmd).setBinding((Binding) target);
		} else if (container instanceof Parameterized
				&& target instanceof Parameter) {
			cmd = new RemoveParameterCommand();
			((RemoveParameterCommand) cmd)
					.setParameterized((Parameterized) container);
			((RemoveParameterCommand) cmd).setParameter((Parameter) target);
		}
		if (cmd != null && cmd.canExecute())
			getCommandStack().execute(cmd);
	}

	@Override
	public void undo() {
		if (getCommandStack().canUndo())
			getCommandStack().undo();
	}

	@Override
	public void redo() {
		if (getCommandStack().canRedo())
		getCommandStack().redo();
	}

}