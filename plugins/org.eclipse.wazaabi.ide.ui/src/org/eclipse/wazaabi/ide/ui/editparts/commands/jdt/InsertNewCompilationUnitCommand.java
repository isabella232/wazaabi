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

package org.eclipse.wazaabi.ide.ui.editparts.commands.jdt;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.gef.commands.Command;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.IPackagesViewPart;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jdt.ui.actions.FormatAllAction;
import org.eclipse.jdt.ui.actions.OrganizeImportsAction;
import org.eclipse.ui.PlatformUI;
import org.eclipse.wazaabi.ide.mapping.sourcecode.CompilationUnitDescriptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class InsertNewCompilationUnitCommand extends Command {
	final static Logger logger = LoggerFactory
			.getLogger(InsertNewCompilationUnitCommand.class);

	private IPackageFragment packageFragment;

	private CompilationUnitDescriptor compilationUnitDescriptor;

	private int index = -1;

	private ICompilationUnit compilationUnit = null;

	public InsertNewCompilationUnitCommand() {
		super("InsertNewCompilationUnitCommand"); // TODO : localize that
	}

	public CompilationUnitDescriptor getCompilationUnitDescriptor() {
		return compilationUnitDescriptor;
	}

	public int getIndex() {
		return index;
	}

	public IPackageFragment getPackageFRagment() {
		return packageFragment;
	}

	public void setCompilationUnitDescriptor(
			CompilationUnitDescriptor compilationUnitDescriptor) {
		this.compilationUnitDescriptor = compilationUnitDescriptor;
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public void setPackageFRagment(IPackageFragment packageFRagment) {
		this.packageFragment = packageFRagment;
	}

	@Override
	public boolean canExecute() {
		return compilationUnitDescriptor != null
				&& compilationUnitDescriptor.getName() != null
				&& compilationUnitDescriptor.getName().length() != 0
				&& compilationUnitDescriptor.getContents() != null
				&& compilationUnitDescriptor.getContents().length() != 0
				&& compilationUnit == null
				&& packageFragment != null
				&& packageFragment.exists()
				&& !packageFragment.getCompilationUnit(
						compilationUnitDescriptor.getName()).exists();
	}

	@Override
	public boolean canUndo() {
		return compilationUnit != null;
	}

	@Override
	public void execute() {
		redo();
	}

	@Override
	public void redo() {
		try {
			compilationUnit = getPackageFRagment().createCompilationUnit(
					getCompilationUnitDescriptor().getName(),
					getCompilationUnitDescriptor().getContents(), true,
					new NullProgressMonitor());
			makeSourceBeautiful(compilationUnit);
		} catch (JavaModelException e) {
			logger.error(
					"Unable to create CompilationUnit {} {} {}",
					new Object[] { compilationUnitDescriptor.getName(),
							e.getException(), e.getCause() });
		}
	}

	@Override
	public void undo() {
		try {
			if (compilationUnit.exists())
				compilationUnit.delete(true, new NullProgressMonitor());
			compilationUnit = null;
		} catch (JavaModelException e) {
			logger.error(
					"Unable to destroy CompilationUnit {} {} {}",
					new Object[] { compilationUnitDescriptor.getName(),
							e.getException(), e.getCause() });
		}
	}

	protected void makeSourceBeautiful(ICompilationUnit compilationUnit) {
		if (compilationUnit != null && compilationUnit.exists()) {
			IPackagesViewPart pep = (IPackagesViewPart) PlatformUI
					.getWorkbench().getActiveWorkbenchWindow().getActivePage()
					.findView(JavaUI.ID_PACKAGES);
			if (pep != null) {
				OrganizeImportsAction organizeImportsAction = new OrganizeImportsAction(
						pep.getSite());

				organizeImportsAction.run(compilationUnit);

				FormatAllAction formatAllAction = new FormatAllAction(
						pep.getSite());
				formatAllAction
						.runOnMultiple(new ICompilationUnit[] { compilationUnit });
			}
		}
	}
}
