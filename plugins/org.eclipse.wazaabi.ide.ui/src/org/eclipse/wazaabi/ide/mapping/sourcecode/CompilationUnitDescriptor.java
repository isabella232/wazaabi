package org.eclipse.wazaabi.ide.mapping.sourcecode;

public class CompilationUnitDescriptor {

	private final String name;
	private final String contents;

	public CompilationUnitDescriptor(String name, String contents) {
		this.name = name;
		this.contents = contents;
	}

	public String getName() {
		return name;
	}

	public String getContents() {
		return contents;
	}

}
