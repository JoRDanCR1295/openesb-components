package test.jbi.integration.test.framework;

import java.io.IOException;

public interface SUAssembler {

	String getName();
	String getDescription();
	String assemble(String workingDir, String base) throws IOException;
	String getComponentName();

}
