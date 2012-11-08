package test.jbi.integration.testbc.core;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;

import javax.jbi.component.ComponentContext;

public class GetClassCommand implements Command {

	private static final ClassLoader CLASSLOADER = GetClassCommand.class
			.getClassLoader();
	private String resourceName;
	
	public Serializable execute(ComponentContext context) throws Exception{
		InputStream in = null;
		Serializable result = null;
		try{
			in = CLASSLOADER.getResourceAsStream(resourceName);
			result = Util.readBytes(in);
		} catch (IOException e) {
			result = e;
		}finally{
			Util.closeStream(in);
		}
		
		return result;
	}
	
	public String getResourceName() {
		return resourceName;
	}

	public void setResourceName(String resourceName) {
		this.resourceName = resourceName;
	}

	
}
