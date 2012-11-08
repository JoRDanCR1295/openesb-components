package test.jbi.integration.testse.core;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;


public interface Command extends Serializable{
	
	Serializable execute(ComponentContext context) throws Exception;
}
