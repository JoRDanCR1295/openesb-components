#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.core;

import java.io.Serializable;

import javax.jbi.component.ComponentContext;


public interface Command extends Serializable{
	
	Serializable execute(ComponentContext context) throws Exception;
}
