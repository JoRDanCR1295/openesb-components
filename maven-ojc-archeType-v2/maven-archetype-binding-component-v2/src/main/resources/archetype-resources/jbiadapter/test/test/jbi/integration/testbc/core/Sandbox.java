#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.core;

public interface Sandbox {

	public byte[] execute(byte[] data,  Object handler);
}
