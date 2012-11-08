package com.sun.jbi.hl7bc.extensions;

/**
 * Client/Server connection mode
 * 
 * @author James Agnew
 */
public enum TcpRoleEnum {

	/** BC is playing default role */
	DEFAULT, 

	/** BC is playing client role */
	CLIENT, 
	
	/** BC is playing server role */
	SERVER
	
}
