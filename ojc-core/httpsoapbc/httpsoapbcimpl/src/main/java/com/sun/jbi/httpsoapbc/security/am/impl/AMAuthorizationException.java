/**
 * 
 */
package com.sun.jbi.httpsoapbc.security.am.impl;

import com.sun.jbi.httpsoapbc.security.api.CredentialValidationException;

/**
 * @author Sujit Biswas
 *
 */
public class AMAuthorizationException extends CredentialValidationException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public AMAuthorizationException() {
		super();
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param message
	 * @param cause
	 */
	public AMAuthorizationException(String message, Throwable cause) {
		super(message, cause);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param message
	 */
	public AMAuthorizationException(String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param cause
	 */
	public AMAuthorizationException(Throwable cause) {
		super(cause);
		// TODO Auto-generated constructor stub
	}
	
	

}
