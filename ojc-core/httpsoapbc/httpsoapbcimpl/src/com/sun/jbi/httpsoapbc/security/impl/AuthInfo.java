/**
 * 
 */
package com.sun.jbi.httpsoapbc.security.impl;

/**
 * @author Sujit Biswas
 * 
 */
public class AuthInfo {

	private String resource ="";
	private String action="POST";

	public String getResource() {
		return resource;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

}
