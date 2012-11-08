 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;

public class FakeJavaBean {

	private String foo;
	private boolean bar;

	public FakeJavaBean() {
		// NOP
	}

	// this method must not be removed from the class
	public void get() {
	}

	// this method must not be removed from the class
	public String getString(String x) {
		return null;
	}

	// this method must not be removed from the class
	public void set() {
	}



	public boolean isBar() {
		return bar;
	}

	public void setBar(boolean bar) {
		this.bar = bar;
	}

	public String getFoo() {
		return foo;
	}

	public void setFoo(String foo) {
		this.foo = foo;
	}

	
}
