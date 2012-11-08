 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.bytecodemanipulation;

public class FakeValueBase implements org.omg.CORBA.portable.ValueBase {

	public String[] _truncatable_ids() {
		return new String [] { FakeValueBaseHelper.id() };
	}


}
