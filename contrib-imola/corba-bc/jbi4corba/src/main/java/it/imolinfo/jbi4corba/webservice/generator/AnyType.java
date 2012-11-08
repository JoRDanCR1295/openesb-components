 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

/**
 * 
 * class AnyType - define union type class
 * 
 * @author lacquaviva
 */
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class AnyType implements SearchedType {

	public static final String CORBA_ANY_TYPE = "org.omg.CORBA.Any";
	public static final String CORBA_ANY_IMPL_TYPE = "com.sun.corba.se.impl.corba.AnyImpl";
	public static final String SEPARATOR_STR = "#";
	public static final String EXCEPTION_STR = "exception";
	public static final String PARAMETER_STR = "parameter";
	public static final String RETURN_STR = "return";

	// if Type is Any type(true) or class containing anyType(false)
	private boolean isAnyType = false;

	// if method signature element is exception(true) or parameter(false)
	private boolean isException = false;

	// represent the mapping construction for any
	private String typeName = null;

	// the enclosing class of any
	private Class clsHavingAny = null;

	// represent the mapping construction for any
	private String fieldName = null;

	// index of method signature element(parameter or exception)
	private int idxParamOrExc = -1;

	// the name of the method containing the Any
	private String methodName = null;

	/**
	 * Constructor
	 */
	public AnyType(Class clsParent, String fldName, boolean isExc,
			int paramOrExceptionIdx, String metName) {
		clsHavingAny = clsParent;
		fieldName = fldName;
		if (clsHavingAny == null)
			isAnyType = true;

		isException = isExc;
		idxParamOrExc = paramOrExceptionIdx;
		methodName = metName;

		constructTypeName();
	}

	private void constructTypeName() {
		if (isAnyType) {
			typeName = methodName + SEPARATOR_STR;
			if (isException) {
				typeName += EXCEPTION_STR + idxParamOrExc;
			} else {
				if (idxParamOrExc > -1)
					typeName += PARAMETER_STR + idxParamOrExc;
				else
					typeName += RETURN_STR;

			}

		} else {
			typeName = clsHavingAny.getName() + SEPARATOR_STR + fieldName;
		}

	}

	public AnyType() {

	}

	public String getTypeName() {

		return typeName;
	}

	public void setTypeName(String typeName) {
		this.typeName = typeName;

	}
	
	public static boolean isAnyType(String type)
	{
		if (CORBA_ANY_TYPE.equals(type) || CORBA_ANY_IMPL_TYPE.equals(type))
			return true;
		return false;
	}

}