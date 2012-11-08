 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

/**
 * TypeAdditionalInformation - additional information needed for deploy time
 * 
 * @author laurlg
 */

public class TypeAdditionalInformation {

	private Class parentClass = null; 
	private String fieldName = null;
	private boolean isException = false;
	private int idxParamOrExc = -1;
	private String methodName = null;
	
	
	
	public TypeAdditionalInformation(Class parentClass, String fieldName,
			boolean isException, int idxParamOrExc, String methodName) {
		super();
		this.parentClass = parentClass;
		this.fieldName = fieldName;
		this.isException = isException;
		this.idxParamOrExc = idxParamOrExc;
		this.methodName = methodName;
	}
	public Class getParentClass() {
		return parentClass;
	}
	public void setParentClass(Class parentClass) {
		this.parentClass = parentClass;
	}
	public String getFieldName() {
		return fieldName;
	}
	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}
	public boolean isException() {
		return isException;
	}
	public void setException(boolean isException) {
		this.isException = isException;
	}
	public int getIdxParamOrExc() {
		return idxParamOrExc;
	}
	public void setIdxParamOrExc(int idxParamOrExc) {
		this.idxParamOrExc = idxParamOrExc;
	}
	public String getMethodName() {
		return methodName;
	}
	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}
}
